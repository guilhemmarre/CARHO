import geopandas as gpd
import pandas as pd
import rasterio
from rasterio.mask import mask
import numpy as np
from rasterio.transform import rowcol
from shapely.geometry import Point
from scipy.spatial import cKDTree
import glob
from tqdm import tqdm

# CONVERTIR LE CARROYAGE EN EMPRISE
parc = gpd.read_file("data/SIG/carroyage_PNMCCA.shp")
parc = parc.dissolve()
parc.to_file("data/SIG/emprise_PNMCCA.gpkg")

# CHARGER LES DONNEES
bathy_path = "../CAREMaerl_2024/ANALYSES_CARTO/data/bathy/bathymetrie_filtre.tif"
parc_path = "data/SIG/emprise_PNMCCA.gpkg"

# === FONCTION DE CONVERSION DE LA BATHY EN POINTS DANS UNE EMPRISE DONNEE ===
def raster_clip_to_points(
    raster_path,
    shp_path,
    feature_index=0,
    band=1,
    all_touched=False,
    value_name="value",
    drop_nodata=True,
):
    """Retourne un GeoDataFrame de points = centroïdes des pixels du raster clippé.
 
    feature_index : index de l'entité du shapefile à utiliser comme masque
    all_touched   : True = garde tout pixel touché par le polygone,
                    False = seulement ceux dont le centre est dans le polygone
    """
    gdf_poly = gpd.read_file(shp_path)
    if gdf_poly.empty:
        raise ValueError("Shapefile vide")
 
    with rasterio.open(raster_path) as src:
        # Reprojection du polygone dans le CRS du raster (indispensable)
        if gdf_poly.crs is not None and src.crs is not None and gdf_poly.crs != src.crs:
            gdf_poly = gdf_poly.to_crs(src.crs)
 
        geom = [gdf_poly.geometry.iloc[feature_index]]
 
        out_image, out_transform = mask(
            src, geom, crop=True, filled=True, all_touched=all_touched
        )
        nodata = src.nodata
        crs = src.crs
 
    arr = out_image[band - 1]  # bande choisie -> 2D
 
    # Grille des indices ligne/colonne
    rows, cols = np.indices(arr.shape)
 
    # Masque des pixels valides : jamais de NaN, et pas de nodata si demandé
    valid = np.ones(arr.shape, dtype=bool)
    if np.issubdtype(arr.dtype, np.floating):
        valid &= np.isfinite(arr)          # écarte NaN et ±inf
    if drop_nodata and nodata is not None and not np.isnan(nodata):
        valid &= arr != nodata
 
    rows, cols, vals = rows[valid], cols[valid], arr[valid]
 
    # Centres de pixels : Affine * (col + 0.5, row + 0.5)
    xs, ys = out_transform * (cols + 0.5, rows + 0.5)
 
    return gpd.GeoDataFrame(
        {value_name: vals},
        geometry = gpd.points_from_xy(xs, ys),
        crs = crs,
    )

# === FONCTION D'EXTRACTION DES VALEURS D'UN RASTER ===
def extract_raster(gdf_object, raster_path, output_path = None, fill_NA = False):
    """
    Extrait les valeurs d'un raster pour chaque point d'un GeoDataFrame.
    Si une valeur est NaN, prend la valeur du pixel valide le plus proche.
    """

    # Ouvrir le raster
    with rasterio.open(raster_path) as src:
        transform = src.transform
        crs = src.crs
        noms_bandes = [desc if desc else f"band_{i+1}" for i, desc in enumerate(src.descriptions)]

        # Ajouter préfixe si plusieurs bandes
        if src.count > 1:
            prefixe = raster_path.split("/")[-1].replace(".tif", "") + "_"
            noms_bandes = [prefixe + nom for nom in noms_bandes]

        # Reprojeter le GeoDataFrame si nécessaire
        gdf_reproj = gdf_object.to_crs(crs)

        # Extraire toutes les coordonnées des points en une seule fois
        coords_points = np.column_stack([gdf_reproj.geometry.x, gdf_reproj.geometry.y])

        # Déterminer les coordonnées des pixels valides sur la première bande (évite répétition)
        raster0 = src.read(1)
        rows, cols = np.where(np.isfinite(raster0))
        xs, ys = rasterio.transform.xy(transform, rows, cols)
        coords_valides = np.column_stack([xs, ys])

        if fill_NA:
            # Construire un seul KDTree
            tree = cKDTree(coords_valides)

        # Stocker les résultats pour toutes les bandes
        data = pd.DataFrame(index=gdf_object.index)

        # Boucle sur les bandes
        for b, nom in enumerate(noms_bandes):
            print(f"🔹 Extraction pour la bande : {nom}")

            # Lecture unique de la bande
            raster_band = src.read(b + 1)
            valeurs = []

            # Boucle optimisée avec tqdm
            for x, y in tqdm(coords_points, total=len(coords_points), desc=f"Bande {b+1}/{len(noms_bandes)}"):
                # Localiser le pixel
                try:
                    row, col = rowcol(transform, x, y)
                    val = raster_band[row, col]
                except IndexError:
                    val = np.nan

                # Si NaN → chercher le plus proche pixel valide
                if np.isnan(val) and fill_NA:
                    _, idx = tree.query([x, y], k=1)
                    x_nn, y_nn = coords_valides[idx]
                    row_nn, col_nn = rowcol(transform, x_nn, y_nn)
                    val = raster_band[row_nn, col_nn]

                valeurs.append(val)

            # Stocker la colonne
            data[nom] = valeurs

    # Fusionner les valeurs avec le GeoDataFrame initial
    gdf_result = gdf_object.join(data)

    # Sauvegarde si demandé
    if output_path:
        gdf_result.to_file(output_path)
        print(f"✅ Extraction terminée. Résultat enregistré dans : {output_path}")

    return gdf_result


# === CLIP ET CONVERSION DE LA BATHY EN POINTS ===
gdf = raster_clip_to_points(raster_path = bathy_path,
                              shp_path = parc_path,
                              value_name = "profondeur")


# === EXTRACTION DES DONNEES DES RASTERS ===

# Liste des rasters env
raster_paths = glob.glob("../CAREMaerl_2024/ANALYSES_CARTO/data/env/*.tif")

for rast_path in raster_paths:

    print(rast_path)
    gdf = extract_raster(gdf, rast_path, fill_NA = True)

# Raster SAR
gdf = extract_raster(gdf, "../CAREMaerl_2024/ANALYSES_CARTO/data/SAR/Marsiglia_2024.tif", fill_NA = True)

# Raster Impact
gdf = extract_raster(gdf, "../CAREMaerl_2024/ANALYSES_CARTO/data/impact/pressions_2021.tif", fill_NA = False)

gdf = gdf.rename(columns = {"pressions_2021_agriculture": "agriculture",
                            "pressions_2021_amenagements": "amenagements",
                            "pressions_2021_aquaculture": "aquaculture",
                            "pressions_2021_apports_cours_deau": "apports_cours_deau",
                            "pressions_2021_erosion": "erosion",
                            "pressions_2021_rejets_industriels": "rejets_industriels",
                            "pressions_2021_mouillages_grands": "mouillages_grands",
                            "pressions_2021_mouillages_petits": "mouillages_petits",
                            "pressions_2021_peche": "peche",
                            "pressions_2021_urbanisation": "urbanisation",
                            "pressions_2021_rejets_urbains": "rejets_urbains",
                            "pressions_2021_tourisme_balneaire": "tourisme_balneaire",
                            "pressions_2021_trafic_maritime": "trafic_maritime"})

# Raster Impact dégradé pour compléter les valeurs manquantes
gdf = extract_raster(gdf, "../CAREMaerl_2024/ANALYSES_CARTO/data/impact/pressions_2021_100m.tif", fill_NA = True)

gdf = gdf.rename(columns = {"pressions_2021_100m_band_1": "agriculture_LD",
                            "pressions_2021_100m_band_2": "amenagements_LD",
                            "pressions_2021_100m_band_3": "aquaculture_LD",
                            "pressions_2021_100m_band_4": "apports_cours_deau_LD",
                            "pressions_2021_100m_band_5": "erosion_LD",
                            "pressions_2021_100m_band_6": "rejets_industriels_LD",
                            "pressions_2021_100m_band_7": "mouillages_grands_LD",
                            "pressions_2021_100m_band_8": "mouillages_petits_LD",
                            "pressions_2021_100m_band_9": "peche_LD",
                            "pressions_2021_100m_band_10": "urbanisation_LD",
                            "pressions_2021_100m_band_11": "rejets_urbains_LD",
                            "pressions_2021_100m_band_12": "tourisme_balneaire_LD",
                            "pressions_2021_100m_band_13": "trafic_maritime_LD"})

# On remplace les valeurs de pressions manquantes
gdf["agriculture"] = gdf["agriculture"].fillna(gdf["agriculture_LD"])
gdf["amenagements"] = gdf["amenagements"].fillna(gdf["amenagements_LD"])
gdf["aquaculture"] = gdf["aquaculture"].fillna(gdf["aquaculture_LD"])
gdf["apports_cours_deau"] = gdf["apports_cours_deau"].fillna(gdf["apports_cours_deau_LD"])
gdf["erosion"] = gdf["erosion"].fillna(gdf["erosion_LD"])
gdf["rejets_industriels"] = gdf["rejets_industriels"].fillna(gdf["rejets_industriels_LD"])
gdf["mouillages_grands"] = gdf["mouillages_grands"].fillna(gdf["mouillages_grands_LD"])
gdf["mouillages_petits"] = gdf["mouillages_petits"].fillna(gdf["mouillages_petits_LD"])
gdf["peche"] = gdf["peche"].fillna(gdf["peche_LD"])
gdf["urbanisation"] = gdf["urbanisation"].fillna(gdf["urbanisation_LD"])
gdf["rejets_urbains"] = gdf["rejets_urbains"].fillna(gdf["rejets_urbains_LD"])
gdf["tourisme_balneaire"] = gdf["tourisme_balneaire"].fillna(gdf["tourisme_balneaire_LD"])
gdf["trafic_maritime"] = gdf["trafic_maritime"].fillna(gdf["trafic_maritime_LD"])

# On supprime les colonnes "_LD"
gdf = gdf.drop(columns=['agriculture_LD', 
                        'amenagements_LD',
                        'aquaculture_LD',
                        'apports_cours_deau_LD',
                        'erosion_LD',
                        'rejets_industriels_LD',
                        'mouillages_grands_LD',
                        'mouillages_petits_LD',
                        'peche_LD',
                        'urbanisation_LD',
                        'rejets_urbains_LD',
                        'tourisme_balneaire_LD',
                        'trafic_maritime_LD'])

# === EXPORT DES DONNEES EN CSV ET GPKG ===
gdf.to_file("outputs/07_grille_pred_PNMCCA.gpkg")
gdf.to_csv("outputs/07_grille_pred_PNMCCA.csv", index = False, encoding = 'utf-8', decimal = ".", sep = ";")

# Raster biocenoses
gdf = extract_raster(gdf, "data/SIG/bioc_dec23_10_PNMCCA.tif", fill_NA = True)
gdf = gdf.rename(columns = {"band_1": "surfstat"})

# On complète les codes manquants pour les zones profondes
gdf.loc[(gdf["surfstat"] == 0) & (gdf["profondeur"] < -60), "surfstat"] = 8
gdf.loc[gdf["surfstat"] == 0, "surfstat"] = 9

# === EXPORT DES DONNEES EN CSV ET GPKG ===
gdf.to_file("outputs/07_grille_pred_PNMCCA.gpkg")
#gdf.to_csv("outputs/07_grille_pred_PNMCCA.csv", index = False, encoding = 'utf-8', decimal = ".", sep = ";")



# === TRANSFORMATION EN RASTER POUR LE PROJET AREA ===
from rasterio.transform import from_origin
x, y = gdf.geometry.x.values, gdf.geometry.y.values
xs, ys = np.unique(x), np.unique(y)
res_x, res_y = np.diff(xs).min(), np.diff(ys).min()

cols = np.round((x - xs.min()) / res_x).astype(int)
rows = np.round((ys.max() - y) / res_y).astype(int)

champs = [col for col in gdf.columns[:-1]]

# cube (bandes, lignes, colonnes)
stack = np.full((len(champs), ys.size, xs.size), np.nan, dtype="float32")
for i, champ in enumerate(champs):
    stack[i, rows, cols] = gdf[champ].to_numpy(dtype="float32")

transform = from_origin(xs.min() - res_x / 2, ys.max() + res_y / 2, res_x, res_y)

with rasterio.open(
    "outputs/07_grille_pred_PNMCCA.tif", "w",
    driver="GTiff",
    height=ys.size, width=xs.size, count=len(champs),
    dtype="float32", crs=gdf.crs, transform=transform,
    nodata=np.nan, compress="deflate", tiled=True,
) as dst:
    dst.write(stack)                      # écrit les N bandes d'un coup
    for i, champ in enumerate(champs, start=1):
        dst.set_band_description(i, champ)   # nom lisible dans QGIS



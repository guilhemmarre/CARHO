import geopandas as gpd
import pandas as pd
import rasterio
import numpy as np
from rasterio.transform import rowcol
from shapely.geometry import Point
from scipy.spatial import cKDTree
import glob
from tqdm import tqdm


# === CHARGER LES STATIONS ===
gdf_carho = gpd.read_file("data/SIG/bio_plongees_PNMCCA_CARHO.gpkg")
gdf_caremaerl = gpd.read_file("../CAREMaerl_2024/ANALYSES_CARTO/data/bio/bio_plongees_all_EPSG4326.gpkg")

# Reprojection même CRS
gdf_carho = gdf_carho.to_crs(gdf_caremaerl.crs)

# Jointure des deux tableaux
cols = gdf_carho.columns.intersection(gdf_caremaerl.columns)
gdf = pd.concat([gdf_carho[cols], gdf_caremaerl[cols]])

# Suppression des points sans le cd_hab
gdf = gdf.dropna(subset = ["cd_hab"]).reset_index(drop = True)

# On supprime les points avec coordonnées nulles (quelques cas...)
coords_points = np.column_stack([gdf.geometry.x, gdf.geometry.y])
null_points = np.where(np.isnan(coords_points))
null_points = np.unique(null_points[0]).tolist()

if len(null_points) > 0:
    gdf = gdf.drop(null_points)
    gdf = gdf.set_geometry("geometry")


# Echantillonnage de points pour tester le script
#random_points = np.ndarray.flatten(np.random.randint(0,gdf.shape[0],size=1000))
#gdf = gdf.iloc[random_points]
#gdf = gdf.set_geometry("geometry")
#gdf.to_file("test.gpkg")
#shapefile_path = "test.gpkg"
#gdf = gpd.read_file(shapefile_path)

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

# Extraction de la bathy
gdf = extract_raster(gdf, "../CAREMaerl_2024/ANALYSES_CARTO/data/bathy/bathymetrie_filtre.tif", fill_NA = False)
gdf = gdf.rename(columns = {"band_1": "profondeur"})

# Raster biocenoses
gdf = extract_raster(gdf, "../CAREMaerl_2024/ANALYSES_CARTO/data/biocenoses/bioc_dec23_50.tif", fill_NA = True)
gdf = gdf.rename(columns = {"band_1": "surfstat"})

# === EXPORT DES DONNEES EN CSV ET GPKG ===
gdf.to_file("outputs/04_bio_plongees_pressions_env.gpkg")
gdf.to_csv("outputs/04_bio_plongees_pressions_env.csv", index = False, encoding = 'utf-8', decimal = ".", sep = ";")




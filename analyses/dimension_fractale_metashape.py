"""
Dimension fractale d'un modele 3D par box-counting.

Principe : on couvre le nuage de points (sommets du maillage) par une grille
de voxels de cote epsilon, on compte le nombre N(epsilon) de voxels occupes,
puis on ajuste log(N) en fonction de log(1/epsilon). La pente = dimension D.

Pour une surface : 2 <= D <= 3 (2 = surface lisse, 3 = surface
maximalement rugueuse remplissant l'espace).

ATTENTION : D n'a de sens que sur la zone de scaling ou le graphe log-log
est lineaire. En dehors :
  - epsilon plus petit que l'espacement des points -> chaque point dans son
    voxel, la pente s'effondre (artefact d'echantillonnage).
  - epsilon proche de l'etendue totale -> 1 ou 2 voxels, plus de scaling.
Il faut donc TOUJOURS inspecter le graphe et n'ajuster que la partie droite.

API Metashape : le nom du nuage dense a change selon les versions
(chunk.dense_cloud en 1.x, chunk.point_cloud en 2.x). Le maillage
(chunk.model) est plus stable. Je n'ai pas pu verifier l'API exacte de
votre version installee : verifiez chunk.model et chunk.transform.matrix
dans la console Python de Metashape avant lancement.
"""

import numpy as np


# --------------------------------------------------------------------------
# 1. Extraction de la geometrie depuis Metashape
# --------------------------------------------------------------------------
def extraire_sommets_modele(chunk):
    """Renvoie les coordonnees (N, 3) des sommets du maillage en repere monde.

    chunk.model.vertices[i].coord est exprime dans le repere interne ;
    chunk.transform.matrix passe au repere monde (echelle reelle).
    L'echelle est importante ici pour choisir des epsilon en metres.
    """
    import Metashape  # importe seulement quand on tourne dans Metashape

    model = chunk.model
    if model is None:
        raise RuntimeError("Aucun maillage (chunk.model) dans ce chunk.")

    T = chunk.transform.matrix  # repere interne -> monde

    coords = np.empty((len(model.vertices), 3), dtype=np.float64)
    for i, v in enumerate(model.vertices):
        p = T.mulp(v.coord)  # mulp = transformation d'un point (avec translation)
        coords[i, 0] = p.x
        coords[i, 1] = p.y
        coords[i, 2] = p.z
    return coords


# Variante decouplee de Metashape : exporter le modele en .ply depuis
# Metashape (Export Model), puis lire ici. Plus rapide sur gros maillages
# et insensible aux changements d'API. Necessite open3d ou trimesh.
def charger_ply(chemin):
    import open3d as o3d
    pcd = o3d.io.read_point_cloud(chemin)  # ou read_triangle_mesh(...).vertices
    return np.asarray(pcd.points)


def extraire_nuage_dense(chunk, chemin_tmp="/tmp/dense_export.ply"):
    """Renvoie les coordonnees (N, 3) du nuage dense en repere monde.

    Methode robuste : on EXPORTE le nuage dense (deja georeference par
    Metashape) puis on le relit avec open3d. C'est volontaire :
      - l'iteration des points denses en Python est lente (millions de pts) ;
      - le nom de l'objet a change selon les versions (chunk.dense_cloud en
        1.x, chunk.point_cloud en 2.x), de meme que la signature d'export.
    L'export contourne ces deux problemes.

    INCERTITUDE : je n'ai pas verifie la signature exacte de exportPointCloud
    pour votre version. Selon la version le parametre s'appelle
    source_data=Metashape.PointCloudData (2.x) ou source=... ; verifiez via
    help(chunk.exportPointCloud) dans la console. En cas de doute, exportez
    le nuage manuellement depuis l'interface puis utilisez charger_ply().
    """
    import Metashape

    # Tentative generique ; adaptez le mot-cle si votre version differe.
    try:
        chunk.exportPointCloud(
            path=chemin_tmp,
            source_data=Metashape.PointCloudData,  # 2.x
            crs=chunk.crs,
        )
    except (TypeError, AttributeError):
        # Repli pour des signatures plus anciennes (1.x)
        chunk.exportPoints(path=chemin_tmp, source_data=Metashape.DenseCloudData)

    return charger_ply(chemin_tmp)


# --------------------------------------------------------------------------
# 2. Box-counting
# --------------------------------------------------------------------------
def box_counting(points, n_echelles=15, eps_min=None, eps_max=None):
    """Retourne (epsilons, N) : taille de voxel et nb de voxels occupes.

    points   : tableau (N, 3)
    eps_min  : plus petite cote de voxel (par defaut L/1000)
    eps_max  : plus grande cote (par defaut L/2)
    """
    pts = points - points.min(axis=0)
    L = pts.max()  # plus grande etendue sur un axe

    if eps_max is None:
        eps_max = L / 2.0
    if eps_min is None:
        eps_min = L / 1000.0

    epsilons = np.logspace(np.log10(eps_min), np.log10(eps_max), n_echelles)

    N = np.empty(n_echelles, dtype=np.int64)
    for k, eps in enumerate(epsilons):
        idx = np.floor(pts / eps).astype(np.int64)
        # nb de voxels distincts occupes
        N[k] = np.unique(idx, axis=0).shape[0]
    return epsilons, N


# --------------------------------------------------------------------------
# 3. Ajustement de la pente sur la zone lineaire
# --------------------------------------------------------------------------
def dimension_fractale(epsilons, N, k_debut=None, k_fin=None):
    """Ajuste log(N) = D * log(1/eps) + c sur l'intervalle [k_debut, k_fin].

    Laisser k_debut/k_fin a None ajuste sur tout l'intervalle : a NE PAS
    faire en aveugle, choisir l'intervalle apres inspection du graphe.
    Retourne D et le R^2 de la regression.
    """
    x = np.log(1.0 / epsilons)
    y = np.log(N)

    sl = slice(k_debut, k_fin)
    coeffs = np.polyfit(x[sl], y[sl], 1)
    D = coeffs[0]

    # R^2
    y_fit = np.polyval(coeffs, x[sl])
    ss_res = np.sum((y[sl] - y_fit) ** 2)
    ss_tot = np.sum((y[sl] - y[sl].mean()) ** 2)
    r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else float("nan")
    return D, r2


def detecter_zone_lineaire(epsilons, N, largeur_min=5):
    """Detecte automatiquement la meilleure zone de scaling lineaire.

    On teste tous les sous-intervalles [i, j] d'au moins largeur_min points,
    on ajuste une droite dans chacun et on retient celui qui maximise le R2.
    largeur_min evite de selectionner un segment trop court qui serait
    trivialement "parfait".

    Retourne (D, r2, i, j) : pente, qualite d'ajustement, bornes retenues.
    Ce n'est qu'une aide : verifiez toujours le resultat sur le graphe.
    L'hypothese est qu'il existe UNE seule zone lineaire dominante ; en
    presence de deux regimes (bi-fractal), inspectez manuellement.
    """
    x = np.log(1.0 / epsilons)
    y = np.log(N)
    n = len(x)

    meilleur = (-np.inf, None)  # (r2, (i, j, D))
    for i in range(0, n - largeur_min + 1):
        for j in range(i + largeur_min, n + 1):
            sl = slice(i, j)
            coeffs = np.polyfit(x[sl], y[sl], 1)
            y_fit = np.polyval(coeffs, x[sl])
            ss_res = np.sum((y[sl] - y_fit) ** 2)
            ss_tot = np.sum((y[sl] - y[sl].mean()) ** 2)
            r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else -np.inf
            if r2 > meilleur[0]:
                meilleur = (r2, (i, j, coeffs[0]))

    r2, (i, j, D) = meilleur
    return D, r2, i, j


def tracer(epsilons, N, chemin="boxcount_loglog.png", zone=None):
    """Trace le graphe log-log pour reperer visuellement la zone lineaire."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    x = np.log(1.0 / epsilons)
    y = np.log(N)
    fig, ax = plt.subplots(figsize=(6, 5))
    ax.plot(x, y, "o-", color="0.5")
    if zone is not None:
        i, j = zone
        ax.plot(x[i:j], y[i:j], "o", color="C3", label="zone retenue")
        ax.legend()
    for k, (xi, yi) in enumerate(zip(x, y)):
        ax.annotate(str(k), (xi, yi), fontsize=7)
    ax.set_xlabel("log(1 / epsilon)")
    ax.set_ylabel("log(N)")
    ax.set_title("Box-counting (la pente de la zone lineaire = D)")
    fig.tight_layout()
    fig.savefig(chemin, dpi=120)
    print("Graphe enregistre :", chemin)


# --------------------------------------------------------------------------
# Exemple d'utilisation
# --------------------------------------------------------------------------
if __name__ == "__main__":
    import Metashape

    # Dataframe for storing results
    data = []

    doc = Metashape.app.document

    for chunk in doc.chunks:

        if chunk.enabled:
            # Choix de la source : sommets du maillage OU nuage dense
            pts = extraire_sommets_modele(chunk)
            # pts = extraire_nuage_dense(chunk)   # variante nuage dense
            print(f"{len(pts)} points extraits")

            eps, N = box_counting(pts, n_echelles=15)

            print("\nidx   epsilon (m)        N(epsilon)")
            for i, (e, n) in enumerate(zip(eps, N)):
                print(f"{i:>3}   {e:>12.5f}   {n:>12d}")

            # Detection automatique de la zone de scaling lineaire
            D, r2, i, j = detecter_zone_lineaire(eps, N, largeur_min=5)
            print(f"\nZone retenue : indices {i} a {j - 1} "
                  f"(epsilon {eps[i]:.4f} a {eps[j - 1]:.4f} m)")
            print(f"D = {D:.3f}   R2 = {r2:.4f}")
            if not (2.0 <= D <= 3.0):
                print("ALERTE : D hors de [2, 3] -> verifiez densite de points "
                      "et bornes eps_min/eps_max, et inspectez le graphe.")

            data.append((chunk.label, D, r2))

            # Trace avec la zone retenue surlignee, pour controle visuel
            #tracer(eps, N, zone=(i, j))

# Transform in dataframe
df = pd.DataFrame(data)

# Export results
df.to_csv("Exports/fractals.csv")

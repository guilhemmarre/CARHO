library(ranger)

# Chargement du modèle
rf_final <- readRDS("outputs/06_RF_final_projection.rds")

# Chargement et préparation de la grille
grille <- sf::st_read("outputs/07_grille_pred_PNMCCA.gpkg")

# Transformation du champ surfstat en facteurs
codes_surfstat_2023 <- c("Association a rhodolithes",
                         "Association de la matte morte de Posidonia oceanica",
                         "Biocenose Coralligene",
                         "Biocenose de l herbier a Posidonia oceanica",
                         "Biocenose de la roche du large",
                         "Biocenose des algues infralittorales",
                         "Biocenose des galets infralittoraux",
                         "Fonds meubles circalittoraux",
                         "Fonds meubles infralittoraux",
                         "Habitats artificiels",
                         "Herbiers a Cymodocees",
                         "Zone bathyale",
                         "Herbier mixte a Zostera noltii",
                         "Herbiers a Zostera noltei")

names(codes_surfstat_2023) <- c(1:14)
grille$surfstat_str <- codes_surfstat_2023[grille$surfstat]
grille$surfstat <- grille$surfstat_str
grille$surfstat_str <- NULL

# Transformation en facteur
grille$surfstat <- as.factor(grille$surfstat)

# Conservation que des habitats potentiels (sinon on produit de la rhodolithe n'importe où jusqu'à la côte)
grille <- grille[grille$surfstat %in% c("Association a rhodolithes",
                                        "Association de la matte morte de Posidonia oceanica",
                                        "Biocenose de l herbier a Posidonia oceanica",
                                        "Biocenose des algues infralittorales",
                                        "Biocenose Coralligene",
                                        "Fonds meubles circalittoraux",
                                        "Fonds meubles infralittoraux"),]

# Pour info le détail des occurrences de rhodolithe par habitat dans le jeu de données complet
# Fonds meubles circalittoraux : 720 271
# Fonds meubles infralittoraux : 19 197
# Biocenose Coralligene : 7799
# Association de la matte morte de Posidonia oceanica : 3172
# Biocenose de l herbier a Posidonia oceanica : 2407
# Biocenose des algues infralittorales : 1166

# On supprime les points dans un range bathy inexploré et improbable pour la présence de rhodolithes
grille <- grille[grille$profondeur < -30,]

# grille <- grille[sample(1:nrow(grille), 100000, replace = FALSE),]
grille_df <- as.data.frame(grille)


# Prédiction
pr <- predict(rf_final, data = grille_df)$predictions

classes <- colnames(pr)
pred_label <- factor(
  colnames(pr)[max.col(as.matrix(pr))],
  levels = classes
)

# On ajoute la colonne prédite au gpkg
grille$cd_hab <- pred_label
grille <- grille[, c("cd_hab","geom")]

# Export des résultats
sf::st_write(grille, "outputs/08_cartographie_predictive_tous_habitats.gpkg", append = FALSE)

# Conversion en raster
df <- cbind(sf::st_coordinates(grille), sf::st_drop_geometry(grille)[, "cd_hab"])
r <- terra::rast(df, type = "xyz", crs = sf::st_crs(grille)$wkt)
names(r) <- "cd_hab"

terra::writeRaster(x = r, 
                   filename = "outputs/08_cartographie_predictive_tous_habitats.tif", 
                   overwrite = TRUE,
                   datatype  = "FLT4S",
                   gdal = c("COMPRESS=DEFLATE", "TILED=YES")
)

### CALCUL SURFACES PAR HABITAT DANS LE PARC
areas <- table(terra::values(r)) * terra::res(r)^2 / 10000
stats <- data.frame("cd_hab" = classes[as.numeric(names(areas))],
                    "area" = as.vector(areas))
stats$Carbone_T_an_bas <- stats$area * 10000 * 0.210 / 1000 # surface en m² x prod basse en kg / m² puis / 1000 pour avoir en tonnes
stats$Carbone_T_an_haute <- stats$area * 10000 * 0.289 / 1000 # surface en m² x prod basse en kg / m² puis / 1000 pour avoir en tonnes

write.table(stats, "outputs/08_Surfaces_production_carbone.csv", sep = ";", dec = ".", row.names = FALSE)

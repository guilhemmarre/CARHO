
# Résolution en output
resolution <- 20


#################################
###### LECTURE DES DONNEES ######
#################################

# Biocénoses
biocenoses <- sf::st_read("data/SIG/test_eco_biocenoses_PNMCCA.shp")
biocenoses <- biocenoses[biocenoses$extrapol == 'Rhodolithes' | biocenoses$cod_mnhn %in% c('asso rhodo indé IV.2.2.', 
                                                                                           'asso rhodo indé IV.2.4.', 
                                                                                           'IV.2.2.a.',
                                                                                           'IV.2.2.b.',
                                                                                           'IV.2.2.c.'), "geometry"]


# Recouvrement
luge <- sf::st_read("data/SIG/test_points_poly_PNMCCA.shp")
luge <- luge[!is.na(luge$rhodo_rec),]
luge <- luge[luge$rhodo_rec > 10,]


#####################
###### KRIGING ######
#####################

# Définir l'emprise
ext <- terra::ext(terra::vect(luge))

# Créer raster vide à 50 m
r <- terra::rast(ext, resolution = resolution, crs = sf::st_crs(luge)$wkt)
gridNewData <- terra::as.points(r)
gridNewData <- sf::st_as_sf(gridNewData)

# Autokriging
krige <- automap::autoKrige(formula = rhodo_rec ~ 1, input_data = luge, new_data = gridNewData)
plot(krige)

# Résultats
results <- krige$krige_output
results <- terra::vect(results)[, "var1.pred"]
names(results) <- "rhodo_rec"

# Export raster
r <- terra::rast(x = results, resolution = resolution)
results_raster <- terra::rasterize(results, r, "rhodo_rec")
terra::writeRaster(x = results_raster, filename = paste0("outputs/Kriging_raster_", resolution, "m.tif"), overwrite = TRUE)


############################
###### CLIP ET EXPORT ######
############################

results <- terra::as.polygons(results_raster, dissolve = FALSE) # dissolve = FALSE = un polygone par pixel
results <- sf::st_as_sf(results)
names(results) <- c("rhodo_rec", "geometry")

# sf::sf_use_s2(TRUE)
intersection <- sf::st_intersection(biocenoses, results)
intersection <- intersection[sf::st_geometry_type(intersection) %in% c("POLYGON", "MULTIPOLYGON"), ]
sf::st_write(intersection, paste0("outputs/Kriging_shapefile_", resolution, "m.shp"))

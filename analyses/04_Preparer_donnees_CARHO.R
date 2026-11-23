
##############################
### CHARGEMENT DES DONNEES ###
##############################

# Points bio carho
pts_carho <- sf::st_read("data/SIG/bio_plongees_PNMCCA_Carho_2025_pt_wgs84.shp")
pts_carho$site <- NULL
pts_carho$date_loc <- NULL
pts_carho$typ_local <- NULL
pts_carho$nom_aut <- NULL
pts_carho$prenom_aut <- NULL
pts_carho$init_ope <- NULL
pts_carho$cod_site <- NULL
pts_carho$lb_hab_fr <- NULL
pts_carho$org_ope <- NULL
pts_carho$obs_act <- NULL
pts_carho$cd_typo <- NULL

# Biocenoses carho
biocenoses_carho <- sf::st_read("data/SIG/eco_biocenoses_2025_PNMCCA_Carho_pol_L93.shp")
biocenoses_carho <- biocenoses_carho[, c("extrapol", "biocenose", "cod_mnhn", "geometry")]
biocenoses_carho <- sf::st_transform(x = biocenoses_carho, crs = sf::st_crs(pts_carho))
biocenoses_carho <- sf::st_make_valid(biocenoses_carho)

# Croisement des deux couches pour récupérer cod_mnhn et biocenose
pts_carho <- sf::st_join(pts_carho, biocenoses_carho) # [seq.int(from = 1, to = nrow(pts_carho), by= 1000),]
pts_carho$cd_hab <- pts_carho$cod_mnhn
pts_carho$cod_mnhn <- NULL

# Correction pour les pralines et boxworks
pts_carho$cd_hab[pts_carho$cd_hab == "IV.2.2.b."] <- "IV.2.2.b.P"
pts_carho$cd_hab[which(pts_carho$cd_hab == "IV.2.2.b.P" & pts_carho$biocenose == "Boxworks")] <- "IV.2.2.b.B"

# Export
sf::st_write(pts_carho, "data/SIG/bio_plongees_PNMCCA_CARHO.gpkg", append = FALSE)


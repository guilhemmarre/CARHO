
library(tidyverse)

##############################
### CHARGEMENT DES DONNEES ###
##############################

# df_base <- sf::st_read("outputs/04_bio_plongees_pressions_env.gpkg")
# save(df_base, file = "outputs/04_bio_plongees_pressions_env.RData")
load(file = "outputs/04_bio_plongees_pressions_env.RData")


#################################
### CODES HABITAT RHODOLITHES ###
#################################

# Suppression des quelques points sans bathymétrie (391)
df_base <- df_base[!is.na(df_base$profondeur),]

# Correction habitat rhodolithe incertain
df_base$cd_hab[df_base$cd_hab == "asso rhodo indé IV.2.2."] <- "IV.2.2." # rhodo indéterminée
df_base$cd_hab[df_base$cd_hab == "asso rhodo indé IV.2.4."] <- "IV.2.4." # rhodo indéterminée

# Remplacement noms explicites habitats
df_base$cd_hab[df_base$cd_hab == "III.2.2."] <- "III.2.2. SFBC"

df_base$cd_hab[df_base$cd_hab == "III.3.2."] <- "IV.2.2. DC" # regroupement / "III.3.2. SGCF - infralittoral"
df_base$cd_hab[df_base$cd_hab == "III.3.2.a."] <- "IV.2.2.a. Maërl DC" # regroupement / "III.3.2.a. Maërl sur SGCF"
df_base$cd_hab[df_base$cd_hab == "III.3.2.b.P"] <- "IV.2.2.b. Pralines DC" # regrouement / "III.3.2.b. SGCF - Pralines"

df_base$cd_hab[df_base$cd_hab == "IV.2.2."] <- "IV.2.2. DC"
df_base$cd_hab[df_base$cd_hab == "IV.2.2.a."] <- "IV.2.2.a. Maërl DC"
df_base$cd_hab[df_base$cd_hab == "IV.2.2.b."] <- "IV.2.2.b. Pralines DC" # par défaut Pralines, sans détail de "biocenose = boxworks"
df_base$cd_hab[df_base$cd_hab == "IV.2.2.b.P"] <- "IV.2.2.b. Pralines DC"
df_base$cd_hab[df_base$cd_hab == "IV.2.2.b.B"] <- "IV.2.2.b. Boxworks DC"
df_base$cd_hab[df_base$cd_hab == "IV.2.2.c."] <- "IV.2.2.c. Peyssonnelia libres"

df_base$cd_hab[df_base$cd_hab == "IV.2.3.b."] <- "IV.2.3.b. Faciès à L. phalangium"

df_base$cd_hab[df_base$cd_hab == "IV.2.4."] <- "IV.2.4. SGCF - circalittoral"


# On supprime les données sans information d'habitat : 0 points
df_base <- df_base[!is.na(df_base$cd_hab),]
df_base <- df_base[df_base$cd_hab != "",]


################
### SURFSTAT ###
################

# On corrige les codes manquants par Fonds meubles circalittoraux / infralittoraux
df_base$surfstat[df_base$surfstat == 0 & df_base$profondeur < -60] <- 8
df_base$surfstat[df_base$surfstat == 0] <- 9

# Codes surfstat 2023
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
df_base$surfstat_str <- codes_surfstat_2023[df_base$surfstat]
df_base$surfstat <- df_base$surfstat_str
df_base$surfstat_str <- NULL


# Biocenoses où sont observées les rhodolithes majoritairement
rhodo <- df_base[df_base$cd_hab %in% c("IV.2.2.a. Maërl DC",
                                                "IV.2.2.b. Pralines DC",
                                                "IV.2.2.b. Boxworks DC",
                                                "IV.2.2.c. Peyssonnelia libres"),]

table(rhodo$surfstat)
summary(rhodo$profondeur)

rhodo_0_30 <- rhodo[rhodo$profondeur > -30,]
sf::st_write(rhodo_0_30, "outputs/05_points_0-30m.gpkg", append = FALSE)

rhodo_30_40 <- rhodo[rhodo$profondeur < -30 & rhodo$profondeur > -40,]
sf::st_write(rhodo_30_40, "outputs/05_points_30-40m.gpkg", append = FALSE)

# Suppression des colonnes inutiles
vars_to_remove <- c("id_stat","lat_dd","long_dd","cod_site","rhodo","geometry")
df_base <- df_base %>% select(-any_of(vars_to_remove))

##############
### IMPACT ###
##############

df_base$agriculture[which(is.na(df_base$agriculture))] <- 0
df_base$amenagements[which(is.na(df_base$amenagements))] <- 0
df_base$aquaculture[which(is.na(df_base$aquaculture))] <- 0
df_base$apports_cours_deau[which(is.na(df_base$apports_cours_deau))] <- 0
df_base$erosion[which(is.na(df_base$erosion))] <- 0
df_base$rejets_industriels[which(is.na(df_base$rejets_industriels))] <- 0
df_base$mouillages_grands[which(is.na(df_base$mouillages_grands))] <- 0
df_base$mouillages_petits[which(is.na(df_base$mouillages_petits))] <- 0
df_base$peche[which(is.na(df_base$peche))] <- 0
df_base$urbanisation[which(is.na(df_base$urbanisation))] <- 0
df_base$rejets_urbains[which(is.na(df_base$rejets_urbains))] <- 0
df_base$tourisme_balneaire[which(is.na(df_base$tourisme_balneaire))] <- 0
df_base$trafic_maritime[which(is.na(df_base$trafic_maritime))] <- 0

# Suppression des colonnes constantes
num_cols_all <- names(df_base)[sapply(df_base, is.numeric)]
zero_var <- names(which(sapply(df_base[, num_cols_all, drop=FALSE],
                               function(x) var(x, na.rm=TRUE) == 0)))
df_base <- df_base[, !(names(df_base) %in% zero_var), drop=FALSE]

# Export des résultats en .RData
save(df_base, file = "./outputs/05_bio_plongees_pressions_env_clean.RData")

# Fichier light pour tester le script de modélisation
set.seed(2021)
df_base <- df_base[df_base$cd_hab %in% c("IV.2.2. DC", "IV.2.2.a. Maërl DC", "IV.2.2.b. Pralines DC"),]
split <- rsample::initial_split(df_base, prop = 0.001, strata = cd_hab)
df_base <- rsample::training(split)
save(df_base, file = "./outputs/05_bio_plongees_pressions_env_clean_light.RData")


# LIBRARIES ---------------------------------------------------------------
library(ggplot2);library(dplyr);library(stringr);library(RColorBrewer);library(tidyr);library(ggExtra);library(svDialogs)

# Fonctions
source("R/degmindec2degdec.R")
source("R/compute_indices.R")


###########################
### LECTURE DES DONNEES ###
###########################

# stations_carho
stations_carho <- read.table("data/stations.csv", sep= ";", dec =".", header = TRUE)
stations_carho$Latitude <- toDegDec(stations_carho$Latitude)
stations_carho$Longitude <- toDegDec(stations_carho$Longitude)

# Liste espèces méthode 2
BDD_especes <- read.table("data/BDD_especes.csv", sep = ";", dec = ".", header = TRUE)

### METHODE ImageJ
files_imageJ <- list.files(path = "data/ImageJ/", pattern = glob2rx("*Output*.csv"), recursive = TRUE, full.names = TRUE)

donnees_imageJ <- data.frame()
for (file in files_imageJ){
  temp_donnees_imageJ <- read.table(file, header = TRUE, sep = ",")
  cat("Fichier :", file, " avec ", nrow(temp_donnees_imageJ), " lignes", "\n")
  donnees_imageJ <- rbind(donnees_imageJ, temp_donnees_imageJ)
  rm(temp_donnees_imageJ)
}

donnees_imageJ$Station <- paste0(donnees_imageJ$Station, "_imJ")

table(donnees_imageJ$Station)

# Correction des erreurs pour Peyssonneliaceae
donnees_imageJ$CatGrid[grep("Peysso", donnees_imageJ$CatGrid)] <- "Peyssonneliaceae"

### METHODE CPCe
files_CPCe <- list.files(path = "data/CPCe/", pattern = glob2rx("*.cpc"), recursive = TRUE, full.names = TRUE)


# Compilation des données CPCe brutes
donnees_cpce <- do.call(rbind.data.frame, lapply(files_CPCe, function(f){
  
  station <- strsplit(f, "/")[[1]][3]
  quadrat_name <- strsplit(f, "/")[[1]]
  quadrat_name <- quadrat_name[length(quadrat_name)]
  quadrat_name <- gsub(".cpc", ".jpg", quadrat_name)
  
  year <- strsplit(quadrat_name, station)[[1]][2]
  year <- strsplit(year, "_")[[1]][2]
  year <- as.numeric(year)
  
  df <- read.table(f, sep = ",", skip = 70, nrows = 64, header = FALSE, stringsAsFactors = FALSE)
  df$V3 <- NULL
  colnames(df) <- c("item_number", "ID_code", "notes")
  
  result <- data.frame("Station" = station,
                       "Temps" = year,
                       "QuadratName" = quadrat_name, 
                       df, stringsAsFactors = FALSE)
  
  return(result)
  
}))

# Replace Peyssonnelia squamaria, Peyssonnelia sp with Erected Peyssonnelia sp
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("PESQ", "PESP")] <- "PESP"

# Replace DIRO and APLI with Aplidium sp
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("DIRO", "APLI")] <- "APLI"

# Replace Peyssonnelia rosa-marina, Peyssonnelia polImorpha with Encrusting Peyssonnelia sp
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("PERO", "PEPO")] <- "PCSP"

# Replace Cystoseira zosteroides with Cystoseira sp
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("CYZO")] <- "CYSP"

# Replace cliona sp, cliona viridis, cliona celata with Cliona sp
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("CLCE", "CLVI")] <- "CLIO"

# Replace Crambe crambe, Antho inconstans, Spirastrella cunctatrix with crambe crambe like
donnees_cpce$ID_code[donnees_cpce$ID_code %in% c("SPCU", "ANIN", "CRAM")] <- "CRAM"

# Jointure major_category
donnees_cpce <- merge(donnees_cpce, BDD_especes[,c("ID_code","major_category")], all.x = TRUE, all.y = FALSE, by = "ID_code")

# Transformation en catégories maerl
donnees_cpce$CatGrid <- NA
donnees_cpce$CatGrid[which(donnees_cpce$ID_code == "PHCA")] <- "Maerl vivant"
donnees_cpce$CatGrid[which(donnees_cpce$ID_code == "PHCA" & donnees_cpce$notes == "NEC")] <- "Maerl mort"

donnees_cpce$CatGrid[which(donnees_cpce$ID_code %in% c("PESP","PERO","PESQ","PEPO","PCSP"))] <- "Peyssonneliaceae"
donnees_cpce$CatGrid[which(donnees_cpce$ID_code %in% c("CRMA","LIST","LIIN","MEAL"))] <- "Pralines vivantes"
donnees_cpce$CatGrid[which(donnees_cpce$ID_code %in% c("CRMA","LIST","LIIN","MEAL") & donnees_cpce$notes == "NEC")] <- "Pralines mortes"

algae <- c("Green macroalgae", "Erected red macroalgae","Brown macroalgae")
donnees_cpce$CatGrid[which(donnees_cpce$major_category %in% algae)] <- "Algues"
donnees_cpce$CatGrid[which(donnees_cpce$major_category %in% c("Sludge, pavement, rubble, sand", "Phanerogam"))] <- "Sediment" # On inclut phanérogames en sédiment car ce qui aurait été fait sous imageJ
donnees_cpce$CatGrid[which(!(donnees_cpce$major_category %in% c(algae, "Tape, wand, shadow", "Unknowns", 
                                                 "Sludge, pavement, rubble, sand","NC",
                                                 "Encrusting red macroalgae",
                                                 "Phanerogam")))] <- "Faune"

donnees_cpce_raw <- donnees_cpce

# Export des tableaux complets
write.table(x = donnees_cpce, file = "outputs/01_Rhodolithes_cpce_cover_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)
write.table(x = donnees_imageJ, file = "outputs/01_Rhodolithes_ImageJ_cover_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)

# On supprime les catégories UNK et TWS
donnees_cpce <- donnees_cpce[!is.na(donnees_cpce$CatGrid),]

# Fusion des données dans un seul tableau
donnees_cpce$Label <- donnees_cpce$item_number
cols <- c("Station","Temps","Label","QuadratName","CatGrid")
donnees <- rbind(donnees_cpce[, cols],
                 donnees_imageJ[, cols])

# Check nombre de quadrats par station (erreurs dans les noms des fichiers...)
for (station in unique(donnees$Station)){
  
  cat("Station: ", station, " / nb de quadrats : ", length(unique(donnees$QuadratName[donnees$Station == station])), "\n")
  
}




################
### Tableaux ###
################

# Table de contingence pour ImageJ
temp <- data.frame(table(paste(donnees_imageJ$Station, donnees_imageJ$QuadratName), donnees_imageJ$CatGrid))
donnees_imageJ <- data.frame(str_split_fixed(temp$Var1," ", 2))
donnees_imageJ$Category<-temp$Var2
donnees_imageJ$NumberOfCells<-temp$Freq
names(donnees_imageJ)[1] <- "Station"
names(donnees_imageJ)[2] <- "QuadratName"
rm(temp)

# Table de contingence pour CPCe
temp <- data.frame(table(paste(donnees_cpce$Station, donnees_cpce$QuadratName), donnees_cpce$CatGrid))
donnees_cpce <- data.frame(str_split_fixed(temp$Var1," ", 2))
donnees_cpce$Category<-temp$Var2
donnees_cpce$NumberOfCells<-temp$Freq
names(donnees_cpce)[1] <- "Station"
names(donnees_cpce)[2] <- "QuadratName"
rm(temp)

# On multiplie par le ratio du nombre de points cpce / cellules pour avoir qqch de comparable
for (quadrat in unique(donnees_cpce$QuadratName)){
  
  nb_cells_quadrat <- sum(donnees_cpce$NumberOfCells[donnees_cpce$QuadratName == quadrat])
  donnees_cpce$NumberOfCells[donnees_cpce$QuadratName == quadrat] <- donnees_cpce$NumberOfCells[donnees_cpce$QuadratName == quadrat] * 225 / nb_cells_quadrat
  
}

# On fusionne les deux tableaux
donnees_rhodo <- rbind(donnees_cpce, donnees_imageJ)
donnees_rhodo$Station <- as.factor(donnees_rhodo$Station)
donnees_rhodo$QuadratName <- as.factor(donnees_rhodo$QuadratName)



# Calculs de surface ------------------------------------------------------
cm2 = 2475
nrow = 15

CoteCellule = sqrt(cm2) / nrow
AreaCellule = CoteCellule ^ 2

donnees_rhodo$Area <- donnees_rhodo$NumberOfCells * AreaCellule #Area est donne en cm2
#donnees_rhodo$PercentArea<-(donnees_rhodo$Area*100)/cm2
donnees_rhodo$PercentArea <- donnees_rhodo$NumberOfCells / 225 * 100

categories <- c("Maerl vivant",
                "Maerl mort",
                "Maerl vivant x Maerl mort",
                "Maerl vivant x Sediment",
                "Maerl mort x Sediment",
                
                "Pralines vivantes",
                "Pralines mortes",
                "Pralines vivantes x Pralines mortes",
                "Pralines vivantes x Sediment",
                "Pralines mortes x Sediment",
                
                "Boxwork vivant",
                "Boxwork mort",
                "Boxwork vivant x Boxwork mort",
                "Boxwork vivant x Sediment",
                "Boxwork mort x Sediment",
                
                "Peyssonneliaceae",
                
                "Corallines vivantes",
                "Corallines encroutantes",
                "Corallines mortes",
                
                "Coquilles vides",
                "Coquilles vides x Sediment",
                
                "Algues",
                "Faune",
                "Sediment"
)

donnees_rhodo$Category <- factor(donnees_rhodo$Category,level = categories)


# Export donnees_rhodo au format csv
write.table(x = donnees_rhodo, file = "outputs/01_Rhodolithes_cover_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)

# Données rhodo moyennes par station
donnees_rhodo_stations <- donnees_rhodo %>%
  group_by(Station,Category) %>%
  summarise(MeanPercent = mean(PercentArea), SdCover = sd(PercentArea))

#Export de donnees_rhodo_stations au format .csv
write.table(x = donnees_rhodo_stations, file = "outputs/01_Rhodolithes_cover_stations.csv", sep = ";", dec = ".", row.names = FALSE)


# Matrice de communautés en pourcentages par quadrat
comMat_substrat_quadrats <- donnees_rhodo[,c("Station", "QuadratName", "Category", "PercentArea")] %>%
  pivot_wider(
    names_from = Category,
    values_from = PercentArea
  )

write.table(x = comMat_substrat_quadrats, file = "outputs/01_Rhodolithes_comMat_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)

# Matrice de communautés en pourcentages par station
comMat_substrat_stations <- donnees_rhodo_stations[,c("Station", "Category", "MeanPercent")] %>%
  pivot_wider(
    names_from = Category,
    values_from = MeanPercent
  )

# On ajoute les colonnes vides et rempli les trous (toutes les catégories non représentées dans CARHO)
comMat_substrat_stations[is.na(comMat_substrat_stations)] <- 0
missing_cols <- categories[!is.element(categories, colnames(comMat_substrat_stations))]
comMat_substrat_stations[, missing_cols] <- 0

write.table(x = comMat_substrat_stations, file = "outputs/01_Rhodolithes_comMat_stations.csv", sep = ";", dec = ".", row.names = FALSE)


# Matrice de communautés en présence pour les stations
comMat_bio_stations <- donnees_cpce_raw
comMat_bio_stations$Nb <- 1
comMat_bio_stations <- comMat_bio_stations[, c("Station","ID_code","Nb")] %>%
  pivot_wider(
    names_from = ID_code,
    values_from = Nb,
    values_fn = sum
  )

comMat_bio_stations[is.na(comMat_bio_stations)] <- 0

write.table(comMat_bio_stations, "outputs/01_comMatrix_bio_stations.csv", sep = ";", dec = ".", row.names = FALSE)

## Listes Flore et Faune
flore <- c("Green macroalgae", 
           "Erected red macroalgae",
           "Encrusting red macroalgae",
           "Brown macroalgae",
           "Phanerogam")
BDD_especes$Type <- NA
BDD_especes$Type[BDD_especes$major_category %in% flore] <- "Flore"
BDD_especes$Type[!(BDD_especes$major_category %in% c(flore, "Tape, wand, shadow", "Unknowns", 
                                                   "Sludge, pavement, rubble, sand","NC"))] <- "Faune"


# Matrice de communautés FLORE et FAUNE pour les stations
taxons_flore <- colnames(comMat_bio_stations)[which(is.element(colnames(comMat_bio_stations), BDD_especes$ID_code[which(BDD_especes$Type == "Flore")]))]
taxons_faune <- colnames(comMat_bio_stations)[which(is.element(colnames(comMat_bio_stations), BDD_especes$ID_code[which(BDD_especes$Type == "Faune")]))]
comMat_flore_stations <- comMat_bio_stations[,c("Station", taxons_flore)]
comMat_faune_stations <- comMat_bio_stations[,c("Station", taxons_faune)]


write.table(comMat_flore_stations, "outputs/01_comMatrix_flore_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)
write.table(comMat_faune_stations, "outputs/01_comMatrix_faune_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)


##########################
### CALCUL DES INDICES ###
##########################


indices_maerl = compute_indices_rhodo(data = donnees_rhodo, type = "maerl")
colnames(indices_maerl) <- c("Station", "QuadratName", "Bizien_maerl", "Mvth_maerl")

indices_all = compute_indices_rhodo(data = donnees_rhodo, type = "all")
colnames(indices_all) <- c("Station", "QuadratName", "Bizien_all", "Mvth_all")

indices = merge(indices_maerl, indices_all, by = c("Station", "QuadratName"), all.x = TRUE, all.y = TRUE)

rm(indices_all, indices_maerl)

# Export des indices par quadrat
write.table(x = indices, file = "outputs/01_Rhodolithes_indices_quadrats.csv", sep = ";", dec = ".", row.names = FALSE)

# Aggrégation par station
indices_stations <- indices
indices_stations$QuadratName <- NULL
indices_stations <- aggregate(.~Station, indices_stations, mean)

# On ajoute les stations_carho et export par station
indices_stations <- merge(stations_carho, indices_stations, by = "Station", all.x = TRUE, all.y = TRUE)

# Définition de l'état pour les deux indicateurs all
indices_stations$E_Bizien_maerl <- cut(indices_stations$Bizien_maerl, breaks = c(0,0.2,0.6,0.8,1), labels = c("Mauvais","Moyen","Bon","Tres bon"))
indices_stations$E_Mvth_maerl <- cut(indices_stations$Mvth_maerl, breaks = c(0,0.5,0.75,1), labels = c("Mauvais","Moyen / bon","Tres bon"))
indices_stations$E_Bizien_all <- cut(indices_stations$Bizien_all, breaks = c(0,0.2,0.6,0.8,1), labels = c("Mauvais","Moyen","Bon","Tres bon"))
indices_stations$E_Mvth_all <- cut(indices_stations$Mvth_all, breaks = c(0,0.5,0.75,1), labels = c("Mauvais","Moyen / bon","Tres bon"))

### Calcul indicateur de vitalité à l'échelle de la station

# MAERL
vitalite <- comMat_substrat_stations

MV <- vitalite$`Maerl vivant`
MVS <- vitalite$`Maerl vivant x Sediment`
MVMM <- vitalite$`Maerl vivant x Maerl mort`
MM <- vitalite$`Maerl mort`
MMS <- vitalite$`Maerl mort x Sediment`
Fauna <- vitalite$Faune
A <- vitalite$Algues

Mvth = (MV + MVS / 2 + Fauna / 3 + A / 3 + MVMM / 2) / 100
MVMMth = MV + MM + MVMM + MVS / 2 + MMS / 2 + Fauna * 2 / 3 + A *2/3
vitalite = Mvth * 100 / MVMMth

indices_stations$Vit_maerl <- vitalite
indices_stations$E_Vit_maerl <- cut(indices_stations$Vit_maerl, breaks=c(0,0.5,0.75,1), labels = c("Mauvais","Moyen / bon","Tres bon"))

# ALL
vitalite <- comMat_substrat_stations

MV <- vitalite$`Maerl vivant` + vitalite$`Pralines vivantes` + vitalite$`Boxwork vivant` + vitalite$Peyssonneliaceae
MVS <- vitalite$`Maerl vivant x Sediment` + vitalite$`Pralines vivantes x Sediment` + vitalite$`Boxwork vivant x Sediment`
MVMM <- vitalite$`Maerl vivant x Maerl mort` + vitalite$`Pralines vivantes x Pralines mortes` + vitalite$`Boxwork vivant x Boxwork mort`
MM <- vitalite$`Maerl mort` + vitalite$`Pralines mortes` + vitalite$`Boxwork mort`
MMS <- vitalite$`Maerl mort x Sediment` + vitalite$`Pralines mortes x Sediment` + vitalite$`Boxwork mort x Sediment`
Fauna <- vitalite$Faune
A <- vitalite$Algues

Mvth = (MV + MVS / 2 + Fauna / 3 + A / 3 + MVMM / 2) / 100
MVMMth = MV + MM + MVMM + MVS / 2 + MMS / 2 + Fauna * 2 / 3 + A *2/3
vitalite = Mvth * 100 / MVMMth

indices_stations$Vit_all <- vitalite
indices_stations$E_Vit_all <- cut(indices_stations$Vit_all, breaks=c(0,0.5,0.75,1), labels = c("Mauvais","Moyen / bon","Tres bon"))

# Rename des colonnes
colnames(indices_stations) <-  c("Station", "Secteur", "Facade", "Longitude", "Latitude", "Profondeur", "Impact", "Peche", "Alien", "Macrodechets",
                                 "Bizien_maerl", "Mvth_maerl", "Bizien_all", "Mvth_all", 
                                 "E_Bizien_maerl", "E_Mvth_maerl", "E_Bizien_all", "E_Mvth_all",
                                 "Vit_maerl", "E_Vit_maerl", "Vit_all", "E_Vit_all")

# Export
write.table(x = indices_stations, file = "outputs/01_Rhodolithes_indices_stations.csv", sep = ";", dec = ".", row.names = FALSE)

indices_stations_shp <- sf::st_as_sf(x = indices_stations, 
                                     coords = c("Longitude", "Latitude"),
                                     crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# Export du shapefile
sf::st_write(obj = indices_stations_shp, layer = "01_Rhodolithes_indices_stations", dsn = "outputs/01_Rhodolithes_indices_stations.shp", delete_dsn = TRUE, delete_layer = FALSE)


#################
### PRESSIONS ###
#################

# Lecture du fichier de positions des autres stations
stations_caremaerl <- read.table("../CAREMaerl_2024/QUADRATS/R/data/stations.csv", sep = ";", dec = ".", header = TRUE)
stations_caremaerl$Longitude <- toDegDec(stations_caremaerl$Longitude)
stations_caremaerl$Latitude <- toDegDec(stations_caremaerl$Latitude)
stations_all <- rbind(stations_carho, stations_caremaerl)
stations_all_shp <- sf::st_as_sf(x = stations_all,
                            coords = c("Longitude", "Latitude"),
                            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

sf::st_write(obj = stations_all, layer = "stations_all", dsn = "outputs/stations_all.shp", delete_dsn = TRUE, delete_layer = FALSE)

# Sauvegarde de l'image du projet
save.image(file = "outputs/01_data.RData")

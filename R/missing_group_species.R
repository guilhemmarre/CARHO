

# BDD espèces manquantes
groupes <- read.table("../CAREMaerl_2024/QUADRATS/R/data/BDD_especes.csv", sep = ";", dec = ".", header = TRUE)

# liste catégories CPCe détaillées
classes <- read.table("outputs/01_comMatrix_bio_stations.csv", sep = ";", dec = ".", header = TRUE)
classes <- colnames(classes)[2:ncol(classes)]

# BDD_especes cpce
BDD_especes <- read.table("data/BDD_especes.csv", sep = ";", dec = ".", header = TRUE)
especes <- data.frame("ID_code" = classes)
especes <- merge(especes, BDD_especes[,c("ID_code","id_name")], all.x = TRUE, all.y = FALSE)
especes$id_name <- gsub(" ","_", especes$id_name)

# Especes manquantes
missing <- especes$id_name[!is.element(especes$id_name, groupes$Espece)]

write.table(missing, "outputs/missing_species_morpho.csv", sep = ";", dec = ".", col.names = FALSE)

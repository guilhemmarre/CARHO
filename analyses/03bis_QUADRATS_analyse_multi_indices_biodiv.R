
##############################
### CHARGEMENT DES DONNEES ###
##############################

# Poissons
poissons <- XLConnect::readWorksheetFromFile("data/CARHO_synthese_inventaires.xlsx", sheet = "ADNe-poissons")
poissons <- poissons[6:nrow(poissons),]
poissons_metadata <- poissons[,1:5]
colnames(poissons_metadata) <- poissons_metadata[1,]
poissons_metadata <- poissons_metadata[-1,]
poissons_data <- poissons[,6:ncol(poissons)]
poissons_data <- poissons_data[, seq(from = 1, to = ncol(poissons_data), by = 2)]
poissons_data[is.na(poissons_data)] <- 0
poissons_data <- poissons_data[-1,]
for (j in 1:ncol(poissons_data)){poissons_data[,j] <- as.numeric(poissons_data[,j])}

for (i in seq(1, ncol(poissons_data), by = 2)){
  
  poissons_data[, i] <- poissons_data[, i] + poissons_data[, i+1]
  
}

poissons_data <- poissons_data[, seq(1, ncol(poissons_data), by = 2)]
poissons_data[poissons_data > 0] <- 1
colnames(poissons_data) <- paste0("R_PNMCCA_0", 1:8)
poissons <- cbind(poissons_metadata, poissons_data)



# EPIFAUNE
epifaune <- XLConnect::readWorksheetFromFile("data/CARHO_synthese_inventaires.xlsx", sheet = "prelevements-epifaune")
colnames(epifaune) <- epifaune[1,]
epifaune <- epifaune[-1,]
ordre_carottes <- colnames(epifaune)[4:ncol(epifaune)]
epifaune_metadata <- epifaune[,1:3]
epifaune_data <- epifaune[,4:ncol(epifaune)]
epifaune_data[epifaune_data == "x"] <- 1
epifaune_data[is.na(epifaune_data)] <- 0
for (j in 1:ncol(epifaune_data)){epifaune_data[,j] <- as.numeric(epifaune_data[,j])}

for (i in seq(1, ncol(epifaune_data), by = 3)){
  
  epifaune_data[, i] <- epifaune_data[, i] + epifaune_data[, i+1] + epifaune_data[, i+2]
  
}

epifaune_data <- epifaune_data[, seq(1, ncol(epifaune_data), by = 3)]
epifaune_data[epifaune_data > 0] <- 1
colnames(epifaune_data) <- paste0("R_PNMCCA_0", 1:8)
epifaune <- cbind(epifaune_metadata, epifaune_data)


# RHODOLITHES
rhodolithes <- XLConnect::readWorksheetFromFile("data/CARHO_synthese_inventaires.xlsx", sheet = "prelevements-rhodolithes")
colnames(rhodolithes) <- rhodolithes[1,]
rhodolithes <- rhodolithes[-1,]
rhodolithes <- rhodolithes[,c("Species ID", ordre_carottes)]
rhodolithes_metadata <- rhodolithes[,1]
rhodolithes_data <- rhodolithes[,2:ncol(rhodolithes)]
rhodolithes_data[is.na(rhodolithes_data)] <- 0
for (j in 1:ncol(rhodolithes_data)){ cat(j, "\n"); rhodolithes_data[,j] <- as.numeric(rhodolithes_data[,j])}

for (i in seq(1, ncol(rhodolithes_data), by = 3)){
  
  rhodolithes_data[, i] <- rhodolithes_data[, i] + rhodolithes_data[, i+1] + rhodolithes_data[, i+2]
  
}

rhodolithes_data <- rhodolithes_data[, seq(1, ncol(rhodolithes_data), by = 3)]
rhodolithes_data[rhodolithes_data > 0] <- 1
colnames(rhodolithes_data) <- paste0("R_PNMCCA_0", 1:8)
rhodolithes <- cbind(rhodolithes_metadata, rhodolithes_data)

# OBS IN SITU
obs_in_situ <- XLConnect::readWorksheetFromFile("data/CARHO_synthese_inventaires.xlsx", sheet = "observations in situ")
colnames(obs_in_situ) <- obs_in_situ[1,]
obs_in_situ <- obs_in_situ[-1,]
obs_in_situ_data <- obs_in_situ[,4:ncol(obs_in_situ)]
obs_in_situ_data[!is.na(obs_in_situ_data)] <- 1
obs_in_situ_data[is.na(obs_in_situ_data)] <- 0
for (j in 1:ncol(obs_in_situ_data)){ cat(j, "\n"); obs_in_situ_data[,j] <- as.numeric(obs_in_situ_data[,j])}
obs_in_situ <- cbind(obs_in_situ[,1:3], obs_in_situ_data)

# CPCe
load("outputs/01_data.RData")


#######################################
### COMPILATION DE TOUS LES INDICES ###
#######################################

poissons_div <- apply(poissons[,6:ncol(poissons)], 2, sum)
rhodolithes_div <- apply(rhodolithes[,2:ncol(rhodolithes)], 2, sum)
epifaune_div <- apply(epifaune[,4:ncol(epifaune)], 2, sum)

diversite <- indices_stations
rownames(diversite) <- diversite[,1]
diversite <- diversite[,11:ncol(diversite)]
diversite <- diversite[,-grep("E_", colnames(diversite))]
diversite <- diversite[-grep("imJ", rownames(diversite)),]
diversite$poissons <- poissons_div
diversite$epifaune <- epifaune_div
diversite$rhodolithes <- rhodolithes_div

div_tot <- as.data.frame(comMat_bio_stations[, !(colnames(comMat_bio_stations) %in% c("Other_live", "Crevice","Rubble","Sand","Sludge","Tape","Wand","Shadow","Unknowns"))])
rownames(div_tot) <- div_tot$Station
div_tot$Station <- NULL
div_tot[div_tot>0] <- 1
div_tot <- apply(div_tot, 1, sum)
div_tot <- div_tot[rownames(diversite)]
diversite$bio <- div_tot

write.table(diversite, "04_diversite_tous_indices.csv", sep = ";", dec = ".", row.names = FALSE)

data <- diversite[, -grep("maerl", colnames(diversite))]

hc <- hclust(dist(x = data, method = "euclidean"), method = "average")
jpeg(filename = "figures/GENERAL_arbre_multi-indices.jpg", width = 1200, 700)
plot(hc, main = "Classification hiérarchique des stations", sub = "", cex = 1.5, cex.main = 3, cex.lab = 1.5)
dev.off()


###########
### ACP ###
###########


library(ggplot2);library(dplyr);library(stringr);library(RColorBrewer);library(tidyr);library(ggExtra);library(svDialogs)
library(leaflet)
library(leaflet.extras)
library(scales)


data <- data %>%
  mutate(across(where(is.numeric), ~ rescale(.)))

# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))

# Plot de l'ACP
jpeg(filename = "figures/GENERAL_ACP_multi-indices.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, geom = c("point","text"), 
                            col.var = "red", 
                            col.ind = "purple",
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()


cor(diversite$epifaune, diversite$poissons, method = "spearman")
cor(diversite$rhodolithes, diversite$Mvth_all, method = "spearman")

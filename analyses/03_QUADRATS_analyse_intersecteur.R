
library(ggplot2);library(dplyr);library(stringr);library(RColorBrewer);library(tidyr);library(ggExtra);library(svDialogs)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(scales)

# Résultats CARHO
load("outputs/01_data.RData")

comMat_substrat_stations_CARHO <- comMat_substrat_stations[comMat_substrat_stations$Station != "R_PNMCCA_01_imJ",]
donnees_rhodo_stations_CARHO <- donnees_rhodo_stations[donnees_rhodo_stations$Station != "R_PNMCCA_01_imJ",]

comMat_bio_stations_CARHO <- comMat_bio_stations[comMat_bio_stations$Station != "R_PNMCCA_01_imJ",]

indices_CARHO <- indices[indices$Station != "R_PNMCCA_01_imJ",]
indices_stations_CARHO <- indices_stations[indices_stations$Station != "R_PNMCCA_01_imJ",]

# Résultats CAREMAERL
load("../CAREMaerl_2024/QUADRATS/R/output/01_data.RData")

comMat_substrat_stations_CAREMAERL <- comMat_substrat_stations
comMat_substrat_stations_CARHO <- comMat_substrat_stations_CARHO[, colnames(comMat_substrat_stations_CAREMAERL)]
comMat_substrat_stations <- rbind(comMat_substrat_stations_CARHO, comMat_substrat_stations_CAREMAERL)

comMat_bio_stations <- dplyr::bind_rows(comMat_bio_stations, comMat_bio_stations_CARHO)
comMat_bio_stations[is.na(comMat_bio_stations)] <- 0

donnees_rhodo_stations <- rbind(donnees_rhodo_stations_CARHO, donnees_rhodo_stations)
indices <- rbind(indices_CARHO, indices)
indices_stations <- rbind(indices_stations_CARHO, indices_stations)

metadata <- stations_all

cols_etat <- c("Mauvais" = "red",
               "Moyen" = "yellow",
               "Moyen / Bon" = "lightgreen",
               "Bon" = "green",
               "Tres bon" = "darkgreen")

myColors <- c("#8b0015","#ff635d","#ff6900","#ffbb8b","#fffdb9",
              "yellow","black","#268b00",
              "#ff8bf3","#b9e2ff","#00485d",
              "#5d3800","#7f008b",
              "#03fcf8",
              "#fc03f8","#850182","#bf04bc","#f571f3","#994598",
              "#e3e3de","#363635","#8f8f8d","#e7e8b3","#5e5e48")

names(myColors) <- c("Maerl vivant", "Maerl vivant x Sediment", "Maerl vivant x Maerl mort", "Maerl mort", "Maerl mort x Sediment",
                     "Sediment","Faune","Algues",
                     "Corallines vivantes", "Corallines encroutantes", "Corallines mortes",
                     "Coquilles vides", "Coquilles vides x Sediment",
                     "Peyssonneliaceae",
                     "Pralines vivantes", "Pralines mortes", "Pralines vivantes x Pralines mortes", "Pralines vivantes x Sediment", "Pralines mortes x Sediment",
                     "Boxwork vivant", "Boxwork mort", "Boxwork vivant x Boxwork mort", "Boxwork vivant x Sediment", "Boxwork mort x Sediment")


colScale <- scale_fill_manual(name = "Categories :",values = alpha(myColors,0.7))


###########################
### CLUSTERING SUBSTRAT ###
###########################

data <- data.frame(comMat_substrat_stations)
rownames(data) <- data$Station
data$Station <- NULL

hc <- hclust(dist(x = data, method = "euclidean"), method = "average")
jpeg(filename = "figures/GENERAL_clustering_methode1.jpg", width = 1200, 700)
plot(hc, main = "Classification hiérarchique des stations", sub = "", cex = 1.5, cex.main = 3, cex.lab = 1.5)
dev.off()

ordre_stations <- hc$labels[hc$order]


########################
### BARPLOT SUBSTRAT ###
########################

donnees_rhodo_stations$Station <- factor(donnees_rhodo_stations$Station, levels = ordre_stations)

jpeg(filename = "figures/GENERAL_histogramme_substrat_all_stations.jpg", width = 1200, 700)
ggplot(data = donnees_rhodo_stations, aes(x = Station, y = MeanPercent, fill = Category))+
  geom_bar(stat="identity")+
  ylab("COUVERTURE MOYENNE (%)")+
  xlab("")+
  labs(fill = "Categories:")+ 
  theme_bw()+
  theme(text = element_text(family = "sans", size = 20),
        axis.title = element_text(color = "gray15", size = 20),
        plot.title = element_text(color = "gray15", face = "bold",hjust = 0),
        axis.text = element_text(color = "gray15", face = "bold", hjust = 0, angle = 90, size = 15),
        panel.grid = element_blank())+
  colScale +
  guides(fill = guide_legend(ncol = 1))
dev.off()


####################
### ACP SUBSTRAT ###
####################


# ACP sur la composition du substrat
data <- data.frame(comMat_substrat_stations)

# On définis les secteurs pour toutes les stations
secteurs <- unname(sapply(data$Station, function(st) metadata$Secteur[metadata$Station == st]))

# On supprime les noms de stations pour la suite
row.names(data) <- data$Station
data <- data[,-1]



# On rescale entre 0 et 1 toutes les variables
data <- data %>%
  mutate(across(where(is.numeric), ~ rescale(.)))

# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))

# Plot de l'ACP
jpeg(filename = "figures/GENERAL_ACP_methode1.jpg", width = 1000, 1000)
factoextra::fviz_pca_var(PCA, col.var = "black", labelsize = 7,
                         ggtheme = theme_minimal() +
                           theme(
                             text = element_text(size = 30),
                             axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()


# Plot des corrélations
jpeg(filename = "figures/GENERAL_corrplot_methode1.jpg", width = 1100, 1000)
corrplot::corrplot(cor(data, method = "spearman"),
                   method = "circle",       
                   order = "hclust",         # Ordering method of the matrix
                   hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
                   addrect = 2,              # If order = "hclust", number of cluster rectangles
                   rect.col = 3,             # Color of the rectangles
                   rect.lwd = 3,
                   tl.cex = 1.7)  
dev.off()


# Graphique des individus sur le 1er plan de l'ACP
jpeg(filename = "figures/GENERAL_ACP_biplot_methode1.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, col.ind = as.factor(secteurs), geom = c("point","text"), 
                            col.var = "black", 
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()


#################################
### BARPLOT INDICES METHODE 1 ###
#################################

# Bizien - all
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Bizien_all, decreasing = FALSE)])

MeanIndice <- indices %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Bizien_all), SdIndice = sd(Bizien_all))

jpeg(filename = "figures/GENERAL_Bizien_all.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.2,0.6,0.8),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text",x=0.7,y=c(0.1,0.4,0.7,0.9),
           label=c("MAUVAIS","MOYEN","BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()

# Bizien - maerl
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Bizien_maerl, decreasing = FALSE)])

MeanIndice <- indices %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Bizien_maerl), SdIndice = sd(Bizien_maerl))

jpeg(filename = "figures/GENERAL_Bizien_maerl.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.2,0.6,0.8),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text",x=0.7,y=c(0.1,0.4,0.7,0.9),
           label=c("MAUVAIS","MOYEN","BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()


# Mvth - all
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Mvth_all, decreasing = FALSE)])

MeanIndice <- indices %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Mvth_all), SdIndice = sd(Mvth_all))

jpeg(filename = "figures/GENERAL_Mvth_all.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  ylim(0,1)+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.5,0.75),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text", x=0.7,y=c(0.2,0.6,0.9),
           label=c("MAUVAIS","MOYEN\nA BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()

# Mvth - maerl
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Mvth_maerl, decreasing = FALSE)])

MeanIndice <- indices %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Mvth_maerl), SdIndice = sd(Mvth_maerl))

jpeg(filename = "figures/GENERAL_Mvth_maerl.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  ylim(0,1)+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.5,0.75),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text", x=0.7,y=c(0.2,0.6,0.9),
           label=c("MAUVAIS","MOYEN\nA BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()


# Vit - all
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Vit_all, decreasing = FALSE)])

MeanIndice <- indices_stations %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Vit_all), SdIndice = sd(Vit_all))

jpeg(filename = "figures/GENERAL_Vit_all.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  ylim(0,1)+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.5,0.75),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text", x=0.7,y=c(0.2,0.6,0.9),
           label=c("MAUVAIS","MOYEN\nA BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()

# Vit - maerl
indices$Station <- factor(indices$Station, levels = indices_stations$Station[order(indices_stations$Vit_maerl, decreasing = FALSE)])

MeanIndice <- indices_stations %>%
  group_by(Station) %>%
  summarise(MeanIndice = mean(Vit_maerl), SdIndice = sd(Vit_maerl))

jpeg(filename = "figures/GENERAL_Vit_maerl.jpg", width = 1200, 700)
ggplot(data = MeanIndice,aes(x=reorder(Station,MeanIndice),y=MeanIndice))+
  ylim(0,1)+
  geom_bar(stat="identity",lwd=0.3,fill="gray55",col="black")+
  geom_errorbar(aes(ymin=MeanIndice, ymax=MeanIndice+SdIndice), width=.1)+
  theme_bw()+
  geom_hline(yintercept=c(0.5,0.75),linetype="dashed",color="gray35",lwd=0.5)+
  annotate(geom="text", x=0.7,y=c(0.2,0.6,0.9),
           label=c("MAUVAIS","MOYEN\nA BON","TRES\nBON"),angle=90,size=7,
           fontface="bold",hjust=0.5, vjust = 1, color="gray25")+
  labs(y="INDICE",x="")+
  theme(text = element_text(family="sans",size=20),
        axis.title = element_text(color = "gray15",size = rel(0.9)),
        plot.title = element_text(color = "gray15",face = "bold",hjust = 0),
        panel.grid = element_line(linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()

######################
### CLUSTERING BIO ###
######################

data <- data.frame(comMat_bio_stations)
rownames(data) <- data$Station
data$Station <- NULL

hc <- hclust(dist(x = data, method = "euclidean"), method = "average")
jpeg(filename = "figures/GENERAL_clustering_methode2.jpg", width = 1200, 700)
plot(hc, main = "Classification hiérarchique des stations", sub = "", cex = 1.5, cex.main = 3, cex.lab = 1.5)
dev.off()

ordre_stations <- hc$labels[hc$order]


###############
### ACP BIO ###
###############


# ACP sur la composition du substrat
data <- data.frame(comMat_bio_stations)

# On définis les secteurs pour toutes les stations
secteurs <- unname(sapply(data$Station, function(st) metadata$Secteur[metadata$Station == st]))

# On supprime les noms de stations pour la suite
row.names(data) <- data$Station
data <- data[,-1]

# On regroupe les espèces par groupe morpho-fonctionnel
groupes_observes <- unique(especes$Groupe[especes$Espece %in% colnames(data)])
groupes_observes <- groupes_observes[groupes_observes != "Autre"]
data <- do.call(cbind.data.frame, lapply(groupes_observes, function(groupe){
  
  especes_groupe <- especes$Espece[especes$Groupe == groupe]
  especes_groupe <- colnames(data)[colnames(data) %in% especes_groupe]
  
  if (length(especes_groupe) == 1){
    
    res <- data[,especes_groupe]
    
  }else{
    
    res <- apply(data[,especes_groupe], 1, sum)
    
  }
  
  return(res)
  
}))

colnames(data) <- groupes_observes

# On rescale entre 0 et 1 toutes les variables
data <- data %>%
  mutate(across(where(is.numeric), ~ rescale(.)))

faune <- especes$Groupe[especes$Type == "Faune"]
flore <- especes$Groupe[especes$Type == "Flore"]
data_faune <- data[,colnames(data) %in% faune]
data_flore <- data[,colnames(data) %in% flore]


# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))


# Graphique des individus sur le 1er plan de l'ACP
jpeg(filename = "figures/GENERAL_ACP_biplot_methode2.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, col.ind = as.factor(secteurs), geom = c("point","text"), 
                            col.var = "black", 
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()



# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data_faune, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))


# Graphique des individus sur le 1er plan de l'ACP
jpeg(filename = "figures/GENERAL_ACP_biplot_methode2_faune.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, col.ind = as.factor(secteurs), geom = c("point","text"), 
                            col.var = "black", 
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()



# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data_flore, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))


# Graphique des individus sur le 1er plan de l'ACP
jpeg(filename = "figures/GENERAL_ACP_biplot_methode2_flore.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, col.ind = as.factor(secteurs), geom = c("point","text"), 
                            col.var = "black", 
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()






###################
### ACP INDICES ###
###################


# ACP sur les indices
data <- indices_stations[,c("Station","Bizien_all","Bizien_maerl","Mvth_all","Mvth_maerl","Vit_all","Vit_maerl")]

# On définis les secteurs pour toutes les stations
secteurs <- unname(sapply(data$Station, function(st) metadata$Secteur[metadata$Station == st]))

# On supprime les noms de stations pour la suite
row.names(data) <- data$Station
data <- data[,-1]



# On rescale entre 0 et 1 toutes les variables
data <- data %>%
  mutate(across(where(is.numeric), ~ rescale(.)))

# ACP avec couleurs en fonction des clusters
PCA <- stats::princomp(data, cor = TRUE)

# Pourcentage d'explication de variance par chaque axe
barplot(100 * PCA$sdev^2 / sum(PCA$sdev^2))
cumsum(100 * (PCA$sdev^2) / sum(PCA$sdev^2))

# Plot de l'ACP
jpeg(filename = "figures/GENERAL_ACP_indices.jpg", width = 2000, 1000)
factoextra::fviz_pca_biplot(PCA, col.ind = as.factor(secteurs), geom = c("point","text"), 
                            col.var = "black", 
                            labelsize = 7,
                            ggtheme = theme_minimal() +
                              theme(legend.title = element_blank(),
                                    text = element_text(size = 30),
                                    axis.text = element_text(size = 25,angle=0,hjust=1,vjust=0.3)))
dev.off()


# Récupération des valeurs d'un indicateurs synthétique de l'état de santé = 1er axe de l'ACP
indices_stations$Etat_sante_global <- as.vector(as.matrix(indices_stations[,c("Bizien_all", "Bizien_maerl", "Mvth_all", "Mvth_maerl", "Vit_all", "Vit_maerl")]) %*% PCA$loadings[,1])
data$Etat_sante_global <- as.vector(as.matrix(data[,c("Bizien_all", "Bizien_maerl", "Mvth_all", "Mvth_maerl", "Vit_all", "Vit_maerl")]) %*% PCA$loadings[,1])

write.table(indices_stations, "outputs/Indices_stations_all_global.csv", sep = ";", dec = ".", col.names = FALSE)

# Plot des corrélations
data_corr <- data[,1:6]
colnames(data_corr) <- c("Bizien - tous", "Bizien - maërl",
                         "Mvth - tous", "Mvth - maërl",
                         "Vitalité - tous", "Vitalité - maërl")#, "Etat de santé global")


jpeg(filename = "figures/GENERAL_corrplotindices.jpg", width = 500, 500)
corrplot::corrplot(cor(data_corr, method = "spearman"),
                   method = "circle",       
                   order = "hclust",         # Ordering method of the matrix
                   hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
                   addrect = 2,              # If order = "hclust", number of cluster rectangles
                   rect.col = 3,             # Color of the rectangles
                   rect.lwd = 2,
                   tl.cex = 2)  
dev.off()


cor(data$Vit_all, data$Bizien_all, method = "spearman")
cor(data$Vit_all, data$Mvth_all, method = "spearman")
cor(data$Bizien_all, data$Mvth_all, method = "spearman")

cor(data$Vit_maerl, data$Bizien_maerl, method = "spearman")
cor(data$Vit_maerl, data$Mvth_maerl, method = "spearman")
cor(data$Bizien_maerl, data$Mvth_maerl, method = "spearman")

# Coefficient négatif
cor(data$Vit_all, data$Mvth_maerl, method = "spearman")


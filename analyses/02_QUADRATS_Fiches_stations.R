
library(ggplot2);library(dplyr);library(stringr);library(RColorBrewer);library(tidyr);library(ggExtra);library(svDialogs)
library(leaflet)
library(leaflet.extras)

load("outputs/01_data.RData")

# On ajoute les autres stations de CAREMAERL pour avoir des histogrammes plus complets
stations <- unique(indices_stations$Station)
caremaerl <- read.table("../CAREMaerl_2024/QUADRATS/R/output/01_Methode1_indices_stations.csv", sep = ";", dec = ".", header = TRUE)
indices_stations <- rbind(indices_stations, caremaerl)

cols_etat <- c("Mauvais" = "red",
               "Moyen" = "yellow",
               "Moyen / Bon" = "lightgreen",
               "Bon" = "green",
               "Tres bon" = "darkgreen")

# Pour toutes les stations
for (station in stations){
  
  # Description site
  profondeur <- gsub("[.]",",", as.character(indices_stations$Profondeur[indices_stations$Station == station]))
  secteur <- indices_stations$Secteur[indices_stations$Station == station]
  
  maerl <- sum(comMat_substrat_stations[comMat_substrat_stations$Station == station, grep("Maerl", colnames(comMat_substrat_stations))])
  pralines <- sum(comMat_substrat_stations[comMat_substrat_stations$Station == station, grep("Pralines", colnames(comMat_substrat_stations))])
  boxwork <- sum(comMat_substrat_stations[comMat_substrat_stations$Station == station, grep("Boxwork", colnames(comMat_substrat_stations))])
  peyssonneliaceae <- sum(comMat_substrat_stations[comMat_substrat_stations$Station == station, "Peyssonneliaceae"])
  substrat <- c("Maerl" = maerl, 
                "Pralines" = pralines,
                "Boxwork" = boxwork,
                "Peyssonneliaceae" = peyssonneliaceae)
  dominante <- names(substrat)[which.max(substrat)]
  
  # Autres classes secondaires n'appartenant pas à la classe dominante (mais > 1%)
  autres <- colnames(comMat_substrat_stations)[-grep(dominante, colnames(comMat_substrat_stations))]
  autres <- comMat_substrat_stations[comMat_substrat_stations$Station == station, autres]
  autres$Station <- NULL
  autres <- t(autres)
  autres <- data.frame("Categorie" = rownames(autres),
                       "MeanCover" = autres[,1])
  rownames(autres) <- NULL
  autres <- autres[autres$MeanCover > 1,]
  
  if (nrow(autres) > 0){
    
    autres <- autres[order(autres$MeanCover, decreasing = TRUE),]
    autres <- lapply(1:nrow(autres), function(i){
      
      categorie <- autres$Categorie[i]
      meancover <- round(autres$MeanCover[i], 1)
      
      meancover <- gsub("[.]", ",", as.character(meancover))
      
      res <- paste0(categorie, " (", meancover," %)")
      
      return(res)
      
    })
    autres <- paste(autres, collapse = ", ")
    
  }else{
    
    autres <- NA
    
  }
 
  # On corrige l'orthographe de maerl
  if (dominante == "Maerl"){dominante = "Maërl"}
  
  # Méthode 1
  indice_bizien_tous <- round(indices_stations$Bizien_all[indices_stations$Station == station], 2)
  indice_bizien_maerl <- round(indices_stations$Bizien_maerl[indices_stations$Station == station], 2)
  indice_Mvth_tous <- round(indices_stations$Mvth_all[indices_stations$Station == station], 2)
  indice_Mvth_maerl <- round(indices_stations$Mvth_maerl[indices_stations$Station == station], 2)
  indice_Vit_tous <- round(indices_stations$Vit_all[indices_stations$Station == station], 2)
  indice_Vit_maerl <- round(indices_stations$Vit_maerl[indices_stations$Station == station], 2)
  
  # Transformation en string avec virgule
  indice_bizien_tous <- gsub("[.]",",",as.character(indice_bizien_tous))
  indice_bizien_maerl <- gsub("[.]",",",as.character(indice_bizien_maerl))
  indice_Mvth_tous <- gsub("[.]",",",as.character(indice_Mvth_tous))
  indice_Mvth_maerl <- gsub("[.]",",",as.character(indice_Mvth_maerl))
  indice_Vit_tous <- gsub("[.]",",",as.character(indice_Vit_tous))
  indice_Vit_maerl <- gsub("[.]",",",as.character(indice_Vit_maerl))
  
  etat_bizien_tous <- indices_stations$E_Bizien_all[indices_stations$Station == station]
  etat_bizien_maerl <- indices_stations$E_Bizien_maerl[indices_stations$Station == station]
  etat_Mvth_tous <- indices_stations$E_Mvth_all[indices_stations$Station == station]
  etat_Mvth_maerl <- indices_stations$E_Mvth_maerl[indices_stations$Station == station] 
  etat_Vit_tous <- indices_stations$E_Vit_all[indices_stations$Station == station]
  etat_Vit_maerl <- indices_stations$E_Vit_maerl[indices_stations$Station == station] 
  
  # Méthode 2
  taxons <- comMat_bio_stations[comMat_bio_stations$Station == station,2:ncol(comMat_bio_stations)]
  taxons <- names(taxons)[taxons > 0]
  
  # On retire les taxons qui ne sont pas faune ni flore
  taxons <- taxons[!(taxons %in% c("Crevice", "Other_live", "Sand", "Shadow", "Unknowns", "Sludge", "Tape", "Wand", "Rubble"))]
  nb_taxons <- length(taxons)
  
  taxons_flore <- taxons[which(is.element(taxons, especes$Espece[especes$Type == "Flore"]))]
  taxons_faune <- taxons[which(is.element(taxons, especes$Espece[especes$Type == "Faune"]))]
  
  if (length(grep("imJ", station)) == 0){
    
    # Sortie tableau de la station
    obs_station <- donnees_cpce_raw[which(donnees_cpce_raw$Station == station & donnees_cpce_raw$major_category != "Tape, wand, shadow"),
                                    c("ID_code","Station","major_category")]
    
    # Nombre d'observations total pour la station, en excluant TWS
    nb_obs <- nrow(obs_station)
    
    obs_station <- aggregate(Station~., obs_station, length)
    obs_station <- merge(obs_station, BDD_especes[, c("ID_code", "id_name")], 
                           all.x = TRUE, all.y = FALSE, by = "ID_code")
    obs_station$Type <- NA
    obs_station$Type[obs_station$ID_code %in% colnames(comMat_faune_stations)] <- "Faune"
    obs_station$Type[obs_station$ID_code %in% colnames(comMat_flore_stations)] <- "Flore"
    obs_station$Type[obs_station$ID_code %in% c("R","S","P","D","C")] <- "Sediment"
    obs_station$Type[obs_station$ID_code %in% "UNK"] <- "Non identifie"
    obs_station$Pourcentage <- round(obs_station$Station / sum(obs_station$Station) * 100,2)
    obs_station <- obs_station[,c("id_name","Type","Pourcentage")]
    colnames(obs_station) <- c("Classe","Type","Pourcentage")
    
    write.table(obs_station, paste0("outputs/Pourcentages_recouvrement_station_", station, ".csv"), sep = ";", dec = ".", row.names = FALSE)

  }
  
  # Flore
  if (length(taxons_flore) > 0){
    
    tableau_flore <- comMat_flore_stations[comMat_flore_stations$Station == station,]
    tableau_flore <- t(tableau_flore[, taxons_flore])
    tableau_flore <- data.frame("Espece" = rownames(tableau_flore),
                                "Nb_obs" = round(tableau_flore[,1], 1))
    rownames(tableau_flore) <- NULL
    tableau_flore <- merge(tableau_flore, especes[, c("Phylum", "Classe", "Espece", "Groupe")], all.x = TRUE, all.y = FALSE)
    tableau_flore <- tableau_flore[, c("Espece","Phylum", "Classe", "Groupe","Nb_obs")]
    groupe_dominant_flore <- aggregate(Nb_obs ~ Groupe, tableau_flore, sum)
    groupe_dominant_flore <- groupe_dominant_flore$Groupe[which.max(groupe_dominant_flore$Nb_obs)]
    tableau_flore$Nb_obs <- round(tableau_flore$Nb_obs / nb_obs * 100, 2)
    colnames(tableau_flore) <- c("Espece", "Phylum", "Classe", "Groupe morpho", "%")
    tableau_flore$Espece <- gsub("_"," ", tableau_flore$Espece)
    nb_taxons_flore <- nrow(tableau_flore)
    
  }else{
    
    nb_taxons_flore <- 0
  }

  
  # Faune
  if (length(taxons_faune) > 0){
    
    tableau_faune <- comMat_faune_stations[comMat_faune_stations$Station == station,]
    tableau_faune <- t(tableau_faune[, taxons_faune])
    tableau_faune <- data.frame("Espece" = rownames(tableau_faune),
                                "Nb_obs" = round(tableau_faune[,1], 1))
    rownames(tableau_faune) <- NULL
    tableau_faune <- merge(tableau_faune, especes[, c("Phylum", "Classe", "Espece", "Groupe")], all.x = TRUE, all.y = FALSE)
    tableau_faune <- tableau_faune[, c("Espece","Phylum", "Classe", "Groupe","Nb_obs")]
    groupe_dominant_faune <- aggregate(Nb_obs ~ Groupe, tableau_faune, sum)
    groupe_dominant_faune <- groupe_dominant_faune$Groupe[which.max(groupe_dominant_faune$Nb_obs)]
    tableau_faune$Nb_obs <- round(tableau_faune$Nb_obs / nb_obs * 100, 2)
    colnames(tableau_faune) <- c("Espece", "Phylum", "Classe", "Groupe morpho", "%")
    tableau_faune$Espece <- gsub("_"," ", tableau_faune$Espece)
    nb_taxons_faune <- nrow(tableau_faune)
    
  }else{
    
    nb_taxons_faune <- 0
  }

  # Groupes morpho
  groupes_morpho <- comMat_morpho_stations[comMat_morpho_stations$Station == station,2:ncol(comMat_morpho_stations)]
  groupes_morpho <- names(groupes_morpho)[groupes_morpho > 0]
  nb_groupes_morpho <- length(groupes_morpho) 
  
  
  # Barplot méthode 1 station
  jpeg(filename = paste0("figures/Barplot_recouvrement_methode1_", station, ".jpg"), width = 1000, height = 700)
    gg <- ggplot(data = donnees_rhodo_stations[donnees_rhodo_stations$Station == station,], aes(x = Category, y = MeanPercent))+
      geom_bar(stat="identity", position=position_dodge(),lwd=0)+
      geom_errorbar(aes(ymin=MeanPercent, ymax=MeanPercent+SdCover), width=.2,position=position_dodge(.9),lwd=0)+
      scale_x_discrete(drop=FALSE)+
      xlab("")+
      ylab("Pourcentage de couverture moyenne")+
      theme_bw()+
      theme(axis.text.x = element_text(size = 15,angle=90,hjust=1,vjust=0.3),
            axis.title = element_text(size = 20),
            panel.grid.major.x = element_blank(),
            panel.grid = element_line(size=0), 
            text=element_text(family="sans", size=15))
  
    plot(gg)
  dev.off()
  
  ### Histogramme Bizien - all
  # jpeg(filename = paste0("figures/Histogramme_indice_Bizien_tous_", station, ".jpg"), width = 700, height = 500)
  #   hist(x = indices_stations$Bizien_all,
  #        xlab = "Indice Bizien - tous",
  #        ylab = "Nombre de stations",
  #        main = "",
  #        cex = 2)
  #   
  #   abline(v = indices_stations$Bizien_all[indices_stations$Station == station], col = 'red', lwd = 2, lty = 'dashed')
  #   
  #   indice_station <- round(indices_stations$Bizien_all[indices_stations$Station == station], 2)
  #   
  #   text(x = indice_station,
  #        y = 1,
  #        labels = paste0(indices_stations$E_Bizien_all[indices_stations$Station == station], " (", indice_station, ")"),
  #        col = "red",
  #        font = 2, 
  #        cex = 2)
  #   
  # dev.off()
  
  
  gg_Bizien_all <- ggplot(indices_stations, aes(x = Bizien_all)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Bizien_all[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Bizien_all[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Bizien_all[indices_stations$Station == station], " : ",
                           round(indices_stations$Bizien_all[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Bizien_all[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Bizien - tous",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  gg_Bizien_maerl <- ggplot(indices_stations, aes(x = Bizien_maerl)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Bizien_maerl[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Bizien_maerl[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Bizien_maerl[indices_stations$Station == station], " : ",
                            round(indices_stations$Bizien_maerl[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Bizien_maerl[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Bizien - maërl",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  gg_Mvth_all <- ggplot(indices_stations, aes(x = Mvth_all)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Mvth_all[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Mvth_all[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Mvth_all[indices_stations$Station == station], " : ",
                            round(indices_stations$Mvth_all[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Mvth_all[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Mvth - tous",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  gg_Mvth_maerl <- ggplot(indices_stations, aes(x = Mvth_maerl)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Mvth_maerl[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Mvth_maerl[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Mvth_maerl[indices_stations$Station == station], " : ",
                            round(indices_stations$Mvth_maerl[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Mvth_maerl[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Mvth - maërl",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  gg_Vit_all <- ggplot(indices_stations, aes(x = Vit_all)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Vit_all[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Vit_all[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Vit_all[indices_stations$Station == station], " : ",
                            round(indices_stations$Vit_all[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Vit_all[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Vitalité - tous",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  gg_Vit_maerl <- ggplot(indices_stations, aes(x = Vit_maerl)) +
    geom_histogram(binwidth = 0.1, fill = "#083D63", color = "black") +
    geom_vline(xintercept = indices_stations$Vit_maerl[indices_stations$Station == station], linetype = "dashed", color = "red", size = 2) +
    annotate("text", 
             x = indices_stations$Vit_maerl[indices_stations$Station == station], 
             y = 1, 
             label = paste0(indices_stations$E_Vit_maerl[indices_stations$Station == station], " : ",
                            round(indices_stations$Vit_maerl[indices_stations$Station == station], 2)),
             vjust = -0.5, 
             hjust = "left", 
             color = cols_etat[indices_stations$E_Vit_maerl[indices_stations$Station == station]], 
             fontface  = 2,
             size = 12,
             angle = 90) +
    labs(title = "",
         x = "Indice Vitalité - maërl",
         y = "Nombre de stations") +
    theme_minimal() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 25))
  
  jpeg(paste0("figures/Histogramme_indices_", station, ".jpg"), width = 1500, height = 1000)
    composite_plot <- cowplot::plot_grid(gg_Bizien_all, gg_Mvth_all, gg_Vit_all,
                                gg_Bizien_maerl, gg_Mvth_maerl, gg_Vit_maerl,
                                labels = NA, ncol = 3, nrow = 2)
    print(composite_plot)
  dev.off()
  
  
  ################################
  ### GENERATION FICHE STATION ###
  ################################
  
  # Stockage des variables nécessaires à la génération de la fiche du site st
  save.image(file = "temp.RData")
  
  # Rendu de la fiche
  rmarkdown::render("R/Fiche_site.Rmd", output_file = paste0("../outputs/Fiche_", station,".docx"))
  
  # Suppression du stockage des variables
  file.remove("temp.RData")
  
}



###################################
### Export fiche secteur PNMCCA ###
###################################

stations <- stations[-grep("imJ", stations)]

for (secteur in "PNMCCA"){
  
  taxons <- colnames(comMat_bio_stations)
  nb_taxons <- length(taxons)

  
  taxons_flore <- taxons[which(is.element(taxons, especes$Espece[especes$Type == "Flore"]))]
  taxons_faune <- taxons[which(is.element(taxons, especes$Espece[especes$Type == "Faune"]))]
  
  # Flore
  if (length(taxons_flore) > 0){
    
    tableau_flore <- comMat_flore_stations[comMat_flore_stations$Station %in% stations,]
    tableau_flore <- data.frame("Espece" = colnames(tableau_flore[,2:ncol(tableau_flore)]),
                                "Nb_obs" = apply(tableau_flore[,2:ncol(tableau_flore)], 2, sum))
    rownames(tableau_flore) <- NULL
    tableau_flore <- merge(tableau_flore, especes[, c("Phylum", "Classe", "Espece", "Groupe")], all.x = TRUE, all.y = FALSE)
    tableau_flore <- tableau_flore[, c("Espece","Phylum", "Classe", "Groupe","Nb_obs")]

    groupe_dominant_flore <- aggregate(Nb_obs ~ Groupe, tableau_flore, sum)
    groupe_dominant_flore <- groupe_dominant_flore$Groupe[which.max(groupe_dominant_flore$Nb_obs)]
    
    colnames(tableau_flore) <- c("Espece", "Phylum", "Classe", "Groupe morpho", "Nb obs")
    tableau_flore$Espece <- gsub("_"," ", tableau_flore$Espece)
    tableau_flore$Phylum <- gsub("_"," ", tableau_flore$Phylum)
    nb_taxons_flore <- nrow(tableau_flore)
    
  }else{
    
    nb_taxons_flore <- 0
  }
  
  
  # Faune
  if (length(taxons_faune) > 0){
    
    tableau_faune <- comMat_faune_stations[comMat_faune_stations$Station %in% stations,]
    tableau_faune <- data.frame("Espece" = colnames(tableau_faune[,2:ncol(tableau_faune)]),
                                "Nb_obs" = apply(tableau_faune[,2:ncol(tableau_faune)], 2, sum))
    rownames(tableau_faune) <- NULL
    tableau_faune <- merge(tableau_faune, especes[, c("Phylum", "Classe", "Espece", "Groupe")], all.x = TRUE, all.y = FALSE)
    tableau_faune <- tableau_faune[, c("Espece","Phylum", "Classe", "Groupe","Nb_obs")]

    groupe_dominant_faune <- aggregate(Nb_obs ~ Groupe, tableau_faune, sum)
    groupe_dominant_faune <- groupe_dominant_faune$Groupe[which.max(groupe_dominant_faune$Nb_obs)]
    
    colnames(tableau_faune) <- c("Espece", "Phylum", "Classe", "Groupe morpho", "Nb obs")
    tableau_faune$Espece <- gsub("_"," ", tableau_faune$Espece)
    tableau_faune$Phylum <- gsub("_"," ", tableau_faune$Phylum)
    nb_taxons_faune <- nrow(tableau_faune)
    
  }else{
    
    nb_taxons_faune <- 0
  }
  
  
  # Groupes morpho
  groupes_morpho <- comMat_morpho_stations[comMat_morpho_stations$Station %in% stations,2:ncol(comMat_morpho_stations)]
  groupes_morpho <- names(groupes_morpho)[apply(groupes_morpho, 2, sum) > 0]
  nb_groupes_morpho <- length(groupes_morpho) 
  
  save.image(file = "temp.RData")
  
  # Rendu de la fiche
  rmarkdown::render("R/Fiche_secteur.Rmd", output_file = paste0("../outputs/Fiche_secteur_",secteur,".docx"))
  
  # Suppression du stockage des variables
  file.remove("temp.RData")
  
}#eo for secteur



compute_indices_rhodo <- function(data, type = "maerl"){
  
  # Dataframe pour stocker les résultats
  df_indice <- data[,c(1,2)] %>%
    group_by(Station,QuadratName) %>%
    distinct()
  
  # Calcul pour chaque quadrat
  for (quadrat in unique(df_indice$QuadratName)) {
    
    # Calcul des proportions de chaque catégorie
    A <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Algues", drop = FALSE)
    Fauna <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Faune", drop = FALSE)
    
    if (type == "maerl"){
      
      MV <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Maerl vivant", drop = FALSE)
      MVS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Maerl vivant x Sediment", drop = FALSE)
      MVMM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Maerl vivant x Maerl mort", drop = FALSE)
      MM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Maerl mort", drop = FALSE)
      MMS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Maerl mort x Sediment", drop = FALSE)
      
    }else if (type == "pralines"){
      
      MV <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Pralines vivantes", drop = FALSE)
      MVS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Pralines vivantes x Sediment", drop = FALSE)
      MVMM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Pralines vivantes x Pralines mortes", drop = FALSE)
      MM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Pralines mortes", drop = FALSE)
      MMS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Pralines mortes x Sediment", drop = FALSE)
      
    }else if (type == "boxwork"){
      
      MV <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Boxwork vivant", drop = FALSE)
      MVS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Boxwork vivant x Sediment", drop = FALSE)
      MVMM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Boxwork vivant x Boxwork mort", drop = FALSE)
      MM <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Boxwork mort", drop = FALSE)
      MMS <- subset(data$PercentArea, data$QuadratName == quadrat & data$Category == "Boxwork mort x Sediment", drop = FALSE)
      
    }else if (type == "all"){
      
      MV <- sum(subset(data$PercentArea, data$QuadratName == quadrat & data$Category %in% c("Maerl vivant", "Pralines vivantes", "Boxwork vivant", "Peyssonneliaceae"), drop = FALSE))
      MVS <- sum(subset(data$PercentArea, data$QuadratName == quadrat & data$Category %in% c("Maerl vivant x Sediment", "Pralines vivantes x Sediment", "Boxwork vivant x Sediment"), drop = FALSE))
      MVMM <- sum(subset(data$PercentArea, data$QuadratName == quadrat & data$Category %in% c("Maerl vivant x Maerl mort", "Pralines vivantes x Pralines mortes", "Boxwork vivant x Boxwork mort"), drop = FALSE))
      MM <- sum(subset(data$PercentArea, data$QuadratName == quadrat & data$Category %in% c("Maerl mort", "Pralines mortes", "Boxwork mort"), drop = FALSE))
      MMS <- sum(subset(data$PercentArea, data$QuadratName == quadrat & data$Category %in% c("Maerl mort x Sediment", "Pralines mortes x Sediment", "Boxwork mort x Sediment"), drop = FALSE))
      
    }#eo if
    
    # Si pas de valeur, on fixe à 0
    A <- ifelse(length(A) == 0, 0, A)
    Fauna <- ifelse(length(Fauna) == 0, 0, Fauna)
    MV <- ifelse(length(MV) == 0, 0, MV)
    MVS <- ifelse(length(MVS) == 0, 0, MVS)
    MVMM <- ifelse(length(MVMM) == 0, 0, MVMM)
    MM <- ifelse(length(MM) == 0, 0, MM)
    MMS <- ifelse(length(MMS) == 0, 0, MMS)
    
    # Indice IUEM 
    R <- (100 - (Fauna + MV + MVS))
    indice_IUEM = ((Fauna / 100) + (MV / 100) + (MVS / 100) - (R / 100) + 1) / 2
    df_indice[df_indice$QuadratName == quadrat, "Indice_IUEM"] <- round(indice_IUEM, digits = 4)
    
    # Indice Mvth
    Mvth = (MV + MVS / 2 + Fauna / 3 + A / 3 + MVMM / 2) / 100
    df_indice[df_indice$QuadratName == quadrat, "Indice_Mvth"] <- round(Mvth, digits = 4)
    
    # Indice vitalité
    # MVMMth = MV + MM + MVMM + MVS / 2 + MMS / 2
    # vitalite = Mvth * 100 / MVMMth
    # df_indice[df_indice$QuadratName == quadrat, "Indice_vitalite"] <- round(vitalite, digits = 4)
    
  }#eo for
  
  return(df_indice)
  
}#eo function compute_indices





compute_biodiversity <- function(data){
  
  vegan::diversity()
  
}

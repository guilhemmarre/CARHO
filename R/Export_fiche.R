
export_fiche <- function(station, output_file){
  
  ##################
  ### GRAPHIQUES ###
  ##################
  

  
  # Stockage des variables nécessaires à la génération de la fiche du site st
  save(st, fdp_df, BDD_CALOR, data.hour.site, data.day.site, data.month.site, max_obs, file = "temp.RData")
  
  # Rendu de la fiche
  rmarkdown::render("R/Fiche_site.Rmd", output_file = output_file)
  
  # Suppression du stockage des variables
  file.remove("temp.RData")
  
}#eo export_fiche


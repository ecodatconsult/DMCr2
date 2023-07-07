checkStandorteNamesCorr <- function(standorte_import_new_names_corr_auto_sf, standorte_names_corr_manual_df){

  namesSF <- names(standorte_import_new_names_corr_auto_sf)

  namesSF[na.omit(match(standorte_names_corr_manual_df$alt, namesSF))] <-  standorte_names_corr_manual_df$korrigert[which(standorte_names_corr_manual_df$alt %in% namesSF)]


  names(standorte_import_new_names_corr_auto_sf) <- namesSF
  sf::st_geometry(standorte_import_new_names_corr_auto_sf) <- "geom"

  return(standorte_import_new_names_corr_auto_sf)
}

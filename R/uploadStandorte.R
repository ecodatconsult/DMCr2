#' Standorte der Session werden in die FVA-Fotofallendatenbank (fotofallen.fotofallen_standorte_import) hochgeladen (PostgreSQL)
#'
#' @param upload_sf simple feature Objekt, Fotofallenstandorte mit korrigierten bzw. überprüften Spaltenbezeichnungen
#' @export
#'

uploadStandorte <- function(upload_sf){

  con <- dbConnection()

  standort_ids_in_DB <- c(
    RPostgreSQL::dbGetQuery(con,"SELECT standort_id FROM fotofallen.fotofallen_standorte;")$standort_id,
    RPostgreSQL::dbGetQuery(con,"SELECT standort_id FROM fotofallen.fotofallen_standorte_import;")$standort_id) %>% unique()

  if(all(upload_sf$standort_id %in% standort_ids_in_DB)){
    message("Alle Standorte sind bereits in der Datenbank (fotofallen.fotofallen_standorte oder fotofallen.fotofallen_standorte_import). Es erfolgt kein Upload!")
  }else{
    upload_sf %>%
      dplyr::relocate(getInfoDB(type = "column_name", schema = "fotofallen", table = "fotofallen_standorte_import") %>% as.vector(), names(upload_sf)) %>%
      dplyr::filter(!standort_id %in% standort_ids_in_DB) %>%
      dplyr::mutate(bilderordner = NA,
                    standort_id_org = NA,
                    sichtfeld_entfernung = NA) %>% #TODO: clarify purpose
      sf::st_write(dsn = con,
                   layer = c("fotofallen", "fotofallen_standorte_import"), #TODO make sure this goes to fotofallen.fotofallen_standorte_import and not (as previously stated) to import.fotofallen_standorte_import
                   delete_layer = FALSE,
                   append = TRUE)
  }

  RPostgreSQL::dbDisconnect(con)
}

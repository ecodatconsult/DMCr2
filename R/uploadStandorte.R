uploadStandorte <- function(upload_sf){

  con <- dbConnection()

  upload_sf %>%
    dplyr::filter(!standort_id %in% RPostgreSQL::dbGetQuery(con,"SELECT standort_id FROM import.fotofallen_standorte;")$standort_id) %>%
    dplyr::mutate(bilderordner = NA,
                  standort_id_org = NA,
                  sichtfeld_entfernung = NA) %>% #TODO: clarify purpose
    sf::st_write(dsn = con,
                 DBI::Id(schema = "import",
                         table = "fotofallen_standorte_import"),
                 delete_layer = FALSE,
                 append = TRUE)

  dbDisconnect(con)
}

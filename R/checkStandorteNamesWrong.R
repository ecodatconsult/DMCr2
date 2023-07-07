checkStandorteNamesWrong <- function(standorte_import_new_sf){

  con <- dbConnection()

  #read column names from database via INFORMATION_SCHEMA
  namesDB <- RPostgreSQL::dbGetQuery(con, paste("SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS
                                                  WHERE TABLE_NAME = N'fotofallen_standorte_import' AND TABLE_SCHEMA = 'fotofallen';")) %>%
    purrr::as_vector()

  RPostgreSQL::dbDisconnect(con)

  #comment Alex: these common wrong names most certainly result from exporting geodata as shp
  wr_co <- data.frame(wrong = c("standort_i", "ts_kamera_", "ts_kamera_.1", "ts_kamer_1" ,"kamera_aus","kamera_hoe","sichtfeld_","standortfo", "aufgehaeng", "bemerkunge", "bilderordn", "standort_1", "sichtfel_1", "geometry"),
             correct = c("standort_id","ts_kamera_start", "ts_kamera_ende", "ts_kamera_ende", "kamera_ausrichtung", "kamera_hoehe", "sichtfeld_hoehe", "standortfotos", "aufgehaengt_von", "bemerkungen", "bilderordner", "standort_id_org", "sichtfeld_entfernung", "geom"))

  namesSF <- names(standorte_import_new_sf)

  #correct common wrong names
  namesSF[na.omit(match(wr_co$wrong, namesSF))] <-  wr_co$correct[which(wr_co$wrong %in% namesSF)]

  names(standorte_import_new_sf) <- namesSF
  sf::st_geometry(standorte_import_new_sf) <- "geom"

  #wrong names
  falsche_names <- namesSF[!is.element(namesSF,namesDB)]
  #TODO: indicate missing names in ShinyApp
  missing_names <- namesDB[!is.element(namesDB, namesSF)]

  return(list(wrong_names_df = dplyr::tibble("alt" = falsche_names, korrigiert = NA),
              missing_names = missing_names,
              standorte_import_new_sf_names_corr = standorte_import_new_sf))
}

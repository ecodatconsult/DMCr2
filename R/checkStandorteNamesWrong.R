checkStandorteNamesWrong <- function(){

  require(RPostgreSQL)
  require(DBI)
  require(sf)
  require(dplyr)
  require(rpostgis)

  dbConnection(type = "read")

  #namesDB
  namesDB <- names(pgGetGeom(con, query = paste("SELECT * FROM fotofallen.fotofallen_standorte_import;")))

  #namesSF
  namesSF <- names(standorte_import_new_sf)

  #correct common wrong names,
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "standort_i"] <- "standort_id"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "ts_kamera_"] <- "ts_kamera_start"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "ts_kamera_.1"] <- "ts_kamera_ende"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "kamera_aus"] <- "kamera_ausrichtung"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "kamera_hoe"] <- "kamera_hoehe"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "sichtfeld_"] <- "sichtfeld_hoehe"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "standortfo"] <- "standortfotos"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "aufgehaeng"] <- "aufgehaengt_von"
  names(standorte_import_new_sf)[names(standorte_import_new_sf) == "bemerkunge"] <- "bemerkungen"

  namesSF <- names(standorte_import_new_sf)

  #wrong names
  falsche_names <- namesSF[!is.element(namesSF,namesDB)]

  #data table
  if(length(falsche_names)>0){
    falsche_names_df <- data.frame("alt" = falsche_names, "korrigiert" = NA)
    assign("falsche_names_df",falsche_names_df,envir = .GlobalEnv)
  }
}

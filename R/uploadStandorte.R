uploadStandorte <- function(){

  require(RPostgreSQL)
  require(DBI)
  require(sf)
  require(dplyr)
  require(rpostgis)
  require(raster)

  dbConnection(type = "write")

  if(exists(c("standorte_import_new_sf","standorte_import_DB_sf"),envir = .GlobalEnv)){

  # correct colnames
  standorte_import_new_sf$bilderordner <- NA
  standorte_import_new_sf$standort_id_org <- NA
  standorte_import_new_sf$sichtfeld_entfernung <- NA

  # merge shapefiles
  all_standorte_import <- raster::union(standorte_import_DB_sf,standorte_import_new_sf)

  # delete duplicated Standorte
  all_standorte_import <-all_standorte_import[which(!duplicated(all_standorte_import$standort_id)), ]

  # upload
  pgInsert(con, c("import","fotofallen_standorte_import"), all_standorte_import, overwrite = TRUE)

  }
  dbDisconnect(con)
}

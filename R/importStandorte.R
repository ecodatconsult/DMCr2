importStandorte <- function(file_path, project){

  require(RPostgreSQL)
  require(DBI)
  require(sf)
  require(dplyr)
  require(stringi)
  require(rgdal)
  require(raster)
  require(rworldmap)
  require(RPostgreSQL)
  require(rpostgis)

  dbConnection(type = "read")

  ## Database
    # data frame Projekte
    df_projects <- dbGetQuery(con,"SELECT * FROM fotofallen.projekte")

    #project_id
    #project = "Grünbrückenmonitoring"
    project_id <- df_projects$projekt_id[df_projects$projekt_name == project]

    # data frame fotofallen_standorte
    standorte <- dbGetQuery(con, paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id = '", stri_enc_toutf8(project_id), "';", sep = ""))
    standorte$projekt_name <- project
    assign("standorte_DB",standorte, envir = .GlobalEnv)

    # shapefile fotofallen_standorte_import
    standorte_import_sf <- pgGetGeom(con, query = "SELECT * FROM import.fotofallen_standorte;")
    standorte_import_sf <- spTransform(standorte_import_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    assign("standorte_import_DB_sf",standorte_import_sf, envir = .GlobalEnv)


    # shapefile fotofallen_standorte
    standorte_sf <- pgGetGeom(con, query = paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id ='", stri_enc_toutf8(project_id), "';", sep = ""))
    standorte_sf <- spTransform(standorte_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    assign("standorte_DB_sf",standorte_sf, envir = .GlobalEnv)

  ## Neue Standorte
    #file_path <- "N:/Projekte/Forschungscluster_1768/03_Daten/Fotofallendaten_Test/fotofallen_standorte.shp"
    # data frame
    standorte_import_n <- data.frame(st_read(file_path))
    assign("standorte_import_new",standorte_import_n, envir = .GlobalEnv)

    # shapefile
    standorte_import_n_sf <- readOGR(dsn=file_path)
    standorte_import_n_sf <- spTransform(standorte_import_n_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    assign("standorte_import_new_sf",standorte_import_n_sf,envir = .GlobalEnv)

  dbDisconnect(con)
  rm(standorte,standorte_import_sf,standorte_sf,standorte_import_n,standorte_import_n_sf,df_projects)
}

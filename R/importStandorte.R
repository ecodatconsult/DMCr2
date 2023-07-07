importStandorte <- function(file_path, project, target_crs = 25832){

  con <- dbConnection()
  ## Database
    # data frame Projekte
    project_id <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT projekt_id FROM fotofallen.projekte WHERE projekt_name = '", stringi::stri_enc_toutf8(project), "'")) %>%
      purrr::as_vector()

    # data frame fotofallen_standorte
      standorte_DB <- suppressWarnings(sf::st_read(con, query = glue::glue_sql("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id = '", stringi::stri_enc_toutf8(project_id), "';")))

    if(nrow(standorte_DB) > 0) standorte_DB$projekt_name <- project
    #assign("standorte_DB",standorte, envir = .GlobalEnv)

    # shapefile fotofallen_standorte_import
    # TODO: check if select * is needed
    standorte_import_DB_sf <- sf::st_read(con, query = "SELECT * FROM import.fotofallen_standorte;")

    #standorte_import_sf <- spTransform(standorte_import_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    #assign("standorte_import_DB_sf",standorte_import_sf, envir = .GlobalEnv)

    # shapefile fotofallen_standorte
    # REMOVED DUPLICATE OF standorte_DB?!
    # standorte_sf <- pgGetGeom(con, query = paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id ='", stri_enc_toutf8(project_id), "';", sep = ""))
    # standorte_sf <- spTransform(standorte_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    # assign("standorte_DB_sf",standorte_sf, envir = .GlobalEnv)

  ## Neue Standorte
    #file_path <- "N:/Projekte/Forschungscluster_1768/03_Daten/Fotofallendaten_Test/fotofallen_standorte.shp"
    # data frame

    #TODO: Remove duplicate
    # standorte_import_n <- data.frame(st_read(file_path))
    # assign("standorte_import_new",standorte_import_n, envir = .GlobalEnv)
    #
    # # shapefile
    # standorte_import_n_sf <- readOGR(dsn=file_path)
    # standorte_import_n_sf <- spTransform(standorte_import_n_sf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    # assign("standorte_import_new_sf",standorte_import_n_sf,envir = .GlobalEnv)

    standorte_import_new_sf <- sf::st_read(file_path) %>%
      sf::st_transform(target_crs)

  RPostgreSQL::dbDisconnect(con)

  return(list(standorte_import_new_sf = standorte_import_new_sf,
              standorte_import_DB_sf = standorte_import_DB_sf,
              standorte_DB = standorte_DB
              ))

  #rm(standorte,standorte_import_sf,standorte_sf,standorte_import_n,standorte_import_n_sf,df_projects)
}

saveShapefile <- function(save_path, species, project){

  require(sf)

  if(exists("fotofallen_standorte_DB",envir = .GlobalEnv)){
    my_sf <- st_as_sf(fotofallen_standorte_DB)
    st_write(my_sf
             , dsn = save_path
             , layer = paste("Fotofallen_Standorte",species,project,sep = "_")
             , driver = 'ESRI Shapefile'
             , layer_options = "ENCODING=UTF-8")
  }

}

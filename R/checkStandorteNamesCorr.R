checkStandorteNamesCorr <- function(){

  require(RPostgreSQL)
  require(DBI)
  require(sf)
  require(dplyr)
  require(rpostgis)

  #data table
  falsche_names <- falsche_names_df$alt

  #correct wrong names
  if(length(falsche_names[!is.na(falsche_names)])>0){

    for(falscher_name in falsche_names){

      #input correct name
      korrigierter_name <- falsche_names_df$korrigiert[falsche_names_df$alt == falscher_name]

      #correct the name in shapefile
      names(standorte_import_new_sf)[names(standorte_import_new_sf) == falscher_name] <- korrigierter_name
      assign("standorte_import_new_sf",standorte_import_new_sf,envir = .GlobalEnv)
    }
  }

}

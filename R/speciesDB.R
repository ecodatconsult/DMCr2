speciesDB <- function(){

  require(RPostgreSQL)
  require(stringi)

  dbConnection(type = "read")

  #species
  df_ereignisse <- dbGetQuery(con,'SELECT standort_id,tierart_1,tierart_2
                                    FROM fotofallen.ereignisse;')
  species <- sort(unique(c(df_ereignisse$tierart_1,df_ereignisse$tierart_2)))
  species <- species[species != ""]

  assign("speciesDB_vector",species, envir = .GlobalEnv)

  dbDisconnect(con)
}

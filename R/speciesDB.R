speciesDB <- function(){
  con <- dbConnection()

  df_ereignisse <- RPostgreSQL::dbGetQuery(con,'SELECT distinct tierart_1, tierart_2
                                    FROM fotofallen.ereignisse')

  species <- sort(unique(c(df_ereignisse$tierart_1,df_ereignisse$tierart_2)))
  species <- species[species != ""]

  dbDisconnect(con)

  return(species)
}


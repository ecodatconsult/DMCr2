#' Gibt alle Tierarten in fotofallen.ereignisse in der Datenbank an
#'
#' @return character vector
#' @export
#'
#' @examples #' speciesDB()
#'

speciesDB <- function(){
  con <- dbConnection()

  df_ereignisse <- RPostgreSQL::dbGetQuery(con,'SELECT distinct tierart_1, tierart_2
                                    FROM fotofallen.ereignisse')

  species <- sort(unique(c(df_ereignisse$tierart_1,df_ereignisse$tierart_2)))
  species <- species[species != ""]

  DBI::dbDisconnect(con)

  return(species)
}


checkLocationnames <- function(importTables_list){

  con <- dbConnection()

  db_standorte <- RPostgreSQL::dbGetQuery(con, 'SELECT standort_id
                                          FROM fotofallen.fotofallen_standorte
                                          ORDER BY standort_id;')[,1]

  import_standorte <- unique(importTables_list$ereignis$standort_id)

  falsche_standorte <- import_standorte[!import_standorte %in% db_standorte]

  dbDisconnect(con)

  return(data.table::data.table("Standort_ID" = falsche_standorte))

}

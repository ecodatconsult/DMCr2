projectsDB <- function(){
  con <- dbConnection()

  df_projects <- RPostgreSQL::dbGetQuery(con,"SELECT distinct projekt_name FROM fotofallen.projekte ORDER BY projekt_name")

  dbDisconnect(con)

  return(df_projects$projekt_name)
}

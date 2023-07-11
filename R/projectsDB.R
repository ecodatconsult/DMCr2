#' Gibt alle Projekte in alphabetischer Reihenfolge in fotofallen.projekte in der Datenbank an
#'
#' @return character vector
#' @export
#'
#' @examples #' projectsDB()
#'

projectsDB <- function(){
  con <- dbConnection()

  df_projects <- RPostgreSQL::dbGetQuery(con,"SELECT distinct projekt_name FROM fotofallen.projekte ORDER BY projekt_name")

  DBI::dbDisconnect(con)

  return(df_projects$projekt_name)
}

#' Baut eine Verbindung zur FVA-Fotofallendatenbank auf (PostgreSQL).
#'
#'
#' @type character, deprecated
#' @return Formal class PostgreSQLConnection
#' @export
#'
#' @examples
#'
dbConnection <- function(type){

  con <- with(read.csv(system.file("db_login.csv", package = "DMCr")), {
    DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     , user = user
                     , password = pw
                     , host = host
                     , port = port
                     , dbname = db)
  })

  #return(con)

  if(exists("con")){
    assign("con",con, envir = .GlobalEnv)
  }

}

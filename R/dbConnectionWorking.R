#' Eine Probe-Verbindung zur FVA-Fotofallendatenbank mit Schreibrechten, um das Passwort zu überprüfen.
#'
#' @return Boolean
#' @export
#'
#' @examples
#'
#'
dbConnectionWorking <- function(psw){

  tryCatch(
    con_check <- with(read.csv(system.file("db_login.csv", package = "DMCr")),{
      DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     user = user,
                     password = psw,
                     host = host,
                     port = port,
                     dbname = db)
    })

  , error=function(e) {})

  if(exists("con_check")) DBI::dbDisconnect(con_check)

  return(exists("con_check"))
}


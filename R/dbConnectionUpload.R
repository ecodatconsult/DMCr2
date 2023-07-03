#' Eine Probe-Verbindung zur FVA-Fotofallendatenbank mit Schreibrechten, um das Passwort zu überprüfen.
#'
#'
#' @return Formal class PostgreSQLConnection
#' @export
#'
#' @examples
#'
dbConnectionUpload <- function(psw){

  require(RPostgreSQL)
  require(DBI)

  if(exists("con")){
    dbDisconnect(con)
    rm(con, envir = .GlobalEnv)
  }
  
  tryCatch(
    con <- with(read.csv(system.file("db_login.csv", package = "DMCr")), {
      dbConnect("PostgreSQL"
                , user = user
                , password = psw
                , host = host
                , port = port
                , dbname = db)
    })
  , error=function(e) {})

  if(exists("con")){
    con_list1 <- dbListTables(con)
    assign("con_list",con_list1, envir = .GlobalEnv)
    dbDisconnect(con)
    rm(con, con_list1)
  }else{
    con_list1 <- "No"
    assign("con_list",con_list1, envir = .GlobalEnv)
    rm(con_list1)
  }
    
  
}

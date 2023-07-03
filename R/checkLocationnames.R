checkLocationnames <- function(){

  if(!exists("ereignis_import") & !exists("daten_import")){
    stop(print("Benötigte Daten 'ereignis_import' und/oder 'daten_import' fehlen. Führe die Funktion import() aus."))
  }

  require(RPostgreSQL)
  require(stringi)

  dbConnection(type = "read")

  fotofallen_standorte <- dbGetQuery(con, 'SELECT standort_id
                                          FROM fotofallen.fotofallen_standorte
                                          ORDER BY standort_id;') [,1]

   #dbSendQuery(con,
  #            "DELETE FROM fotofallen.fotofallen_standorte_import
  #            WHERE standort_id is null;")

  falsche_standorte <-
    unique(ereignis_import$standort_id)[!is.element(unique(ereignis_import$standort_id),
                                                    fotofallen_standorte)]

  # falsche_standorte <-
  #     unique(daten_import$ordner)[!is.element(unique(daten_import$ordner),
  #                                             fotofallen_standorte)]

  #df
  falsche_standorte_all <- data.table("Standort_ID" = falsche_standorte)
  assign("falsche_standorte_all",falsche_standorte_all,envir = .GlobalEnv)

  if (length(falsche_standorte) >0) {
    print(paste("Folgende Standort-IDs sind nicht in der Tabelle fotofallen_standorte vorhanden: ",
               paste(falsche_standorte, collapse = ", "),sep = ""))
  } else {
    print('Alle Standort_IDs stimmen überein!')
  }

  dbDisconnect(con)

}

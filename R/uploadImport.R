#' Datentabellen (daten und ereignis) der Session werden in die FVA-Fotofallendatenbank hochgeladen (PostgreSQL)
#'
#' @param importTables_list list, Liste mit Daten und Ereignissen als data.tables nach Durchführung der Korrekturfunktionen
#' @return Nothing
#' @import data.table
#' @export
#'
# Fotofallen_Standorte
uploadImport <- function(importTables_list){

  con <- dbConnection()
  #TODO remove folder and standort_id_folder earlier?!
  daten_import <- importTables_list$daten
  daten_import[, c("folder", "standort_id_folder") := NULL,]

  RPostgreSQL::dbWriteTable(con, c("import", "bilder"),
               daten_import, row.names = FALSE, append = TRUE)

  # lösche Daten, die doppelt importiert wurden
  #TODO: check this SQL-code
  RPostgreSQL::dbSendQuery(con, "DELETE FROM import.bilder
  WHERE bild_id IN (SELECT bild_id
  FROM (SELECT bild_id,
  ROW_NUMBER() OVER (partition BY ordner, bildname, ts ORDER BY bild_id DESC) AS rnum
  FROM import.bilder) t
                           WHERE t.rnum > 1);")


  ereignis_import <- importTables_list$ereignis

  #TODO remove pfad, folder and standort_id_folder earlier?!
  ereignis_import[, c("pfad", "folder", "standort_id_folder") := NULL,]

  RPostgreSQL::dbWriteTable(con, c("import", "ereignisse"),
                            ereignis_import, row.names = FALSE, append = TRUE)

   # lösche Daten, die doppelt importiert wurden
  #TODO: check this SQL-code
  RPostgreSQL::dbSendQuery(con, "DELETE FROM import.ereignisse
  WHERE ereignis_id IN (SELECT ereignis_id
  FROM (SELECT ereignis_id, ROW_NUMBER() OVER (partition BY ordner, s_ereignis_id, ts ORDER BY ereignis_id DESC) AS rnum
  FROM import.ereignisse) t
                           WHERE t.rnum > 1);")

  dbDisconnect(con)
}

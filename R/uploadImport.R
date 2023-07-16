#' Datentabellen (daten und ereignis) der Session werden in die FVA-Fotofallendatenbank hochgeladen (PostgreSQL)
#'
#' @param importTables_list list, Liste mit Daten und Ereignissen als data.tables nach Durchführung der Korrekturfunktionen
#' @param schema character, Schema in das der Upload durchgeführt werden soll, Duplikate werden nicht in das gewählte Schema importiert
#' @return Nothing
#' @import data.table
#' @export
#'
# Fotofallen_Standorte
uploadImport <- function(importTables_list, schema = "import"){

  con <- dbConnection()
  # upload bilder
  importTables_list <- processImportTables(importTables(file_path = "inst/test_data/", data = c("daten", "ereignis")))

  locs2upload <- unique(importTables_list$daten$ordner)
  locs_in_db <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("select distinct ordner from ", schema, ".bilder where ordner in ({upload_locs*})", upload_locs = locs2upload, .con = con)) #TODO make sure that ordner is sufficient id!!

  if(any(!locs2upload %in% locs_in_db)){
    daten_import <- importTables_list$daten[!ordner %in% locs_in_db,]

    #TODO remove folder and standort_id_folder earlier?!
    daten_import[, c("folder", "standort_id_folder") := NULL,]

    RPostgreSQL::dbWriteTable(con,
                              c(schema, "bilder"),
                              daten_import,
                              row.names = FALSE,
                              append = TRUE)

    fun_message1 <- paste0(nrow(daten_import), " Zeilen wurden in ", schema, ".bilder hochgeladen!")
  }else{
    fun_message1 <- paste0("Bereits alle Standorte in ", schema, ".bilder vorhanden. Es ist kein Upload erfolgt!")
  }

  # upload ereignisse
  locs2upload <- unique(importTables_list$ereignis$ordner)
  locs_in_db <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("select distinct ordner from ", schema, ".ereignisse where ordner in ({upload_locs*})", upload_locs = locs2upload, .con = con))  #TODO make sure that ordner is sufficient id!!

  if(any(!locs2upload %in% locs_in_db)){
    ereignis_import <- importTables_list$ereignis[!ordner %in% locs_in_db,]

    #TODO remove folder and standort_id_folder earlier?!
    ereignis_import[, c("folder", "standort_id_folder") := NULL,]

    RPostgreSQL::dbWriteTable(con,
                              c(schema, "ereignisse"),
                              ereignis_import,
                              row.names = FALSE,
                              append = TRUE)

    fun_message2 <- paste0(nrow(ereignis_import), " Zeilen wurden in ", schema, ".ereignis hochgeladen!")
  }else{
    fun_message2 <- paste0("Bereits alle Standorte in ", schema, ".ereignis vorhanden. Es ist kein Upload erfolgt!")
  }

  DBI::dbDisconnect(con)

  return(data.frame(
    bilder_upload_message = fun_message1,
    ereignisse_upload_message = fun_message2
  ))
}

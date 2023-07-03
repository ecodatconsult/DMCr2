#' Datentabellen (daten und ereignis) der Session werden in die FVA-Fotofallendatenbank hochgeladen (PostgreSQL)
#'
#' @return
#' @export
#'
#' @examples
#'
# Fotofallen_Standorte
uploadImport <- function(){

  require(RPostgreSQL)
  require(DBI)

  dbConnection(type = "write")

  rm(daten,ereignis)

    daten <- c("daten","ereignis")

    for(data in daten){

      if(data == "daten"){

        daten_import[, folder := NULL,]
        daten_import[, standort_id_folder := NULL,]

        dbWriteTable(con, c("import", "bilder"),
                     daten_import, row.names = FALSE, append = TRUE)

        # lösche Daten, die doppelt importiert wurden
        dbSendQuery(con, "DELETE FROM import.bilder
                    WHERE bild_id IN (SELECT bild_id
                    FROM (SELECT bild_id,
                    ROW_NUMBER() OVER (partition BY ordner, bildname, ts ORDER BY bild_id DESC) AS rnum
                    FROM import.bilder) t
                    WHERE t.rnum > 1);")
      }

      if(data=="ereignis"){

        ereignis_import[, pfad := NULL,]
        ereignis_import[, folder := NULL,]
        ereignis_import[, standort_id_folder := NULL,]

        dbWriteTable(con, c("import", "ereignisse"),
                     ereignis_import, row.names = FALSE, append = TRUE)

        # lösche Daten, die doppelt importiert wurden
        dbSendQuery(con, "DELETE FROM import.ereignisse
                    WHERE ereignis_id IN (SELECT ereignis_id
                    FROM (SELECT ereignis_id,
                    ROW_NUMBER() OVER (partition BY ordner, s_ereignis_id, ts ORDER BY ereignis_id DESC) AS rnum
                    FROM import.ereignisse) t
                    WHERE t.rnum > 1);")
      }
    }

    dbDisconnect(con)

}

#' Datentabellen (daten und ereignis) der Session werden in die FVA-Fotofallendatenbank hochgeladen (PostgreSQL)
#'
#' @return
#' @export
#'
#' @examples
#'
# Fotofallen_Standorte
upload <- function(){

  require(RPostgreSQL)
  require(DBI)

  dbConnection(type = "write")

    daten <- c("daten","ereignis")

    for(data in daten){

      if(data == "daten"){

        daten_import[, folder := NULL,]
        daten_import[, standort_id_folder := NULL,]

        dbWriteTable(con, c("wba", "bilder"),
                     daten_import, row.names = FALSE, append = TRUE)

        # lösche Daten, die doppelt importiert wurden
        dbSendQuery(con, "DELETE FROM wba.bilder
                    WHERE bild_id IN (SELECT bild_id
                    FROM (SELECT bild_id,
                    ROW_NUMBER() OVER (partition BY ordner, bildname, ts ORDER BY bild_id DESC) AS rnum
                    FROM wba.bilder) t
                    WHERE t.rnum > 1);")
      }

      if(data=="ereignis"){

      ereignis_import[, pfad := NULL,]
      ereignis_import[, folder := NULL,]
      ereignis_import[, standort_id_folder := NULL,]

      dbWriteTable(con, c("wba", "ereignisse"),
                   ereignis_import, row.names = FALSE, append = TRUE)

      # lösche Daten, die doppelt importiert wurden
      dbSendQuery(con, "DELETE FROM wba.ereignisse
                    WHERE ereignis_id IN (SELECT ereignis_id
                    FROM (SELECT ereignis_id,
                    ROW_NUMBER() OVER (partition BY ordner, s_ereignis_id, ts ORDER BY ereignis_id DESC) AS rnum
                    FROM wba.ereignisse) t
                    WHERE t.rnum > 1);")

      # aktualisiere einzelne Parameter der Fotofallen-Standort
      ################################################################################

      # Zeitstempel der Fotofallenstandorte

      dbSendQuery(con,
                  "update
                  wba.fotofallen_standorte
                  set
                  ts_startbild = ereignisse.ts
                  from
                  wba.ereignisse
                  where
                  fotofallen_standorte.ts_startbild is null and
                  ereignisse.session_startbild = 1 and
                  fotofallen_standorte.standort_id = ereignisse.standort_id;")

      dbSendQuery(con,
                  "update
                    wba.fotofallen_standorte
                   set
                    ts_endbild = ereignisse.ts
                  from
                    wba.ereignisse
                  where
                    fotofallen_standorte.ts_endbild is null and
                    ereignisse.session_endbild = 1 and
                    fotofallen_standorte.standort_id = ereignisse.standort_id;")

      dbSendQuery(con,
                  "update
                    wba.fotofallen_standorte
                  set
                    ts_kamera_start = ts_startbild
                  where
                    ts_kamera_start is NULL;")
      dbSendQuery(con,
                  "update
                  wba.fotofallen_standorte
                  set
                  ts_kamera_ende = ts_endbild
                  where
                  ts_kamera_ende is NULL;")

      ## markiere alle Standorte, bei denen einzelne Variablen kontrolliert werden müssen
      # folgende Kriterien werden kontrolliert:
      # * ts_kamera_start OR ts_kamera_ende IS NULL
      # * date(ts_kamera_start) != date(start_aufnahme)
      # * date(ts_kamera_ende) != date(ende_aufnahme)

      standorte_zum_kontrollieren <-
        dbGetQuery(con,"select
                            standort_id,
                            ts_kamera_start,
                            ts_kamera_ende,
                            ts_startbild,
                            ts_endbild,
                            bemerkungen as Bemerkungen,
                            CASE
                            when (ts_kamera_start is null or ts_kamera_ende is null) THEN
                            'Ein Zeitstempel der Kamera fehlt!'
                            when date(ts_kamera_start) != date(ts_startbild) THEN
                            'Aufhaengedatum und der Zeitstempel des Aufhaengebildes weichen mehr als einen Tag ab!'
                            when date(ts_kamera_ende) != date(ts_endbild) THEN
                            'Abhaengedatum und der Zeitstempel des Abhaengebildes weichen weichen mehr als einen Tag ab!'
                            END as Grund
                            from
                            wba.fotofallen_standorte
                            where
                            (ts_kamera_start is null or
                            ts_kamera_ende is null or
                            date(ts_kamera_start) != date(ts_startbild) or
                            date(ts_kamera_ende) != date(ts_endbild))
                            order by standort_id;")

      ## bis hierher korrigiert
      ################################################################################
      op <- 'B'
      if ( op=='A') {
      dbSendQuery(con,
                  paste("update
                wba.fotofallen_standorte
                set ueberpruefen = 1
                where
                  session_id = ",session, "and
                  (ts_kamera_start is null or
                  ts_kamera_ende is null or
                  date(ts_kamera_start) != date(start_aufnahme) or
                  date(ts_kamera_ende) != date(ende_aufnahme))", sep = ""))
      dbSendQuery(con,
                  paste("update
                      wba.fotofallen_standorte
                      set ueberpruefen = 0
                      where
                      ueberpruefen IS NULL", sep = ""))


      write.table(standorte_zum_kontrollieren,
                  file = paste("./standorte_zum_kontrollieren/",
                               session,
                               "_ts_kamera_fehlt.csv", sep = ""),
                  row.names = FALSE, sep = ";")
      }

      # Ändere kleinere Fehler

      dbSendQuery(con,
                  "update wba.ereignisse set fluchtreaktion = 0
                  where tierart_1 in ('Rothirsch','Reh') and fluchtreaktion is null;")

      dbSendQuery(con,
                  "UPDATE wba.ereignisse SET kamera_id = lower(kamera_id);")

      dbSendQuery(con,
                  "update
                  wba.fotofallen_standorte
                 set
                  n_rothirsch = 0
                 where
                  n_rothirsch is null;")

      #99-f-db_update_all_columns.R
      #############################
      #############################
      #############################
      dbSendQuery(con,
                  "with a as
                  (select
                      standort_id, sum(n_tierart_1) as n_rothirsch
                   from
                      wba.ereignisse
                   where
                      tierart_1 = 'Rothirsch' group by standort_id)
                  update
                    wba.fotofallen_standorte
                  set
                    n_rothirsch = a.n_rothirsch
                  from
                    a
                  where
                    a.n_rothirsch is not null and
                    fotofallen_standorte.standort_id = a.standort_id;")

      # aktualisiere die Laufzeit der Kamera in "Wocheneinheiten"
      dbSendQuery(con,
                  "update wba.fotofallen_standorte
                set laufzeit =
                extract(EPOCH from (ts_kamera_ende - ts_kamera_start))/(60*60*24*7);")

      dbSendQuery(con,
                  "update wba.fotofallen_standorte
                set verwertbar = 1 where verwertbar is null;")


      # update fotofallen_raster

      #dbSendQuery(con, stri_enc_toutf8(
      #  "update wba.fotofallen_standorte set raster_id = fotofallen_raster.raster_id from wba.fotofallen_raster
    #where st_intersects(fotofallen_raster.geom, fotofallen_standorte.geom) and fotofallen_standorte.raster_id is null;"))


      # add the total number of events per species for each deployment

      require(stringi)

      spaltenliste <-
        c( "n_auerhuhn", "n_auerhuhn_woche", "n_dachs", "n_dachs_woche", "n_damhirsch",
           "n_damhirsch_woche", "n_eichhörnchen", "n_eichhörnchen_woche",
           "n_fahrrad", "n_fahrrad_woche", "n_fuchs", "n_fuchs_woche",
           "n_gams", "n_gams_woche", "n_hase", "n_hase_woche",
           "n_hauskatze", "n_hauskatze_woche", "n_hund", "n_hund_woche",
           "n_kfz", "n_kfz_woche", "n_leeres bild", "n_leeres bild_woche",
           "n_luchs", "n_luchs_woche", "n_marder", "n_marder_woche",
           "n_marderhund", "n_marderhund_woche", "n_mensch", "n_mensch_woche",
           "n_mensch u hund", "n_mensch u hund_woche", "n_mufflon", "n_mufflon_woche",
           "n_reh", "n_reh_woche", "n_reh_0", "n_reh_0_woche", "n_reh_1w",
           "n_reh_1w_woche", "n_reh_1m", "n_reh_1m_woche", "n_reh_1u", "n_reh_1u_woche",
           "n_reh_2w", "n_reh_2w_woche", "n_reh_2m", "n_reh_2m_woche", "n_reh_2u",
           "n_reh_2u_woche", "n_reh_3w", "n_reh_3w_woche", "n_reh_3m", "n_reh_3m_woche",
           "n_reh_3u", "n_reh_3u_woche", "n_rothirsch", "n_rothirsch_woche", "n_rothirsch_0",
           "n_rothirsch_0_woche", "n_rothirsch_1w", "n_rothirsch_1w_woche", "n_rothirsch_1m",
           "n_rothirsch_1m_woche", "n_rothirsch_1u", "n_rothirsch_1u_woche", "n_rothirsch_2w",
           "n_rothirsch_2w_woche", "n_rothirsch_21m", "n_rothirsch_21m_woche", "n_rothirsch_22m",
           "n_rothirsch_22m_woche", "n_rothirsch_2m", "n_rothirsch_2m_woche", "n_rothirsch_2u",
           "n_rothirsch_2u_woche", "n_rothirsch_3w", "n_rothirsch_3w_woche", "n_rothirsch_3m",
           "n_rothirsch_3m_woche", "n_rothirsch_3u", "n_rothirsch_3u_woche", "n_sikahirsch",
           "n_sikahirsch_woche", "n_unbekannt", "n_unbekannt_woche", "n_vogel", "n_vogel_woche",
           "n_waschbär", "n_waschbär_woche", "n_wildkatze", "n_wildkatze_woche", "n_wildschwein",
           "n_wildschwein_woche", "n_wildschwein_0", "n_wildschwein_0_woche", "n_wildschwein_1w",
           "n_wildschwein_1w_woche", "n_wildschwein_1m", "n_wildschwein_1m_woche",
           "n_wildschwein_1u", "n_wildschwein_1u_woche", "n_wildschwein_2w",
           "n_wildschwein_2w_woche", "n_wildschwein_2m", "n_wildschwein_2m_woche",
           "n_wildschwein_2u", "n_wildschwein_2u_woche", "n_wildschwein_3w",
           "n_wildschwein_3w_woche", "n_wildschwein_3m", "n_wildschwein_3m_woche",
           "n_wildschwein_3u", "n_wildschwein_3u_woche", "n_wolf", "n_wolf_woche")

      tierartenliste <-
        c("Auerhuhn","Baummarder", "Dachs", "Damhirsch", "Eichhörnchen", "Fahrrad", "Fledermaus",
          "Forstschlepper", "Fuchs", "Gams", "Habicht", "Hase", "Hauskatze",
          "Hund", "Kfz", "Leeres Bild", "Luchs", "Marder", "Marderhund", "Maus", "Mensch",
          "Mensch u Hund", "Mufflon", "Reh", "Rothirsch", "Sikahirsch", "Steinmarder",
          "Unbekannt", "Vogel", "Wildkatze", "Waschbär", "Wildschwein", "Wolf")
      tierartenliste_short <-
        c("Auerhuhn","Baummarder", "Dachs", "Damhirsch", "Eichhörnchen", "Fahrrad",
          "Fuchs", "Gams",  "Hase", "Hauskatze",
          "Hund", "Kfz", "Leeres Bild", "Luchs", "Marder", "Marderhund", "Mensch",
          "Mensch u Hund", "Mufflon", "Reh", "Rothirsch", "Sikahirsch", "Steinmarder",
          "Unbekannt", "Vogel", "Wildkatze", "Waschbär", "Wildschwein", "Wolf")

      altersklassen <- c("0w", "0m", "0u",
                         "1w", "1m", "1u",
                         "2w", "2m", "2u", "21m", "22m",
                         "3w", "3m", "3u")
      tierartenliste_altersklassen <- c("Rothirsch", "Reh", "Wildschwein")
      #  for (i in tierartenliste_short) {
      #      dbSendQuery(con, stri_enc_toutf8(paste(
      #        "alter table wba.fotofallen_standorte add column \"n_", tolower(i),"\" integer;", sep = "")))
      #    dbSendQuery(con, stri_enc_toutf8(paste(
      #      "alter table wba.fotofallen_standorte add column \"n_", tolower(i),"_woche\" double precision;", sep = "")))}

      # for (i in tierartenliste_altersklassen) {
      #   for (j in altersklassen) {
      #     dbSendQuery(con, stri_enc_toutf8(paste(
      #       "alter table wba.fotofallen_standorte add column \"n_", tolower(i),"_", tolower(j),"\" integer;", sep = "")))
      #   dbSendQuery(con, stri_enc_toutf8(paste(
      #     "alter table wba.fotofallen_standorte add column \"n_", tolower(i),"_", tolower(j),"_woche\" double precision;", sep = "")))}}

      #
      # dbSendQuery(con, stri_enc_toutf8(
      #   "alter table wba.fotofallen_standorte add column n_rothirsch_2u3w integer; -- 2w + 3w"))
      # dbSendQuery(con, stri_enc_toutf8(
      #     "alter table wba.fotofallen_standorte add column n_rothirsch_2u3w_woche double precision;"))
      # dbSendQuery(con, stri_enc_toutf8(
      #   "alter table wba.fotofallen_standorte add column n_rothirsch_2u3m integer; -- 2m + 21m + 22m + 3m"))
      # dbSendQuery(con, stri_enc_toutf8(
      #   "alter table wba.fotofallen_standorte add column n_rothirsch_2u3m_woche double precision;"))
      # dbSendQuery(con, stri_enc_toutf8(
      #   "alter table wba.fotofallen_standorte add column n_rothirsch_0 integer;"))
      # dbSendQuery(con, stri_enc_toutf8(
      #   "alter table wba.fotofallen_standorte add column n_rothirsch_0_woche double precision;"))


      ## update n_tierartenliste
      for (i in tierartenliste_short) {
        dbSendQuery(con, stri_enc_toutf8(paste(
          "update
        wba.fotofallen_standorte
        set
        \"n_",tolower(i),"\" = NULL;", sep = "")))

        dbSendQuery(con, stri_enc_toutf8(paste(
          "with a as
          (select
            standort_id, sum(n_tierart_1) as n
            from
            wba.ereignisse
            where
            tierart_1 = \'",i ,"\' group by standort_id)
          update
          wba.fotofallen_standorte
          set
          \"n_",tolower(i),"\" = a.n
          from
          a
          where
          a.n is not null and
         fotofallen_standorte.standort_id = a.standort_id and
          verwertbar = 1;", sep = "")))

        dbSendQuery(con, stri_enc_toutf8(paste(
          "update
          wba.fotofallen_standorte
          set
          \"n_", tolower(i),"\" = 0
        where \"n_",tolower(i),"\" is NULL and
          verwertbar = 1;", sep = "")))

        #dbSendQuery(con, stri_enc_toutf8(paste(
        #  "with a as (select raster_id, sum(\"n_", tolower(i),"\") as sum
        #from wba.fotofallen_standorte where verwertbar = 1
        #group by raster_id)
        #update
        #wba.fotofallen_raster
        #set
        #\"n_", tolower(i),"\" = sum from a
        #where a.raster_id = fotofallen_raster.raster_id;", sep = "")))
      }

      ## update n_tierartenliste_woche
      for (i in tierartenliste_short) {

        dbSendQuery(con, stri_enc_toutf8(paste(
          "update
        wba.fotofallen_standorte
        set
       \"n_",tolower(i),"_woche\" = NULL;", sep = "")))

        dbSendQuery(con, stri_enc_toutf8(paste(
          "with a as
        (select
        standort_id, sum(n_tierart_1) as n
        from
        wba.ereignisse
        where
        tierart_1 = \'",i ,"\' group by standort_id)
        update
        wba.fotofallen_standorte
        set
        \"n_",tolower(i),"_woche\" = round((a.n / laufzeit)::numeric,4)
        from
        a
        where
        a.n is not null and
        fotofallen_standorte.standort_id = a.standort_id and
        verwertbar = 1;", sep = "")))

        dbSendQuery(con, stri_enc_toutf8(paste(
          "update
        wba.fotofallen_standorte
        set
        \"n_", tolower(i),"_woche\" = 0
        where \"n_",tolower(i),"_woche\" is NULL and
        verwertbar = 1;", sep = "")))

        #dbSendQuery(con, stri_enc_toutf8(paste(
        #  "with a as (select raster_id, round(avg(\"n_", tolower(i),"_woche\")::numeric, 4) as avg
         #             from wba.fotofallen_standorte where verwertbar = 1
         #             group by raster_id)
       #update
        #wba.fotofallen_raster
        #set
        #\"n_", tolower(i),"_woche\" = avg from a
        #where a.raster_id = fotofallen_raster.raster_id;", sep = "")))
      }

      }
    }

    dbDisconnect(con)

}

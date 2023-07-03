#' Bereinigt die mittels importTables geladenen Tabellen
#'
#' @param daten_ereignis_list list, enthält die beiden data.tables daten und ereignis (output von importTables)
#'
#' @return data.table
#' @export
#'
#' @examples
#'

processImportTables <- function(daten_ereignis_list) {

    lapply(daten_ereignis_list, function(data){

    names(data)[stringr::str_detect(names(data),"AUSSGR")] <- "AUSSGRßE"

    table_import <- data[, .(
      "s_foto_id" = s_foto_id
      ,"s_ereignis_id" = E_ID
      ,"pfad" = PFAD #only column that exists in ereignis but not in daten #ALEX: kommt bei beiden Datensätzen vor
      ,"ordner" = ORDNER
      ,"bildname" = BILDNAME
      ,"datum" = DATUM
      ,"zeit_utc" = ZEIT_UTC
      ,"ereignis_ende" = E_ENDE
      ,"ereignis_dauer" = E_DAUER
      ,"ereignis_n_bilder" = E_ANZBLD
      ,"standort_id" = STO_ID
      ,"kamera_id" = KMRA_ID
      ,"bearbeiter" = BEARBTNG
      ,"tierart_1" = Tierart_1
      ,"tierart_2" = Tierart_2
      ,"bildname_best" = BESTBILD
      ,"n_tierart_1" = ANZ_TA1_ges
      ,"n_tierart_2" = ANZ_TA2_ges
      ,"n_0w" = as.integer(ANZ_0W)
      ,"n_0m" = as.integer(ANZ_0M)
      ,"n_0u" = as.integer(ANZ_0U)
      ,"n_1w" = as.integer(ANZ_1W)
      ,"n_1m" = as.integer(ANZ_1M)
      ,"n_1u" = as.integer(ANZ_1U)
      ,"n_2w" = as.integer(ANZ_2W)
      ,"n_2m" = as.integer(ANZ_2M)
      ,"n_2u" = as.integer(ANZ_2U)
      ,"n_21m" = as.integer(ANZ_21M)
      ,"n_22m" = as.integer(ANZ_22M)
      ,"n_3m" = as.integer(ANZ_3M)
      ,"n_3w" = as.integer(ANZ_3W)
      ,"n_3u" = as.integer(ANZ_3U)
      ,"ereignis_id_neu" = E_ID_NEU
      ,"session_startbild" = Anf_Bild
      ,"session_endbild" = End_Bild
      ,"fluchtreaktion" = Fluchtreak
      ,"bildqualitaet" = Bildquali
      ,"n_markierte_tiere" = Anz_Mark_T
      ,"individuum" = Individuum
      ,"bemerkungen_1" = Bemerk_1
      ,"bemerkungen_2" = Bemerk_2
      ,"bildausschnitt" = AUSSGRßE
      ,"ausrichtung_hang" = AUSRICHT
      ,"ausrichtung_vertikal" = ACHSE
      ,"kamera_bemerkt" = Sieht_Cam
      ,"schnee" = SCHNEE
      ,"standort_id_folder" = standort_id_folder # standort_id based on folder name
      ,"folder" = folder #current file path
    ),]

    table_import$ts <- as.POSIXct(paste(table_import$datum,
                                        table_import$zeit_utc, sep = " "),
                                  format = "%d.%m.%Y %H:%M:%S")

    # Ersetze in Spalten mit Integers NAs durch 0
    int_cols_with_na <- which(str_detect(names(table_import), "^n_"))

    table_import[,
                 (int_cols_with_na) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
                 .SDcols = int_cols_with_na]

    return(table_import)
  })
}

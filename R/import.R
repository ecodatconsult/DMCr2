import <- function() {

  # Hier ist unklar auf was sich die Funktion bezieht

  require(dplyr)
  require(data.table)
  require(stringr)



  #req_col_names <- c("s_foto_id","E_ID","ORDNER","BILDNAME","DATUM","ZEIT_UTC","E_ENDE","E_DAUER","E_ANZBLD","STO_ID","KMRA_ID","BEARBTNG","Tierart_1","Tierart_2","BESTBILD","ANZ_TA1_ges","ANZ_TA2_ges","ANZ_0W","ANZ_0M","ANZ_0U","ANZ_1W","ANZ_1M","ANZ_1U","ANZ_2W","ANZ_2M","ANZ_2U","ANZ_21M","ANZ_22M","ANZ_3M","ANZ_3W","ANZ_3U","E_ID_NEU","Anf_Bild","End_Bild","Fluchtreak","Bildquali","Anz_Mark_T","Individuum","Bemerk_1","Bemerk_2","AUSSGRßE","AUSRICHT","ACHSE","Sieht_Cam","SCHNEE")

  data = c("daten","ereignis")

  names(ereignis)[stringr::str_detect(names(ereignis),"AUSSGR")] <- "AUSSGRßE"
  names(daten)[stringr::str_detect(names(daten),"AUSSGR")] <- "AUSSGRßE"

  for(i in 1:length(data)){

    if(data[i] == "ereignis" && length(ereignis$s_foto_id) > 0){

      #correct column names
      #req_col_names[!req_col_names %in% names(ereignis)]



      table_import <- ereignis[, .(
                                "s_foto_id" = s_foto_id
                                ,"s_ereignis_id" = E_ID
                                ,"pfad" = PFAD #only column that exists in ereignis but not in daten
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
    }

    if(data[i] == "daten" && length(daten$s_foto_id) > 0){

      #correct column names
      table_import <- daten[, .(
                                "s_foto_id" = s_foto_id
                                ,"s_ereignis_id" = E_ID
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
    }

    #time stamp: date + time
    table_import$ts <- as.POSIXct(paste(table_import$datum,
                                             table_import$zeit_utc, sep = " "),
                                       format = "%d.%m.%Y %H:%M:%S")

    #na -> 0

    # TODO: simplify replacing NA with 0
    # apply(table_import[,names(table_import)[str_detect(names(table_import), "^n_[0-9]")]],
    #       1,
    #       function(x) ifelse(is.na(x), 0, x))

    table_import[is.na(n_0w),n_0w := 0,][
      is.na(n_0m),n_0m := 0,][
        is.na(n_0u),n_0u := 0,][
          is.na(n_1w),n_1w := 0,][
            is.na(n_1m),n_1m := 0,][
              is.na(n_1u),n_1u := 0,][
                is.na(n_2w),n_2w := 0,][
                  is.na(n_2m),n_2m := 0,][
                    is.na(n_2u),n_2u := 0,][
                      is.na(n_21m),n_21m := 0,][
                        is.na(n_22m),n_22m := 0,][
                          is.na(n_3w),n_3w := 0,][
                            is.na(n_3m),n_3m := 0,][
                              is.na(n_3u),n_3u := 0,]

    #name the table after "data" = ereignis or daten
    assign(paste(data[i],"_import",sep = ""),table_import, envir = .GlobalEnv)

    rm(table_import)
  }
  rm(i)
}

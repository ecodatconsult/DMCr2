checkFoldernames <- function(){

  if(!exists("ereignis_import") & !exists("daten_import")){
    stop(print("Benötigte Daten 'ereignis_import' und/oder 'daten_import' fehlen. Führe die Funktion import() aus."))
  }

  #wrong folder names --> "standort_id_folder" is the correct one
  falsche_ordnernamen <- unique(ereignis_import[standort_id != standort_id_folder,
                                                .(standort_id,standort_id_folder),])

  #df
  falsche_ordnernamen_all <- falsche_ordnernamen
  colnames(falsche_ordnernamen_all) <- c("alt","korrigiert")
  assign("falsche_ordnernamen_all",falsche_ordnernamen_all, envir =  .GlobalEnv)

  if (nrow(falsche_ordnernamen) >0) {
    print("Folgende Standorte haben eine falsche Ordnerbenennung, die nicht mit dem Standortnamen übereinstimmt: ")
    print(falsche_ordnernamen)

    #name "standort_id" and "ordner" like "standort_id_folder"
    for (i in 1:nrow(falsche_ordnernamen)) {
      #daten
      daten_import[standort_id_folder == falsche_ordnernamen$standort_id_folder[i],
                     ':='(ordner = falsche_ordnernamen$standort_id_folder[i],
                          standort_id = falsche_ordnernamen$standort_id_folder[i])
                  ,]
      #ereignis
      ereignis_import[standort_id_folder == falsche_ordnernamen$standort_id_folder[i],
                      ':='(ordner = falsche_ordnernamen$standort_id_folder[i],
                          standort_id = falsche_ordnernamen$standort_id_folder[i])
                  ,]
    }

    print("Die Ordnernamen folgender Standorte wurden korrigiert: ")
    print(falsche_ordnernamen)

  } else {
    print('Alle Ordnernamen stimmen überein!')
  }

}

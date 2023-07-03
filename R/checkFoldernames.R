checkFoldernames <- function(importTables_list){

  falsche_ordnernamen_df <- unique(importTables_list$daten[standort_id != standort_id_folder, .(standort_id,standort_id_folder),])

  if(nrow(falsche_ordnernamen_df) > 0){
    print("Die Ordnernamen folgender Standorte werden korrigiert: ")
    print(falsche_ordnernamen_df$standort_id_folder)
  }

  c(lapply(importTables_list, function(data){
    data[standort_id_folder != standort_id,
         ':='(ordner = falsche_ordnernamen$standort_id_folder[i],
              standort_id = falsche_ordnernamen$standort_id_folder[i])
         ,]
  }),
  falsche_ordnernamen_df)

}

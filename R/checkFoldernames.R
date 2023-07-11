#' Ersetzt Ordnernamen durch tatsächlichen Ordnernamen
#'
#' @param importTables_list list, Liste die mit DMCr2::importTables() und DMCr2::processImportTables() erstellt wurde
#' @export

checkFoldernames <- function(importTables_list){
  falsche_ordnernamen_df <- unique(importTables_list$daten[standort_id != standort_id_folder, .(standort_id,standort_id_folder),])

  c(lapply(importTables_list, function(data){
    data[,ordner:=ifelse(standort_id_folder!=standort_id, standort_id_folder, ordner)]
    data[,standort_id:=ifelse(standort_id_folder!=standort_id, standort_id_folder, standort_id)]
  }),
  falsche_ordnernamen_df)
}

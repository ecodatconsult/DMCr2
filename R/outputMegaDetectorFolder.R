#' Fotos mit Detection des MegaDetectors werden in einen separaten Ordner verschoben
#'
#' @param file_path Dateipfad zum Foto (.jpg)
#'
#' @return
#' @export
#'
#' @examples
#'
outputMegaDetectorFolder <- function(file_path){

  require(dplyr)

  #extrahiert korrekten Dateipfad zum Ordner "Fotos"
  dir_path = unlist(strsplit(file_path, "/" ))
  nr_fotos <- which(dir_path == "Fotos")
  dir_path <- paste(dir_path[1:c(nr_fotos-1)], collapse = "/")

  #extrahiert die Dateipfade der Fotos mit Detection im Ordner "Fotos"
  files_names_in_fotos <- list.files(paste(dir_path,"/Fotos",sep = ""), pattern = "_detections", recursive = TRUE)
  files_path_in_fotos_old <- paste(dir_path,files_names_in_fotos,sep = "/Fotos/")

  #neue Dateipfade im Ordner "Detections"
  files_path_in_fotos_new <- paste(dir_path,files_names_in_fotos,sep = "/Detections/")

  #verschiebt Fotos von "Fotos" nach "Detections"
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }

  for(i in 1:length(files_path_in_fotos_old)){
    my.file.rename(from=files_path_in_fotos_old[i],to=files_path_in_fotos_new[i])
  }
}

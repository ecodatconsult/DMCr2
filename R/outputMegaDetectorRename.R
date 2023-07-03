#' Bereits existierende MegaDetector-Datei wird umbenannt, damit sie nicht überschrieben wird
#'
#' @param file_path Pfad zur Datei "Megadetector_output.json"
#'
#' @return
#' @export
#'
#' @examples
#'
outputMegaDetectorRename <- function(file_path){

  #führe nur aus, wenn die Datei existiert
  if(file.exists(paste(file_path,"/Megadetector_output.json",sep = ""))){

    #extrahiere Datum und Zeit der Dateierstellung
    ctime <- file.info(paste(file_path,"/Megadetector_output.json",sep = ""))$ctime
    ctime <- gsub(":","-",ctime)
    ctime <- gsub(" ","_",ctime)

    #hänge Erstellungszeit an Dateinamen an
    file.rename(paste(file_path,"/Megadetector_output.json",sep = ""),paste(paste(file_path,"/Megadetector_output_",sep = ""),ctime,".json",sep = ""))
  }

  #führe nur aus, wenn die Datei existiert
  if(file.exists(paste(file_path,"/cmd_output.txt",sep = ""))){

    #extrahiere Datum und Zeit der Dateierstellung
    ctime <- file.info(paste(file_path,"/cmd_output.txt",sep = ""))$ctime
    ctime <- gsub(":","-",ctime)
    ctime <- gsub(" ","_",ctime)

    #hänge Erstellungszeit an Dateinamen an
    file.rename(paste(file_path,"/cmd_output.txt",sep = ""),paste(paste(file_path,"/cmd_output_",sep = ""),ctime,".txt",sep = ""))
  }

}

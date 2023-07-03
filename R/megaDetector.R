#' Ausführung des MegaDetectors
#'
#' @param file_path Pfad zum Ordner der Session (wenn run = "batch") oder zum Foto (wenn run = "simple"). Beispielsweise "N:/Projekte/Gruenbrueckenmonitor_1708/Daten/Feldarbeit/Fotofallenmonitoring/session_07" bzw. ".../session_07/Fotos/Böblingen/GBB_ff01_210805/I_00008a.JPG".
#' @param run run = "batch" führt den MegaDetector für mehrere Fotos aus, run = "simple" für ein einzelnes.
#'
#' @return JSON-Datei (wenn run = "batch) oder ein Foto als JPG mit bounding box (wenn run = "simple").
#' @export
#'
#' @examples
#'
megaDetector <- function(file_path, run = c("batch","simple")){

  #Pfad zur .bat Datei
  bat_path <- system.file('data', package = 'DMCr')
  bat_path <- sub("Program Files","PROGRA~1",bat_path)

  #prüfe, ob GPU vorhanden
  if(any(grep("NVIDIA",system("wmic path win32_VideoController get name", intern = TRUE))) == TRUE){
    GPU <- TRUE
  }else{
    GPU <- FALSE
  }

  #für die Ausführung von nur einem Foto wird der Pfad zum Ordner und zum Foto separat benötigt
  if(run == "simple"){
    file_path_zero <- paste(unlist(strsplit(file_path,"/"))[1:length(unlist(strsplit(file_path,"/")))-1],collapse = "/")
  }

  #führt die jeweilige batch Datei aus
    #übergebe Dateipfad als %1 (bzw. %2)

  #keine Grafikkarte = CPU
  if(GPU == FALSE){
    #mehrere Bilder
    if(run == "batch"){
      system("cmd.exe", input = paste(bat_path,"/runMegaDetectorCPUbatch.bat \"",file_path,"\"",sep = "")
           , wait = FALSE #wenn du es in R-Studio ausführst, setze wait = TRUE
           , invisible = FALSE
      )
    }
    #ein Bild
    if(run == "simple"){
      system("cmd.exe", input = paste(bat_path,"/runMegaDetectorCPUsimple.bat \"",file_path_zero,"\" \"",file_path,"\"",sep = "")
      )
    }
  }

  #Grafikkarte = GPU
  if(GPU == TRUE){
    #mehrere Bilder
    if(run == "batch"){
      #system("cmd.exe", input = paste(bat_path,"/runMegaDetectorGPUbatch.bat \"",file_path,"\" > cmd_output.txt",sep = "")
      system("cmd.exe", input = paste(bat_path,"/runMegaDetectorGPUbatch.bat \"",file_path,"\"",sep = "")
           , wait = FALSE #wenn du es in R-Studio ausführst, setze wait = TRUE
           , invisible = FALSE
           )
    }
    #ein Bild
    if(run == "simple"){
      system("cmd.exe", input = paste(bat_path,"/runMegaDetectorGPUsimple.bat \"",file_path_zero,"\" \"",file_path,"\"",sep = "")
      )
    }
  }
}

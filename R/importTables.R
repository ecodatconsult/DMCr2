#' Lädt alle Tabellen einer Session und vereinigt sie zu einer großen Tabelle. Die Tabellen entstammen der Klassifikation mit dem Programm FFM2 und liegen im csv-Format vor.
#'
#' @param file_path Pfad zum Ordner der Session. Beispielsweise "N:/Projekte/Gruenbrueckenmonitor_1708/Daten/Feldarbeit/Fotofallenmonitoring/session_07".
#' @param data Character vector,  bestimmt welche Tabellen geladen werden sollen: data = c("daten","ereignis").
#'
#' @return data.table/data.frame
#' @export
#'
#' @examples
#'
importTables <- function(file_path, data = c("daten", "ereignis")) {

  # require(dplyr)
  # require(data.table)
  # require(stringr)

# Alex: umständliche Formulierung. data sollte direkt als c("daten", "ereignis") in der Funktion übergeben werden,

  #file_path = "N:/Projekte/Forschungscluster_1768/02_Projektmodule/01_Kamerafallenthemen/AutomatisierteKlassifizierung/Daten/Fuchsprojekt_Lino/Spring2017"

  #decide which "data"
  #  data_both = data

#   if(any(data != "both") & length(data) == 1){
#     i <- which(data_both == data)
#   }
#   if(data == "both"){
#     data <- data_both
#   }
#   rm(data_both)
  i <- 1
  for(i in 1:length(data)){


    #catch the names of all files
    filenames <- list.files(path = file_path,
                            recursive=T,
                            pattern= paste("^_",data[i],".csv",sep = ""),
                            full.names = TRUE)
    # Anja: Das Scannen der Ordner nach den Exceldateien kann sehr lange dauern. Daher habe ich das zweite Scannen abgekürzt, indem ich bei den filenames "ereignisse" und "daten" tausche. Es lässt sich in deiner Version nicht direkt implementieren. In der nächsten Zeile findest du den Code-Schnipsel, falls du es nutzen möchtest.
    # filenames <- str_replace(filenames, "_ereignis.csv", "_daten.csv" )

    #TODO: Alex: mglw schnellere Alternative wäre Kombination aus list.dirs, paste0 (_ereignis.csv bzw _daten.csv) + file.exists

    if(length(filenames) >0){

      #load the files and bind them together
      # x <- filenames[1]

      table <-
        do.call(rbind,
                c(lapply(filenames,
                         function(x) {
                           d <- data.table::fread(x, encoding = "UTF-8")
                           # Anja: die drei Zeilen unten können raus.Dafür habe ich den Ordnerpfad und den Ordnernamen bei beiden Tabellen mit aufgenommen, da es immer wieder vorgekommen ist, dass diese nach dem Start com FFM2 aktualisiert worden sind.
                           # if (nchar(d$ORDNER[1]) == 20) {
                           #   d$ORDNER <- tail(unlist(str_split(x, pattern = "/")), 2)[1]
                           #}
                           d <- cbind(d,
                                      "standort_id_folder" = tail(unlist(stringr::str_split(x, pattern = "/")), 2)[1],  #TODO: replace with basename(dirname(x)) ?!
                                      "folder" = rep(stringr::str_remove(x, paste("/_",data[i],".csv",sep = "")), nrow(d))) #TODO: replace with dirname(x)?!
                           return(d)}
                ), fill = TRUE))

      #correct column name
      colnames(table)[1] <- "s_foto_id"

      #name the table after "data" = ereignis or daten
      #TODO replace with return(table)
      assign(data[i],table, envir = .GlobalEnv)

      rm(filenames,table)
    }

  }

rm(i)

}

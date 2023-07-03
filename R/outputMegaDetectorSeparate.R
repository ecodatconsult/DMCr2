#' Durch den MegaDetector klassifizierte Fotos werden nach Kategorie der Detectino in separate Ordner kopiert.
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
#'
outputMegaDetectorSeparate <- function(file_path){

  require(dplyr)

  if(file.exists(paste(file_path,"/Megadetector_output.csv",sep = ""))){

    #MegaDetector-Datei csv
    bilder <- read.csv2(paste(file_path,"/Megadetector_output.csv",sep = ""))

    if(!all(is.na(bilder$detections.category))){

      #detection categories
      categories <- unique(bilder$detections.category)
      categories[which(is.na(categories))] <- "NA" #zusätzliche Kategorie "NA" = MegaDetector hat nicht funktoniert -> nochmal über diesen Ordner drüberlaufen lassen

      if(length(categories)>0){

        #führe für jede Kategorie separat aus
        for(category in categories){

          #alle Bilder der Kategorie
          if(category == "NA"){
            bilder_category <- bilder[is.na(bilder$detections.category),]
          }else{
            bilder_category <- bilder[bilder$detections.category == category & !is.na(bilder$detections.category),]
          }

          #teilt die Fotos in sichere und unsichere Detections auf
          safety_list <- c("safe","uncertain")

          for(safety in safety_list){

            if(safety == "safe"){
              if(category == "empty" | category == "NA"){
                bilder_category_new <- bilder_category
              }else{
                bilder_category_new <- bilder_category[bilder_category$max_detection_conf > 0.9,] #Detection mit Sicherheit > 0,9 = "safe"
              }
            }else{
              if(category == "empty" | category == "NA"){
                bilder_category_new <- NULL
              }else{
                bilder_category_new <- bilder_category[bilder_category$max_detection_conf <= 0.9,] #Detection mit Sicherheit > 0,9 = "uncertain"
              }
            }

            if(length(bilder_category_new)>0){

              #Dateipfad
              files_path_in_fotos_old <- paste(file_path,gsub("\\\\","/",bilder_category_new$file),sep="/")

              #neuer Dateipfad mit Kategorie
              if(category == "NA"){
                files_path_in_category_new <- gsub("Fotos",paste("Fotos",category,"Fotos",sep = "/"),files_path_in_fotos_old)
              }else if(category == "empty"){
                files_path_in_category_new <- gsub("Fotos",paste("Fotos",category,sep = "/"),files_path_in_fotos_old)
              }else{
                files_path_in_category_new <- gsub("Fotos",paste("Fotos",category,safety,sep = "/"),files_path_in_fotos_old)
              }

              #kopiert Bilder in Ordner gemäß ihrer detection category
              my.file.copy <- function(from, to) {
                todir <- dirname(to)
                if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
                file.copy(from = from,  to = to)
              }

              for(i in 1:length(files_path_in_fotos_old)){
                my.file.copy(from=files_path_in_fotos_old[i],to=files_path_in_category_new[i])
              }

            }

          }

        }
      }
    }
  }
  rm(list=c(ls()))
}



#' Fotos mit "Personen" (Klassifikation vom MegaDetector) drauf werden verpixelt + Thumbnail aktualisiert
#'
#' @param file_path Pfad zur Session
#'
#' @return
#' @export
#'
#' @examples
#'
outputMegaDetectorblurPerson <- function(file_path){

  require(magick)
  require(dplyr)

  if(file.exists(paste(file_path,"/Megadetector_output.csv",sep = ""))){

    #MegaDetector-Datei als csv
    bilder <- read.csv2(paste(file_path,"/Megadetector_output.csv",sep = ""))

    if(!all(is.na(bilder$detections.category))){

      #Pfad zur batch-Datei (thumbnails)
      bat_path <- system.file('data', package = 'DMCr')
      bat_path <- sub("Program Files","PROGRA~1",bat_path)

      #führe nur aus, wenn "Person" detectiert
      if(any(bilder$detections.category == "Person")){

        #alle Bilder mit Personen
        bilder_mensch <- bilder[bilder$detections.category == "Person" & !is.na(bilder$detections.category ),]

        #führe für jedes Bild einzeln aus
        for (i in 1:length(bilder_mensch$X)){

          #Name des Bildes
          image_name <- gsub("\\\\","/",bilder_mensch$file[i])

          #lade das Bild + speicher den Pfad
          bildpfad <- paste(file_path,image_name, sep = "/")
          bild <- image_read(bildpfad)

          #belasse untere Leiste da
          height <- image_info(bild)[3]
          per_lf <- 0.1
          crop_height <- height - round(height*per_lf)
          small_crop_height <- 480 - round(480*per_lf)

          bild_sh <- image_crop(bild, paste("x", crop_height, sep = ""))

          #scale it
          bild_sh <- image_scale(bild_sh, paste("x", small_crop_height, sep = ""))
          bild <- image_scale(bild, "x480")

          #blur
          bild_sh <- image_blur(bild_sh, 50, 20) #radius, sigma

          #finales Bild
          bild_final <- image_mosaic(c(bild, bild_sh))

          #speichern
          image_write(bild_final, path = bildpfad)

          #update thumbnail
          system("cmd.exe", input = paste(bat_path,"/exiftool.exe -overwrite_original -m \"-thumbnailimage<=",bildpfad,"\" \"",bildpfad,"\"",sep = ""))
        }
      }
    }
  }
  rm(list=c(ls()))
}

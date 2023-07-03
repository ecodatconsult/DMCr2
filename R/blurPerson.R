#' Augewählte Fotos werden verpixelt und ggf. verkleinert
#'
#' @param file_path Pfad zum Ordner der Session. Beispielsweise "N:/Projekte/Gruenbrueckenmonitor_1708/Daten/Feldarbeit/Fotofallenmonitoring/session_07".
#' @param scale Gibt an, ob das Foto verkleinert werden soll. Default = TRUE
#' @param size Größe des Fotos, wenn diese verändert werden soll. Default = "x480"
#' @param column Name der Spalte, in der die Klassifizierung steht. Beispielsweise "Tierart_1".
#' @param selection Character Vector mit den Klassen, die verpixelt werden sollen. Beispielsweise c("Mensch","Mensch u Hund").
#'
#' @return Die Fotos werden am Speicherort überschrieben.
#' @export
#'
#' @examples
#'
blurPerson <- function(file_path, scale = TRUE, size= 480, column, selection = c("Mensch","Mensch u Hund",...)){

  require(magick)

  #spath to .bat file
  bat_path <- system.file('data', package = 'DMCr')
  bat_path <- sub("Program Files","PROGRA~1",bat_path)

  #catch all daten with a person on it
  bilder_mensch <- data.frame()
  for (selec in selection){
    daten_selec <- as.data.frame(daten)[as.data.frame(daten)[,column] == selec,]
    bilder_mensch <- rbind(bilder_mensch,daten_selec)
  }

    #run for each image
    for (j in 1:length(bilder_mensch$s_foto_id)){

      tryCatch({

        #image name
        image_name <- paste(bilder_mensch$ORDNER[j],bilder_mensch$BILDNAME[j],sep = "/")

        #load image
        bildpfad <- paste(file_path,"Fotos",image_name, sep = "/")
        bild <- image_read(path = bildpfad)

        #belasse untere Leiste da
        height <- image_info(bild)[3]
        per_lf <- 0.1
        crop_height <- height - round(height*per_lf)
        small_crop_height <- size - round(size*per_lf)

        bild_sh <- image_crop(bild, paste("x", crop_height, sep = ""))

        #scale it
        if(scale == TRUE){
          bild_sh <- image_scale(bild_sh, paste("x", small_crop_height, sep = ""))
          bild <- image_scale(bild, paste("x", size, sep = ""))
        }

        #blur
        if(scale == TRUE){
          bild_sh <- image_blur(bild_sh, 50, 20) #radius, sigma
        }else{
          bild_sh <- image_blur(bild_sh, 100, 50) #radius, sigma
        }

        #finales Bild
        bild_final <- image_mosaic(c(bild, bild_sh))

        #save
        image_write(bild_final, path = bildpfad)

        #update thumbnail
        system("cmd.exe", input = paste(bat_path,"/exiftool.exe -overwrite_original -m \"-thumbnailimage<=",bildpfad,"\" \"",bildpfad,"\"",sep = ""))
      }

      #print error message if image could not be found (=already deleted)
      , error=function(e){
      print(paste("Picture not found",image_name,sep = ": "))
    })
  }
}

# Kopiere das beste Bild eines jeden Ereignis in einen export_Ordner
# (hierbei wird das Bild verkleinert und ein Overlay hinzugefügt)
################################################################################

bestPictures <- function(file_path, mask = TRUE, scale = TRUE, size = "x480"){

  require(magick)
  require(dplyr)

  #path to Fotos
  foto_path <- paste(file_path,"/Fotos",sep = "")

  #get folder names
  d_ordner <- unique(daten$ORDNER)[nchar(unique(daten$ORDNER))!=0]

  #create save directory
  save_path <- paste(foto_path,"/bestPictures", sep = "")
  dir.create(file.path(save_path), showWarnings = FALSE)

  #mask
  if(mask == TRUE){
    #import
    maske <- image_read("N:/Projekte/Forschungscluster_1768/02_Projektmodule/01_Kamerafallenthemen/FVA_Fotofallendatenbank/R/R-Paket/DMCr/data/Fotofallenmaske_FVA.png")
    #scale it
    if(scale == TRUE){
      maske <- image_scale(maske, size)
    }
  }

  #run for each folder
  for (i in d_ordner) {
    print(paste("Kopiere Bilder von Standort: ", i))

    #catch pictures
    bilder_export <- daten[ORDNER == i &
                              !(Tierart_1 %in% c("Mensch",
                                                  "Mensch u Hund",
                                                  "Kfz",
                                                  "Forstschlepper")), unique(BESTBILD)]

    if (length(bilder_export)!=0) {
      if (!is.na(bilder_export[1])){
        bilder_export <- bilder_export[nchar(bilder_export) != 0]

        #run for each picture
        for (j in bilder_export){
          tryCatch({

            #file path to original picture
            d_foto_origin <- paste(foto_path, i ,j, sep = "/")

            #file path to copied picture
            d_foto_destination <- paste(save_path,"/",
                                      unlist(strsplit(i, split = "_"))[2],"_",
                                      j, sep = "")

            #name of the picture
            bildname <- gsub(x = paste(unlist(strsplit(i, split = "_"))[2],"_",
                                      j, sep = ""),
                                      pattern = ".JPG", replacement = "")

            #load the picture
            bild <- image_read(d_foto_origin)

            #scale it
            if(scale == TRUE){
              bild <- image_scale(bild, size)
            }

            #add meta data
            bild <- image_annotate(bild, bildname,
                               size = 30,
                               color = "black",
                               boxcolor = "white",
                               location = "+10+10")

            #add mask
            if(mask == TRUE){
              bild <- c(bild, maske)
            }

            #save new picture
            image_write(image_mosaic(bild),
                          path = d_foto_destination)
            }
          , error=function(e){
            cat("ERROR :",conditionMessage(e), "\n")
          })
        }
      }
    }
  }
  cat(paste("fertig :-)"))
}

extractPictures <- function(save_path, scale = TRUE, size = "x480", tags){

  if(exists("ergebnisse", envir = .GlobalEnv)){

    require(RPostgreSQL)
    require(DBI)
    require(stringi)
    require(magick)
    require(dplyr)

    dbConnection(type = "read")

    # data frame
    df_standorte <- dbGetQuery(con,"SELECT * FROM fotofallen.fotofallen_standorte")

    dbDisconnect(con)

    # pfade
    ergebnisse_mit_pfaden <- ergebnisse

    pfade_nicht_vorhanden <-c()
    for(i in 1:nrow(ergebnisse_mit_pfaden)){
      ordner <- ergebnisse_mit_pfaden$ordner[i]
      bildname <- ergebnisse_mit_pfaden$bildname[i]
      ordner_bildname <- paste(ordner,bildname, sep = "/")
      pfad_ordner <- df_standorte$bilderordner[grep(ordner, df_standorte$bilderordner)]
      pfad_bild <- paste(pfad_ordner, bildname, sep = "/")
      if(length(pfad_bild[grep(":/",pfad_bild)])>0){
        ergebnisse_mit_pfaden$pfad_bild[i] <- pfad_bild
      }else{
        pfade_nicht_vorhanden <- append(pfade_nicht_vorhanden,ordner_bildname)
        ergebnisse_mit_pfaden$pfad_bild[i] <- NA
      }
    }
    assign("pfade_nicht_vorhanden_df",pfade_nicht_vorhanden, envir = .GlobalEnv)
    rm(pfade_nicht_vorhanden)

    # bilder
    pfade_ohne_zugriff <- c()
    for(i in 1:nrow(ergebnisse_mit_pfaden)){
      tryCatch(
        bild <- image_read(ergebnisse_mit_pfaden$pfad_bild[i])
      , error=function(e) {})
      if(exists("bild")){
          #scale it
          if(scale == TRUE){
            bild <- image_scale(bild, size)
          }
          #annotate
          bild <- image_annotate(bild, paste(paste(tags,ergebnisse_mit_pfaden[i,tags],sep = ": "),collapse = ", "),
                                 size = 20,
                                 color = "black",
                                 boxcolor = "white",
                                 location = "+10+10")
          #save
          image_write(image_mosaic(bild),path = paste(save_path,"/",ergebnisse_mit_pfaden$ordner[i],"-",ergebnisse_mit_pfaden$bildname[i], sep = ""))
          rm(bild)
      }else{
          pfade_ohne_zugriff <- append(pfade_ohne_zugriff, ergebnisse_mit_pfaden$pfad_bild[i])
      }
    }
    pfade_ohne_zugriff <- pfade_ohne_zugriff[!is.na(pfade_ohne_zugriff)]
    assign("pfade_ohne_zugriff_df",pfade_ohne_zugriff, envir = .GlobalEnv)
    rm(pfade_ohne_zugriff)

  }
}

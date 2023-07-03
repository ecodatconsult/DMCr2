mapNewStandorte <- function(file_path, project){

  require(rgdal)
  require(raster)
  require(rworldmap)
  require(RPostgreSQL)
  require(DBI)
  require(rpostgis)
  require(sf)

  # map of BW
  ger <- raster::getData("GADM"
                         , country = "Germany"
                         , level = 1)
  bw <- ger[ger$NAME_1 == "Baden-Württemberg",]
  rm(ger)

  if(exists(c("standorte_import_new_sf","standorte_DB_sf"),envir = .GlobalEnv)){

    # Map
    plot.new()
    dev.control("enable")
    par(xpd=NA)
      plot(bw) +
        plot(standorte_DB_sf
             , pch = 21
             , col = "black"
             , bg = "grey"
             , add = TRUE) +
        plot(standorte_import_new_sf
             , pch = 24
             , col = "black"
             , bg = "green"
             , add = TRUE)
        legend("topleft"
               , legend = c("Standorte in DB","Neue Standorte")
               , col = c("grey","green")
               , pch = c(19,17)
               )

    standorte_map <- recordPlot()
    dev.off()
    assign("standorte_new_map",standorte_map,envir = .GlobalEnv)
    rm(standorte_map)

  }
}


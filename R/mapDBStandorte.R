#' Plottet eine Karte von Baden-Württemberg mit den ausgwählten Standorten von Fotofallenkameras.
#'
#' @return recordedplot (als Liste)
#' @export
#'
#' @examples
#'
mapDBStandorte <- function(){

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

  # standorte DB
  dbConnection(type = "read")
  df_projects <- dbGetQuery(con,"SELECT * FROM fotofallen.projekte")

  #extract()
  standort_ids <- unique(ergebnisse$standort_id)
  standort_ids <- paste(standort_ids,collapse = "', '")

  fotofallen_standorte_DB <- pgGetGeom(con, query = paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE standort_id IN ('", stri_enc_toutf8(standort_ids), "');", sep = ""))
  fotofallen_standorte_DB <- spTransform(fotofallen_standorte_DB, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  assign("fotofallen_standorte_DB",fotofallen_standorte_DB, envir = .GlobalEnv)

  # Map
  plot.new()
  dev.control("enable")
  par(xpd=NA)
        plot(bw) +
          plot(fotofallen_standorte_DB
               , pch = 21
               , col = "black"
               , bg = "darkred"
               , add = TRUE)

  standorte_map <- recordPlot()
  dev.off()
  assign("standorte_db_map",standorte_map,envir = .GlobalEnv)
  rm(list=setdiff(c(ls()), 'standorte_db_map'))
}


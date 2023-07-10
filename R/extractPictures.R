#' Ruft Bilder aus dem Ordnerverzeichnis anhand der Datenbankinfo ab
#'
#' @param bilder_table data.table, Datentabelle mit Information zu den Pfaden zu den Bildern (i.d.R. erstellt mit downloadDB("bilder", "alle Arten", "alle Projekte"))
#' @param save_path character, Pfad zu dem Speicherort für die Bilder
#' @param scale character, gibt die Bildhöhe an. "x480" ändert die Bildhöhe auf 480 Pixel, der aspect ratio wird beibehalten
#' @param tags character vector, gibt die tags an, die in den Bildern eingetragen werden sollen
#'
#' @return data.frame
#' @export
#'
#' @examples

extractPictures <- function(bilder_table, save_path, scale = TRUE, size = "x480", tags){

    con <- dbConnection()

    df_standorte_bilderordner <- RPostgreSQL::dbGetQuery(con,"SELECT DISTINCT(bilderordner) FROM fotofallen.fotofallen_standorte")$bilderordner

    RPostgreSQL::dbDisconnect(con)

    # construct path 2 images, detect missing path info and accessbility issues
    bilder_table <-
    bilder_table %>%
      dplyr::select(ordner, bildname) %>%
      dplyr::mutate(pfad_ordner = sapply(ordner, function(x) df_standorte_bilderordner[grep(x, df_standorte_bilderordner)])) %>%
      dplyr::mutate(pfad_bild = paste0(pfad_ordner, .Platform$file.sep, bildname)) %>%
      dplyr::mutate(pfad_gegeben = !any(is.na(c(pfad_ordner, ordner, bildname)))) %>%
      dplyr::mutate(pfad_abrufbar = file.exists(pfad_bild)) %>%
      dplyr::as_tibble()

    # read and process images based on the input
    apply(bilder_table %>%
            filter(pfad_gegeben & pfad_abrufbar),
          1,
          function(x){
            img <- magick::image_read(x$pfad_bild)

            img %>%
              magick::image_scale(ifelse(scale, size, paste0("x", magick::image_info(img)$height))) %>%
              magick::image_annotate(paste(paste(tags,x[,tags],sep = ": "),collapse = "\n"),
                                     size = 20,
                                     color = "black",
                                     boxcolor = "white",
                                     location = "+10+10") %>%
              magick::image_mosaic() %>%
              magick::image_write(paste0(save_path,.Platform$file.sep,x$ordner,"-",x$bildname))
          })

    # return missing and inaccesible paths
    return(list(pfade_ohne_zugriff_df = bilder_table$pfad_bild[!bilder_table$pfad_abrufbar],
                pfade_nicht_vorhanden_df = bilder_table$ordner[!bilder_table$pfad_gegeben]))
}

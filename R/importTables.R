#' Lädt alle Tabellen einer Session und vereinigt sie zu einer großen Tabelle. Die Tabellen entstammen der Klassifikation mit dem Programm FFM2 und liegen im csv-Format vor.
#'
#' @param file_path character, Pfad zum Ordner der Session. Beispielsweise "N:/Projekte/Gruenbrueckenmonitor_1708/Daten/Feldarbeit/Fotofallenmonitoring/session_07".
#' @param data character vector, bestimmt welche Tabellen geladen werden sollen: data = c("daten","ereignis").
#'
#' @return data.table/data.frame
#' @export
#'
#' @examples
#'
importTables <- function(file_path, data = c("daten", "ereignis")) {
  # read daten und ereignis csvs
  lapply(as.list(data) %>% setNames(data), function(i){
    filenames <- list.files(path = file_path,
                            recursive=T,
                            pattern= paste("^_",i,".csv",sep = ""),
                            full.names = TRUE)
    if(length(filenames) >0){

      #load the files and bind them together
      table <-
        do.call(rbind,
                c(lapply(filenames,
                         function(x) {
                           d <- data.table::fread(x, encoding = "UTF-8")
                           d <- cbind(d,
                                      "standort_id_folder" = basename(dirname(x)),
                                      "folder" = dirname(x))
                           return(d)}
                ), fill = TRUE))

      #correct column name
      colnames(table)[1] <- "s_foto_id"

      return(table)
    }
  })
}

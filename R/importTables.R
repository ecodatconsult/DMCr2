#' Lädt alle Tabellen einer Session und vereinigt sie zu einer großen Tabelle. Die Tabellen entstammen der Klassifikation mit dem Programm FFM2 und liegen im csv-Format vor.
#'
#' @param file_path character, Pfad zum Ordner der Session. Beispielsweise "N:/Projekte/Gruenbrueckenmonitor_1708/Daten/Feldarbeit/Fotofallenmonitoring/session_07".
#' @param data character vector, bestimmt welche Tabellen geladen werden sollen: data = c("daten","ereignis").
#'
#' @return data.table/data.frame
#' @export
#'
#'
importTables <- function(file_path, data = c("daten", "ereignis")) {
  # read daten und ereignis csvs

  # faster way to search folders (checked with microbenchmark-package) with many data compared to list.files if subfolders contain many files (such as images)
  # list all directories recursively
  all_dirs = list.dirs(path = file_path,
            full.names = TRUE,
            recursive = TRUE)

  # guess potential ereignis and daten files
  potential_files_df <- data.frame(
    path = paste0(
      rep(all_dirs, each = length(data)),
      paste0(
        .Platform$file.sep,
        "_",
        data,
        ".csv")
      )
    )

  # check if these files actually exist and filter
  files_df <-
  potential_files_df %>%
    dplyr::mutate(dir_id = rep(seq(dplyr::n()/length(data)), each = length(data))) %>%
    dplyr::mutate(data_type = rep(data, dplyr::n()/length(data))) %>%
    dplyr::mutate(file_exists = file.exists(path)) %>%
    dplyr::filter(file_exists)

  # check if all folders contain both - _daten.csv and _ereignis.csv
  files_per_dir <-
  files_df %>%
    dplyr::group_by(dir_id) %>%
    dplyr::summarise(count = dplyr::n())

  # if not - throw error
  if(!all(files_per_dir$count == length(data))){
    stop(paste0("Daten or Ereignis.csv missing in ", paste(dirname(files_df[files_df$dir_id %in% files_per_dir$dir_id, "path"]), collapse = ", ")))
  }


# iterate
  lapply(as.list(data) %>% setNames(data), function(i){
    filenames <- files_df$path[files_df$data_type == i]
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

checkSpeciesWrong <- function(importTables_list, speciesDB_vector){
  # session_path12 <- function() "C:/Fotofallen-App/Beispieldatensatz"
  # importTables_list = session_path12() %>%
  #   DMCr2::importTables() %>%
  #   DMCr2::processImportTables()
  #names in ereignis_import
  falsche_tierarten_df <- c(importTables_list$ereignis$tierart_1,
                            importTables_list$ereignis$tierart__2) %>%
    sort() %>%
    data.frame() %>%
    setNames("alt") %>%
    group_by(alt) %>%
    summarise(anzahl = n()) %>%
    filter(!alt %in% speciesDB_vector) %>%
    mutate(korrigiert = NA)

    return(falsche_tierarten_df)
}


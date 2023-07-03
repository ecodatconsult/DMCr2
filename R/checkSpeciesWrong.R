checkSpeciesWrong <- function(importTables_list, speciesDB_vector){
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


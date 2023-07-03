checkSpeciesCorr <- function(importTables_list, falsche_tierarten_df){

  ft_df <- data.table::as.data.table(falsche_tierarten_df)

  lapply(importTables_list, function(dt){
    dt <- data.table::merge.data.table(dt, ft_df, by.y = "alt", by.x = "tierart_1", all.x = TRUE)
    dt[, tierart_1 := ifelse(is.na(korrigiert), tierart_1, korrigiert)]
    dt[, c("korrigiert", "anzahl"):= NULL]

    dt <- data.table::merge.data.table(dt, ft_df, by.y = "alt", by.x = "tierart_2", all.x = TRUE)
    dt[, tierart_2 := ifelse(is.na(korrigiert), tierart_2, korrigiert)]
    dt[, c("korrigiert", "anzahl"):= NULL]

    return(dt)
  })
}


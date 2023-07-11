#' Korrigiert Tierartenbezeichnungen gemäß der manuellen Eingabe
#'
#' @param importTables_list list, Liste die mit DMCr2::importTables() und DMCr2::processImportTables() erstellt wurde
#' @param speciesDB_vector character vector, Tierartbezeichnungen in der Datenbank
#'
#' @return data.frame, Tabelle mit falschen, zu korrigierenden bzw überprüfenden Tierartbezeichnungen
#' @import data.table
#' @export
#'

checkSpeciesWrong <- function(importTables_list, speciesDB_vector){
  falsche_tierarten_df <- c(importTables_list$ereignis$tierart_1,
                            importTables_list$ereignis$tierart__2) %>%
    sort() %>%
    data.frame() %>%
    setNames("alt") %>%
    dplyr::group_by(alt) %>%
    summarise(anzahl = dplyr::n()) %>%
    dplyr::filter(!alt %in% speciesDB_vector) %>%
    dplyr::mutate(korrigiert = NA)

    return(falsche_tierarten_df)
}


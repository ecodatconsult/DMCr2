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
    mutate(alt = ifelse(alt == "", NA, alt)) %>%
    dplyr::summarise(anzahl = dplyr::n()) %>%
    dplyr::filter(!alt %in% speciesDB_vector)

  if(nrow(falsche_tierarten_df) > 0){
    falsche_tierarten_df <- falsche_tierarten_df %>%
    dplyr::mutate(vorschlag = sapply(alt, function(wrong_term) speciesDB_vector[agrep(pattern = wrong_term, x = speciesDB_vector)][1])) %>%
      dplyr::mutate(vorschlag = ifelse(is.na(vorschlag), "Kein ähnlicher\nEintrag in DB!", vorschlag)) %>%
      dplyr::mutate(korrigiert = "")

  }

    return(falsche_tierarten_df)
}


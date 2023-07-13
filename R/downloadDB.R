#' Ruft Daten von der FVA-Fotofallendatenbank ab.
#'
#' @param data Character, Bestimmt welche Art von Daten (data = "ereignisse", data = "bilder" oder data = "standorte")  abgefragt werden sollen. Es ist nur ein Wert erblaubt.
#' @param species Character vector mit Arten, von denen Daten abgefragt werden sollen. Der Wert "alle Arten" ruft Daten für alle in der Datenbank vorhandene Arten ab. Falls data = "standorte" werden nur Standorte abgefragt, an denen eine bestimmte Tierart in fotofallen.ereignisse vorkommt (tierart_1 oder tierart_2).
#' @param project Character vector, Auswahl eines Projekts, mehrerer oder aller Projekte, für das Daten abgefragt werden sollen. Der Wert "alle Projekte" ruft Daten für alle in der Datenbank vorhandene Projekte ab.
#'
#' @return data.frame
#' @export
#'
downloadDB <- function(data, species, project){

  con <- dbConnection()

  if("alle Arten" %in% species) species <- speciesDB()
  if("alle Projekte" %in% project) project <- projectsDB()


  standorte_with_species <- RPostgreSQL::dbGetQuery(con,glue::glue_sql("SELECT DISTINCT(standort_id) FROM fotofallen.ereignisse WHERE tierart_1 IN ({sps*}) OR tierart_2 IN ({sps*})", sps = species, .con = con)) %>%
    purrr::as_vector()

  projekt_ids <- RPostgreSQL::dbGetQuery(con,glue::glue_sql("SELECT projekt_id FROM fotofallen.projekte WHERE projekt_name IN ({pns*})",
                                                            pns = project, .con = con)) %>%
    purrr::as_vector()

  standorte_with_projekt <- RPostgreSQL::dbGetQuery(con,glue::glue_sql("SELECT standort_id FROM fotofallen.fotofallen_standorte WHERE projekt_id IN ({pns*})",
                                                            pns = projekt_ids, .con = con)) %>%
    purrr::as_vector()

  if(data == "ereignisse"){
    output <- RPostgreSQL::dbGetQuery(con,glue::glue_sql("SELECT * FROM fotofallen.ereignisse
                                                         WHERE standort_id IN ({sid*}) AND (tierart_1 IN ({sps*}) OR tierart_2 IN ({sps*}))",
                                                         sid = standorte_with_projekt,
                                                         sps = species, .con = con))
  }
  if(data == "bilder"){
    output <-RPostgreSQL::dbGetQuery(con,glue::glue_sql("SELECT * FROM fotofallen.bilder
                                                        WHERE standort_id IN ({sid*}) AND (tierart_1 IN ({sps*}) OR tierart_2 IN ({sps*}))",
                                                        sid = standorte_with_projekt,
                                                        sps = species, .con = con))
  }
  if(data == "standorte"){
    output <-sf::st_read(con,query = glue::glue_sql("SELECT * FROM fotofallen.fotofallen_standorte
                                                    WHERE standort_id IN ({sid*}) AND standort_id IN ({sps*})",
                                                    sid = standorte_with_projekt,
                                                    sps = standorte_with_species, .con = con))
  }
  DBI::dbDisconnect(con)
  return(output)
}

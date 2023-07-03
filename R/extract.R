#' Ruft Daten von der FVA-Fotofallendatenbank ab.
#'
#' @param data Bestimmt welche Art von Daten abgefragt werden sollen. Es ist nur ein Wert erblaubt.
#' @param species Character vector mit Arten, von denen Daten abgefragt werden sollen. Der Wert "alle Arten" ruft Daten für alle in der Datenbank vorhandene Arten ab.
#' @param project Auswahl eines Projekts oder alle, für das Daten abgefragt werden sollen. Der Wert "alle Projekte" ruft Daten für alle in der Datenbank vorhandene Projekte ab.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
extract <- function(data, species, project){

  require(RPostgreSQL)
  require(DBI)
  require(stringi)

  dbConnection(type = "read")

  # data frames
  df_ereignisse <- dbGetQuery(con,"SELECT standort_id, tierart_1 FROM fotofallen.ereignisse")
  df_bilder <- dbGetQuery(con,"SELECT standort_id, tierart_1 FROM fotofallen.bilder")
  df_standorte <- dbGetQuery(con,"SELECT standort_id, projekt_id FROM fotofallen.fotofallen_standorte")
  df_projects <- dbGetQuery(con,"SELECT * FROM fotofallen.projekte")

  # Standorte, wenn NICHT alle Arten
  if(species != "alle Arten"){
    rows_standorte_species_new <- c()
    for(species_one in species){
      rows_standorte_species <- which(df_ereignisse$tierart_1 == species_one)
      rows_standorte_species_new <-  append(rows_standorte_species_new,rows_standorte_species)
    }
    standorte_species <- df_ereignisse$standort_id[rows_standorte_species_new]
    standorte_species <- paste(unique(standorte_species),collapse = "', '")
  }

  # Artenliste
  species <- paste(unique(species),collapse = "', '")

  # Standorte, wenn EIN Projekt
  if(project != "alle Projekte"){
    #project_id
    project_id <- df_projects$projekt_id[df_projects$projekt_name == project]
    #standorte with project_id
    standorte_project <- df_standorte$standort_id[df_standorte$projekt_id == project_id]
    standorte_project <- paste(unique(standorte_project),collapse = "', '")
  }

  ## alle Projekte, alle Arten
    if(project == "alle Projekte" && species == "alle Arten"){
      if(data == "ereignisse"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.ereignisse;", sep = ""))
      }
      if(data == "standorte"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.fotofallen_standorte;", sep = ""))
      }
      if(data == "bilder"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.bilder;", sep = ""))
      }
    }

  ## alle Projekte, NICHT alle Arten
    if(project == "alle Projekte" && species != "alle Arten"){
      if(data == "ereignisse"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.ereignisse WHERE tierart_1 IN ('", stri_enc_toutf8(species), "');", sep = ""))
      }
      if(data == "standorte"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE standort_id IN ('", stri_enc_toutf8(standorte_species), "');", sep = ""))
      }
      if(data == "bilder"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.bilder WHERE tierart_1 IN ('", stri_enc_toutf8(species), "');", sep = ""))
      }
    }

  ## EIN Projekt, alle Arten
    if(project != "alle Projekte" && species == "alle Arten"){
      if(data == "ereignisse"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.ereignisse WHERE standort_id IN ('", stri_enc_toutf8(standorte_project), ")';", sep = ""))
      }
      if(data == "standorte"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id = '", stri_enc_toutf8(project_id), "';", sep = ""))
      }
      if(data == "bilder"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.bilder WHERE standort_id IN ('", stri_enc_toutf8(standorte_project), "');", sep = ""))
      }
    }

  ## EIN Projekte, NICHT alle Arten
    if(project != "alle Projekte" && species != "alle Arten"){
      if(data == "ereignisse"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.ereignisse WHERE tierart_1 IN ('", stri_enc_toutf8(species), "') AND standort_id IN ('", stri_enc_toutf8(standorte_project), "');", sep = ""))
      }
      if(data == "standorte"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.fotofallen_standorte WHERE standort_id IN ('", standorte_species, "') AND projekt_id = '", stri_enc_toutf8(project_id), "';", sep = ""))
      }
      if(data == "bilder"){
        df <- dbGetQuery(con, paste("SELECT * FROM fotofallen.bilder WHERE tierart_1 IN ('", stri_enc_toutf8(species), ")' AND standort_id IN ('", stri_enc_toutf8(standorte_project), "');", sep = ""))
      }
    }

  # Extrahieren des Wochentag
  #df$dof <- strftime(df$ts, format = "%a")

  assign("ergebnisse",df, envir = .GlobalEnv)
  rm(list=setdiff(c(ls()), 'ergebnisse'))

}

checkSpeciesWrong <- function(){

  require(svDialogs)
  require(stringi)
  require(dplyr)

  if(!exists("ereignis_import") & !exists("daten_import")){
    stop(print("Benötigte Daten 'ereignis_import' und/oder 'daten_import' fehlen. Führe die Funktion import() aus."))
  }

  #names in ereignis_import
  tierarten <- sort(c(ereignis_import$tierart_1,ereignis_import$tierart_2))

  #wrong names in ereignis_import = not in speciesList
  falsche_tierarten <- tierarten[!is.element(tierarten,speciesDB_vector)]

  #delete ""
  falsche_tierarten <- falsche_tierarten[falsche_tierarten != ""]

  #data table
  if(length(falsche_tierarten)>0){
    falsche_tierarten_all <- data.frame("alt" = falsche_tierarten)
    falsche_tierarten_df <- falsche_tierarten_all %>% count(alt)
    falsche_tierarten_df$korrigiert <- NA

    assign("falsche_tierarten_df",falsche_tierarten_df,envir = .GlobalEnv)

    rm(falsche_tierarten_all, species, falsche_tierarten, tierarten)
  }
}


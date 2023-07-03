checkSpecies <- function(){

  require(svDialogs)
  require(stringi)

  if(!exists("ereignis_import") & !exists("daten_import")){
    stop(print("Benötigte Daten 'ereignis_import' und/oder 'daten_import' fehlen. Führe die Funktion import() aus."))
  }

  #load species
  species <- species$new

  #names in ereignis_import
  tierarten <- sort(unique(c(ereignis_import$tierart_1,ereignis_import$tierart_2)))

  #wrong names in ereignis_import = not in speciesList
  falsche_tierarten <- tierarten[!is.element(tierarten,species)]

  #data table
  falsche_tierarten_all <- data.frame("alt" = falsche_tierarten, "korrigiert" = "")
  assign("falsche_tierarten_df",falsche_tierarten_all,envir = .GlobalEnv)

  #correct wrong species names
  if(length(falsche_tierarten[!is.na(falsche_tierarten)])>0){

    for(falsche_tierart in falsche_tierarten){

      #input correct name
      korrigierte_tierart <- dlgInput(paste("'",falsche_tierart,"' existiert nicht.\nWähle eine Bezeichnung von der Liste:\n", paste(stri_enc_toutf8(species), collapse = ", "),"\n\nKorrektur:",sep = ""), Sys.info()[""])$res

      #check if correction is in species
      fehler <- korrigierte_tierart[!is.element(korrigierte_tierart,species)]

      #run in loop while species name is not correct
      while(length(fehler)>0){
        korrigierte_tierart <- dlgInput(paste("'",falsche_tierart,"' existiert nicht.\nWähle eine Bezeichnung von der Liste:\n", paste(stri_enc_toutf8(species), collapse = ", "),"\nKorrektur:",sep = ""), Sys.info()[""])$res
        fehler <- korrigierte_tierart[!is.element(korrigierte_tierart,species)]
      }

      #correct the name in ereignis_import
      ereignis_import$tierart_1[ereignis_import$tierart_1 == falsche_tierart] <- korrigierte_tierart
      ereignis_import$tierart_2[ereignis_import$tierart_2 == falsche_tierart] <- korrigierte_tierart

      #correct the name in daten_import
      daten_import$tierart_1[daten_import$tierart_1 == falsche_tierart] <- korrigierte_tierart
      daten_import$tierart_2[daten_import$tierart_2 == falsche_tierart] <- korrigierte_tierart

      #df
      falsche_tierarten_all[falsche_tierarten_all$alt == falsche_tierart,"korrigiert"] <- korrigierte_tierart
    }
  }

  assign("falsche_tierarten_all",falsche_tierarten_all,envir = .GlobalEnv)

}


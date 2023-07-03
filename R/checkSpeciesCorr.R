checkSpeciesCorr <- function(){

  require(stringi)

  #data table
  falsche_tierarten <- falsche_tierarten_df$alt

  #correct wrong species names
  if(length(falsche_tierarten[!is.na(falsche_tierarten)])>0){

    for(falsche_tierart in falsche_tierarten){

      #input correct name
      korrigierte_tierart <- falsche_tierarten_df$korrigiert[falsche_tierarten_df$alt == falsche_tierart]

      #correct the name in ereignis_import
      ereignis_import$tierart_1[ereignis_import$tierart_1 == falsche_tierart] <- korrigierte_tierart
      ereignis_import$tierart_2[ereignis_import$tierart_2 == falsche_tierart] <- korrigierte_tierart
      assign("ereignis_import",ereignis_import,envir = .GlobalEnv)

      #correct the name in daten_import
      daten_import$tierart_1[daten_import$tierart_1 == falsche_tierart] <- korrigierte_tierart
      daten_import$tierart_2[daten_import$tierart_2 == falsche_tierart] <- korrigierte_tierart
      assign("daten_import",daten_import,envir = .GlobalEnv)

    }
  }

}


check <- function(data){

  if(data == "both"){
    data = c("daten","ereignis")
  }

  for(i in 1:length(data)){

    if(data[i] == "ereignis"){

      # check if all species names in ereignis_import are correct
      checkSpecies()

      # check if folders were renamed
      checkFoldernames()

      # load table with GPS positions and check if names of locations are correct
      checkLocationnames()

    }
  }
}

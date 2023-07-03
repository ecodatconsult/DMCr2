UIinput <- function(value, save = FALSE){

  #check if variable exists, run function only, if it does not
  if(!exists(value)){

    #ask for user input
    x <- readline(prompt = paste("Enter ",value,": ",sep = ""))

    #convert it to character type
    x <- as.character(x, encoding = 'UTF-8')

    if(save == TRUE){
      #name the variable after the value and save it
      assign(value, x, envir = .GlobalEnv)
    }else if(save == FALSE){
      #name the variable after the value
      assign(value, x)
    }

  #if variable exists, take it
  }else{
    assign("x", get(value))
  }

}

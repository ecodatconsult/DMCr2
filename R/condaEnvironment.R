#' Erstellung einer Conda-Umgebung für den MegaDetector
#'
#' @return conda env
#' @export
#'
#' @examples
#'
condaEnvironment <- function(){

  require(reticulate)


  if(!("MegaDetector-CPU" %in% conda_list()$name | "MegaDetector-GPU" %in% conda_list()$name)){
  shell("conda config")
  shell("conda config --set proxy_servers.http 10.127.255.17:8080")
  shell("conda config --set proxy_servers.https 10.127.255.17:8080")
  }


?Negate

  #prüfe, ob GPU verfügbar
  if(any(grep("NVIDIA",system("wmic path win32_VideoController get name", intern = TRUE))) == TRUE){
    GPU <- TRUE
  }else{
    GPU <- FALSE
  }

  #lege eine Conda-Umgebung mit den nötigen Paketen an, wenn noch nicht vorhanden

  #CPU
  if(GPU == FALSE){
    if(all(conda_list()$name != "MegaDetector-CPU")){
      conda_create(
        envname = "MegaDetector-CPU"
        ,packages = c("tensorflow==2.6" #neueste Version
                      ,"pandas"
                      ,"pillow"
                      ,"humanfriendly"
                      ,"matplotlib"
                      ,"tqdm"
                      ,"jsonpickle"
                      ,"requests"
                      )
        ,forge = TRUE
        ,channel = character()
        ,conda = "auto"
        ,python = "3.7"

      )
    }
  }

  #GPU
  if(GPU == TRUE){
    if(all(conda_list()$name != "MegaDetector-GPU")){
        conda_create(
          envname = "MegaDetector-GPU"
          ,packages = c("tensorflow-gpu==1.15" #ältere Version (letzte mit Spezifikation "-gpu")
                        ,"pandas"
                        ,"pillow"
                        ,"humanfriendly"
                        ,"matplotlib"
                        ,"tqdm"
                        ,"jsonpickle"
                        ,"requests"
                        )
          ,forge = TRUE
          ,channel = character()
          ,conda = "auto"
          ,python = "3.7"
      )
    }
  }

}

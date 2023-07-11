#' Debugged copy of shinyFiles::getVolumes()
#' This function is intended as an input to the roots parameter in fileGetter() and shinyFileChoose(). It returns a function that returns a named vector of available volumes on the system. This construction makes it dynamic so that a shinyFiles instance reflects new volumes as they get added (e.g. usb drives). The function takes a single argument giving names of volumes the developer wants removed from the return value.
#' @param exclude A vector of volume names to be excluded from the return value
#' @return character vector of available volumes
#' @export

shinyFiles_getVolumes <- function(exclude = NA){
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- dir_ls("/Volumes")
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(dir_exists("/media"))) {
      media <- dir_ls("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num = as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$", ":",
                                              volumes), ")")
    } else {
      volumes <- system(paste(wmic, "logicaldisk get Caption"),
                        intern = TRUE, ignore.stderr = TRUE)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- gsub(":$", ":/", volumes)
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  return(volumes)
}

#' Umwandlung der MegaDetector-Datei (JSON) in eine csv-Tabelle
#'
#' @param file_path Pfad zur Session
#'
#' @return
#' @export
#'
#' @examples
#'
outputMegaDetectorcsv <- function(file_path){

  require(rjson)
  require(dplyr)

  if(file.exists(paste(file_path,"/Megadetector_output.json",sep = ""))){

    #input: JSON file
    data <- fromJSON(file=paste(file_path,"/Megadetector_output.json",sep = ""))

    #data frame mit Informationen aus der JSON-Datei
    output_df <- data.frame(file = character(),max_detection_conf = double(), detections.category = character())
    for(i in 1:length(data$images)){
      if(length(data$images[[i]]$max_detection_conf) > 0 && data$images[[i]]$max_detection_conf != 0){
        output <- as.data.frame(data$images[[i]])
        output <- output[1,-c(grep("bbox",colnames(output)))]
      }else if(length(data$images[[i]]$max_detection_conf) == 0){
        output <- data.frame("file" = data$images[[i]]$file, "max_detection_conf" = NA, "detections.category" = NA)
      }else{
        output <- data.frame("file" = data$images[[i]]$file, "max_detection_conf" = 0, "detections.category" = "empty")
      }
      assign(paste("output",i,sep = ""),output)
      output_df <- full_join(output_df,output, all = TRUE)
    }

    #ersetze 1,2,3 mit Tier, Person, KFZ
    cols_category <- colnames(output_df)[grep("category",colnames(output_df))]
    for(col in cols_category){
      output_df[,col] <- sub("1","Tier", output_df[,col])
      output_df[,col] <- sub("2","Person", output_df[,col])
      output_df[,col] <- sub("3","KFZ", output_df[,col])
    }

    #output: csv-Datei
    write.csv2(output_df,file = paste(file_path,"/Megadetector_output.csv",sep = ""), append = FALSE)
  }
  rm(list=c(ls()))
}

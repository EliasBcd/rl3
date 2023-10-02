library(readr, include.only = 'read_csv')
library(base64enc)

decode_results <- function(path){
  if(grepl("Androw Daoud_1871270_assignsubmission_file_Manipulations_simples", path)){
    return(path)
  }
  else if(grepl('Manipulations|Fonctions|Exercice', path)){
  
  encoded <- tryCatch(suppressMessages(suppressWarnings(read.csv(path))),
                      # error = function(){cat("Error reading file", path, "\n")}
                      error = function(e){return(path)}
  )
  decoded <- list()
  # print(path)
  for(i in 1:nrow(encoded)){
    # print(i)
    decoded[[i]] <- tryCatch(suppressMessages(
      read_csv(
        rawToChar(
          base64decode(
            as.character(encoded[i,]))))),
      # error = function(e){cat("Error reading file", path, ", ", conditionMessage(e), "\n")})
      error = function(e){return(path)})
  }
  
  result <- as.data.frame(do.call("rbind", decoded))
  attributes(result)$spec <- NULL
  attributes(result)$row.names <- 1:nrow(result)
  return(result)
  }
  else{
    return(path)
    # print(path)
  }
}

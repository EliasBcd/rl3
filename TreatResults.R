library(readr, include.only = 'read_csv')
library(base64enc)

decode_results <- function(path){
  if(grepl("NotNow", path)){
    return(path)
  }
  else if(grepl('Manipulations|Fonctions|Exercice|Analyse|Representations|Logique|Control_Flow', path)){
  encoded <- tryCatch(suppressMessages(suppressWarnings(read.csv(path))),
                      # error = function(){cat("Error reading file", path, "\n")}
                      error = function(e){return(path)}
  )
  decoded <- list()
  # print(path)
  for(i in 1:nrow(encoded)){
    # print(path)
    decoded[[i]] <- tryCatch(
      suppressMessages(
      
      read_csv(
        rawToChar(
          base64decode(
            as.character(encoded[i,]))))) 
    ,
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

library(readr, include.only = 'read_csv')

decode_results <- function(file){
  encoded <- read_csv(file)
  
  for(i in 1:nrow(encoded)){
    decoded[[i]] <- suppressMessages(
      read_csv(
        rawToChar(
          base64decode(
            as.character(encoded[i,])))))
  }
  
  result <- as.data.frame(do.call("rbind", decoded))
  attributes(result)$spec <- NULL
  attributes(result)$row.names <- 1:nrow(result)
  return(result)
}
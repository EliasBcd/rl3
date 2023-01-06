library(readr, include.only = 'read.csv')
library(base64enc)

decode_results <- function(path){
  if(grepl(".txt.txt|Nages", path)){
    return(path)
  }
  else if(grepl('Manipulations|Assignations|travail|Vecteurs|Scripts|Tirages_aleatoires|Types|Valeurs|Representations|Logique|Dataframes|Control', path)){
  
  encoded <- tryCatch(suppressMessages(suppressWarnings(read.csv(path))),
                      # error = function(){cat("Error reading file", path, "\n")}
                      error = function(e){return(path)}
  )
  decoded <- list()
  #print(path)
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

dir <-  "R-Exercices/"
a <- list.files(dir, pattern = ".")
b <- lapply(a, function(x) paste(dir, x, sep=""))
c <- sapply(b, decode_results)

data <- NULL
failures <- c()
for (tab in c){
  if(is.data.frame(tab)){
    if(is.null(data)){
      data <- tab
    }
    else {
      tryCatch(data <- rbind(data, tab),
               error = function(e) cat("ERROR: ", conditionMessage(e), "\n"))
    }
  }
  else(
    failures <- c(failures, tab)
  )
}
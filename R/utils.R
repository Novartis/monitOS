wrap <- function(X, decimal=3) return(paste(round(X, decimal), collapse = ","))
unwrap <- function(X) return(as.numeric(unlist(strsplit(gsub(" ", "", X),","))))

formatNumeric <- function(myDT, cols){
  for(col in cols){
    myDT$x$data[[col]] <- as.numeric(myDT$x$data[[col]])
  }
  return(myDT)
}
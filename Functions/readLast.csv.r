
## reads latest version of a csv file in path (folder), identified with unique pattern

readLast.csv <- function(path, pattern, read = T, ...){
  
  # path, pattern, as in list.files. 
  # read whether to read csv and assign to output of function (TRUE), or return string of filename
  # ... more args to read.csv
  
  #extras <- match.call(expand.dots=FALSE)$...
  # make a list of arguments and then pass to do.call,,, 
  #extras <- list(...)
  #csv.call <- extras[which(names(extras) %in% formalArgs(list.files))]
  
  csv <- list.files(path, pattern = paste0(".*", pattern, ".*\\.csv$"), full.names = T)

  csv2 <- csv[which.max(file.mtime(csv))]
  cat(paste0("Reading: ", "'",basename(csv2),"'"))
  
  if(read) read.csv(csv2, ...) else return(csv2)

}

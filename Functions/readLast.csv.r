
## reads latest version of a csv file in path (folder), identified with unique pattern

readLast.csv <- function(path, pattern, read = T, ...){
  
  # path, pattern, as in list.files. 
  # read whether to read csv and assign to output of function (TRUE), or return string of filename
  # ... more args to read.csv OR list.files
  
  #extras <- match.call(expand.dots=FALSE)$...
  
  # make a list of arguments and then pass to do.call,,, 
  extras <- list(...)
  
  list.files.call <- c(list(path = path, pattern = paste0(".*", pattern, ".*\\.csv$"), full.names = T),
                       extras[which(names(extras) %in% formalArgs(list.files))])
  csv <- do.call(list.files, list.files.call)

  # get latest
  csv2 <- csv[which.max(file.mtime(csv))]

  cat(paste0("Reading: ", "'",basename(csv2),"'"))

  if(read) {
    # make csv call
    csv.call <- c(list(file = csv2), extras[which(names(extras) %in% formalArgs(read.table))])
    do.call(read.csv, csv.call)

  } else return(csv2)
  
}

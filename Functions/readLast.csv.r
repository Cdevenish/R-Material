
# pattern <- "audio_locations"
# path <- "C:/Users/55116479/Dropbox (Manchester Met)/R_online/JavaR/inData"

## reads latest version of a csv file in path (folder), identified with unique pattern

readLast.csv <- function(path, pattern, read = T, ...){
  
  # path, pattern, as in list.files. 
  # read whether to read csv and assign to output of function (TRUE), or return string of filename
  # ... more args to list.files or read.csv
  
  csv <- list.files(path, pattern = paste0(".*", pattern, ".*\\.csv$"), full.names = T, ...)
  #csv2 <- csv[grepl(pattern, csv)] # or read all and get time for all.. 
  csv2 <- csv[which.max(file.mtime(csv))]
  
  if(read) read.csv(csv2, ...) else return(csv2)
  cat(paste0("File read: ", "'",basename(csv2),"'"))
  
}



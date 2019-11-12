### Function to get mp3 xtracts from a gdetects object


## TO DO>..  change file name with new start time... 


getExtract <- function(x, fn, buffer = 5, format = c("wav", "mp3"), dir){
  
  
  library(tuneR)
  
  # x is a gedetect object
  # fn = vector of filenames (same nrow as gdetect), or a cdetects or cssores object corresponding to gdetect object
  # buffer is time in seconds on either side of detection hit time
  # format for exported audio extract, - only wav at the moment
  # dir - directory for extract, if missing, working directory is used.
  
  # # edit file name - insert new start time and section number
  # newStartTime <- dateTime.start[i] + from[i] * f
  # new.bn <- basename(sub("_[[:digit:]]{6}_", format(newStartTime, "_%H%M%S_"), x[i]))
  # new.bn <- sub("\\.wav$", "_mod.wav", new.bn)
  
  from <- x$time - buffer
  to <- x$time + buffer
  
  ## need to work this out for multplie survey and gdetects objects. t... 
  
  if(any(class(fn) %in% c("detectionList", "templateScores"))) { fn <- rep(fn@survey.name, nrow(x))}
  
  #dirname <- dirname(fn)
  # get new filenames
  new.fn <- vector(mode = "character", length = nrow(x))
  
  for(i in 1:nrow(x)){
    
    tmp <- tuneR::readWave(fn[i], from[i], to[i], units = "seconds")
    
    new.bn <- basename(fn[i])
    new.bn <- sub("\\.wav$", sprintf("_extract%02d.wav",i), new.bn)
    
    #newStartTime <- dateTime.start[i] + from[i] * f
    # new.bn <- basename(sub("_[[:digit:]]{6}_", format(newStartTime, "_%H%M%S_"), x[i]))
    # new.bn <- sub("\\.wav$", "_mod.wav", new.bn)
    
    
    if(missing(dir)) {
      
      new.file.name <- file.path(dirname(fn[i]), new.bn)} else {
        
        new.file.name <- file.path(dir, new.bn)
      }
    
    tuneR::writeWave(tmp, filename = new.file.name, extensible = TRUE)
    
    new.fn[i] <- new.file.name
    
  }
  
  invsible(data.frame(filename = fn, extract = new.fn))
  
}
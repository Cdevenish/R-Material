
## function to split wav files into smaller chunks, eg 15 second blocks for analysis or 10 minutes for 
## easier viewing in audacity

#tz <- "America/La_Paz"
#tz <- "Asia/Jakarta"

splitWav <- function(x, interval = 600, units = c("seconds", "minutes"), dir, tz = "Asia/Jakarta"){
  
  library(tuneR)
  
  # x is the vector of paths to wav files on disk, eg from list.files()
  
  # interval - can be one of three options:
  
  # 1) time length of regular intervals to split file into, in units (minutes or seconds). 
  # Note: Last segment may not be of this length
  
  # 2) interval is a start and stop time, as a length 2 character vector, as in c('HH:MM:SS','HH:MM:SS') 
  # All files will be split by same time interval 
  
  # 3) A list of length two vectors with two datetime objects showing start and end points, with date corresponding 
  # to date of file to be split. In this case length(interval) == length(x)
  
  # Note: interval must correspond to same date (ie not over midnight) in all cases
  
  # Units for regular interval, either seconds or minutes. Otherwise ignored 
  # dir - is folder to save wav in, (within working directory, or full path)
  
  # tz - timezone
  
  units <- match.arg(units)
  ## set multiplier if units in minutes
  if(units == "seconds" | missing(units)) f <- 1 else f <- 60
  
  # Check format of interval
  
  ## date time for each file
  date.text <- gsub("_", "", regmatches(x, regexpr("_[[:digit:]]{8}_", text = x)))
  time.text <- gsub("_", "", regmatches(x, regexpr("_[[:digit:]]{6}_", text = x)))
  dateTime.start <- strptime(paste(date.text, time.text), tz = tz, format = "%Y%m%d %H%M%S")
  
  # get duration of each audio
  w <- lapply(x, readWave, header = T)
  duration <- sapply(w, function(z) (z$samples/z$sample.rate)) # duration in seconds
  
  fn <- vector() # create vector for new filenames
  
  if(length(interval)==1 & is.numeric(interval)){
  
    duration <- duration/f # convert duration to units of interval
    
    # Create vectors of sequences of starting and ending points (one vector for each filename)
    from <- lapply(duration, function(x) seq(0, x, interval))
    to <- mapply(function(x, y) c(x[-1], y), from, duration)
    
    # do the splitting here - could make this neater by combining this section for both types of interval
    
    # read sections and save for each file
    pb <- txtProgressBar(min = 0, max = sum(sapply(from, length)), style = 3, width = 100)
    
    fn.old <- x
    
    n <- 0
    
    for(i in seq_along(from)){
      
      for(j in seq_along(from[[i]])){
        
        n <- n+1
        setTxtProgressBar(pb, n)
        
        # get wav file
        tmp <- readWave(x[i], from = from[[i]][j], to = to[[i]][j], units = units)
        
        # edit file name - insert new start time and section number
        newStartTime <- dateTime.start[i] + from[[i]][j] * f
        new.bn <- basename(sub("_[[:digit:]]{6}_", format(newStartTime, "_%H%M%S_"), x[i]))
        new.bn <- sub("\\.wav$", sprintf("_s%02d.wav", j), new.bn)
        
        # add path to file name
        if(missing(dir)) {
          new.file.name = file.path(dirname(x[i]), new.bn)} else {
            new.file.name = file.path(dir, new.bn)
          }
        
        writeWave(tmp, filename = new.file.name)
        # or use seewave::savewav if need to change file type?...
        
        fn[i] <- new.file.name
        
      }
    }
    
    close(pb)
    
    
  } else {
    
    if(class(interval) %in% c("POSIXt", "list", "character")){ # not so good, but for now...
      
      # initial check that the interval is specified in the correct format, ie HH:MM:SS, length 2, chr vector
      if(length(interval) == 2 & all(grepl("[[:digit:]]{2}\\:[[:digit:]]{2}\\:[[:digit:]]{2}$", x = interval))){
        
        # does all the files for the same interval:
        # get desired start and end times for each date of audio file
        start.times <- strptime(paste(date.text, interval[1]), tz = tz, format = "%Y%m%d %H:%M:%S")
        end.times <- strptime(paste(date.text, interval[2]), tz = tz, format = "%Y%m%d %H:%M:%S")
      }
      
      # In case of POSIX - list of c(POSIX from, POSIX to). Must be same length as x
      if(all(sapply(lapply(interval, class), function(x) "POSIXt" %in% x))){
        
        if(length(x) != length(interval)) stop("interval must be same length as x if using POSIXt format")
        
        # get duration specified by two times in interval
        # get desired start and end times for each date of audio file
        start.times <- lapply(interval, function(x) x[1])
        end.times <- lapply(interval, function(x) x[2])
        
      }
      
      # duration (in seconds) of desired audio extract
      int.duration <- mapply(function(x,y) difftime(x, y, units = "secs"), end.times, start.times)
      
      
      #actual audio start and end times
      # dateTime.start  is start
      dateTime.end <- dateTime.start + duration
      
      # filter out those files whose audio is not within the interval specified by `interval` 
      
      # difftime(t2, t1) == t2 - t1
      
      # which files are within these times? Get Time in seconds from start of audio file to start of desired time
      dt <- mapply(function(x, y) difftime(x, y, units = "secs"), start.times, dateTime.start)
      # dt is period (in seconds) of desired start time after start of audio file
      
      
      ind <- (dt + int.duration) > 0 & (dt + int.duration) < duration 
      # index of files where desired length of audio is within file
      
      from <- rep(NA, length(start.times))
      to <- rep(NA, length(start.times))
      
      from[ind] <- ifelse(dt[ind]<0 , 0, dt[ind])
      
      des.end <- from + int.duration # desired duration in seconds after start of audio file
      to <- ifelse(des.end<duration, des.end, duration)
      # from; to
      
      ## do the wave splitting here... same as above
      
      fn.old <- x[ind]
      
      # read sections and save for each file
      # max = sum(sapply(from, length))
      pb <- txtProgressBar(min = 0, max = sum(!is.na(from)), style = 3, width = 100)
      
      n <- 0
      
      for(i in seq_along(from)[!is.na(from)]){
        
        n <- n+1
        setTxtProgressBar(pb, n)
        
        # get wav file
        tmp <- readWave(x[i], from = from[i], to = to[i], units = units)
        
        # edit file name - insert new start time and section number
        newStartTime <- dateTime.start[i] + from[i] * f
        new.bn <- basename(sub("_[[:digit:]]{6}_", format(newStartTime, "_%H%M%S_"), x[i]))
        new.bn <- sub("\\.wav$", "_mod.wav", new.bn)
        
        # add path to file name
        if(missing(dir)) {
          new.file.name <- file.path(dirname(x[i]), new.bn)} else {
            new.file.name <- file.path(dir, new.bn)
          }
        
        writeWave(tmp, filename = new.file.name)
        # or use seewave::savewav if need to change file...
        
        # use which to get sequential fn 1:n, rather than filling with NAs..  cause of i... is not sequential
        fn[which(i == seq_along(from)[!is.na(from)])] <- new.file.name
        
      }
      
      close(pb)
      
    } else stop("interval must be either a numeric vector (length 1), character vector (length 2), or list (POSIXt)")
  }
  
  #res <- data.frame(originalFile = x, newFile = fn, start = from, end = to) #, startTime = )
  return(list(newFile = fn, oldFile = fn.old))
}



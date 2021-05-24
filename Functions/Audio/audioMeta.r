## function to extract date and start times and recorder ID

## eg non reproducible example
# f <- list.files("PATH TO AUDIO FILES", "\\.wav$", full.names = T)
# audio.df <- audioMeta(f)


audioMeta <- function(x, tz = "Asia/Jakarta", sort = F, parallel = FALSE, nCores, ...){
  
  library(tuneR)
  
  # x is a vector of filepaths (eg from list.files() with files of the type:
  # without coords
  # 22_20181013_081816_Loreto_time.wav
  # OR
  # with coords
  # 22_20181014_090035_Loreto_time [-15.2091 -64.7611].wav
  
  # tz is the time zone in format given by OlsonNames()
  # sort. T F 
  
  
  # get duration
  if(parallel){
    
    if(missing(nCores)) nCores <- parallel::detectCores()-1
    
    library(parallel)
    cl <- makeCluster(nCores)
    clusterExport(cl, c("x"), envir=environment()) # export filenames - search in local function environment
    
    clusterEvalQ(cl, {
      library(tuneR)
    })
    
    w <- parLapply(cl, x, function(z) try(tuneR::readWave(z, header = T), silent = T))
    
    stopCluster(cl)
    
  } else w <- lapply(x, function(z) try(tuneR::readWave(z, header = T), silent = T))
  
  
  
  tryError.ind <- vapply(w, function(x) inherits(x, "try-error"), logical(1))
  noErr <- sum(tryError.ind)

  # if any wavs failed to read, then remove from w AND x and continue, with warning at end
  if(noErr != 0){
    
    w <- w[!tryError.ind]
    dodgy.files <- x[tryError.ind]
    x <- x[!tryError.ind]
    warning(paste(noErr, "wav files failed to read - possibly corrupt. Check these files:\n", 
                  paste(dodgy.files, collapse = "\n")), call. = T)
    
  }
  
  duration <- sapply(w, function(z) (z$samples/z$sample.rate)/60) # in minutes
  
  ## get some file info
  f.size <- file.size(x) * 0.000001 # in megabytes
  
  xn <- basename(x) # remove directories 
  
  #extract date and time texts
  date.text <- gsub("_", "", regmatches(xn, regexpr("_[[:digit:]]{8}_", text = xn))) # could modify m to one less
  # character each side... 
  time.text <- gsub("_", "", regmatches(xn, regexpr("_[[:digit:]]{6}_", text = xn)))
  
  #reg <- "\\[[[:punct:]]?[[:digit:]]+\\.[[:digit:]]+\\s[[:punct:]]?[[:digit:]]+\\.[[:digit:]]+" # both coords
  reg <- "\\[.+\\]"
  
  coord.text <- gsub("\\[|\\]", "", regmatches(xn, regexpr(reg, text = xn)))
  xycoords <- strsplit(coord.text, split = " ")
  y.coord <- sapply(xycoords, function(x) as.numeric(x[1]))
  x.coord <- sapply(xycoords, function(x) as.numeric(x[2]))
  
  coord.ind <- grepl(pattern = "\\[", x = xn) # are coords included in filename?
  
  date_start <- as.Date(date.text, format ="%Y%m%d")
  dateTime_start <- strptime(paste(date.text, time.text), tz = tz, format = "%Y%m%d %H%M%S")
  
  dateTime_end <- dateTime_start + duration*60 # duration in minutes, so *60
  #date_end <- date + (duration/60) %/% 24  # duration in hours, as an integer division of 24
  date_end <- as.Date(dateTime_end, tz = tz, format = "%Y%m%d") # maybe safer... 
  
  # recorder ID - for recorders to start, optionally, with a single letter and then two digits
  id <- gsub("_", "", regmatches(xn, regexpr("^[[:alpha:]]?[[:digit:]]{1,2}_", text = xn)))
  
  mountain = sub(".*\\/Audio\\/([[:alpha:]]*_?[[:alpha:]]*)_\\d*\\/.*", "\\1", x)
  folder = sub(".*\\/Audio\\/\\d{0,2}_?([[:alpha:]]*_?[[:alpha:]]*_\\d*)\\/.*", "\\1", x)
  SID <- sub(".*_(\\d)", "S\\1", folder)
  
  info.df <- data.frame(fileLoc = dirname(x), # file location
                        filename = xn,
                        path = file.path(dirname(x), xn),
                        size = f.size,
                        recorder = id,
                        mountain = mountain,
                        S_ID = SID,
                        date_start = date_start,
                        time_start = format(dateTime_start, "%H:%M"),
                        dateTime_start = dateTime_start,
                        date_end = date_end,
                        time_end = format(dateTime_end, "%H:%M"),
                        dateTime_end = dateTime_end,
                        x = NA,
                        y = NA,
                        duration = duration, stringsAsFactors = F,...)
  
  info.df[coord.ind,c("x", "y")] <- cbind(x.coord, y.coord)
  
  ## order by date and start time
  if(sort) info.df <- info.df[order(info.df$dateTime_start),]
  
  #rm(date.text, time.text, coord.text, xycoords, coord.ind, x.coord, y.coord)
  
  info.df
  #str(info.df)
  
}


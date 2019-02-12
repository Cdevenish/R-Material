## function to extract date and start times and recorder ID

audioMeta <- function(x, tz = "America/La_Paz"){
  
  library(tuneR)
  
  # x is a vector of filepaths (eg from list.files() with files of the type:
  # without coords
  # 22_20181013_081816_Loreto_time.wav
  # OR
  # with coords
  # 22_20181014_090035_Loreto_time [-15.2091 -64.7611].wav
  
  # tz is the time zone in format given by OlsonNames()
  
  # get duration
  w <- lapply(x, readWave, header = T)
  duration <- sapply(w, function(z) (z$samples/z$sample.rate)/60) # in minutes
  
  ## get some file info
  f.size <- file.size(x) * 0.000001 # in megabytes
  
  x <- basename(x) # remove directories 
  
  #extract date and time texts
  date.text <- gsub("_", "", regmatches(x, regexpr("_[[:digit:]]{8}_", text = x))) # could modify m to one less
  # character each side... 
  time.text <- gsub("_", "", regmatches(x, regexpr("_[[:digit:]]{6}_", text = x)))
  
  #reg <- "\\[[[:punct:]]?[[:digit:]]+\\.[[:digit:]]+\\s[[:punct:]]?[[:digit:]]+\\.[[:digit:]]+" # both coords
  reg <- "\\[.+\\]"
  
  coord.text <- gsub("\\[|\\]", "", regmatches(x, regexpr(reg, text = x)))
  xycoords <- strsplit(coord.text, split = " ")
  y.coord <- sapply(xycoords, function(x) as.numeric(x[1]))
  x.coord <- sapply(xycoords, function(x) as.numeric(x[2]))
  
  coord.ind <- grepl(pattern = "\\[", x = x)
  
  date <- as.Date(date.text, format ="%Y%m%d")
  dateTime <- strptime(paste(date.text, time.text), tz = tz, format = "%Y%m%d %H%M%S")
  
  # recorder ID - for recorders to start, optionally, with a single letter and then two digits
  id <- gsub("B|_", "", regmatches(x, regexpr("^[[:alpha:]]?[[:digit:]]{1,2}_", text = x)))
  
  
  info.df <- data.frame(file = x,
                        size = f.size,
                        recorder = id,
                        date = date,
                        time = format(dateTime, "%H:%M"),
                        dateTime = dateTime,
                        x = NA,
                        y = NA,
                        duration = duration)
  
  info.df[coord.ind,c("x", "y")] <- cbind(x.coord, y.coord)
  
  #rm(date.text, time.text, coord.text, xycoords, coord.ind, x.coord, y.coord)
  
  info.df
  
}


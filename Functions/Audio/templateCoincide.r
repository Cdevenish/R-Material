## How many templates coincide at the same place?

## threshold of within how many seconds?



templateCoincide <- function(x, round = 0, threshold, save.label = T, w){
  
  
  # x is getdetections object, or data frame with getdetections object, and filename as extra column
  # w is character vector to append to filenames if saving labels
  
  if("filename" %in% colnames(x)) filenames <- T else filenames <- F
  
  # if no threshold, then use 60% of number of templates
  if(missing(threshold)) threshold <- ceiling(length(unique(x$template)) * 0.6)
  
  # round time to nearest round... 
  if(round == 0) x$round.time <- round(x$time, 0) else x$round.time <- x$time - (x$time %% round)
  
  # for each hit time, get names of templates
  
  #z <- length(split(x, x$filename))
  
  if(filenames) {
    
    tmp2 <- lapply(split(x, x$filename), function(z) tapply(z$template, z$round.time, unique))
    
    } else {
    
    tmp2 <- list(tapply(x$template, x$round.time, unique))
  }
  
  res.list <- lapply(tmp2, function(z){
    
    ## reduce to only those where number of templates at hit is above threshold
    ind <- which(sapply(z, length) >= threshold)
    
    ## get number of templates per time
    count <- unname(sapply(z[ind], length))
    
    res <- data.frame(start = as.numeric(names(z[ind])),
                      stop = as.numeric(names(z[ind])), 
                      count = count,
                      label = unname(sapply(z[ind], paste, collapse = ","))) 
    
  })
  
  res <- do.call(rbind, res.list)
  
  # # reduce to only those where number of templates at hit is above threshold ## previous version for single gdetects
  # ind <- which(sapply(tmp2, length) >= threshold)
  # 
  # # get number of templates per time
  # count <- unname(sapply(tmp2[ind], length))
  # 
  # res <- data.frame(start = as.numeric(names(tmp2[ind])),
  #                   stop = as.numeric(names(tmp2[ind])), 
  #                   count = count,
  #                   label = unname(sapply(tmp2[ind], paste, collapse = ",")))
  
  
  
  if(save.label) {
    
    if(missing(w)) fn <- "labels_" else fn <- paste0(gsub("\\.[[:alpha:]]{3,}", "", basename(w)), "_label")
    write.table(res, row.names = F, col.names = F, sep = "\t",
                file = tempfile(pattern = fn, tmpdir = getwd(), fileext = ".txt"))
  }
  
  res
}
### Function to read txt files output from MAVIS ####

fn <- "mavis/MAVIS out NVC constancy.TXT"
idCol <- "unID"

mavis_IN <- function(fn, idCol, group = T){
  
  # fn - full path to file (a text file)
  # idCol - a length one character vector to name ID column in resulting data frame
  # logical, if MAVIS is run using groups. Always T for moment.
  
  #load data as basic lines
  mavis.raw <- readLines(fn)
  
  ## By group option
  if(group) plotName = "Group"
  
  #mavis.raw <- gsub(pattern = "Group",replacement = plotName,x = mavis.raw)
  # check group is the ID name
  
  ## extract group headings - per line
  #sum(grepl(plotName, mavis.raw)) # should be number of quadrats
  mHead <- mavis.raw[grepl(plotName, mavis.raw)]
  
  #sum(sapply(mavis.raw, nchar)==0) # should equal number of quadrats = the empty lines between
  #sum(grepl("^\\s*$", mavis.raw)) # better with regex
  
  # get separator indices
  mSep <- which(grepl("^\\s*$", mavis.raw)) # lines between each set of quadrat NVC options
  # head(mSep); tail(mSep)
  
  # extract data blocks between seps
  # plus two, one for heading, one for space. 
  start <- c(2, mSep[1:(length(mSep)-1)]+2) # remove empty line after last quadrat (last line of mavis out)
  stop <- mSep-1
  
  mData <- mapply(function(x,y) mavis.raw[x:y], start, stop, SIMPLIFY = F)
  #mData[[1]]
  #str(mData[1:3])
  mRaw = unlist(mData)
  mav.df <- data.frame(id = seq_along(mRaw), Groups =rep(mHead, lengths(mData)), mRaw = mRaw, stringsAsFactors = F)
  
  # ad ID column
  mav.df$tmp <- as.numeric(sub(paste0(plotName, " "), "", mav.df$Groups))
  colnames(mav.df)[4] <- idCol
  
  #head(mav.df)
  mav.df$NVC <- sub("NVC:\\s*([[:alnum:]]*)\\s([[:digit:]]*\\.[[:digit:]]*)", "\\1", mav.df$mRaw)
  mav.df$NVC_base <- gsub(pattern = "a|b|c|d|e|f|g|h", replacement = "", x = mav.df$NVC)
  mav.df$score <- as.numeric(sub("NVC:\\s*([[:alnum:]]*)\\s([[:digit:]]*\\.[[:digit:]]*)", "\\2", 
                                 mav.df$mRaw))
  mav.df$plot_rank <- unlist(tapply(mav.df$score, mav.df[,idCol], 
                                    function(x) rank(-x, ties.method = "first")))  
  #head(mav.df)
  
  mav.df$Groups <- NULL
  mav.df$mRaw <- NULL
  
  mav.df
  
}


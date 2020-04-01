### Function to read txt files output from MAVIS ####

mavis_IN <- function(fn, idCol, group = T, type = c("NVC", "ELL")){
  
  # fn - full path to file (a text file)
  # idCol - a length one character vector to name ID column in resulting data frame
  # logical, if MAVIS is run using groups. Always T for moment.
  # type: NVC to return NVC categories, ELL to return Ellenberg, CSR, CVS values. Data sets must be 
  # suitable outputs from MAVIS in choosing either option.
  
  type <- match.arg(type)
  
  if(class(idCol) != "character" & length(idCol) > 1) stop("idCol must be a length 1 character vector")
  
  if(!file.exists(fn)) stop("Input file does not exist")
  if(!tolower(sub(".*\\.([[:alpha:]]{3,})$", "\\1", fn)) == "txt") {
    stop("fn must be a text file (output from MAVIS)")
    }
  
  #load data as basic lines
  mavis.raw <- readLines(fn)
  
  if(type == "NVC"){
  
    ## By group option
    if(group) plotName = "Group"
    if(missing(idCol)) idCol <- plotName

    # check group is the ID name - not done
    
    ## extract group headings - per line
    #sum(grepl(plotName, mavis.raw)) # should be number of quadrats
    mHead <- mavis.raw[grepl(plotName, mavis.raw)]
    
    # get separator indices
    mSep <- which(grepl("^\\s*$", mavis.raw)) # lines between each set of quadrat NVC options
    # head(mSep); tail(mSep)
    
    # extract data blocks between seps.  plus two, one for heading, one for space. 
    start <- c(2, mSep[1:(length(mSep)-1)]+2) # remove empty line after last quadrat (last line of mavis out)
    stop <- mSep-1
    
    mData <- mapply(function(x,y) mavis.raw[x:y], start, stop, SIMPLIFY = F)
    
    mRaw = unlist(mData)
    mav.df <- data.frame(id = seq_along(mRaw), 
                         Groups =rep(mHead, lengths(mData)), 
                         mRaw = mRaw, stringsAsFactors = F)
    
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
    # remove these unneeded cols
    mav.df$Groups <- NULL
    mav.df$mRaw <- NULL
    
  }
  
  if(type == "ELL"){
    
    plotName = "Plot"
    if(missing(idCol)) idCol <- plotName
    
    mHead <- mavis.raw[grepl(plotName, mavis.raw)]
    mSep <- which(grepl("^\\s*$", mavis.raw)) # lines between each set of quadrat NVC options
    
    # extract data blocks between seps.  plus two, one for heading, one for space. 
    start <- c(2, mSep[1:(length(mSep)-1)]+2) # remove empty line after last quadrat (last line mavis out)
    stop <- mSep-1
    
    mData <- mapply(function(x,y) mavis.raw[x:y], start, stop, SIMPLIFY = F)
    
    # Get CVS data
    CVS <- lapply(mData, function(x) {
      
      cvs <- x[grepl("CVS", x)]
      cvs.spp <- sub("CVS:.*with.no.data:\\s(.*)", "\\1", cvs[1])
      cvs.class <- sub("CVS:.class.([[:digit:]]*)", "\\1", cvs[2])
      cbind(cvs.spp, cvs.class)
    })
    CVS[[1]]
    
    # Get ELLenberg data
    ELL <- lapply(mData, function(x) {
      
      ell.regex <- "ELL:\\sLight\\s([[:digit:]]+\\.[[:digit:]]*);\\sWetness\\s([[:digit:]]+\\.[[:digit:]]*);\\spH\\s([[:digit:]]+\\.[[:digit:]]*);\\sFertility\\s([[:digit:]]+\\.[[:digit:]]*)"
      
      ell <- x[grepl("ELL", x)]
      ell.spp <- sub("ELL:.*species:\\s(.*)", "\\1", ell[1])
      ELL_Light <- as.numeric(sub(ell.regex, "\\1", ell[2]))
      ELL_Wet <- as.numeric(sub(ell.regex, "\\2", ell[2]))
      ELL_pH <- as.numeric(sub(ell.regex, "\\3", ell[2]))
      ELL_Fert <- as.numeric(sub(ell.regex, "\\4", ell[2]))
      list(ell.spp, cbind(ELL_Light, ELL_Wet, ELL_pH, ELL_Fert))
    })

    # GET CSR data 
    CSR <- lapply(mData, function(x) {
      
      csr.regex <- "CSR:\\sC:\\s([[:digit:]]+\\.[[:digit:]]*)\\s{1,}S:\\s([[:digit:]]+\\.[[:digit:]]*)\\s{1,}R:\\s([[:digit:]]+\\.[[:digit:]]*)"
      
      # -1.#J  when no CSR data.. this is converted to NA by as.numeric. Suppressing warning to avoid
      # a warning about this.
      
      csr <- x[grepl("CSR", x)]
      csr.spp <- sub("CSR:.*species.with.no.data:\\s(.*)", "\\1", csr[1])
      C <- suppressWarnings(as.numeric(sub(csr.regex, "\\1", csr[2])))
      S <- suppressWarnings(as.numeric(sub(csr.regex, "\\2", csr[2])))
      R <- suppressWarnings(as.numeric(sub(csr.regex, "\\3", csr[2])))
      list(csr.spp, cbind(C, S, R))
    })
    
    ## TO DO.... 
    # BIO
    ## Not returning species lists
    
    CVS.df <- data.frame(do.call(rbind, CVS), stringsAsFactors = F)
    CVS.df$cvs.class <- as.numeric(CVS.df$cvs.class)
    
    ELL.df <- data.frame(do.call(rbind, lapply(ELL, function(x) x[[2]])), stringsAsFactors = F)
    CSR.df <- data.frame(do.call(rbind, lapply(CSR, function(x) x[[2]])), stringsAsFactors = F)
    
    plotID <- as.numeric(sub(paste0(plotName, " "), "", mHead))
    
    mav.df <- data.frame(id = seq_along(mHead), 
                         tmp = plotID,
                         CVS_class = CVS.df$cvs.class,
                         ELL.df, CSR.df, stringsAsFactors = F)
    
    colnames(mav.df)[2] <- idCol
    
  }
  
  #head(mav.df)
  mav.df
  
}



## Reads audacity label files

# testing
# type = "Audacity"
# rename = T
# includeErrors = T

## TODO 
## at moment, errors are sequential. eg if removed for frq erros, then won't show up in name errors.
## Change res names so that error checking is indiependent. for now with repeated rows in out


readLabels <- function(x, type, names2match, rename = T, includeErrors = F){
  
  # x are file paths to label text files
  # type is software used to create labels ie one of c("Audacity", "Raven")
  # renames is to rename each template with unique id within name (eg. in species)
  #
  # names2match is optional character vector of (species) names/codes to match exactly with label name.
  # If names do not match a warning is given, showing where they do not match.
  
  # includeErrors - logical. if T, then a list is returned with first element a data frame with 
  # label info, and a second element with errors in freq/time/name filters
  # NOTE this argument changes the class and length of the returned object.
  
  #type <- c("Audacity", "Raven")
  #
  
  if(missing(type)){
    
        type <- sapply(x, function(y) {
          ifelse(substr(readLines(y, 1, warn = F), 1,5) == "Selec", "Raven", "Audacity")
        })
  } else type <- match.arg(type)
  
  # table(type)
  if(!length(type) %in% c(1, length(x))) stop("Check label files")
  
  
  audacity.read <- function(x){
    
    labs <- read.table(x, header = F, sep = "\t", stringsAsFactors= F)
    e.ind <- seq(2,nrow(labs),2)
    o.ind <- seq(1,nrow(labs),2)
    
    res <- cbind(labs[o.ind,], labs[e.ind, 2:3]) 
    colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
    res <- data.frame(path = x,
                      id  = basename(x), 
                      name = res$name, 
                      lapply(res[-3], as.numeric), 
                      stringsAsFactors = F)
  }
  
  raven.read <- function(x) {
    
    labs <- read.table(x, header = T, sep = "\t", stringsAsFactors = F)
    res <- labs[,c("Begin.Time..s.","End.Time..s.", 
                   "Low.Freq..Hz.", "High.Freq..Hz.", "Annotation")]
    colnames(res) <- c("start", "stop", "minFreq", "maxFreq", "name")
    res <- data.frame(path = x,
                      id  = basename(x), 
                      name = res$name, 
                      res[,c(1:4)], 
                      stringsAsFactors = F)
    
  }
  
  if(length(type) == 1){
    
    labs.df <- switch(type, 
                    
                    Audacity = lapply(x, audacity.read),
                    Raven = lapply(x, raven.read)
                    )
      } else {
        
        labs.df <- mapply(function(z,w) {
          
          switch(z, 
                 
                 Audacity = audacity.read(w),
                 Raven = raven.read(w)
          )}, type, x, SIMPLIFY = F)
    
      }
  
  # head(labs.df)
  
  res <- do.call(rbind, labs.df) ## produces a data frame
  rownames(res) <- NULL
  # head(res); tail(res)
  
  ## Check minFreq maxFreq and timings for mistakes
  
  ## Freq filter
  ## check frequencies - for misclassified freqs
  ind <- res[,"minFreq"] <= res[,"maxFreq"] 
  # sum(ind)
  
  # get data frame of mismatched freqs
  outFreq <- res[!ind,]
  
  # remove from results
  res <- res[ind,]
  
  # TIME FILTER
  ## and for misclasified times
  ind2 <- res[,"start"] <= res[,"stop"] # sum(!ind2)
  
  #sum(ind2) == nrow(res)
  outTimes <- res[!ind2,]
  
  # remove misclassified
  res <- res[ind2,]
  
  ## Name filter
  if(!missing(names2match)){
    
    if(class(names2match) != "character") stop("names2match must be a character vector")
    namesData <- unique(res[,"name"])
    ind3 <- !namesData %in% names2match
    namesMismatch <- namesData[ind3]
    
    outNames <- res[res[, "name"] %in% namesMismatch, ]
  
    
    # remove from results
    res <- res[!res[, "name"] %in% namesMismatch, ]
    
    # sum(ind3)
  }
  
  
  if(any(!c(ind, ind2, ind3))){
    out <- rbind(outFreq, outTimes, outNames)
    out$type <- rep(c("Frequency mismatch", "Time mismatch", "Name mismatch"), 
                    c(nrow(outFreq), nrow(outTimes), nrow(outNames)))
    
    warning(paste0("Frequency/Time/Name mismatch in ", nrow(out), " labels.\n"))
    #print(out)
  }
  
  
  ## filter for required names
  # if(!missing(namesUse)) {
  #   
  #   # could check here whether namesUSe match anything at all?
  #   res <- subset(res, name %in% namesUse)
  #   
  # }
  # 
  ## check no template label files have ended up with no rows in the res df
  ind4 <- basename(x) %in% unique(res$id)
  # sum(!ind3)
  
  if(any(!ind4)) {
    warning(paste0(sum(!ind4), " label files have no valid labels after filtering.\n"))
    #print(paste(basename(x)[!ind4], sep = "\n"))
  }
  
  
  
  
  # rename duplicate template names and keep original
  if(rename) {
    
    res <- res[order(res$name),]
    
    # check that names are dupicated
    if(any(duplicated(res$name))) {
      res$original <- res$name
      
      rn <- lapply(split(res$name, res$name), function(x) {
        sapply(seq_along(x), function(y) paste(unique(x),y, sep ="_"))
      })
      res$name <- unname(unlist(rn))
    }
  }
  
  
  if(includeErrors){
    
    return(list(res, out))
    
  } else return(res)
  
}

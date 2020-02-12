
## Reads audacity label files

# testing
# type = "Audacity"
# rename = T


readLabels <- function(x, type, rename = T){
  
  # x are file paths to label text files
  # type is software used to create labels ie one of c("Audacity", "Raven")
  # renames is to rename each template with unique id within name (eg. in species)
  #
  
  type <- match.arg(type)
  
  if(missing(type) | is.null(type)){
    
        type <- sapply(x, function(y) {
          ifelse(substr(readLines(y, 1, warn = F), 1,5) == "Selec", "Raven", "Audacity")
        })
  }
  # table(type)
  if(!length(type) %in% c(1, length(x))) stop("Check label files")
  
  
  audacity.read <- function(x){
    
    labs <- read.table(x, header = F, sep = "\t", stringsAsFactors= F)
    e.ind <- seq(2,nrow(labs),2)
    o.ind <- seq(1,nrow(labs),2)
    
    res <- cbind(labs[o.ind,], labs[e.ind, 2:3]) 
    colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
    res <- data.frame(id  = basename(x), 
                      name = res$name, 
                      lapply(res[-3], as.numeric), 
                      stringsAsFactors = F)
  }
  
  raven.read <- function(x) {
    
    labs <- read.table(x, header = T, sep = "\t", stringsAsFactors = F)
    res <- labs[,c("Begin.Time..s.","End.Time..s.", 
                   "Low.Freq..Hz.", "High.Freq..Hz.", "Annotation")]
    colnames(res) <- c("start", "stop", "minFreq", "maxFreq", "name")
    res <- data.frame(id  = basename(x), 
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
  
  ## check frequencies - for misclassified freqs
  ind <- res[,"minFreq"] <= res[,"maxFreq"] # sum(ind)
  
  # remove
  res <- res[ind,]
  
  # get data frame of mismatched freqs
  outFreq <- res[!ind,]
  
  ## and for misclasified times
  ind2 <- res[,"start"] <= res[,"stop"] # sum(ind2)
  
  # remove misclassified
  res <- res[ind2,]
  outTimes <- res[!ind2,]
  
  if(any(!c(ind, ind2))){
    out <- rbind(outFreq, outTimes)
    warning(paste0("Frequency/Time mismatch in ", nrow(out), " labels"))
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
  ind3 <- basename(x) %in% unique(res$id)
  
  if(any(!ind3)) {
    
    warning(paste0(sum(ind3), "label files have no valid labels:\n", basename(x)[!ind3]))
    
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
  
  res
  
}

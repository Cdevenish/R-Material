readLabels <- function(x, type = c("Audacity", "Raven"), rename = T){
  
  # x are file paths to label text files
  # type is software used to create labels
  # renames .
  #
  
  type <- match.arg(type)
  
  labs.df <- switch(type, 
                    
                    Audacity = lapply(x, function(y) {
                      
                      labs <- read.table(y, header = F, sep = "\t", stringsAsFactors= F)
                      
                      e.ind <- seq(2,nrow(labs),2)
                      o.ind <- seq(1,nrow(labs),2)
                      
                      res <- cbind(labs[o.ind,], labs[e.ind, 2:3]) 
                      colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
                      res <- data.frame(id  = basename(y), 
                                        name = res$name, 
                                        lapply(res[-3], as.numeric), 
                                        stringsAsFactors = F)
                    }),
                    Raven = stop("Not implemented yet")
  )
  
  res <- do.call(rbind, labs.df) ## produces a data frame
  
  ## Check minFreq maxFreq and timings.
  
  ## check frequencies - for misclassified freqs
  ind <- res[,"minFreq"] >= res[,"maxFreq"]
  # remove
  res <- res[ind,]
  
  # get data frame of mismatched freqs
  outFreq <- res[!ind,]
  
  ## and for misclasified times
  ind2 <- res[,"start"] >= x[,"stop"]
  
  # remove misclassified
  res <- res[ind2,]
  outTimes <- res[!ind2,]
  
  if(any(c(ind, ind2))){
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

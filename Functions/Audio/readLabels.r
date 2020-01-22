## function to read audacity labels:
readLabels <- function(x, type = c("Audacity", "Raven")){
  
  
  # x are file paths to label text files
  # type is software used to create labels
  
  
  type <- match.arg(type)
  
  labs <- lapply(x, read.table, header = F, sep = "\t", stringsAsFactors= F)
  
  labs.df <- switch(type, 
                    
        Audacity = lapply(labs, function(x) {
                      
        e.ind <- seq(2,nrow(x),2)
        o.ind <- seq(1,nrow(x),2)
                      
        res <- cbind(x[o.ind,], x[e.ind, 2:3]) 
        colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
        res <- data.frame(name = res$name, lapply(res[-3], as.numeric), stringsAsFactors = F)
        }),
        Raven = stop("Not implemented yet")
          )
  
  labs.df
}
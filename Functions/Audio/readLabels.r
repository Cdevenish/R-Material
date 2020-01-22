## function to read audacity labels:
readLabels <- function(x, type = c("Audacity", "Raven")){
  
  
  # x are file paths to label text files
  # type is software used to create labels
  
  type <- match.arg(type)
  
  labs.df <- switch(type, 
                    
        Audacity = lapply(x, function(x) {
          
        labs <- read.table(x, header = F, sep = "\t", stringsAsFactors= F)
                      
        e.ind <- seq(2,nrow(labs),2)
        o.ind <- seq(1,nrow(labs),2)
                      
        res <- cbind(labs[o.ind,], labs[e.ind, 2:3]) 
        colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
        res <- data.frame(id  = basename(x), 
                          name = res$name, 
                          lapply(res[-3], as.numeric), 
                          stringsAsFactors = F)
        }),
        Raven = stop("Not implemented yet")
          )
  
  labs.df
}
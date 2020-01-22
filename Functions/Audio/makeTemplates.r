
## Function to make multiple templates from a folder containing audio (.wav) files and labels (.txt) files


## Function will match labels and sound files based on common filename, then extract templates for each 
# labelled audio.

makeTemplates <- function(path, tmptxt="", dens=1, tz="Asia/Jakarta", labels=c("Audacity", "Raven"), ...){
  
  library(tuneR) # to read waves in makeCorTemplate
  library(monitoR)
  
  
  ## path is to a folder with .wav files and .txt of the same name.
  ## .txt fils are label files exported from Audacity (or Raven). ## Labels should include time start 
  ## (seconds from start of file), time end, min freq, max freq and template name (must be unique)
  
  ## tmptxt is an optional tag appended to the label filename, eg "..._Template.txt"

  ## corTemplate control options see ?makeCorTemplate():
  # dens - density (pixel resolution of sonogram) at which to create templates
  # tz - time zone of audio data
  
  ## labels = origin of labels..   Audacity, Raven.. generic dataframe... TODO
  
  ## function to read audacity labels:
  readLabels <- function(x, type = c("Audacity", "Raven"), rename = T){
  
    # x are file paths to label text files
    # type is software used to create labels
    
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
    
    res <- do.call(rbind, labs.df)
    
    # rename duplicate template names and keep original
    if(rename) {
      
      res <- res[order(res$name),]
      
      # check that names are dupicated
      if(any(duplicated(res$name))) {
        res$original <- res$name
        
        dups <- duplicated(res$name)
        
        
        rn <- lapply(split(res$name, res$name), function(x) {
          sapply(seq_along(x), function(y) paste(unique(x),y, sep ="_"))
        })
        res$name <- unname(unlist(rn))
      }
    }
    
    
  }
  
  ## function to read Raven labels... TO DO
  
  
  ## Get reference call label filenames in
  labs.fn <- list.files(path = path, pattern = paste0(".*", tmptxt, "\\.txt$"), full.names = T, recursive = T)
  
  ## Get reference audio filenames 
  wavs.fn <- list.files(path = path, pattern = "\\.wav$", full.names = T, recursive = T)
  
  # Match audio to labels
  wavs.bn <- gsub(pattern = "\\.[[:alpha:]]{3}", "", basename(wavs.fn)) # get filenames without extensions
  wav.ind <- lapply(wavs.bn, function(x) which(grepl(x, labs.fn, fixed = T))) # match to text label filenames
  
  wavs.mtch <- wavs.fn[sapply(wav.ind, function(x) length(x) > 0)]
  
  # get label info
  labs <- readLabels(labs.fn, type = labels) # returns a dataframe
  labs <- split(labs, labs$id)
  
  ## Match up label and wav files - put in same order
  # Get reference call label filenames in
  #labs.fn <- list.files(path = path, pattern = paste0(".*", tmptxt, "\\.txt$"), full.names = T, recursive = T)
  
  labs.fn <- names(labs)
  
  ## Get reference audio filenames 
  wavs.fn <- list.files(path = path, pattern = "\\.wav$", full.names = T, recursive = T)
  
  # Match audio to labels
  wavs.bn <- gsub(pattern = "\\.[[:alpha:]]{3}", "", basename(wavs.fn)) # get filenames without extensions
  wav.ind <- lapply(wavs.bn, function(x) which(grepl(x, labs.fn, fixed = T))) # match to text label filenames
  
  wavs.mtch <- wavs.fn[sapply(wav.ind, function(x) length(x) > 0)]
  
  #cbind(wavs.mtch, labs.fn)
  
  
  # length(wavs.mtch) == length(labs)
  
  ## Make templates
  t1 <- mapply(function(x, y) {
    
    lapply(1:nrow(y), function(z){
      makeCorTemplate(x, t.lim = c(y[z,"start"], y[z,"stop"]), 
                      frq.lim = c(y[z,"minFreq"]/1000, y[z,"maxFreq"]/1000), 
                      name = y[z,"name"],
                      comment = y[z, "id"],
                      dens = dens, ...)})
  }, wavs.mtch, labs)
  
  ## TURN OFF PLOT?? 
  
  ## put all templates into a single list (note unlist first before do.call)
  ctemps <- do.call(combineCorTemplates, unlist(t1))
  ctemps

}
  


## Function to make multiple templates from a folder containing audio (.wav) files and labels (.txt) files


## Function will match labels and sound files based on common filename, then extract templates for each 
# labelled audio.

makeTemplates <- function(path, tmptxt = "TEMPLATE", dens = 0.1, tz = "", labels = c("Audacity", "Raven"), ...){
  
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
  
  ## function to read audacity labels.
  read.audacity.lab <- function(x){
    
    labs <- lapply(x, read.table, header = F, sep = "\t", stringsAsFactors= F)
    labs.df <- lapply(labs, function(x) {
      
      e.ind <- seq(2,nrow(x),2)
      o.ind <- seq(1,nrow(x),2)
      
      res <- cbind(x[o.ind,], x[e.ind, 2:3]) 
      colnames(res) <- c("start", "stop", "name", "minFreq", "maxFreq")
      res <- data.frame(name = res$name, lapply(res[-3], as.numeric), stringsAsFactors = F)
    })
    labs.df
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
  
  #cbind(wavs.mtch, labs.fn)
  
  # get label info
  labs <- read.audacity.lab(labs.fn)
  # length(wavs.mtch) == length(labs)
  
  ## Make templates
  t1 <- mapply(function(x, y) {
    
    lapply(1:nrow(y), function(z){
      makeCorTemplate(x, t.lim = c(y[z,"start"], y[z,"stop"]), 
                      frq.lim = c(y[z,"minFreq"]/1000, y[z,"maxFreq"]/1000), 
                      name = y[z,"name"],
                      dens = dens, ...)})
  }, wavs.mtch, labs)
  
  ## TURN OFF PLOT?? 
  
  ## put all templates into a single list (note unlist first before do.call)
  ctemps <- do.call(combineCorTemplates, unlist(t1))
  ctemps

}
  

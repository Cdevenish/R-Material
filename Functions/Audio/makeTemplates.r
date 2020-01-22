
## Function to make multiple templates from a folder containing audio (.wav) files and labels (.txt) files


## Function will match labels and sound files based on common filename, then extract templates for each 
# labelled audio.

makeTemplates <- function(path, tmptxt="", dens=1, tz="Asia/Jakarta", labels, ...){
  
  library(tuneR) # to read waves in makeCorTemplate
  library(monitoR)
  
  
  ## path is to a folder with .wav files and .txt of the same name.
  ## .txt fils are label files exported from Audacity (or Raven). ## Labels should include time start 
  ## (seconds from start of file), time end, min freq, max freq and template name (must be unique)
  
  ## tmptxt is an optional tag appended to the label filename, eg "..._Template.txt"

  ## corTemplate control options see ?makeCorTemplate():
  # dens - density (pixel resolution of sonogram) at which to create templates
  # tz - time zone of audio data
  
  ## labels, if present, a data frame with label info, otherwise will be taken from path
  
  
  if(missing(labels)){
    
    ## function to read audacity labels:
    source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/Audio/readLabels.r")
  
    # get label info
    labs.fn <- list.files(path = path, pattern = paste0(".*", tmptxt, "\\.txt$"), 
                        full.names = T, recursive = T)
  
    labels <- readLabels(labs.fn) # returns a dataframe
    
  } else if(class(labels) != "data.frame" |
            any(!c("id", "start", "stop", "minFreq", "maxFreq", "name") %in% colnames(labels))) {
    stop("labels must be a data frame, with columns: id, start, stop, minFreq, maxFreq, names")
  }
  
  labs <- split(labels, labels$id)
  
  ## Match up label and wav files - put in same order
  
  # template filenames
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
                      comment = y[z, "original"],
                      dens = dens, plot = F, ...)})
  }, wavs.mtch, labs)
  
  ## TURN OFF PLOT?? 
  
  ## put all templates into a single list (note unlist first before do.call)
  ctemps <- do.call(combineCorTemplates, unlist(t1))
  ctemps

}
  

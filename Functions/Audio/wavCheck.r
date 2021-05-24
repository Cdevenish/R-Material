
# fn <- wavs.mtch
# samp =44100; duration.min = 5; channels = 1; bits = 16
# duration = NULL
# samp = NULL


wavCheck <- function(fn, samp =44100, duration.min = 5, duration.exact = NULL, 
                     channels = 1, bits = 16, rm = F, fix, parallel = FALSE, nCores){
  
  # fn are complete file paths for wav files
  # samp is required sample rate (or NULL not to test)
  # duration.min is minimum duration (s) required (ie files must be equal or longer) (or NULL not to test)
  # duration.exact is the exact duration (s) to test for (or NULL not to test)
  # Channels are the required number of channels (or NULL not to test)
  # bits are the required bit depth of the wav (or NULL not to test)
  # rm. Logical , return vector of filepaths without those with duration < duration, and with 
  # sample rates not equal to samp. Otherwise will return a data frame with results of check.  With columns 
  # equal to the samp, duration, chaqnnel, bits that are not null
  # fix is a folder to store resampled files, if present, then files not at samp will be resampled and stored here.
  
  # Check libraries av tuneR
  if(!require("av",character.only = TRUE)) stop("av package not found")
  if(!require("tuneR",character.only = TRUE)) stop("tuneR package not found")
  
  # rownames(installed.packages())
  
  if(parallel){
    
    if(missing(nCores)) nCores <- parallel::detectCores()-1
    
    library(parallel)
    cl <- makeCluster(nCores)
    clusterExport(cl, c("fn"), envir=environment()) # multiple items in a vector.. c("dhiy.rList", "wgs.aoi")
    
    clusterEvalQ(cl, {
      library(tuneR)
      })
    
    wav.info <- parLapply(cl, fn, tuneR::readWave, header = T)
    
    stopCluster(cl)
    
  } else wav.info <- lapply(fn, tuneR::readWave, header = T)
  
  # check length 4
  if(any(!lengths(wav.info) == 4)) stop("check!!")
  
  wav.m <- matrix(unlist(wav.info), ncol = 4, byrow = T)
  colnames(wav.m) <- c("sample.rate", "channels", "bits", "samples")
  wav.m <- cbind(wav.m, duration = wav.m[,4]/wav.m[,1])
  
  # head(wav.m)
  
  # check sample rate
  # If any of the criteria are NULL, then they are dropped from this matrix
  ind <- cbind(sample.rate = wav.m[,"sample.rate"] == samp,
               channels = wav.m[,"channels"] == channels,
               bits = wav.m[,"bits"] == bits,
               duration.min = wav.m[,"duration"] >= duration.min,
               duration.exact = wav.m[, "duration"] == duration.exact)
  
  # head(ind)
  rm.ind <- apply(ind, 1, all)
  
  print(paste(sum(!rm.ind), "files failed"))
  
  if(sum(!rm.ind)>0) print(nrow(ind) - colSums(ind))
        
  if(!missing(fix)) {
    
    if(!dir.exists(fix)) stop("fix must be a valid directory to store the reprocessed files")
    if(is.null(samp) & is.null(channels)) stop("samp and/or channels must be present for reprocessing option")
    
    sav <- c("sample.rate", "channels")[!c(is.null(samp), is.null(channels))]
    sav.ind <- apply(!ind[,sav,drop = F], 1, any)
    
    inFiles <- fn[sav.ind]
    
    outFiles <- file.path(fix, basename(inFiles))
    if(any(wav.m[,"sample.rate"] < samp)) warning("Sample rates lower than samp will be returned unchanged")
    
    mapply(function(x, y) av::av_audio_convert(x,y, channels = channels, sample_rate = samp, verbose = F),
           inFiles, outFiles)
    
    print(paste0(length(inFiles), " files reprocessed and saved to '", fix, "'"))
    
  }
  
  if(rm) return(fn[rm.ind]) else invisible(data.frame(fn = fn, wav.m))
  
}


# fn <- wavs.mtch
# samp =44100; duration = 5; channels = 1; bits = 16
# duration = NULL
# samp = NULL


wavCheck <- function(fn, samp =44100, duration = 5, channels = 1, bits = 16, rm = F, fix){
  
  # fn are complete file paths for wav files
  # samp is required sample rate (or NULL not to test)
  # duration is minimum duration required (or NULL not to test)
  # Channels are the required number of channels (or NULL not to test)
  # bits are the required bit depth of the wav (or NULL not to test)
  # rm. Logical , return vector of filepaths without those with duration < duration, and with 
  # sample rates not equal to samp
  
  # Check libraries av tuneR
  
  wav.info <- lapply(fn, tuneR::readWave, header = T)
  
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
               duration = wav.m[,"duration"] > duration)
  
  # head(ind)
  rm.ind <- apply(ind, 1, all)
  
  print(paste(sum(!rm.ind), "files failed"))
  
  if(sum(!rm.ind)>0) print(nrow(ind) - colSums(ind))
        
  if(!missing(fix)) {
    
    if(!dir.exists(fix)) stop("fix must be a valid directory to store the reprocessed files")
    
    outFiles <- file.path(fix, basename(fn))
    if(any(wav.m[,"sample.rate"] < samp)) warning("Sample rates lower than samp will be returned unchanged")
    
    mapply(function(x, y) av::av_audio_convert(x,y, channels = channels, sample_rate = samp, verbose = F),
           fn, outFiles)
    
  }
  
  if(rm) return(fn[rm.ind]) else invisible(wav.m)
  
}

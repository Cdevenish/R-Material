
# fn <- wavs.mtch
# samp =44100; duration = 5; channels = 1; bits = 16

wavCheck <- function(fn, samp =44100, duration = 5, channels = 1, bits = 16, rm = T){
  
  # fn are complete file paths for wav files
  # samp is required sample rate
  # duration is minimum duration required
  # rm. Logical , return vector of filepaths without those with duration < duration, and with 
  # sample rates not equal to samp
  
  wav.info <- lapply(fn, tuneR::readWave, header = T)
  
  # check length 4
  if(any(!lengths(wav.info) == 4)) stop("check!!")
  
  wav.m <- matrix(unlist(wav.info), ncol = 4, byrow = T)
  colnames(wav.m) <- c("sample.rate", "channels", "bits", "samples")
  wav.m <- cbind(wav.m, duration = wav.m[,4]/wav.m[,1])
  
  head(wav.m)
  
  # check sample rate
  ind <- cbind(sample.rate = wav.m[,"sample.rate"] == samp,
               channels = wav.m[,"channels"] == channels,
               bits = wav.m[,"bits"] == bits,
               duration = wav.m[,"duration"] > duration)
  
  head(ind)
  rm.ind <- apply(ind, 1, all)
  
  print(paste(sum(!rm.ind), "files failed"))
  print(nrow(ind) - colSums(ind))
        
  if(rm) return(fn[rm.ind])

}

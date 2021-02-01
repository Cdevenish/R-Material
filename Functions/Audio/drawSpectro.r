## Spectrograms 

## Draw spectrograms using base::image(). Optionally, draw boxes around calls within them and label above spectrogram
## wavs imported with tuneR::readWave()
## Fourier transform with fftw via seewave::spectro()

## Options to do noise reduction (median subtraction over time); binary image


## file settings
# fn - wav file location
# wd # folder to save spectrograms
# sName # file name for spectogram (eg taken from wav filename - paste0(sub(".*(extr_\\d*)_.*", "\\1_", fn)) )
# 
# draw boxes around sounds
# boxes - T/F to draw boxes
# data - data frame with these column names: file (==fn),  start, stop, minFreq, maxFreq (in kHz), origina (if labels)
# labels - place test label above each box in lines above spectrogram. Usese column 'original' in data
# data.names - if data is included, and column names do not match above, can include named vector as dictionary for col
# eg data.names <- c(file = "wav.path", start = "X_min", stop = "X_max", minFreq = "Y_min", maxFreq = "Y_max", Label = "original")

# ## FT settings
# flim = c(0,10) # frequency limits kHz
# wl = 512
# wn = "hanning"
# zp = 0
# ovlp = 0
# dB = "max0"
# 
# ##Audio processing settings
# NR = T # do noise reduction?
# binary = T # do binary image?
# sBinary = F # use single amplitude cut off dbH only
# dBH = 26 # upper threshold for binary image
# dBL = 20 # lower threshold for binary image
# 
# 
# ##  spectrogram image settings
# axes = logical, will draw axes on spectrogram
# col = "grey" # colourname
# nCol = 60 # number of color shades in paleltte
# pal = seewave::reverse.gray.colors.1(nCol) # colour palette
# # pal = viridis::viridis(nCol)
# brks_min <- -20 # min for color assignment on image() breaks, can use as cut off for low amp.
# brksQ = 0.01 # low quantile for amplitude cut off for assigning colors
# will use full range of amplitude brksQ = 0
# hght = 600 # height in pixels
# wdth = 1200 # pixels
# aspect = 0 ## set this to 1, for square pixels,takes dimensions from time/freq axes.Overides hght, wdth
# save = TRUE # save the spectrogram to file. If false, will plot to graphics
# fName = use full name for testing settings (these will appear in filename), otherwise, simple name of sName png
# # function part


drawSpectro <- function(fn, wd, sName,
                        flim = c(0,12), wl = 512,wn = "hanning",zp = 0,ovlp = 0,dB = "max0",
                        NR = TRUE, binary= FALSE, sBinary = FALSE, dBH = 26, dBL = 20,
                        pal = seewave::reverse.gray.colors.1, col ="grey", nCol = 60,
                        brks_min, brksQ = 0, axes = T,
                        hght = 600,wdth =1200, aspect = 0, save = TRUE,
                        boxes = FALSE, labels = TRUE, fName = FALSE, data, data.names){
  
  
  # process data frame for boxes and/or labels, if present
  if(boxes){
    
    if(missing(data.names)) colnames(data) %in% c("file", "start", "stop", "minFreq", "maxFreq") else {
      
      data <- data[, data.names]
      # rename 
      colnames(data) <- names(data.names)
      
    }
    
    if(labels)
      if(missing(data.names)) colnames(data) %in% c("file", "start", "stop", "minFreq", "maxFreq")
    
    # TODO check label column prsent..  but above will bring it in already.. 
    }
  
  
  ## helper functions for freq / time pixel clumping
  # Adjacency functions :  (can also use transpose of freq for time) so don't really need two functions... 
  ## get Trues that have at least one T above or below them
  #                           1. row below      2# row above  
  frqAdj <-function(m){m & (rbind(m[-1L,],F) | rbind(F, m[-nrow(m),]) )}
  
  ## Adjacency by column - within 3 time steps
  timeAdj <-function(m){
    nc <- ncol(m)
    ##  1: n cols to right
    m | (cbind(m[,-1L],F) | cbind(m[,-c(1:2)],F,F)| cbind(m[,-c(1:3)],F,F,F)) & # 1: n cols to left 
      ( cbind(F, m[,-nc]) | cbind(F, F, m[, -((nc-1):nc)])| cbind(F, F, F, m[,-((nc-2):nc)]) ) 
  }
  
  # read wav
  w <- tuneR::readWave(fn)
  w@samp.rate <- as.numeric(w@samp.rate)
  f <- w@samp.rate
  dur <- seewave::duration(w)
  
  # do F transform
  spect <- seewave::spectro(w, wl = wl, wn = wn, zp = zp, ovlp = ovlp, dB = dB, fftw = TRUE,
                            noisereduction = FALSE, plot = FALSE, flim = flim)
  # str(spect, max.level = 1)
  # sapply(spect, range)
  # sapply(spect, quantile, p = c(0,0.01, 0.05,0.25,0.50,0.75,1))
  # hist(spect$amp)
  
  # get axes for plotting boxes on top
  # time resolution # ie the length of each time step in seconds (the columns in the matrix)
  tr <- wl/f
  # frequency resolution  # ie the frequency length (in Hz) of each band (row) in the matrix
  fr <- f/wl
  
  # so the total freq sequence is
  yFreq <- seq(0, f/2, fr)/1000 # but the end point is last but one in sequence - beacuse they are the start of the bands
  # same for time:
  xTime <- seq(0, dur, tr)
  
  # gets closest column, either above or below
  min.freq.cols <- which.min(abs(round(spect$freq, digits = 2) - flim[1]))
  max.freq.cols <- which.min(abs(round(spect$freq, digits = 2) - flim[2])) # in kHz
  
  # so new freq range is
  newMinFreq <- min.freq.cols * fr # min.freq.cols is the column number above the minF
  newMaxFreq <- max.freq.cols * fr
  
  # make new y axis
  yFreq_flim <- seq(newMinFreq, (max.freq.cols+1) * fr, fr) / 1000 # back to KHz

  # for square pixels
  if(aspect == 1){
    hght <- length(spect$freq)
    wdth <- length(spect$time)
  }
  
  # Do noise reduction (from tadarida)
  # Frequency equalizing is performed by substracting from each frequency line in the matrix its proper noise 
  # floor (5%-quantile in amplitude). 
  
  if(NR){ ## NOTE that this changes range of amplitude, now includes positive values
    noise5Q <- apply(spect$amp, 1, quantile, probs = 0.05)
    # plot(spect$freq,noise5Q, xlab = "Freq (Hz)")
    # subtract from each amplitude row
    spect$amp <- spect$amp - noise5Q
  }

  # sapply(spect, range)
  # sapply(spect, quantile, p = c(0,0.01, 0.05,0.25,0.50,0.75,1))
  # hist(spect$amp)
  # 
  
  # Do binary image
  # From tadarida... plus added freq pixel grouping... could do this as t()
  
  # -	the high threshold first determines the presence of a DSE, i.e. every element of the matrix over this threshold are part of a DSE.
  # -	the low threshold second determines the limit of DSEs. An element in the matrix is indeed part of a DSE if it meets the following conditions: being over this low threshold AND (being adjacent in rows (frequency band) to an element of the DSE OR being less than 5 columns (approx. 1.5 ms in HF mode and 15 ms in LF mode) away from an element of the DSE).
  # Authors found optimal values at 26 dB for the high threshold and 20 dB for the low threshold. 
  
  if(binary){
    
    ampHigh <- spect$amp > dBH
    
    # can use a single amplitude cut off here. and other adjustments below
    
    if(sBinary){
      
      spect$amp <- ampHigh
      
    } else {
      
    
    ampLow <- spect$amp > dBL
    
    # Filter amplitude for signals:
    cond2 <- frqAdj(ampHigh) # one row above or below include
    # cond3 <- timeAdj(ampHigh) # three col right and left
    cond3 <- t(frqAdj(t(ampHigh))) # one col to left or right include
    
    ampFin <- (ampLow + cond2) | (ampLow  + cond3)
    spect$amp <- ampFin
    
    }
  }
  
  ## do color breaks
  if(missing(brks_min)) brks_min <- quantile(spect$amp, p=brksQ) # minimum as default
  brks <- seq(brks_min, max(spect$amp),length.out = nCol+1)
  
  # get colors
  pal <- pal(nCol)
  # need to fill in background colour if using low cut off. not par("bg") - just plotting area pal[1]
  
  
  
  #If saving...  get file path to save image of spectrogram
  
  ## full name
  if(save){
    if(fName){
      fp <- file.path(wd, paste0(sName, "_col_", col, 
                                 "_wn_", wn, "_zp_", zp, "_ovlp_", ovlp, 
                                 "_bin_", substr(as.character(binary),1,1), 
                                 "_NR_", substr(as.character(NR),1,1), ".png"))
    } else {
      # simple name
      fp <- file.path(wd, paste0(sName, ".png"))
    }
  }
    
    
  # print(fp)
  
  # save spectrogram to file or plot to screen
  if(save){
    png(fp, width = wdth, height = hght)
    
    if(!axes) par(mar = c(0,0,0,0), oma = c(0,0,0,0))
    
    graphics::image(z = t(spect$amp),
                    x = xTime,
                    y = yFreq_flim,
                    axes=axes,
                    col = pal,
                    breaks = brks,
                    useRaster = TRUE,
                    xlab = "Time (s)",
                    ylab = "Frequency (kHz)")
    
    if(boxes){
      
      # get data for this fn
      mms <- subset(data, file == fn)
      # data must have cols file,  start,stop, minFreq, maxFreq
      
      if(!is.null(mms)){
        for (x in 1:nrow(mms)) {
          rect(mms[x, "start"],
               mms[x, "minFreq"],
               mms[x, "stop"],
               mms[x, "maxFreq"],
               border = "red")}
      }
      
      # draw labels over boxes, or at top - data must have name column..  original... 
      if(labels){
        # mid points for plot
        mms$at <- mms$start + (mms$stop - mms$start)/2
        
        # make sure is ordered
        mms <- mms[order(mms$at),]
        line = rep_len(c(0,1), length.out = nrow(mms)) # could increase to 0,1,3 for many labels
        
        mtext(mms$original, side = 3, line = line, at = mms$at)
      }
    }
    dev.off()
    
  } else {
    
    graphics::image(z = t(spect$amp),
                    x = xTime,
                    y = yFreq_flim,
                    axes=axes,
                    col = pal,
                    breaks = brks,
                    useRaster = TRUE,
                    xlab = "Time (s)",
                    ylab = "Frequency (kHz)")
    
    if(boxes){
      
      # get data for this fn
      mms <- subset(data, file == fn)
      # data must have cols file,  start,stop, minFreq, maxFreq
      
    if(!is.null(mms)){
      for (x in 1:nrow(mms)) {
        rect(mms[x, "start"],
             mms[x, "minFreq"],
             mms[x, "stop"],
             mms[x, "maxFreq"],
             border = "red")}
    }
      
      # draw labels over boxes, or at top - data must have name column..  original... 
      if(labels){
        
        # mid points for plot
        at <- mms$start + (mms$stop - mms$start)/2
        mtext(mms$original, side = 3, line = 0, at = at, cex = 0.9)
      }
      
    }
  }
  
  
  # return filepath of save image if wanted
  invisible(fp)
  
}

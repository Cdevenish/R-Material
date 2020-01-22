### adjust extent to nearest d value

adjExt <- function(ext, d = 1000, expand = TRUE, outF){
  
  ## ext is extent object as in,  sf::st_bbox or raster::extent()
  ## d is the value to which the extent should be rounded to, in spatial object units (eg m)
  ## expand is whether to expand the extent to the nearest d, or shrink it.
  ## outF controls format of returned exent. 'Extent' for raster::Extent class, 'bbox' for sf::st_bbox()
  ## if missing, then output format follows input format of ext
  
  if(! class(ext) %in% c("Extent", "bbox")) stop("ext must be a sf bbox or raster extent object")
  
  outF <- match.arg(outF, c("Extent", "bbox"))
  
  if(!missing(outF)) cls <- outF else cls <- class(ext)
  
  ext.std <- st_bbox(ext)
  
  if(expand) {
    fun1 <- floor
    fun2 <- ceiling } else {
      fun1 <- ceiling
      fun2 <- floor
    }
  
  ext.new <- c(fun1(ext.std[c("xmin", "ymin")]/d)*d,
                       fun2(ext.std[c("xmax", "ymax")]/d)*d)
    
    res <- switch(cls, 
                  
                  Extent = raster::extent(ext.new[c("xmin", "xmax", "ymin", "ymax")]),
                  bbox = sf::st_bbox(ext.new))

  return(res)
}


# class(sp::bbox(ext))
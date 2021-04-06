### adjust extent to nearest d value

adjExt <- function(ext, d = 1000, expand = TRUE, outF, projTo, projFrom){
  
  ## ext is extent object as in,  sf::st_bbox or raster::extent()
  ## d is the value to which the extent should be rounded to, in spatial object units (eg m)
  ## expand is whether to expand the extent to the nearest d, or shrink it.
  ## outF controls format of returned exent. 'Extent' for raster::Extent class, 
  ## bbox' for sf::st_bbox() or 'sf; for sf object
  ## if missing, then output format follows input format of ext
  ## to project the extent to a new projection, both projTo and projFrom must be present as crs arguments
  ## in a format accepted by st_transform()
  
  if(! class(ext)[1] %in% c("Extent", "bbox", "sf")) stop("ext must be a sf bbox or raster extent object")
  
  if(missing(outF)){ outF <- class(ext)[1]} else { outF <- match.arg(outF, c("Extent", "bbox", "sf"))}
  
  if(class(ext)[1] == "sf") {
    
    crs <- sf::st_crs(ext)
    ext <- sf::st_bbox(ext)
  
  }
  
  if(!missing(outF)) cls <- outF else cls <- class(ext)[1]
  
  ext.std <- sf::st_bbox(ext)
  
  if(expand) {
    fun1 <- floor
    fun2 <- ceiling } else {
      fun1 <- ceiling
      fun2 <- floor
    }
  
  ext.new <- c(fun1(ext.std[c("xmin", "ymin")]/d)*d,
                       fun2(ext.std[c("xmax", "ymax")]/d)*d)
  
  if(!missing(projTo) & !missing(projFrom)){
    
    # check crs? # sf does this check. and throws a sensible error 
    # is.numeric(crs) || is.character(crs) || inherits(crs, "crs") is not TRUE
    
    # Make points
    cc <- matrix(ext.new[c("xmin", "ymin", "xmin", "ymax", "xmax", "ymax", "xmax", "ymin", "xmin", "ymin")], 
                 ncol = 2, byrow= T)
    pol <- sf::st_sfc(sf::st_polygon(list(cc)), crs = projFrom)
    pol.t <- sf::st_transform(pol, crs = projTo)
    ext.new <- st_bbox(pol.t)
  }
  
    
    res <- switch(cls, 
                  
                  Extent = raster::extent(ext.new[c("xmin", "xmax", "ymin", "ymax")]),
                  bbox = sf::st_bbox(ext.new),
                  sf = st_sf(data.frame(id = 1, geometry = sf::st_as_sfc(sf::st_bbox(ext.new))), crs = crs)
    )
    

  return(res)
}


# class(sp::bbox(ext))

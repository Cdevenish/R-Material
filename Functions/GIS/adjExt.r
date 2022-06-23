### adjust extent to nearest d value, scale and buffer

adjExt <- function(ext, d = 1000, expand = TRUE, scale = 1, buffer = 0, outF, projTo, projFrom){
  
  ## ext is extent object as in,  sf::st_bbox or raster::extent()
  ## d is the value to which the extent should be rounded to, in spatial object units (eg m)
  ## expand is whether to expand the extent to the nearest d, or shrink it.
  ## scale is scale factor for enlarging / shrinking bbox.
  ## buffer - enlarge/shrink bbox by absolute value in projection units
  ## if both expF and buffer are used, then will first buffer, then scale
  ## outF controls format of returned exent. 'Extent' for raster::Extent class, 
  ## bbox' for sf::st_bbox() or 'sf; for sf object
  ## if missing, then output format follows input format of ext
  ## to project the extent to a new projection, both projTo and projFrom must be present as crs arguments
  ## in a format accepted by st_transform()
  
  if(!inherits(ext, c("Extent", "SpatExtent", "bbox", "sf", "sfc"))) stop("ext must be a sf bbox, raster or terra extent object")
  
  if(missing(outF)) {
    outF <- class(ext)[1] } else {
      outF <- match.arg(outF, c("Extent", "SpatExtent", "bbox", "sf"))}
  ## standardise sfc_POLYON to sfc ....
  
  
  if(inherits(ext, c("sf", "sfc"))) {
    
    crs <- sf::st_crs(ext)
    ext <- sf::st_bbox(ext)
  
  }
  # get CRS from raster...  if using raster to get extent... 
  
  # get extents in standardised format, sfc
  ext.sfc <- sf::st_as_sfc(sf::st_bbox(ext))
  
  ## do buffer / expF first, then make sure fits with d
  if(buffer != 0){
    
    ext.sfc <- sf::st_buffer(ext.sfc, dist = buffer)
    
    
  }
  
  # do scale
  if(scale != 1){
    
    cntr = st_centroid(ext.sfc)
    ext.sfc = (ext.sfc - cntr) * scale + cntr
    
  }
  
  # back to bbox
  ext.std <- sf::st_bbox(ext.sfc)
  
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
    cc <- matrix(ext.new[c("xmin", "ymin", "xmin", "ymax", "xmax", 
                           "ymax", "xmax", "ymin", "xmin", "ymin")], 
                 ncol = 2, byrow= T)
    pol <- sf::st_sfc(sf::st_polygon(list(cc)), crs = projFrom)
    pol.t <- sf::st_transform(pol, crs = projTo)
    ext.new <- st_bbox(pol.t)
  }
  
    
    res <- switch(outF, 
                  
                  Extent = raster::extent(ext.new[c("xmin", "xmax", "ymin", "ymax")]),
                  SpatExtent = terra::ext(ext.new[c("xmin", "xmax", "ymin", "ymax")]),
                  bbox = sf::st_bbox(ext.new),
                  sf = st_sf(data.frame(geometry = sf::st_as_sfc(sf::st_bbox(ext.new))), crs = crs),
                  sfc_POLYGON = sf::st_as_sfc(sf::st_bbox(ext.new), crs = crs)
    )
    

  return(res)
}


# class(sp::bbox(ext))

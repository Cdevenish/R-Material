

### extent from raster, vector, dataframe, list, etc #######

# x as vector in order>>> or named> xmin, xmax, ymin, ymax


ext2poly <- function(x, crs = NULL, id = NULL, ...){
  
  
  library(sf)
  
  nms <- c("xmin", "xmax", "ymin", "ymax")
  
  ## add stack.. 
  
  # careful, ID can come out as different class column type (in data frame) depending on what goes in , chr, numeric
  
  # include stack, brick, etc
  if(class(x) %in% c("RasterLayer", "RasterStack", "RasterBrick")){ # all same, single extent for multi raster objects
    
    library(raster)
    bbx <- list(extent(x))
    crs <- proj4string(x)
    if(missing(id) | is.null(id)) id <- raster::nlayers(x)
      
  }
  
  if(class(x) == "Extent") {
    bbx <- list(x)
    if(missing(id) | is.null(id)) id <- length(bbx)
    crs <- NA
  }
  
  if(class(x) == "numeric" && length(x) != 4) stop("vector must be numeric length four")
  
  if(class(x) == "numeric" && length(x) == 4){
    
    if(missing(id) | is.null(id)) id <- 1
    # re arrange according to names
    if(!is.null(names(x))) {
      names <- names(x)
      if(!all(names %in% nms)) stop ("Names must match: 'xmin', 'xmax', 'ymin','ymax'")
      bbx <- list(x[nms])
      
      } else {
        names(x) <- nms
        bbx <- list(x)
      }
    }
  
  
   # unviersalise for all objects length, nrwo, etc
  
  poly.lst <- lapply(bbx, function(x) sf::st_as_sfc(sf::st_bbox(x, crs = crs)))
  
  ext.poly <- st_sf(data.frame(id = id, stringsAsFactors = F), 
                   geometry = do.call(rbind, poly.lst), crs = crs, ...) # eg can add agr ="constant"  
  
  ext.poly
  
}


# 
# x <- c(-62,-43,-24,4)
# 
# x
# crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # as in arcgis
# 
# ext2poly(x, crs = crs)
# 
# x <- r.tmpl
# ext2poly(r.tmpl)


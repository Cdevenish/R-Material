## 4.4. Square buffer around centroid ####


sqBuffer <- function(x, r, crs = NULL, write = NULL, od = NULL, outp = c("sf", "ext", "bbox", "sp")){
  
  ## function creates square buffer around each point and returns spatialpolygons object
  ## x is a spatialpointsdataframe, sf object or data.frame/matrix of xy coordinates
  ## r is the square side length (resolution) in map units (ie m in utm)
  ## write - whether to write to disk (NULL - don't write), in which format, SHP, KML
  ## OJO, for KML, should only be geographical coords
  ## od is out directory, if NULL, 
  
  
  require(sp)
  require(rgdal)
  library(sf)
  
  # get half square side to add to coords
  s <- r/2
  
  outp <- match.arg(outp)
  
  if(inherits(x, "SpatialPointsDataFrame")){
    data <- data.frame(x)
    coords <- data.frame(coordinates(x))
    crs <- proj4string(x)
    ID <- 1:nrow(data)
    
  } else if(inherits(x, "sf")){
    
    data <- data.frame(x)
    data$geometry <- NULL
    coords <- data.frame(st_coordinates(x))
    crs <- st_crs(x)
    ID <- 1:nrow(data)
    
  } else {
    coords <- data.frame(x)
    ID = 1:nrow(x)
    data <- data.frame(id = ID, row.names = ID)
  }
  
  if(missing(crs)) crs <- 
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  colnames(coords) <- c("x", "y")
  
  # get coords of square vertices
  xmax <- coords$x + s
  xmin <- coords$x - s 
  ymax <- coords$y + s
  ymin <- coords$y - s
  
  buffers <- cbind(xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin, xmax, ymin)
  
  # Do extent bit here and use ext2poly below.. 
  
  
  ## ouput
  
  switch(outp, 
         
         sp = {
           
           polys <- SpatialPolygons(mapply(function(poly, id) {
             xy <- matrix(poly, ncol=2, byrow=TRUE)
             Polygons(list(Polygon(xy)), ID=id)},
             split(buffers, row(buffers)), ID), proj4string = CRS(crs))
           
           # make spdf
           polys.df <- SpatialPolygonsDataFrame(polys, data)
           
           if(!missing(write)){
             
             if(missing(od)) od <- choose.dir()
             r.pretty <- ifelse(r > 1000, paste0(r/1000, "k"), r)
             fn <- paste(deparse(substitute(x)), r.pretty, "buffer", sep = "_")
             
             switch(write, 
                    KML = writeOGR(polys.df, dsn = paste(od, "/", fn, ".kml", sep=""), layer = fn, driver="KML"),
                    SHP = writeOGR(polys.df, dsn = od, layer = fn, driver="ESRI Shapefile"))
           }
           
           return(polys.df)
           
           
         },
         
         ext = {
           exts <- cbind(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
           extL <- apply(exts, 1, raster::extent)
           return(extL)
           
         },
         
         bbox = {
           exts <- cbind(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
           extL <- apply(exts, 1, raster::extent)
           ext.sf <- lapply(extL, st_bbox, crs = crs)
           return(ext.sf)
           
         },
         
         sf = {
           
          polys <- lapply(split(buffers, row(buffers)), function(x) {list(matrix(x, ncol=2, byrow=TRUE))})
          pols <- lapply(polys, st_polygon)
          #str(pols)
          p <- st_sfc(pols, crs = crs)
          pf <- st_sf(data, p)
          return(pf)
         }
         
         )
  
  
}


# eg import spatial points
# siteCentr <- readOGR("G:/GIS_Proj/tesis/File_Tesis_2012/s_utm17", "Site_Centroids")
# bf <- sqBuffer(siteCentr, 5000, write = "SHP")
# bf <- sqBuffer(siteCentr, 5000)
# plot(bf, col = "red")
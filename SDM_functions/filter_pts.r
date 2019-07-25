


filter_pts <- function(r, pts, ext, crs, res, orig = c(0,0), writeID = F){
  
  library(raster)
  library(sf) # fix for converting within raster.. but should code all separately... 
  
  # r is one of the prediction rasters (ie same extent, resolution, crs as all predictors), 
  ## if used, ignores ext, crs and res
  # pts is either sp or sf points spatial object
  # writeID - logical. If TRUE, then returns unchanged points with a grid ID as new field. Default is false
  
  # make regular grid
  if(missing(r)) r <- raster(ext, crs = crs, res = res) else r <- raster(r)
  
  origin(r) <- orig
  
  ## add ID
  #values(r) <- cellFromRowColCombine(r, 1:nrow(r), 1:ncol(r))
  values(r) <- 1:ncell(r) # add values - ID number for each cell
  names(r) <- "ID"
  #r
  
  # extract id value to each occ point
  grd.pts.id <- raster::extract(r, pts)
  
  # how many repeated values?
  print(sprintf("%s grid cells have more than one record", sum(table(grd.pts.id)>1)))
  
  if(!writeID){
  
  ## keep first duplicated point, delete rest
  dups <- duplicated(grd.pts.id)
  
  #sum(dups) # this is the total number of duplicated occurrence records (more than above as more 
  # than 2 per cell duplicated in some cases)
  
  print(sprintf("%s records eliminated", sum(dups)))
  
  pts.filt <- pts[!dups,]
  
  return(pts.filt)
  
  } else {
    
    ## check column name doesn't already exist... 
    if("gridID" %in% colnames(pts)){
      
      l <- length(colnames(pts))
      pts <- cbind(pts, grd.pts.id)
      colnames(pts)[l+1] <- paste0("gridID", paste(as.integer(runif(5, 0,9)), collapse = ""))
      
    } else pts$gridID <- grd.pts.id
    
    return(pts)
    
  } 
}

# 
# ## Extract a random record for each raster cell (if repeated), per species
# resample <- function(x, ...) {x[sample.int(length(x), ...)]} #OJO! see sample?  but why???
# trx <- by(grd_spp, list(grd_spp$SciName, grd_spp$layer), function(x) resample(x$MyID,1))
# 
# # or use min ID record 
# trx <- by(grd_spp, list(grd_spp$SciName, grd_spp$layer), function(x) min(x$MyID))
# 
# # or closest to grid cell center? Is it worth it??

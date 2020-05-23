


filter_pts <- function(r, pts, byID, ext, crs, res, orig = c(0,0), writeID = F){
  
  library(raster)
  library(sf) # fix for converting within raster.. but should code all separately... 
  
  # r is one of the prediction rasters (ie same extent, resolution, crs as all predictors), 
  ## if used, ignores ext, crs and res
  # pts is either sp or sf points spatial object
  # byID. Column name of group if points to be thinned by groups (eg species), otherwise all pts are used
  # writeID - logical. If TRUE, then returns unchanged points with a gridID as new field. Default is false
  
  # make regular grid
  if(missing(r)) {
    r <- raster(ext, crs = crs, res = res)
    origin(r) <- orig
    
  } else r <- raster(r)
  
  
  ## add ID
  values(r) <- 1:ncell(r) # add values - ID number for each cell
  names(r) <- "ID"
  #r
  
  # group pts by byid and extract for each group
  if(!missing(byID)){
    
    if(any(!is.character(byID), length(byID)!=1)) stop("byID must be a length one, character vector")
    
    f = pts[,byID] # isn't there a way to get a column from a sf object?
    f$geometry <- NULL
    f <- f[,,drop = T]
    
  } else f <- 1
  
  pts.lst <- split(pts, f)
  
  grd.pts <- lapply(pts.lst, function(x) raster::extract(r, x))
  
  ## keep first duplicated point, delete rest
  dups <- lapply(grd.pts, duplicated)
  # lengths(dups)
  
  # pts.lst[[1]][!dups[[1]],]
  
  if(writeID){
    
    # how many repeated values?
    # print(sprintf("%s grid cells have more than one record", sum(table(grd.pts.id)>1)))
    
    ## check column name doesn't already exist... 
    
    # if(gridID %in% colnames(pts)){
    
    pts.res <- mapply(function(x,y){
      x <- cbind(x, gridID = y)
    }, pts.lst, grd.pts, SIMPLIFY = F)
    
    pts.id <- do.call(rbind, pts.res)
    
    return(pts.id)
    
  } else {
    
    # ## keep first duplicated point, delete rest
    # dups <- duplicated(grd.pts.id)
    # 
    #sum(dups) # this is the total number of duplicated occurrence records (more than above as more 
    # than 2 per cell duplicated in some cases)
    
    print(sprintf("%s records eliminated in total", sum(unlist(dups))))
    
    if(!missing(byID)) {
      print(sapply(dups, sum))
    }
    # pts.filt <- pts[!dups,]
    # 
    # return(pts.filt)
    
    pts.res <- mapply(function(pts, dup) pts[!dup,], pts.lst, dups, SIMPLIFY = F)
    
    # bind together again
    pts.filt <- do.call(rbind, pts.res)
    rownames(pts.filt) <- NULL
    return(pts.filt)
    
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

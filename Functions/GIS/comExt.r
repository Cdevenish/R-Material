# get common extent of several rasters  [ADD sf objects too... ]
comExtent <- function(..., out = c("extent", "bbox", "vector")) {
  
  dots <- list(...)
  
  # # split objects by class
  # split(dots, sapply(dots, class))
  # 
  
  out <- match.arg(out)
  
  exts <- do.call(rbind, lapply(dots, function(x) as.vector(raster::extent(x))))
  colnames(exts) <- c("xmin", "xmax", "ymin", "ymax")
  
  #common
  res <- c(xmin = max(exts[,"xmin"]), 
           xmax = min(exts[, "xmax"]),
           ymin = max(exts[,"ymin"]),
           ymax = min(exts[, "ymax"]))
  
  
  switch(out, 
         
         extent = raster::extent(res),
         bbox = sf::st_bbox(res),
         vector = res
  )
  
}


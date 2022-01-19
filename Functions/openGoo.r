##### Open coordinates in google earth ####

## update to include sf objects as well
## use shell to open programm rather than relying on file extention associations
# updated to crs to EPSG from +proj=longlat +datum=WGS84

openGoo <- function(sp, crs = "EPSG:4326", yx = FALSE, lonlat = c("lon", "lat")){
  
  ## opens coordinates from clipboard/spatial object(sp, sf)/dataframe in google earth by 
  ## creating a kml in temporary directory
  
  ## crs is the projection of the coordinates, if copied from clipboard or in dataframe, ignored if sp is present
  ## sp can be an sf or sp spatial points object, or missing, if coordinates are copied onto clipboard
  
  ## yx is logical, if data is Lat Lon, then use TRUE to switch to Lon Lat (x y)
  
  ## lonlat is a length two character vector with column names corresponding to longitude and latitude, respectively
  
  # wgs <- "+proj=longlat +datum=WGS84"
  # wgs.epsg <- "EPSG:4326"
  
  # require(sp)
  # require(rgdal)
  
  if(missing(sp)) {
    
    df <- read.table("clipboard", sep="\t", header=FALSE) 
    
    if(yx == TRUE){
      df[,3] <- df[,1]
      df[,1] <- NULL
    }
    
    colnames(df) <- c("x", "y")
    df$ID <- seq(1, nrow(df), 1)
    coordinates(df) <- c("x", "y")
    proj4string(df) <- CRS(crs)
    
  } else if(inherits(sp, "data.frame")){
    
    df <- sf::st_as_sf(sp, coords = lonlat, crs= crs)
    
  } else if(inherits(sp, "sp")) {
    df <- sf::st_as_sf(sp) } else if(!inherits(sp, "sf")) {
      stop("sp must be either data.frame, sp or sf object")
    }
  
  # proj4string(df)
  ## no need to project, kml driver does this...  just need projection
  # if(!identical(sf::st_crs(df)$input, wgs.epsg)) df <- sf::st_transform(df, wgs.epsg)
  
  tmp <- tempfile(pattern = "coords", fileext=".kml")
  sf::st_write(df, dsn=tmp, layer=basename(tmp), driver="KML")
  
  shell.exec(tmp) # open using default program in windows
}

# e.g 
# openGoo()
# openGoo(yx = TRUE)

# shell(paste("start", "C:\\Program Files\\Google\\Google Earth\\client\\googleearth.exe", tmp)) ## doesn't like the spaces, where is the google earth exe path??

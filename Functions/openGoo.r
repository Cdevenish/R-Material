# 5.3 open coords in google earth ####

## Also on GIT ojo...

## update to include sf objects as well
## use shell to open programm rather than relying on file extention associations

openGoo <- function(sp, crs = "+proj=longlat +datum=WGS84", yx = FALSE){
  
  ## opens coordinates (in latlon) copied into clipboard in google earth by 
  ## creating a kml in temporary directory
  
  ## crs is the projection of the coordinates, if copied from clipboard, ignored if sp is present
  ## sp can be an sf or sp spatial points object, or missing, if coordinates are copied onto clipboard
  
  ## yx is logical, if data is Lat Lon, then use TRUE to switch to Lon Lat (x y)
  
  wgs <- "+proj=longlat +datum=WGS84"
  
  require(sp)
  require(rgdal)
  
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
    
  } else if("sf" %in% class(sp)) df <- as(sp, "Spatial")
  
  # proj4string(df)
  
  
  if(!identical(proj4string(df), wgs)) df <- spTransform(df, wgs)
  
  tmp <- tempfile(pattern = "coords", fileext=".kml")
  writeOGR(df, dsn=tmp, layer=basename(tmp), driver="KML")
  
  shell.exec(tmp) # open using default program in windows
}

# e.g 
# openGoo()
# openGoo(yx = TRUE)

# shell(paste("start", "C:\\Program Files\\Google\\Google Earth\\client\\googleearth.exe", tmp)) ## doesn't like the spaces, where is the google earth exe path??

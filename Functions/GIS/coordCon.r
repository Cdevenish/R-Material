# 3.1 Convert coordinates ####

# "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


coordCon <- function(df=NULL, x="Lon", y="Lat", prjFROM="+proj=longlat +datum=WGS84", prjTO, newXY){
  
  # converts coordinates between projections
  # df is either a dataframe with named columns "x" and "y" (default "Lon" and "Lat")
  # or a vector c(x, y)
  # or coords are taken from clipboard - columns must be in order: x, y
  # prjFROM and prjTO are proj4 strings
  #WGS84 <- "+proj=longlat +datum=WGS84"
  #UTM17S <- "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  # source w.xls # depends on this route being accessible if rows > 500
  
  require(rgdal)
  require(sp)
  library(sf)
  
  if(missing(newXY)) newXY <- c("prjx", "prjy")
  
  
  cls <- class(df)
  if (!cls %in% c("data.frame", "numeric", "NULL")) stop("wrong input type - must be vector, dataframe or from clipboard")  ## TAKE COMILLAS OFF NULL?
  if (cls=="NULL"){
    df <- read.table("clipboard", sep="\t", header=FALSE)
    colnames(df) <- c("x", "y")
  }
  if (cls %in% c("numeric", "data.frame")){
    df <- data.frame(x=df[,x], y=df[,y])
    colnames(df) <- c("x", "y")
  }
  
  
  ## add id to df, then take out NAs, then join up again at end to preserve NAs
  
  df$ID <- 1:nrow(df)
  df.na <- df[complete.cases(df),] 
  
  coordinates(df.na) <- c("x", "y")
  proj4string(df.na) <- CRS(prjFROM)
  res <- spTransform(df.na, CRS(prjTO))
  
  res <- data.frame(res)
  res[,4] <- NULL
  colnames(res)[2:3] <- newXY
  df.res <- merge(df, res, by = "ID", all = T)
  df.res$ID <- NULL
  
  
  if (cls=="NULL"){
    if(nrow(df.res) < 500){ # clipboard maxes out at two columns and about 900 rows....
      
      write.table(df.res[,c("prjx","prjy")], "clipboard", sep="\t", col.names=F, row.names=F)
    }
    else {
      source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")
      w.xls(df.res)
    }
    
  }
  else{
    return (df.res)
  }
}

## eg. 
## coordCon()
# new <- coordCon(c(487958,9502778))
# df <- c(487958,9502778)
# ds <- data.frame(x=c(487958, 500000, 6), y=c(9502778,9400000,8))

# coordCon(c(641660,9365225), prjTO="+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#coordCon(prjFROM="+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# nt <- coordCon(ds)
# coordCon()

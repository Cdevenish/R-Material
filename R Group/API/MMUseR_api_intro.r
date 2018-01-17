####

## JSON
library(jsonlite)

j1 <- fromJSON("dogs2.json", simplifyVector = T)
j1
str(j1)


j2 <- fromJSON("dogs2.json", simplifyVector = F)
str(j2)

j3 <- fromJSON("products.json")
str(j3)

### 1. A simple API example ###

load("ssdf18.rdata")

library(httr)
library(jsonlite)

# http://sunrise-sunset.org/api

date1 <- "2018-01-01" # of form 2017-02-11  yyyy-mm--dd
date2 <- "2018-12-31" # of form 2017-02-11  yyyy-mm--dd


# latitude and longitude
lat <- "53.477087" # as a string e.g. "36.7201600" # Manchester
lon <- "-2.245053" 

dates <- seq.Date(as.Date(date1, "%Y-%m-%d"), as.Date(date2, "%Y-%m-%d"), 1)
head(dates)

url.base <- "http://api.sunrise-sunset.org/json?"


## Get all urls - with all date combinations - in a single vector
urls <- paste0(url.base, "lat=", lat, "&lng=", lon, "&date=", dates, "&formatted=0")
# formatted = 0 returns dates in ISO 8601 format with daylength in seconds

urls[1] # TRY IN WEB

## Try getting the data from the web page:
tmp <- fromJSON(urls[1])
tmp

class(tmp)

str(tmp, max.level = 2)

# make results into vector (later to bind into data frame)
tmp2 <- unlist(c(tmp$results, status = tmp$status))
tmp2
class(tmp2)


# Get all results with a loop.

## create holder for data
tmpList <- list()


for (i in seq_along(urls)){
  
  tmp <- fromJSON(urls[i])
  tmpList[[i]] <- unlist(c(tmp$results, status = tmp$status))
  
}

head(tmpList) # list of character vectors (same length)

## bind the character vectors into a dataframe
ssdf <- data.frame(do.call(rbind, tmpList), stringsAsFactors = F)

str(ssdf)

# get column names for all times
cols <- colnames(ssdf)[c(1:3, 5:10)]
cols

# loop through time columns and convert to POSIXct time format
for(col in cols) ssdf[,col] <- as.POSIXct(ssdf[,col], "%Y-%m-%dT%H:%M:%S", tz="GMT")

# change daylength to hours
ssdf$day_length <- as.numeric(ssdf$day_length)/3600
# Make a date column
ssdf$date <- as.Date(ssdf$sunrise)

str(ssdf)

## Plot daylength over 2018
par(bg = "bisque")
plot(ssdf$date, ssdf$day_length, type = "l", xlab = "Date", ylab = "Day length (hours)", col = "darkblue")

# Plot where we are today....
abline(v = as.Date("2018-01-17"), lty = 2, col = "darkred")

## show solstice and equinox approx
plot(ssdf$date, ssdf$day_length, type = "l", xlab = "Date", ylab = "Day length (hours)", col = "darkblue")

abline(v = as.Date("2018-06-21"), lty = 3)
abline(v = as.Date("2018-12-21"), lty = 3)

abline(v = as.Date("2018-03-21"), lty = 4)
abline(v = as.Date("2018-09-21"), lty = 4)

save(ssdf, tmpList, file = "ssdf18.rdata")



### 2. IUCN Red List API example ######

## See documentation here:

# http://apiv3.iucnredlist.org/api/v3/docs
shell.exec("http://apiv3.iucnredlist.org/api/v3/docs")


library(httr)
library(jsonlite)


# http://apiv3.iucnredlist.org/api/v3/country/list?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee

base.url <- "http://apiv3.iucnredlist.org/api/v3/"
token <- "?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee" # test token on web site

## Country list
cty.list <- GET(paste0(base.url, "country/list", token), encode = "json")
http_status(cty.list)

class(cty.list)

cty.doc <- jsonlite::fromJSON(content(cty.list, "text"), simplifyVector = T) # returns a list
str(cty.doc, max.level = 2)
head(cty.doc[[2]])

cty.df <- cty.doc[[2]]
cty.df

## Species by country

# eg  from web
# "http://apiv3.iucnredlist.org/api/v3/country/getspecies/AZ?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"

cty <- "CO"

sp <- GET(paste0(base.url, "country/getspecies/", cty, token))

# Check status of request
http_status(sp)

sp.doc <- jsonlite::fromJSON(content(sp, "text"), simplifyVector = T) # returns a list

str(sp.doc, max.level = 2)
head(sp.doc[[3]])

## table of categories per country
table(sp.doc[[3]]$category)


## Get numbers for multiple countries
ctys <- c("AR","BO","BR","CL","CO","EC","FK","GF","GY","PY","PE","SR","UY","VE")

library(maptools)
data(wrld_simpl)
plot(wrld_simpl)
head(wrld_simpl@data)

## subset world polygon with South America countries
sa <- wrld_simpl[wrld_simpl@data$ISO2 %in% ctys,]
plot(sa)

cty.df[cty.df$isocode %in% ctys,]

## Create all urls to query
sa.urls <- paste0(base.url, "country/getspecies/", ctys, token)
sa.urls

## send the query and get results
sp.sa <- lapply(sa.urls, GET)

## Check status of each request
sapply(sp.sa, http_status)
sapply(sp.sa, function(x) http_status(x)$reason == "OK")
sum(sapply(sp.sa, function(x) http_status(x)$reason == "OK"))

sp.doc <- lapply(sp.sa, function(x) fromJSON(content(x, "text"), simplifyVector = T)) # returns a list

str(sp.doc, max.level = 2)


## Look at results - no country name in data frame
head(sp.doc[[1]]$result)

## Add country name to each data frame
sp.doc <- lapply(sp.doc, function(x) {x$result$cty <- x$country; x})
head(sp.doc[[1]]$result)

## put all into a single dataframe
sa.df <- do.call(rbind, lapply(sp.doc, function(x) x$result))
head(sa.df)

## check numbers
sum(sapply(sp.doc, function(x) x$count))
## same as 
nrow(sa.df)

table(sa.df$category[sa.df$category %in% c("CR", "EN", "VU")])

sa.gts <- subset(sa.df, category %in% c("CR", "EN", "VU"))

## get frequency tables per IUCN category
sa.res <- by(sa.gts$category, list(sa.gts$cty), table)
sa.res <- data.frame(do.call(rbind, sa.res))
str(sa.res)
sa.res$ISO2 <- row.names(sa.res)
sa.res

## Merge with polygon... this has the following table - merge on ISO2
sa@data

sa <- merge(sa, sa.res, by = "ISO2")
head(sa@data)
summary(sa)

## Make colours for graduated map display
cols <- colorRampPalette(c("darkred", "pink"))

## plot in order of number of CR species
plot(sa, col = cols(14)[rank(-sa@data$CR)])

## plot in order of all GTS
plot(sa, col = cols(14)[rank(-rowSums(sa@data[,c("CR","EN","VU")]))])

######

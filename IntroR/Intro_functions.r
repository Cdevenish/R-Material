####### Intro to Functions in R #######
#                                     #
#             MMUseR                  #
#                                     #
## Manchester Metropolitan University #
#                                     #
###### Christian Devenish #############



## 1. Simple function

myMean <- function(x) {
  
  res <- sum(x)/length(x)
  res
  
  
}

a <- rnorm(100)

myMean(a)


### Why make functions 
## 1. - for own convenience...(automating)
## 2. - to share with other people (share methods, analysis techniques, etc)



## 2. Example of convenience function

head(iris)


tmp <- subset(iris, Species == "setosa")

c.test <- cor.test(tmp$Sepal.Length, tmp$Sepal.Width)

plot(tmp$Sepal.Length, tmp$Sepal.Width)
legend("topleft", paste("r = ", round(c.test$estimate, 2)))



cor.iris <- function(sp){
  
  tmp <- subset(iris, Species == sp)
  
  c.test <- cor.test(tmp$Sepal.Length, tmp$Sepal.Width)
  
  plot(tmp$Sepal.Length, tmp$Sepal.Width)
  legend("topleft", paste("r = ", round(c.test$estimate, 2)))
  # return(c.test)
  
}

cor.iris("setosa")
cor.iris("versicolor")
cor.iris("virginica")

vir.res <- cor.iris("virginica")
vir.res



#### Points to include #####

## 0 Load libraries and write comments

myMean <- function(x) {

  # function to calculate mean
  # x is a numeric vector
  
  library(ggplot2)
  plot(ggplot2::qplot(data.frame(x)$x, geom = "histogram"))
  
  res <- sum(x)/length(x)
  res

}

myMean(a)


## 1 Check data types

?stop

myMean <- function(x) {
  
  # function to calculate mean
  # x is a numeric vector
  
  if(!is.numeric(x)) stop("x is not numeric")
  
  res <- sum(x)/length(x)
  res
  
}

myMean(letters)
myMean(a)

## 2. Check arguments (use switch() to handle choices)

?match.arg

myMean <- function(x, fun = c("mean", "median", "sum")) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(!is.numeric(x)) stop("x is not numeric")
  
  fun <- match.arg(fun)
  
  res <- switch(fun,
         
         mean = mean(x),
         median = median(x),
         sum = sum(x)
         )
  
  res
  
}


myMean(a, fun = "means")
myMean(a, fun = "mean")
myMean(a)
myMean(a, fun = "s")
myMean(a, fun = "sum")


## 3. Add default values for your arguments

myMean <- function(x, fun = c("mean", "median", "sum"), plot = T) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(!is.numeric(x)) stop("x is not numeric")
  
  fun <- match.arg(fun)
  
  res <- switch(fun,
                
                mean = mean(x),
                median = median(x),
                sum = sum(x)
  )
  
  if(plot) hist(x)
  
  res
  
}

myMean(a)
myMean(a, plot = F)



## 4. Calculate default values if arguments are missing

myMean <- function(x, fun = c("mean", "median", "sum"), plot = T) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(missing(x)) x <- rnorm(100)
  if(!is.numeric(x)) stop("x is not numeric")
  
  fun <- match.arg(fun)
  
  res <- switch(fun,
                
                mean = mean(x),
                median = median(x),
                sum = sum(x)
  )
  
  if(plot) hist(x)
  
  res
  
}

myMean()

## 5. Add additional arguments to pass to internal functions

# ...

myMean <- function(x, fun = c("mean", "median", "sum"), plot = T, ...) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(missing(x)) x <- rnorm(100)
  if(!is.numeric(x)) stop("x is not numeric")
  
  fun <- match.arg(fun)
  
  res <- switch(fun,
                
                mean = mean(x, ...),
                median = median(x, ...),
                sum = sum(x, ...)
  )
  
  if(plot) hist(x)
  
  res
  
}

x <- c(1,3,5,7,9,20, NA)
myMean(x)

myMean(x, na.rm = T)


## 6 Retun multiple arguments .... in a data frame

mySumm <- function(x, ...) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(!is.numeric(x)) stop("x is not numeric")
  
  res <- data.frame(mean = mean(x), median = median(x), sum = sum(x))
  
  res
  
}

mySumm(a)
sum.a <- mySumm(a)
sum.a

### Or as a list
mySumm <- function(x, ...) {
  
  # function to calculate mean, median or sum
  # x is a numeric vector
  
  if(!is.numeric(x)) stop("x is not numeric")
  
  res <- list(mean = mean(x), median = median(x), sum = sum(x), data = x)
  
  res
  
}

summ.a <- mySumm(a)

summ.a$mean
summ.a$data

### 7. Environments

# functions are evaluated within their own environment. 
# use global assignment if you want to create objects outside the function evnvironment... at your own risk....

myMean <- function(x) {
  
  tmp <<- sum(x)/length(x)
  tmp
  
  
}

myMean(1:10)




### Showing source code for functions in R

## S3
mean # some functions will show source code like this..
methods(mean)  ## others have several methods for different clases
?methods
getAnywhere('mean.default') 

## S4 classes 
if (!require(raster)) install.packages('raster')

methods(raster)
showMethods(raster::raster)
getMethod("raster", "asc")







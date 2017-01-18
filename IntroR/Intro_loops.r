#####     Introduction to loops in R   ######
#                                           #
#         Christian Devenish                #
#         MMUseR Group                      #
#         18 January 2017                   #
#############################################

## Some exmaples of how to do loops, when to use them and when not to use them...


setwd("") # set your working directory


## 1. Some example loops ####

## Whatever you put inside the loop will be repeated 

for (i in 1:10){
  print ("hello")
}


## This shows how the 'i' (iterator) takes on the values that you use in the for() function:

for (i in 1:10){
  print (i)
}


## The iterator can take on any value you give it.
## How about with just even numbers?

1:10*2 # notice order of operations

for (i in 1:10*2){
  print (i)
}

## Or you can also do:

for (i in c(2,4,6,8,10)){
  print (i)
}

## sometimes this can be useful, but then you can run into problems storing the results...


## Do something to each element of a vector or list
# eg loop through a vector of numbers 1-10 and square them.

v1 <- 1:10

for(i in 1:10){
  print(v1[i]^2) # each element of v1 is squared in turn, the ith element
}


## If writing functions, or if you are likely to reuse your code with different values (different lengths of vectors),
# then it's useful to use the length of the vector instead of 'hard coding' the exact length of 'i'

for(i in 1:length(v1)){
  
  print(v1[i]^2)
}

## This is the same as using the function seq_along()
seq_along(v1)
?seq

for(i in seq_along(v1)){
  
  print(v1[i]^2)
}


### Over a text vector

s1 <- c("This", "loop", "should", "print", "each", "element", "in", "this", "vector")

length(s1)

for (i in 1:9){
  print (s1[i])
}


### You can loop directly over a list, or vector, without setting the iterator to numeric values as above.
## eg 
for (s in s1){ # note that we don't have to use 'i', it can be any other character(s)
  print (s)
}

## However, although it makes the code neater, normally we need a numeric iterator to help store the data.


## 2. Storing the result of a loop ####

# Run this loop from above. 

for(i in 1:length(v1)){
  
  v1[i]^2
}

# It runs fine, but where are the results?
#

# We need to store the results of a loop inside the loop as we repeat each operation. A common way to do this is by
# creating a holding vector or list to store the results first

## eg. 

res <- vector() # set up an empty vector
res

# remember indexing vectors??

for(i in 1:length(v1)){
  
  res[i] <- v1[i]^2 # note that the result of each iteration is stored in the corresponding position in the vector
}

# check the results
res

## If you have lots of results (1000s), then it is a good idea to set the size of the vector/list first.
# eg res <- vector(mode = "numeric", length = 1000), and optionally the type of data


## Of course, we wouldn't use a loop for the above example. What's the best way of doing this in R?

v1^2


## Better example. Storing the results of a regression in a list. Here we use a list instead of a vector to store the
# results, but the idea is the same. 

## Using the LifeCycleSavings data:

?LifeCycleSavings
head(LifeCycleSavings)

# What factors influence people's ability to save?
# Before doing a multiple regression, could be useful to assess the effect of each variable on the response. 
# In this case, there are only 4 predictors, but with many more, a loop makes this easy.

# What do we want to loop over? The predictor variables:
vars <- colnames(LifeCycleSavings)[2:5]
vars

# Make a list for the results:
life.res <- list()

for(i in seq_along(vars)){ # note that we're using the length of predictor names for the length of i
  
  life.res[[i]] <- lm(sr ~ get(vars[i]), data = LifeCycleSavings) # do the model with each predictor column and store in the list
  
}

# check the resuls

str(life.res, max.level = 1) # note the second argument - useful for long lists!!! try without max.level
str(life.res)

life.res

summary(life.res[[1]])

## we can use lapply to loop through the results and pull out the summaries
lapply(life.res, summary)

## Or to get the r squared values easily, with sapply
sapply(life.res, summary)

## We can use this to graph the results and see which predictor gives the highest R squared
barplot(unlist(sapply(life.res, summary)[8,])*100, names.arg = vars, xlab = "predictors", ylab = expression(R^2))

# Can combine this with check for collinearity for choosing predictor variables
pairs(LifeCycleSavings)


## Can also do similar things with lapply instead of loops... but sometimes easier to read loops.
## eg above regression example with lapply (we don't need to set up a holding vector or list first)

life.res2 <- lapply(vars, function(x) lm(sr ~ get(x), data = LifeCycleSavings))
sapply(life.res2,summary)



## Multiple runs, simulations, bootstraps, etc.

set.seed(2000)

n <- 100
x <- rnorm(n)
mean(x)
# 95% confidence intervals 
se <- sd(x)/sqrt(n)
ci <- 1.96 * se * c(-1,1)
ci

## Or from bootstrap
res <- vector(length = n)

for(i in 1:(n-1)){
  
  res[i] <- mean(sample(x, size = n, replace = T))  
}

ci.bs <- quantile(res, c(0.025, 0.975)) # 95% Confidence intervals we use 2.5th and 97.5th percentiles of bootstrapped means
ci.bs

## compare the confidence intervals graphically
plot(mean(x), pch = 19, col = "black", ylim = c(-0.3, 0.3), xlab = "")
arrows(0.99, ci[1], y1 = ci[2], angle = 90, code = 3)
arrows(1.01, ci.bs[1], y1 = ci.bs[2], angle = 90, code = 3, col= "blue")


### Doing more things in a loop and combining into a data.frame

head(iris)

## Let's get the mean, and coefficient of variation for each species for Sepal.Length

# Run loop over species
spp <- as.character(unique(iris$Species))


## Make holder
res <- data.frame()

for(i in seq_along(spp)){ 
  
  dat <- subset(iris, Species == spp[i])
  mn <- mean(dat$Sepal.Length)
  cv <- sd(dat$Sepal.Length)/ mn
  
  res <- rbind(res, cbind(mn, cv))
}

res

## Can get messy or inefficient by binding to a data frame each time... 

## can make a matrix instead of data.frame
res <- matrix(nrow = 3, ncol= 2)

for(i in seq_along(spp)){ 
  
  dat <- subset(iris, Species == spp[i])
  mn <- mean(dat$Sepal.Length)
  cv <- sd(dat$Sepal.Length)/ mn
  
  res[i,] <- cbind(mn, cv)
}

res

# Tidy up 
colnames(res) <- c("mean", "CV") # add column names
res <- data.frame(spp = spp, res) # add species' names as a column and convert to dataframe
res

## With large projects and different data types, lists are probably easier to handle in the loop, and then convert after.

res <- list()

for(i in seq_along(spp)){ 
  
  dat <- subset(iris, Species == spp[i])
  mn <- mean(dat$Sepal.Length)
  cv <- sd(dat$Sepal.Length)/ mn
  
  res[[i]] <- data.frame(spp = spp[i], mean = mn, CV = cv)
  
}

res

res.df <- do.call(rbind, res)
res.df
str(res.df)


## Of course, if we wanted to get summaries by species, we could use tapply() or aggregate()
tapply(iris$Sepal.Length, iris$Species, mean)

aggregate(iris$Sepal.Length, list(iris$Species), mean)

# it gets a bit more complicated with more than one function...

aggregate(iris$Sepal.Length, list(iris$Species), function(x) cbind(mean(x), sd(x)/mean(x)))
# but better than the loop. 

# even for more options... 
aggregate(iris[,1:4], list(iris$Species), mean)
aggregate(iris[,1:4], list(iris$Species), function(x) cbind(mean(x), sd(x)/mean(x)))

# However, there are lots of times when slightly more complex operations deserve a loop and then 
# you'll need to combine the results into a dataframe.


#### Reading multiple files from disk ####

df1 <- list() 


fileNames <- list.files("specdata", full.names=T)

for (i in seq_along(fileNames)){
  a <- read.csv(fileNames[i])
  df1[[i]] <- a
}

df1 <- do.call(rbind, df1)

str(df1)
head(df1)



## Drawing multiple lines on a single graphic. ####
# eg draw normal curves with mean = 0, and standard deviation with values from 0.2 to 0.8

# set up the plot area first - but don't actually plot anything (type = "n")
plot(0,0, xlim = c(-1.5,1.5), ylim = c(0,2.5), type = "n", xlab = "", ylab = "")

for(i in seq(0.2, 0.8, 0.1)){
  
  curve(dnorm(x, sd = i), add = TRUE, col = "blue", lwd = 2) # this draws the normal curve  
  
}

## use a vector of colours to give each line a different colour:
## note that now we run into problems without a change to the iterator

cols <- rainbow(6)
sds <- seq(0.2, 0.8, 0.1)

plot(0,0, xlim = c(-1.5,1.5), ylim = c(0,2.5), type = "n", xlab = "", ylab = "")

for(i in seq_along(sds)){
  
  curve(dnorm(x, mean = 0, sd = sds[i]), add = TRUE, col = cols[i], lwd = 2) # this draws the normal curve  
  
}

## add a legend
legend("topright", legend = sds, title = "Standard deviation", col = cols, lwd = 2, bty = "n")



## Doing multiple graphics
?CO2
head(CO2)

co2 <- data.frame(CO2)
head(co2)
str(co2)

spp <- unique(co2$Plant)

# i = 1

for(i in seq_along(spp)){
  
  plot(co2$conc[co2$Plant == spp[i]], co2$uptake[co2$Plant == spp[i]], 
       main = spp[i], xlab = expression(CO[2]~concentration), ylab = expression(CO[2]~uptake))
  
}


## On same page
op <- par(mfrow = c(6,2)) # change plot options to a 6 x 2 arrangement
for(i in seq_along(spp)){
  
  plot(co2$conc[co2$Plant == spp[i]], co2$uptake[co2$Plant == spp[i]], 
       main = spp[i], xlab = expression(CO[2]~concentration), ylab = expression(CO[2]~uptake))
  
}

par(op) # go back to original par settings

## Or save as an image
png("CO2.png", width = 180, height = 270, units = "mm", res = 100)
par(mfrow = c(6,2))
for(i in seq_along(spp)){
  
  plot(co2$conc[co2$Plant == spp[i]], co2$uptake[co2$Plant == spp[i]], 
       main = spp[i], xlab = expression(CO[2]~concentration), ylab = expression(CO[2]~uptake))
  
}
dev.off()

# remember where the image is stored?
getwd()

## Also useful to use with() to make code less cluttered

png("CO2.png", width = 180, height = 270, units = "mm", res = 100)
par(mfrow = c(6,2))
for(i in seq_along(spp)){
  
  with(co2[co2$Plant == spp[i],], plot(conc, uptake,
                                       main = spp[i], 
                                       xlab = expression(CO[2]~concentration), 
                                       ylab = expression(CO[2]~uptake)))
}
dev.off()


## Or save as individual images
png("CO2_%02d.png")
for(i in seq_along(spp)){
  
  plot(co2$conc[co2$Plant == spp[i]], co2$uptake[co2$Plant == spp[i]], 
       main = spp[i], xlab = expression(CO[2]~concentration), ylab = expression(CO[2]~uptake))
  
}
dev.off()


## but also see ggplot2

library(ggplot2)
ggplot(CO2, aes(x = conc, y = uptake)) + 
  geom_point() +
  facet_wrap(~ Plant, nrow = 6, ncol = 2)


## Also file operations, eg unzipping, processing data files, simulations, bootstraps, etc.


## When doing loops... 
  ## Do you need one?
  ## How will you store the results?
  ## What will you iterate over?
  

### Now you try....


## 1. Write a  loop to print each of the elements in this vector in turn to the console:
v1 <- c("This", "is", "a", "simple", "loop")




## 2. Write a loop to calculate the natural log of even numbers from 2 to 100

seq(2,100,2)


# What would be a better way to do this?

## 3. write a loop to store the results of cor.test in a list for correlations between mpg and disp, hp, drat, wt and qsec
# in the mtcars dataset

vars <- colnames(mtcars)[3:7]
vars

res <- list()

## ..... comnplete


## Extract the correlation coefficients from the list


## 4. Write a loop to draw a histogram for mpg, disp, hp, drat, wt, qsec in the mtcars data set, include a main title


## 5. Write a loop to draw a random sample of 100 elements of the vector x below (from a uniform distribution) and 
## calculate the mean for each. Store the means in a vector called res. Draw a histogram of res. 

x <- runif(1000)
hist(x)

## ... do loop 



hist(res)



# 6. Write a loop to draw regression lines per species onto a scatter plot of Petal length against Sepal Length of the iris data


# Now colour the points and lines with the same colour



######  Data manipulation in R ######

### Some notes for MMUseR session ###

###### Christian Devenish ###########
######### 15 March 2017 #############

setwd("J:/Documents_on_FreeAgent/MMU_ConsBiol/PhD/R/R group/manipulation")
setwd("H:/manipulation")


## 1. Data entry ## 

## Rstudio "import dataset" button

## or through script with 

# read.csv()
# read.table()

# eg 

write.csv(iris, "iris.csv", row.names = F)
write.table(iris, "iris.txt", row.names = F, sep = "\t")

myDF <- read.table("iris.txt", header = T, sep = "\t")
head(myDF)

myDF2 <- read.table("iris.csv", header = T, sep = ",")

identical(myDF, myDF2)

#myDF3 <- read.table("clipboard", header = T, sep = "\t")
#identical(myDF, myDF3)


## Directly from excel
# library(ODBC)
## 64 32 issue....


## Indexing ####

v0 <- c(4, 7, 9)
v1 <- c(3, 7, 13, 20, 30, 55, 60, 99, 4, 3)


## By position
v1[3]
v1[-1]

v1[c(1:2, 5)]


## by value (and logical vectors)
v0[c(F, T, F)]

v0 == 7 # this is a logical vector
# use the logical vector to index
myIndex <- v0 == 7
myIndex

v0[myIndex]
v0[v0==7]

## in multiple values
v1[v1 %in% c(20, 55, 60)]

## negation
v1[!v1 %in% c(20, 55, 60)] ## TIP! test small parts of the code if you're not sure what it does. eg

!v1 %in% c(20, 55, 60) # compare to 
v1 %in% c(20, 55, 60)

## Replacing values in a vector
v0
v0[v0==7] <- 99
v0

# replace even values
v1
v1[v1%%2==F] # identify the even numbers
v1[v1%%2==F] <- 0 # replace with zero
v1

## can use any logical expression as index (ie must give True or False)
v1[v1 ==3 | v1 > 50 & v1 < 99]

## Data frame indices ####

# Same idea as above, but with 2 dimensions: [row, column] [r, c]
## can use either positions (ie row number or column number) or column names

iris

# what is value in 3rd row, 4th column? (Using positions)
iris[3,4]

## using names
iris[3,"Petal.Length"] # safer if likely to columns positions likely to change.. 

# replace this with 0
iris[3,4] <- 0

iris
iris[3,4]

## bring back the data set!
data(iris)
iris[3,4]

##  get all even numbers in 3rd column?
iris[iris$Petal.Length%%2 == 0, 3]

## Return data set for those records with Sepal.Width > 3
iris[iris$Sepal.Width > 3, ]


## Can also use a matrix of indices.... 

# eg 
df1 <- data.frame(a = c(1,3,5,7), b = c(2,4,6,8), c = c(10, 20, 30 , 40))

# make a logical matrix to use as index
m <- df1 > 5
m

df1
df1[m]

## replace these values with 

df1[m] <- 0

df1


## replacing values
## replace all values above 4 with 999 in iris data set

# a bit difficult to read, but works on the complete dataframe, could split into several lines
iris[,1:4][iris[,1:4]>4] <- 999
iris

data(iris)


## Making  subsets of data with subset()
# Can also use indices, but subset is better for taking care of NAs
## subset()

iris.setosa <- subset(iris, Species == "setosa")
iris.setosa

# second argument is logical criteria...  can build up a longer expression here, eg
iris.sub <- subset(iris, Species == "setosa" | Species == "virginica" & (Sepal.Length < 6.0 & Sepal.Length > 3.5))
iris.sub

## 3rd argument also allows a subset of columns to be returned. eg

iris.setosa2 <- subset(iris, Species == "setosa", select = 1:4) # positions
iris.setosa3 <- subset(iris, Species == "setosa", select = c("Sepal.Length", "Sepal.Width")) # names


### ifelse
v1
v2 <- ifelse(v1 == 3, T, F)
v2


## cut

## cut() creates a factor based on cutting up a numeric variable at specified break points

# eg
v4 <- 1:10*10
f4 <- cut(v4, breaks = seq(0,100, 25))

## intervals are used as labels by default:
cut(v4, breaks = seq(0,100, 25)) # notice square brackets are closed (on right), ie first group, (0, 25] is >0, <=25

# put both factor and values into dataframe
df2 <- data.frame(v4, f4)
df2


## we can control with argument right whether intervals are closed or open on right (and vice versa on left)
cut(v4, breaks = seq(0,100, 25), right = F) # brackets and parenthesis have reversed
# ie first group is [0, 25) means >=0, <25

f5 <- cut(v4, breaks = seq(0,100, 25), right = F)

df2 <- cbind(df2, f5)
df2

## also include.lowest (or highest) is useful... 
f6 <- cut(v4, breaks = seq(0,100, 25), right = F, include.lowest = T)

df2 <- cbind(df2, f6)
df2


## can also use labels instead of default intervals
f7 <- cut(v4, breaks = seq(0,100, 25), right = F, include.lowest = T, labels = c("first", "second", "third", "fourth"))

df2 <- cbind(df2, f7)
df2

## Useful combining with quantiles to set breaks, with table(), 

set.seed(2000)
v5 <- rnorm(100, 50, 5)
quantile(v5)
q <- cut(v5, breaks = quantile(v5), include.lowest = T, labels = c("q1", "q2", "q3", "q4"))

df3 <- data.frame(v5, q)
df3

## USeful with table()
table(cut(v5, breaks = quantile(v5), include.lowest = T))

boxplot(df3$v5)
points(rep(1, 100), df3$v5, col = df3$q, pch = 19)

# with jitter to see points better
boxplot(df3$v5)
points(jitter(rep(1, 100)), df3$v5, col = df3$q, pch = 19)
legend("topright", pch = 19, col = 1:4, legend = paste("quartile", 1:4), bty = "n")

## Can specify a set number of breaks instead of absolute values

g3 <- cut(v5, breaks = 3, labels = c("g1", "g2", "g3"))
df3 <- cbind(df3, g3)
df3

points(jitter(rep(0.75, 100)), df3$v5, col = c(5:7)[df3$g3], pch = 19)
table(cut(v5, breaks = 3))

range(v5)
breaks <- (max(v5)-min(v5))/3 *1:2 + min(v5) 
breaks
segments(x0 = 0.5, x1 = 1.5, y0 = breaks)

## sort() and order()

iris


## merging data frames and using lookup tables (eg vlookup in excel)

?merge()

#merge()

df6 <- data.frame(Species = unique(iris$Species), x = rnorm(3))
df6

iris.new <- merge(iris, df6, by = "Species")

iris.new


# Merge by column names or rownames



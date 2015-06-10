#################    Introduction to R Programming   ##################
#                                                                     #
#                           MMU FUTURES                               #
#                       Christian Devenish                            #
#                                                                     #
#                     June 2015, Manchester, UK                       #
#                 Manchester Metropolitan University                  #
#                                                                     #
#######################################################################

####  Activity 3. Working with Data Frames in R

#1.0 We're going to use the classic iris data set.

# R has many inbuilt data sets. You can see them all with:
data()

# Look at the description file of the Iris data:
?iris

# Get the data into your workspace
data(iris)

## First things first. Have a look at your data and check the data types:
head(iris)
str(iris)

# The head() function provides the first six lines of a data set.
# The str() function tells us the kind of object (data frame), its dimensions (150 rows with 5 columns), the name of each column and its data type (4 numeric and 1 factor). A factor is a categorical variable, in this case, species name.

## If you just want to get the column names, use names() or colnames()
colnames(iris)

# We can get to each column by using the $ operator:
# DataframeName $ ColumnName
iris$Sepal.Length

# What is the maximum Sepal Length? Use the max() function on the Sepal Length column

# What is the minimum value for Petal width? 

# What is the mean Sepal.Width?

# A useful function is summary() for columns or whole data frames. Try it. What information is given?

summary(iris)

# Remember, all vectors in a data frame must be of the same length, therefore the length of each column should be the same as the number of rows in the data frame. Check the length of a column. Is it the same as nrow() of the data frames

nrow(iris)

## We can index a data frame using the same [] square brackets as for vectors. But now we have two dimensions, [row, column]. 

iris[2,3] # gives us the the value of the second row, third column. Check with head(iris)
head(iris)

# Use [] to get the 5th value of the 4th column. 

# Use [] to get the 10th value of the 5th column.

## If you all the values in a row or column. Leave this index blank. 

# eg All columns of first row:
iris[1,]

## Can you get all columns of the first 10 rows


# Now can you get all the values in the 1st column?



## How many species are there? 
# We already saw this with str(iris). How?

# We can check this with unique(). This function gives us only the unique values of a vector. Run unique() on the Species column

## We can also obtain the values in a column using the column name inside the square brackets:

iris[, "Sepal.Length"]

# Now get values from 1st to 50th row for Sepal.Length.

## If we want to isolate the data for one species, we can use a logical expression in the index:

iris[iris$Species == "setosa", "Sepal.Length"]

# This mean, get all values of the column "Sepal.Length", where the rows have "setosa" in the Species column.

## Try another index with a different species and different column. Find the mean of this column.

## A useful way to subset data is to use the subset() function.

# 
setosa.subset <- subset(iris, Species == "versicolor")

# This function takes the data set we want to subset as the first argument, then a logical expression (one that can evaluate to TRUE or FALSE) as the second, giving the criteria for our subset. An optional third argument is to specify the columns we want to include

?subset

## eg get just first 4 columns for versicolor

setosa <- subset(iris, Species == "versicolor", 1:4)

# Now get a summary of this subset of the data using summary

## Other criteria for subsetting
# Remember the logical operators: > < == != | &

# Can you make a subset of iris with just values for Petal.Length greater than 4.0

# How many observations are in the data set?

# Can you make a subset of iris for all species apart from setosa
# What is the mean of Sepal.Width for this subset?

# Can you make a subset of iris for species of virginica with Sepal.Width greater than 3.0. 
# How many observations are there? What is the range() of the Sepal.Length?


## Renaming columns or rows

## we can rename columns using the same colnames() function as before, but assigning new values with a <- operator

# eg Let's look at this more closely
# We want to rename the first column of the setosa subset as sep_len

colnames(setosa) # gives us a vector with 4 elements. 

# To rename just the 1st column we need to index this vector to get just the first value:

colnames(setosa)[1]

# Now we can assign just this value a new name

colnames(setosa)[1] <- "sep_len"

# check it:
head(setosa)

# Rename the third column as pet_len

## Now rename columns 2 and 4 as sep_wid and pet_wid respectively, using just one line.
# Use the c() to combine the names into a vector. Use an [] to get 2nd and 4th col names.


## What do you notice about the row names of our setosa subset?
setosa

# Let's rename the rows as numbers from 1 to 50, eg 1:50

# Can you think how to do this with the rownames() function, and the <- operator?


# Check with head

## These exercises try to get you to think about how to work with dataframes, about how indexing works, and how to use the []. You'll be doing this a lot. It's not just about renaming columns, etc.


## More functions with data frames

# tapply()
## This function splits up a vector according to a grouping variable and then applies a function on each group. In our case, we want to split the vector of Petal Lengths into groups according to species, and then calculate the mean of each of these: 

tapply(iris$Petal.Length, iris$Species, mean)


## Remember

## tapply(VECTOR, GROUP, FUNCTION)

# Use the same example, but change the function to max or min

# Now find the median sepal.width per species

## And one more, find the range of Petal lengths for each species (note the different output format)


# sapply()
## Suppose we want to find the mean for each column, rather than by species

# sapply() takes each column of a dataframe (or each component of a list) and applies a particular function to it. In our case, we calculate the mean for the first four columns of our iris data (it wouldn't make sense to apply the mean to the fifth column)

sapply(iris[1:4], mean)

# Do the same, but with the max, min and median.

## Ok, so what about a summary statistic for each column, but also per category (eg species)?

## aggregate() takes a dataframe, divides it into subsets according to a grouping variable (eg species) and then applies a function to each column in the data set.

aggregate(iris[1:4], list(iris$Species), mean)

## aggregate(DATAFRAME, list(GROUP), FUNCTION) # note that the group must go inside a list() function.  

# What is the max/min value of the numeric columns per species?
# Do you know how to do this in excel?


# aggregate can take several groups inside the list(), if you want to divide further, eg. summaries statistics of a data set of bird measurements by species and by sex.





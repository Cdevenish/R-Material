#################    Introduction to R Programming   ##################
#                                                                     #
#                           MMU FUTURES                               #
#                       Christian Devenish                            #
#                                                                     #
#                     June 2015, Manchester, UK                       #
#                 Manchester Metropolitan University                  #
#                                                                     #
#######################################################################

####  Activity 4. Importing data into R

## Set your working directory to the course folder on your computer

# Use setwd()
# check it with 
getwd()

# Create a data directory inside your working folder (in Windows) and copy the course data there.


## EXAMPLE 1. Importing from text files

## The most common way of importing data at the beginning

# We can use read.table() for all text files, or read.csv() for csv text files. These functions convert our data into data frames. They must already be in a column format.

## Look at the Sparrow_First.txt file, open it in windows. (Data set modified from Zuur 2007). Can you see the arrangement of the data? The columns are separated by tabs.

# eg Assign the contents of a text file to an object called df1 as a dataframe
df1 <- read.table("data/Sparrow_First.txt", header = T, sep = "\t")

## The arguments header indicate that the data has a first row with the column names, and the sep is the separator. 

## Remember, always check what you import straight away
head(df1)
str(df1)

# Remember how to get a column with the $ operator?

df1$Weight

# EXAMPLE 2. CSV FILES. Use read.table() to import the Fisher.iris.csv data. csv is Comma Separated Values, we need to change the sep argument to "," for this.

# Import this file as df2

# Check it with two functions. Use summary() now on the data set.


## EXAMPLE 3. Importing from the clipboard (Windows)

## Quick way to get data in (not recommended for scripts you want to run more than once)
# Also not recommended for large and complex data sets (with lots of text), but useful 
# for quick imports

## Open the excel file, mtcars.xlsx, select just the rows and columns with data and press copy (ctrl + c)

# Now run this command

df3 <- read.table("clipboard", sep = "\t", header = T)

## Remember, always check what you import straight away
head(df3)
str(df3)


## Remember, it's usually best to save your data from excel as a text file and then import.

## Save the mtcars data as a .txt file in your data folder from the excel. Go to save as, navigate to your data folder and choose text(tab delimited) from the drop down "save as type" list. And then choose yes to keep the data in this format, close the file and "Don't save" any changes.

## Now import the file as df4 using read.table and the file route as the first argument

## df4 and df3 should be exactly the same. Are they?

identical(df3, df4)

## Another useful function for checking your data is tail() it's very similar to head(), what's the difference?

## EXAMPLE 4. Importing from an rdata file.

## Often after you import and manipulate your data you save it as a .rdata file, or you may receive an .rdata file from a colleague. This files are imported with load() function

load("data/CherryTrees.rdata")
# Note that these files can contain several objects, and that their names are already assigned within the file. What does this file contain?

## To save R objects that you have been working with, use the save() function.

save(df1, df2, df3, df4, trees, file = "myData.rdata")

## Find the rdata file in your folder in windows.
## You can save as many objects as you like in one file, remember to use the argument "file"

### More work with data frames.

# Use some of the functions from the dataframes worksheet and explore the Sparrows (df1)  data set. 

# Careful with the weight column. Can you get a mean value for just weight? Look at the values. What do you notice? Is there an NA? A missing value? Use ?mean to see how you can take account of missing values.

# Use a plot to check the data
plot(df1$Weight)

## Is anything peculiar about the last data point?

## Is this a typo in the data file? Assume it is, correct the data in the text file and re import it. 

# Get some summary information per species (eg mean, max, etc) and per sex. Use differnt ways of getting this information.

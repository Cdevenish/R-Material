#################    Introduction to R Programming   ##################
#                                                                     #
#                           MMU FUTURES                               #
#                       Christian Devenish                            #
#                                                                     #
#                     June 2015, Manchester, UK                       #
#                 Manchester Metropolitan University                  #
#                                                                     #
#######################################################################

####  Activity 2 -- OBJECTS - VECTORS

### 2.1 Numeric vectors ####

## Numeric data 

# Assign these values to the named objects (a, A, c)

a <- 1
A <- 5
b <- 6

# Are a and A different objects in R?

# What happens now if we assign 100 to A?

A <- 100

# Check the value of A
A

# Create two more vectors
x <- 10
y <- 100

## What data type are these objects?
class(a)

a + b

(a + b) * x

## What do these functions do?

sum(b, x)
sqrt(y)

## Assign the result of an operation to a new object

d <- a + b

# See the result in d
d

## Now practise!

#1# Create three vectors with the values 7, 81, 42 named, dat1, dat2, dat3, respectively.



#2# Calculate the result of dat1 + dat2, dat2/dat3, the square root of dat2, the sum of all three, and the log of dat3 in base 9.

#Hint: look up the function log with the help function:
?log


## 2.2 Vectors with more than one element ####
1:10

v1 <- c(1,2,3,4,5,6,7,8,9,10)
v2 <- 1:10

## What happens in this operation?
v1 * 3

#2# Do these two operations give the same result?

mean(v1)

sum(v1)/length(v1)

# Practise

#3#  Create a vector of 10 even numbers between 2 and 20 (inclusive). Call the vector v3

v3 <- c(2,4,6,8,10,12,14,16,18,20)


#4# Can you find the sum, mean and total number of elements in v3 using functions in R?


#5# Create a vector of the square numbers of v1, call it v4. 

# Did you use the ^ operator?

#6# Create a new vector, called v5, consisting of each element of v3 multiplied by 15



#### 2.3 Indices ####

# EXAMPLES

v1[3]  # get the 3rd element of v1

v1[2:4] # obtain elements 2 to 4 of v1

v1[-1] # get all the elements of v1 except for the first

# Practise

#1# a. Write code to obtain the 5th element of v1
#   b. Elements 2 to 6 of v2
#   c. All the elements of v2 except for the 6th


#2# How could you obtain the 3rd, 5th and 7th elements of v3? Think about using the c() function

#3# Now get the 7th, 8th and 10th elements of v3

#4# You can combine vectors with c(). Try combining v1 and v2, assign the result to a new object. Look at it. 

#5# What happens if you try to mix data types in a vector? Try it.

vs1 <- c(1, 2, 3, 4, "a", "b", "c")

# Look at the result. What do you notice?
vs1

# What do you notice about the way the numbers are printed in the console from vs1, and v2
vs1
v2

#7#  Check the data type of vs1. Do you remember the function to do that?
class(vs1)


### 2.4 Character vectors ####

a <- "Hello"  # texto entre "comillas"
b <- "World"
sp1 <- "Phytotoma raimondii"

# Look at the vectors
a
sp1


# You can concatenate text with paste()
paste(a, b)

# Change the separator
paste(a, sp1, sep=", ")

## Look at the help function for paste()

#1# How many objects can we concatenate with this function?

#2# How do you define the separator? What is the default value for the separator?

#4# Look at the inbuilt object, called letters
letters

# This is a ready made vector, always available in R.

#4# Create a vector called s2, consisting of the letters "a" to "h". Use the [] to index these first 8 letters.

#5# Can  you create a vector, called myName, consisting of the letters of your name? Use the object letters with an index using c(), inside the []. 


#6# How many letters are there in your name? Work it out using the length() function. 


## 2.5 Logical vectors ####

# Another important type of vector are logical vectors. Elements of logical vectors are either TRUE or FALSE. Note that these must be written in capitals, and can be abbreviated to T or F. Don't use these words for variable names!

# Look up the page of reserved words:
? reserved

#1# Here is a simple logical vector?

l1 <- c(TRUE, TRUE)

#2# Can you create a logical vector consisting of 5 elements, of which the first 3 are TRUE, and the last two are FALSE. Call it l2:

## There are two useful properties of logical vectors. 
#a. We can use them to index other vectors
#b. We can use them as numeric values 1 (TRUE) or 0 (FALSE)

#3 Make a numeric vector called v6, consisting of multiples of 5, from 5 to 25

# Now use l2 to index this vector, like this v6[l2]

# Which values are returned? What logical values do they correspond to in l2?

# 4. Try summing the vector l2? What would you expect to be the answer if True = 1, and False = 0. 

# Check it with sum(l2)


#5# Remember how we multiplied vectors by another number?
# Can you multiply l2 by 5?

#6# Create another logical vector of 5 elements, like this, T, F, F, T, F. You can use the c() function in the same way as for numbers or letters. Call it l3.

# Now add l2 and l3. What result do you expect? Assign the resul to a vector called s1.

#7# Can you combine l2 and l3 into a single logical vector? 




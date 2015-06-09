Introduction to R programming
========================================================
author: Christian Devenish
date: June 2015
width: 1420
height: 800

1. R as a calculator

2. Vectors, variables and other objects


1.
=========================
type: sub-section
<br><br>
# R as a calculator

1.1 R as a calculator
========================================================


```r
1 + 1
```

```
[1] 2
```

```r
10 - 5
```

```
[1] 5
```

```r
10 / 2
```

```
[1] 5
```
***

```r
2^3  # to the power of
```

```
[1] 8
```

```r
2 * 5  # with or without spaces
```

```
[1] 10
```

```r
2*5
```

```
[1] 10
```
<small>Spaces can make it easier to read the code</small>

1.2 R as a calculator
========================================================


```r
10 %% 5 # modulo (remainder after division)
```

```
[1] 0
```

```r
10 %% 6
```

```
[1] 4
```
1.3 R as a calculator
========================================================

```r
10 %/% 5 # integer division (result of division to nearest integer)
```

```
[1] 2
```

```r
10 %/% 6
```

```
[1] 1
```

```r
18 %% 12 # e.g. conver 24h to 12h
```

```
[1] 6
```

1.4 R as a calculator
============================

Get help with arithmetic operators:
`? Arithmetic`

Or using the `help()` function:
`help(Arithmetic)`

Careful with the order of operators...

- `^`
- `%%  %/%`
- `*  /`
- `+  -`

See
`? Syntax`


1.5 R as a calculator
===============================


```r
10 + 10 / 2 - 1
```

```
[1] 14
```

```r
(10 + 10) /2 - 1
```

```
[1] 9
```

```r
8 - 4 * 4 + 2 
```

```
[1] -6
```


1.6 R as a calculator
===============================

If you're not sure about the order... use round brackets (parenthesis)

```r
10 + (10 / 2) - 1
```

```
[1] 14
```

```r
10 + 10 / 2 - 1
```

```
[1] 14
```

2.
======================
type: sub-section
<br><br>
# Vectors, variables and other objects

2.1 What is a vector?
=========================


### Ordered collection of data  
For example, it could be a single column of a table 
<br>

```
[1] 0.79 0.63 0.53 0.46 0.46 0.29 0.80 0.74
```
Or a single value
<br>

```
[1] 3.141593
```

2.2 Types of vector
=================
<br>
- Numeric
 - continuous
 - integer

- Character (string)
 - text

- Logical
TRUE or FALSE

2.3 Vector with a single numeric item
========================


```r
a <- 1
b <- 5
x <- 100
# note the assign operator: <-  
# this means assign value on the right hand side to the variable/object name on the left
```

```r
a
```

```
[1] 1
```

```r
x
```

```
[1] 100
```
***
<small> **Naming objects...**
- Don't use *reserved* words:
`? reserved`
- Best not to use function names
- Don't begin with a number
- Sensitive to upper and lower case
- Don't use spaces
- Don't use symbols such as £ $ % ^ & * ( ) # ? < > / | \ [ ] { }
</small>

2.4 Operations with vectors 1
======================


```r
a + b
```

```
[1] 6
```

```r
(a + b) * x
```

```
[1] 600
```

```r
a / x
```

```
[1] 0.01
```
***

```r
sum(a, b)
```

```
[1] 6
```

```r
sqrt(x)
```

```
[1] 10
```

```r
log10(x)
```

```
[1] 2
```

2.5 Character vectors
=======================


```r
a <- "Hello"  # text ALWAYS in "quotes"
b <- "World"
sp1 <- "Phytotoma raimondii"
```

```r
a  # the object name is not in quotes
```

```
[1] "Hello"
```

```r
sp1
```

```
[1] "Phytotoma raimondii"
```

2.6 Text operations
=======================


```r
# Print to the console
print(a)
```

```
[1] "Hello"
```

```r
# Concatenate text
paste(a, b)
```

```
[1] "Hello World"
```

```r
# and with a separator
paste(a, sp1, sep=", ")
```

```
[1] "Hello, Phytotoma raimondii"
```

2.7 Vectors with more than one element
========================


```r
1:10 # generate a series in steps of 1
```

```
 [1]  1  2  3  4  5  6  7  8  9 10
```

```r
# Use the combine c() function and assign to a vector called v1
v1 <- c(1,2,3,4,5)
v2 <- 5:10
```

```r
v2
```

```
[1]  5  6  7  8  9 10
```
**Vectors only allow one data type (numeric, character, logical)**

2.8 Vector operations 2
==================================


```r
v1 * 3  # cf. to excel
```

```
[1]  3  6  9 12 15
```

```r
v1 + v2
```

```
[1]  6  8 10 12 14 11
```

```r
mean(v1)
```

```
[1] 3
```

```r
sum(v1)/length(v1)
```

```
[1] 3
```

2.9 Getting a value within a vector with indices []
============================


```r
v1 <- c(1,2,3,4,5); v1
```

```
[1] 1 2 3 4 5
```

```r
v1[3]  # get the 3rd element of the vector v1
```

```
[1] 3
```

```r
v1[2:4] # get elements 2 to 4 of v1
```

```
[1] 2 3 4
```

```r
v1[-1] # get all elements of v1 except for the first
```

```
[1] 2 3 4 5
```

2.9 Series of letters
========================


```r
s1 <- c("a", "b", "c", "d", "e") # note the c() function
s2 <- letters[1:5]  # letters is a ready-made object inside R
s3 <- LETTERS[1:5]  # see ? Constants
```

```r
s2
```

```
[1] "a" "b" "c" "d" "e"
```

```r
s3
```

```
[1] "A" "B" "C" "D" "E"
```


2.10 Getting an elements in a character vector
========================


```r
s1[1]
```

```
[1] "a"
```

```r
s1[4:5]
```

```
[1] "d" "e"
```

```r
sp1[1] # Is this what you expected?
```

```
[1] "Phytotoma raimondii"
```

```r
s1[-2]
```

```
[1] "a" "c" "d" "e"
```

2.11 The <<data frame>>  a 'table' in R
==========================
We can think of a data frame as a collection of vectors of the same length. For example, let's combine two vectors into a data frame

```r
# Think back to this vector:
v1
```

```
[1] 1 2 3 4 5
```

```r
# How long is it?
length(v1)
```

```
[1] 5
```

```r
# What type are they?
class(v1)
```

```
[1] "numeric"
```

2.11 The <<data frame>>  a 'table' in R
======================================

```r
# Think back to this vector:
s1
```

```
[1] "a" "b" "c" "d" "e"
```

```r
# How long is it?
length(s1)
```

```
[1] 5
```

```r
# What type are they?
class(s1)
```

```
[1] "character"
```

2.13 Data frame
==========================
  
- a basic object to store data in  
  

```r
df1 <- data.frame(Type = s1, Value = v1)
df1
```

```
  Type Value
1    a     1
2    b     2
3    c     3
4    d     4
5    e     5
```
**Vectors combined into a data frame must be of same length**

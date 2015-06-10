### Simple graphics ###


## Boxplots ##

boxplot(iris$Sepal.Length ~ iris$Species)

## There are many graphical arguments that can go inside boxplot(), or plot()

boxplot(iris$Sepal.Length ~ iris$Species, ylab = "Sepal length (cm)")

# eg xlab, ylab for x and y axis labels. col for colour - as colour names. Look them up on the web. Use a c() to combine into a vector. eg c("red", "green", "blue)
## use main for a title


# Scatter plots ###
plot(iris$Petal.Length, iris$Sepal.Length)

## Add x and y labels.

## Colour by species
plot(iris$Petal.Length, iris$Sepal.Length, col = c("blue", "green", "red") [iris$Species])


## Change symbols with pch
?pch

## Set x and y axis limits with xlim, ylim . Use c() for c(min, max) vector. 
plot(iris$Petal.Length, iris$Sepal.Length, ylim = c(0, 20))
plot(iris$Petal.Length, iris$Sepal.Length, ylim = c(5, 10))
plot(iris$Petal.Length, iris$Sepal.Length)



## Line graphs

## Look at the data set
?pressure

head(pressure)

plot(pressure[,1], pressure[,2])

## Change the type with "type" argument. Try different lines, points, combinations, etc.
?plot


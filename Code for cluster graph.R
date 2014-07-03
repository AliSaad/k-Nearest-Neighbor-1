#Michael Frasco
#7/3/2014

#Using ggplot2 to examine Fisher's iris data
data(iris)

#By examining the first six observations, we can see that it will be difficult
#to graphically display this dataset. Each flower has four numerical features
#and one categorical feature.
head(iris)

#To start, we can use ggplot2's qplot() function and pass in two of the
#variables in the iris data set.
qplot(Sepal.Length, Petal.Length, data = iris)

#We can improve this plot by passing in the color argument and setting
#it equal to the Species variable in the data set.
#This will allow us to start to identify three clusters of flower species.
qplot(Sepal.Length, Petal.Length, data = iris, color = Species)

#While it appears from this graph that there are three clusters of flowers,
#remember that we have not plotted two of the four variables in the data set.
#We can create a new plot for the other two variables to see if the
#distinction between the species is constant.
qplot(Sepal.Width, Petal.Width, data = iris, color = Species)
#Once again, setosa flowers are very isolates, but there is more overlap
#between versicolor and virginica in this plot. Let's examine others.

qplot(Petal.Length, Petal.Width, data = iris, color = Species)
qplot(Sepal.Length, Sepal.Width, data = iris, color = Species)
#From this last graph, there is significant overlap between versicolor and
#virginica. However, graphing all of the different combinations is
#not effective as I would hope. Are there better solutions?

#In order to examine three of the four variables at once, we can pass the
#size argument into the qplot() function.
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width)
#Here, we can see that the virginica flowers seem to have wider petals
#on average than the versicolor flowers. However, we might also want
#numerical data to help us understand this graph.

tapply(iris$Petal.Width, iris$Species, mean)
#From this table, we recognize that the mean Petal.Widths are different
tapply(iris$Petal.Width, iris$Species, max)
tapply(iris$Petal.Width, iris$Species, min)
#However, from these last two tables, we can see that there is overlap
#between the widest versicolor and the smallest virginica

#We suspect that the mean petal widths for large versicolor flowers
#(as measured by the petal length) will be similar to the mean petal
#widths for small virginica flowers
condition1 = iris$Petal.Length >= 4.5 & iris$Species == 'versicolor'
mean(iris[condition1,4])
condition2 = iris$Petal.Length <= 5.5 & iris$Species == 'virginica'
mean(iris[condition2,4])

#Back to the graphs, in order to reduce the effects of overlap, we can
#use the alpha argument
qplot(Sepal.Length, Petal.Length, data = iris, color = Species,
      size = Petal.Width, alpha = I(0.7), xlab = "Sepal Length",
      ylab = "Petal Length", main = "Visualization of Clusters in 
      Fisher's Iris Data")

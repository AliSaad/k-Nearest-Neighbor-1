#Michael Frasco
#6/26/2014

#Practicing k-Nearest-Neighbors machine learning algorithm
#I will use Edgar Allen's iris data, in the hope of classifying
#flowers of different species according to it's measurements.

#This command loads the built in dataset from R.
data(iris)

#From this summary, we can see that there are 5 variables.
#We will use the first four features to classify the species.
summary(iris)

#Number of observations
m = nrow(iris)
#Number of features
n = ncol(iris) - 1

#Now we will split the iris data into the predictors and the label
x = iris[,1:n]
y = iris[,ncol(iris)]
#I notice that the species column in iris data set is a Factor.
#This was causing me difficulty, so I convert it to characters.
y_character = as.character(y)

#We need to normalize all features with mean 0 and variance 1,
#so that each feature contributes equally to the distance metric.
#The argument for feature normalization is not that strong here,
#as all features are within 1-10.
for (i in 1:n) {
  x[,i] = (x[,i] - mean(x[,i])) / sd(x[,i])
}

#The new point must be four dimensional, with the same features
#as those in the iris dataset.
test_point = rnorm(4)

#I also need to create a distance vector to record the distance
#from the test point to each point in the data set.
distances = rep(0,m)

#Now I will set my value of k.
k_value = 3

knn_function = function(test_point, k_value){

#Need to create the distance metric, which will be used to measure
#the distance of a new point from every other point in the dataset.
for (i in 1:m){
  for (j in 1:n){
    distances[i] = distances[i] + (test_point[j] - x[i,j])^2
  }
}

#In order to know where the k smallest distances are I will use
#the order function on the distances vector, which returns a vector
#of the same length, where the first element indicates the position
#of the smallest element in the original matrix.
orders = order(distances)

#But this works, and this is what matters.
k_lowest_labels = rep(0,k_value)
for (i in 1:k_value) {
  k_lowest_labels[i] = y_character[orders[i]]
}

#Now I will create a scoring algorithm that counts each species in
#k_lowest_labels, weighting each by the inverse distance. I need
#to create my score variables
scores = rep(0,k_value)
for (i in 1:k_value) {
  if (k_lowest_labels[i] == "setosa") {
    scores[1] = scores[1] + 1 * distances[orders[i]]
  } else if (k_lowest_labels[i] == "versicolor") {
    scores[2] = scores[2] + 1 * distances[orders[i]]
  } else scores[3] = scores[3] + 1 * distances[orders[i]]
}

winner = max(scores)
if (winner == scores[1]){
  test_point[ncol(iris)] = 'setosa'
  return(test_point)
  print('The new flower is probably setosa')
} else if (winner == scores[2]){
  test_point[ncol(iris)] = 'versicolor'
  return(test_point)
  print('The new flower is probably versicolor')
} else {
  test_point[ncol(iris)] = 'virginica'
  return(test_point)
  print('The new flower is probably virginica')
}

}
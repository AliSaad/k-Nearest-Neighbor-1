#Michael Frasco
#6/26/2014

#Practicing k-Nearest-Neighbors machine learning algorithm
#I will use Edgar Allen's iris data, in the hope of classifying
#flowers of different species according to it's measurements.

#This command loads the built in dataset from R.
data(iris)

#Now we will split the iris data into the predictors and the label
x = iris[,1:ncol(iris)-1]
y = iris[,ncol(iris)]
#I notice that the species column in iris data set is a Factor.
#This was causing me difficulty, so I convert it to characters.
y_character = as.character(y)

#We need to normalize all features with mean 0 and variance 1,
#so that each feature contributes equally to the distance metric.
#The argument for feature normalization is not that strong here,
#as all features are within 1-10.
for (i in 1:ncol(x)) {
  x[,i] = (x[,i] - mean(x[,i])) / sd(x[,i])
}

#The new point must be four dimensional, with the same features
#as those in the iris dataset.
test_point = rnorm(4)

#Now I will set my value of k. It can change with the function
k_value = 3

#This is a stand alone function. It should be able to handle
#any data set. It returns the classification for the test point.
knn_function = function(test_point, feature_set, labels, k_value){

  #Number of observations
  m = nrow(feature_set)
  #Number of features
  n = ncol(feature_set)
  
  #I need to create a distance vector to record the distance
  #from the test point to each point in the data set.
  distances = rep(0,m)
  
  #Need to create the distance metric, which will be used to measure
  #the distance of a new point from every other point in the dataset.
  for (i in 1:m){
    for (j in 1:n){
      distances[i] = distances[i] + (test_point[j] - feature_set[i,j])^2
    }
  }

  #In order to know where the k smallest distances are I will use
  #the order function on the distances vector, which returns a vector
  #of the same length, where the first element indicates the position
  #of the smallest element in the original matrix.
  orders = order(distances)

  #This piece of code gives me the class of the k-nearest-neighbors
  #for the test point. I will use this to determine the test point's
  #classification.
  k_lowest_labels = rep(0,k_value)
  for (i in 1:k_value) {
    k_lowest_labels[i] = labels[orders[i]]
  }

  #Now I will create a scoring algorithm that counts each species in
  #k_lowest_labels, weighting each by the inverse distance. I need
  #to create my score variables
  scores = rep(0,k_value)
  for (i in 1:k_value) {
    if (k_lowest_labels[i] == "setosa") {
      scores[1] = scores[1] + 1 * (1 / distances[orders[i]])
    } else if (k_lowest_labels[i] == "versicolor") {
      scores[2] = scores[2] + 1 * (1 / distances[orders[i]])
    } else scores[3] = scores[3] + 1 * (1 / distances[orders[i]])
  }

  #This pice of code determines the classification and
  #returns the classification as a string.
  winner = max(scores)
  if (winner == scores[1]){
    test_point[n+1] = 'setosa'
    return(test_point[n+1])
  } else if (winner == scores[2]){
    test_point[n+1] = 'versicolor'
    return(test_point[n+1])
  } else {
    test_point[n+1] = 'virginica'
    return(test_point[n+1])
  }

}

#In order to test the accuracy of the function I created above, I want to
#classify each point in the iris data set. However, in order to do this,
#I first need to remove the point I am testing from the data set.
#Then I will run the test, obtain a classification. And repeat the process.
#At the end, I hope that all (or most) classifications are correct.

#I will create a function that tests the accuracy of this algorithm.
accuracy_function = function(feature_set, labels, k_value){
  x_tester = feature_set
  y_tester = labels
  results = character()
  accuracy = numeric()

  #putting the negative sign in from of the index removes that observation
  #from the dataset. This is important to do before we test each point.
  for (i in 1:nrow(feature_set)) {
    x_tester = x_tester[-i,]
    y_tester = y_tester[-i]
    results[i] = knn_function(feature_set[i,], x_tester, y_tester, k_value)
  }
  
  #This will create a vector that records whether each classification
  #was correct or not.
  for (i in 1:nrow(feature_set)) {
    if (results[i] == labels[i]){
      accuracy[i] = 1
    } else{
      accuracy[i] = 0
    }
  }

  #I want to know the proportion of classifications that the algorithm
  #got correct, so I return the following.
  return(sum(accuracy)/nrow(feature_set))
}

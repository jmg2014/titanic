######################################################################################
# Approach using a CART (Classification and Regression Tree) algorithm
######################################################################################
# Load R packages  
library(rpart)    #tree
library(rattle)   #display tree

#Load train data
train <- read.csv(file="train.csv",head=TRUE,sep=",",stringsAsFactors = FALSE)

#Load test data
test <- read.csv(file="test.csv",head=TRUE,sep=",",stringsAsFactors = FALSE)



# Build the decision tree
my_tree<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")


# A fancy plot from rattle
fancyRpartPlot(my_tree)


# Make a prediction using the test set
my_prediction <- predict(my_tree, test, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains my predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# The data frame should has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "cart.csv", row.names = FALSE)

#result ->0.78469


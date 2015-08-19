######################################################################################
# Approach using a classification tree (rpart library) using cross validation
######################################################################################
# Load R packages  
library(rpart)    #tree
library(rattle)   #display tree

#Load train data
train <- read.csv(file="train.csv",head=TRUE,sep=",",stringsAsFactors = FALSE)

#Load test data
test <- read.csv(file="test.csv",head=TRUE,sep=",",stringsAsFactors = FALSE)


# Install cross-validation packages
library(caret)
library(e1071)
set.seed(1)

train$Dr<-grepl('Dr.', train$Name,  fixed=TRUE)
train$Master<-grepl('Master', train$Name,  fixed=TRUE)
train$Don<-grepl('Don', train$Name,  fixed=TRUE)
train$Sir<-grepl('Sir', train$Name,  fixed=TRUE)


test$Dr<-grepl('Dr.', test$Name,  fixed=TRUE)
test$Master<-grepl('Master', test$Name,  fixed=TRUE)
test$Don<-grepl('Don', test$Name,  fixed=TRUE)
test$Sir<-grepl('Sir', test$Name,  fixed=TRUE)



# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +
      Dr+Master+Don+Sir, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create the new CART model using the cp from the previous step
tree.CV = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked+
                Dr+Master+Don+Sir, data = train, method="class", cp = 0.02)

# A fancy plot from rattle
fancyRpartPlot(tree.CV)

# Make a prediction using the test set
my_prediction <- predict(tree.CV, test, type = "class")


# Create a data frame with two columns: PassengerId & Survived. Survived contains my predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)


# The data frame should has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "tree_decission_cv02.csv", row.names = FALSE)
##result ->0.79904




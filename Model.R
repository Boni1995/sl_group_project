
matches_foreign = read.csv('matches_foreign.csv')

library(caret)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)

#Treating outcome as a categorical value
matches_foreign$outcome = as.factor(matches_foreign$outcome)

#Training and Testing Data split
set.seed(123)  
partition = createDataPartition(matches_foreign$outcome, p = 0.8, list = FALSE)
X = matches_foreign[partition, ]  
y = matches_foreign[-partition, ] 

#Decision Tree
tree = rpart(outcome ~ home_team + away_team + home_foreign + away_foreign, data = X)

prediction = predict(tree, y, type='class')

#Evaluation
ConfusionMatrix(prediction, y$outcome)
printcp(tree)

#RandomForest
rf = randomForest(outcome ~ home_team + away_team + home_foreign + away_foreign, data = X, ntry=4, ntree=2001, importance=TRUE)


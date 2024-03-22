
matches_foreign = read.csv('matches_foreign_final.csv')
matches_foreign = matches_foreign[,-1]
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

#Decision Tree without Foreign
tree = rpart(outcome ~ home_rank + away_rank + home_points + away_points, data = X)

prediction = predict(tree, y, type='class')

confusionMatrix(prediction, y$outcome)
printcp(tree)

#Decision Tree with Foreign
tree = rpart(outcome ~ home_rank + away_rank + home_points + away_points + home_foreign + away_foreign, data = X)

prediction = predict(tree, y, type='class')

confusionMatrix(prediction, y$outcome)
printcp(tree)

#RandomForest with Foreign
rf = randomForest(outcome ~ home_rank + away_rank + home_points + away_points + home_foreign + away_foreign, data = X, ntry=4, ntree=2001, importance=TRUE)
predictions <- predict(rf, newdata = y)

confusionMatrix(predictions, y$outcome)

#RandomForest without Foreign
rf = randomForest(outcome ~ home_rank + away_rank + home_points + away_points, data = X, ntry=4, ntree=2001, importance=TRUE)
predictions <- predict(rf, newdata = y)

confusionMatrix(predictions, y$outcome)

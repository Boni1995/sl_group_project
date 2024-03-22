
matches_foreign = read.csv('matches_foreign_final.csv')
matches_foreign = matches_foreign[!(matches_foreign$outcome == 't'), ]
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
tree = rpart(outcome ~ home_rank + away_rank + home_points + away_rank + away_points, data = X)

prediction = predict(tree, y, type='class')

#Evaluation
confusionMatrix(prediction, y$outcome)
printcp(tree)

prp(tree, extra=1, cex=0.8, varlen = 0)

#Treating outcome as a categorical value
matches_foreign$outcome = as.factor(matches_foreign$outcome)

#Training and Testing Data split
set.seed(123)  
partition = createDataPartition(matches_foreign$outcome, p = 0.8, list = FALSE)
X = matches_foreign[partition, ]  
y = matches_foreign[-partition, ] 

#Decision Tree without Foreign
tree = rpart(outcome ~ home_rank + away_rank + home_points + away_rank + away_points, data = X)
prediction = predict(tree, y, type='class')

confusionMatrix(prediction, y$outcome)
printcp(tree)

#Decision Tree with Foreign
tree_with = rpart(outcome ~ home_rank + away_rank + home_points + away_rank + away_points + home_foreign + away_foreign, data = X)
predictions = predict(tree, y, type='class')

confusionMatrix(predictions, y$outcome)
printcp(tree_with)

prp(tree, extra=1, cex=0.8, varlen = 0)

#RandomForest without Foreign
library(randomForest)
rf = randomForest(outcome ~ home_rank + away_rank + home_points + away_points, data = X, ntry=4, ntree=2001, importance=TRUE)
predictions <- predict(rf, newdata = y)
confusionMatrix(predictions, y$outcome)
varImpPlot(rf)
#RandomForest with Foreign
library(randomForest)
rf = randomForest(outcome ~ home_rank + away_rank + home_points + away_points + home_foreign + away_foreign, data = X, ntry=4, ntree=2001, importance=TRUE)
predictions <- predict(rf, newdata = y)
confusionMatrix(predictions, y$outcome)
varImpPlot(rf)
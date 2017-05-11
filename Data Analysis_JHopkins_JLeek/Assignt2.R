setwd("~/Coursera/Coursera_Data Analysis/Assignment 2")
load('samsungData.rda')
names(samsungData)
unique(samsungData$subject)
dim(samsungData)
library("glm2", lib.loc="C:/Users/Valerie/Documents/R/win-library/3.0")
fix(samsungData)
table(samsungData$activity)
samsungData$activity <- as.factor(samsungData$activity)
samsungData$activity.num <- as.numeric(as.factor(samsungData$activity))
names(samsungData) <- sapply(names(samsungData), function(x){gsub("[()]", "", gsub("[,-]", ".", x))})
fix(samsungData)
samsungData.dedup <- data.frame(samsungData)
test.set <- samsungData.dedup[samsungData.dedup$subject %in% c(27,28,29,30),]
train.set <- samsungData.dedup[!(samsungData.dedup$subject %in% c(27,28,29,30)),]
validation.set <- samsungData.dedup[samsungData.dedup$subject %in% c(22,23,25,26),]
subtrain.set <- samsungData.dedup[!(samsungData.dedup$subject %in% c(22,23,25,26,27,28,29,30)),]
fix(test.set)
fix(train.set)
fix(validation.set)
fix(subtrain.set)
plot(table(samsungData.dedup$subject, samsungData.dedup$activity), main="Amount of activities per Subject", xlab="Subject IDs", ylab="Activities")
accuracy <- function(model, outcome, dataset, predict_type="class") {
    + confusion.matrix <- as.matrix(table(outcome, predictdimdel, dataset, type=predict_type)))
    +     sum(diag(confusion.matrix)/sum(confusion.matrix))}
install.packages("tree")
library("tree", lib.loc="C:/Users/Valerie/Documents/R/win-library/3.0")
activity.tree <- tree(as.factor(train.set$activity)~., data=train.set)
summary(activity.tree)
plot(activity.tree); text(activity.tree)
train.svd <- train.set[,-c(562:564)]
fix(train.svd)
fix(train.set)
for (c in names(train.svd)) {train.svd <- train.svd[!is.na(train.svd[[c]]),]}
svd1 = svd(scale(train.svd))
par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
svd1Pct <- svd1$d^2/sum(svd1$d^2)
plot(svd1Pct, xlab="Column", ylab="Percent of variance explained", pch=19)
svd1$d[svd1$d >= 200]
maxContrib <- which.max(svd1$v[,2])
maxContrib
second <- svd1$v[,2]
second <- second[! second %in% c(second[maxContrib])]
maxContrib2 <- which.max(second)
maxContrib2 
third <- second[! second %in% c(second[maxContrib2])]
maxContrib3 <- which.max(third)
maxContrib3 
fourth <- third[! third %in% c(third[maxContrib3])]
maxContrib4 <- which.max(fourth)
maxContrib4 
fifth <- fourth[! fourth %in% c(fourth[maxContrib4])]
maxContrib5 <- which.max(fifth)
maxContrib5 
maxContribs <- c(maxContrib, maxContrib2, maxContrib3, maxContrib4, maxContrib5)
maxContribs
names(train.svd)[maxContribs]
maxContribs.names <- sapply(maxContribs, function(x) names(train.set)[x])
all.maxContribs <- apply(svd1$v, 2, which.max)
length(all.maxContribs)
length(unique(all.maxContribs))
dim(train.set)
dim(test.set)
activity.rf <- randomForest(as.factor(train.set$activity)~., data=train.set)
summary(activity.rf)
accuracy(activity.rf, train.set$activity, train.set)
## start over
load("samsungData.rda")
fix(samsungData)
plot(table(samsungData$subject, samsungData$activity), main="Amount of activities per Subject", xlab="Subject IDs", ylab="Activities")
## transform into a dataframe: takes care of all improper labeling in the variable names
samsungData.dedup <- data.frame(samsungData)
## create a training set, validation set and a test set
samsungData.train<-samsungData.dedup[samsungData.dedup$subject %in% c(1,3,5,6,7,8,11,14,15,16,17),]
samsungData.validation<-samsungData.dedup[samsungData.dedup$subject %in% c(19,21,22,23,25),]
samsungData.test<-samsungData.dedup[samsungData.dedup$subject %in% c(26,27,28,29,30),]
## create a function that will calculate the accuracy of each model
## accuracy is defined as:
accuracy <- function(model, outcome, dataset, predict_type="class") 
  + {
    +     confusion.matrix <- as.matrix(table(outcome, predict(model, dataset, type=predict_type)))
    +     sum(diag(confusion.matrix)/sum(confusion.matrix))
    + }
samsungData.train<-samsungData.dedup[samsungData.dedup$subject %in% c(1,3,5,6),]
samsungData.test<-samsungData.dedup[samsungData.dedup$subject %in% c(27,28,29,30),]
activity.glm <- glm(as.factor(samsungData.train$activity)~., family="binomial", da
                    a=samsungData.train)
accuracy(activity.glm, samsungData.train$activity, samsungData.train, "response")
activity.tree <- tree(as.factor(samsungData.train$activity)~., data=samsungData.train)
summary(activity.tree)
plot(activity.tree); text(activity.tree)
accuracy(activity.tree, samsungData.train$activity, samsungData.train)
activity.cvtree <- cv.tree(activity.tree)
plot(activity.cvtree$size, activity.cvtree$dev, pch=19, col="blue", xlab="size", ylab="deviance")
activity.cvtree$size; activity.cvtree$dev
activity.pruneTree <- prune.tree(activity.tree, best=9)
summary(activity.pruneTree)
accuracy(activity.pruneTree, samsungData.test$activity, samsungData.test)
activity.rf <- randomForest(as.factor(samsungData.train$activity)~., data=samsungData.train)
summary(activity.rf)
accuracy(activity.rf, samsungData.train$activity, samsungData.train)
accuracy(activity.rf, samsungData.test$activity, samsungData.test)
par(mfrow=c(1,1))
varImpPlot(activity.rf, pch=19, col="blue", main="Random forest prediction model variables importance")
install.packages("caret")
activity.tree.cm <- confusionMatrix(samsungData.test$activity, predict(activity.tree, samsungData.test, type="class"))
library("caret")
install.packages("e1071")
activity.tree.cm <- confusionMatrix(samsungData.test$activity, predict(activity.tree, samsungData.test, type="class"))
activity.rf.cm <- confusionMatrix(samsungData.test$activity, predict(activity.rf, samsungData.test, type="class"))
activity.tree.cm    
activity.rf.cm
activity.tree.cm$table
activity.rf.cm$table
activity.tree.cm$byClass[,1:2]
activity.rf.cm$byClass[,1:2]



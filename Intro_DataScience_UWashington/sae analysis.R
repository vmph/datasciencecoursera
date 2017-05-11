sea<- read.csv("seaflow_21min.csv")
dim(sea)
names(sea)
str(sea)
summary(sea)
library(caret)
intrain<-createDataPartition(y=sea$file_id, p=0.5,list=FALSE)
seatrain<-sea[intrain,]
seatest<-sea[-intrain,]
dim(seatrain)
summary(seatrain)
dim(seatest)
library(ggplot2)
# chl_small against pe by pop
qplot(pe, chl_small, color=pop, data=seatrain)
# pe agains chl_small by pop
qplot(chl_small, pe, color=pop, data=seatrain)
library(rpart)
#training an rpart tree as a 
#function of the sensor measurements: fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small
# here we use the full tree by default
mod<- rpart(pop ~ fsc_small + fsc_perp + fsc_big + pe+chl_big + chl_small, data=seatrain)
print(mod)
summary(mod)
#plotting the rpart Tree and annotating with text
plot(mod, uniform=TRUE, main="Classification for seaFloor data")
text(mod, use.n=TRUE, all=TRUE, cex=.8)
library(partykit)
moda<-as.party(mod)
plot(moda)
#using rpart to predict new values
seapred<- predict(mod, seatest, type="class")
# comparing predicted values to actual values in Test
confusionMatrix(seapred, seatest$pop)
# what is that ?
table(seatrain$pop, predict(mod, type="class"))
# Try random Forest
mod1<-randomForest(pop ~ fsc_small + fsc_perp + fsc_big + pe+chl_big + chl_small, data=seatrain)
print(mod1)
summary(mod1)
seapred1<- predict(mod1, seatest, type="class")
table(seatrain$pop, predict(mod1, type="class"))
confusionMatrix(seapred1, seatest$pop)
#The function importance(model) prints the mean decrease in gini importance for each variable. 
#The higher the number, the more the gini impurity score decreases by branching on this variable, 
#indicating that the variable is more important.
importance(mod1)
#training a svm model
mod2<-svm(pop ~ fsc_small + fsc_perp + fsc_big + pe+chl_big + chl_small, data=seatrain)
print(mod2)
# predictin test data using svm model
seapred2<- predict(mod2, seatest, type="class")
table(seatrain$pop, predict(mod2, type="class"))
confusionMatrix(seapred2, seatest$pop)
# make copy of training data
seatrain11<-seatrain
fix(seatrain11)
summary(seatrain11)
#plot the various predictors agains file_id
plot(seatrain11$file_id, seatrain11$fsc_big
plot(seatrain11$file_id, seatrain11$chl_big)    
plot(seatrain11$file_id, seatrain11$fsc_perp)     
plot(seatrain11$file_id, seatrain11$pe)
plot(seatrain11$file_id, seatrain11$chl_small)
plot(seatrain11$file_id, seatrain11$fsc_small)
#remove file_id==208 from training
seatrain22<-subset(seatrain, subset=(file_id!="208"))
fix(seatrain22)
seatest22<- subset(seatest, subset=(file_id!="208"))
dim(seatrain22)   
dim(seatest22)
#retraining svm on train data without id=208
modsvm2<- svm(pop ~ fsc_small + fsc_perp + fsc_big + pe+chl_big + chl_small, data=seatrain2
print(modsvm2)
#predicting new values based on modesvm2
seapredsvm2<- predict(modsvm2, seatest22, type="class")
confusionMatrix(seapredsvm2, seatest22$pop)
#count how many observations per class label
synechocount<- subset(sea, select=c(pop), subset=pop=="synecho")
dim(synechocount)
cryptocount<- subset(sea, select=c(pop), subset=pop=="crypto")
dim(cryptocount)
nanocount<- subset(sea, select=c(pop), subset=pop=="nano")
dim(nanocount)
picocount<- subset(sea, select=c(pop), subset=pop=="pico")
dim(picocount)
ultracount<- subset(sea, select=c(pop), subset=pop=="ultra")
dim(ultracount)




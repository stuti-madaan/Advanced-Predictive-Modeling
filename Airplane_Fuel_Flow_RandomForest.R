library(randomForest)

train1=  read.csv("E:/data/consolidated_1.csv")
dim(train1)

train_index = sample(1:1070248, 100000, replace = FALSE, prob = NULL)

test_index = sample(c(1:1070248)[-train_index], 100000, replace = FALSE, prob = NULL)

train_local = train1[train_index,]
test_local = train1[test_index,]

RF_trainmini = randomForest(FF~.,data=train_local ,mtry=78, ntree =300)
yhat.rf = predict(RF_trainmini ,newdata =test_local)
sqrt(mean((yhat.rf-test_local$FF)^2))

varImpPlot(RF_trainmini)
sort(importance(RF_trainmini))

#-------Phase 1 -------------

ph_1=  read.csv("E:/data/ph_1.csv")
dim(ph_1)

set.seed(123)
train_index = sample(1:469087, 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:469087)[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_1[train_index,]
test_local = ph_1[test_index,]

library(randomForest)
set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

importance(RF_trainmini)


#-------Phase 2 -------------

ph_2=  read.csv("E:/data/ph_2.csv")
dim(ph_2)

set.seed(123)
train_index = sample(c(1:dim(ph_2)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_2)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_2[train_index,]
test_local = ph_2[test_index,]


set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

importance(RF_trainmini)




#-------Phase 3 -------------

ph_3=  read.csv("E:/data/ph_3.csv")
dim(ph_3)

set.seed(123)
train_index = sample(c(1:dim(ph_3)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_3)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_3[train_index,]
test_local = ph_3[test_index,]

set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)

yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

importance(RF_trainmini)


#-------Phase 4 -------------

ph_4=  read.csv("E:/data/TRAIN_1/ph_4.csv")
dim(ph_4)

set.seed(123)
train_index = sample(c(1:dim(ph_4)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_4)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_4[train_index,]
test_local = ph_4[test_index,]

summary(train_local)

set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

varImpPlot(RF_trainmini)
save(RF_trainmini, file = "E:/data/phase_4.rda")




#-------Phase 5 -------------

ph_5=  read.csv("E:/data/TRAIN_1/ph_5.csv")
dim(ph_5)

set.seed(123)
train_index = sample(c(1:dim(ph_5)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_5)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_5[train_index,]
test_local = ph_5[test_index,]

summary(train_local)

set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

varImpPlot(RF_trainmini)
save(RF_trainmini, file = "E:/data/phase_5.rda")


#-------Phase 5 -------------

ph_5=  read.csv("E:/data/TRAIN_1/ph_5.csv")
dim(ph_5)

set.seed(123)
train_index = sample(c(1:dim(ph_5)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_5)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_5[train_index,]
test_local = ph_5[test_index,]

summary(train_local)

set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

varImpPlot(RF_trainmini)
save(RF_trainmini, file = "E:/data/phase_5.rda")


#-------Phase 5 -------------

ph_7=  read.csv("E:/data/TRAIN_1/ph_7.csv")
dim(ph_7)

set.seed(123)
train_index = sample(c(1:dim(ph_7)[1]), 60000, replace = FALSE, prob = NULL)
set.seed(123)
test_index = sample(c(1:dim(ph_7)[1])[-train_index], 60000, replace = FALSE, prob = NULL)

train_local = ph_7[train_index,]
test_local = ph_7[test_index,]

summary(train_local)

set.seed(123)
RF_trainmini = randomForest(FF~.,data=train_local[,-c(1,2,3,4,5,6,7,8)] ,mtry=78, ntree =300)


yhat.rf = predict(RF_trainmini ,newdata =test_local[,-c(1,2,3,4,5,6,7,8)])
sqrt(mean((yhat.rf-test_local$FF)^2))

varImpPlot(RF_trainmini)
save(RF_trainmini, file = "E:/data/phase_7.rda")


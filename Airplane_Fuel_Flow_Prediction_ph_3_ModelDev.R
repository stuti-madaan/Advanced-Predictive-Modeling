# Code for Phase 3 of the flight: "Take-Off"
# xgboost : Phase 3

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(lattice)
library(ggplot2)
library(data.table)
library(Matrix)

ph_3=read.csv("E:/data/TRAIN_1/ph_4.csv")
colnames(ph_3)

library(Hmisc)
ph_3 = ph_3[,(names(ph_3) %nin% c("ACID","Flight_instance_ID","Year","Month","Day","Hour","Minute","Second"))]

colnames(ph_3)
ph_3_test = read.csv("E:/data/ph_4_test.csv")

dim(ph_3)
dim(ph_3_test)

set.seed(1)
head(ph_3[,'FF'])

set.seed(123)
train <- ph_3[sample(nrow(ph_3),20000,replace=FALSE),]
test <- ph_3_test[sample(nrow(ph_3_test),20000,replace=FALSE),colnames(ph_3) ]

ptm <- proc.time()

train= data.table(train) #convert train set to data table format
test= data.table(test) #convert test set to data table format
sparse.matrix.train= sparse.model.matrix(FF~.-1, data = train) #converts train set factors to columns
sparse.matrix.test=  sparse.model.matrix(FF~.-1, data = test)
output_vector =train[,FF]

set.seed(101)
xgb <- xgboost(data = sparse.matrix.train, 
               label = output_vector,
               eta = 0.3,
               max_depth = 25,
               nround=25,
               subsample = 0.6,
               colsample_bytree = 0.7,
               seed = 1,
               eval_metric = "rmse",
               objective = "reg:linear",
               nthread = 2
)

y_pred <- predict(xgb, sparse.matrix.test)
(mean((y_pred-test[,FF])**2))**0.5

print (proc.time() - ptm)



# Random Forest :Phase 3#
data= read.csv("E:/data/processd_data_phase_3_new.csv", header=TRUE)
dim(data)
data=data[6:6754,]
head(data)

indices=sample(6749,2000)

train=data[-indices,]
test=data[indices,]

###RANDOM FOREST ##

library(randomForest)
model1 = randomForest(FF ~ . , data=train)
pred = predict(model1,train)
sqrt(mean(pred-train$FF)**2)
pred_test = predict(model1,test)
sqrt(mean(pred_test-test$FF)**2)


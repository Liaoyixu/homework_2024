##HEADER --------------------------------------------------------------------
#
#Scrip name:   "homework-03"
#Purpose:      "create a machine learning model using caret-pakage"
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/4/2        
#
#SETUP ---------------------------------------------------------------------
################################################################################
#安装所需包
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost',
                   'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
#载入包
library(tidyverse)
library(caret)

#载入数据，选择使用‘mtcars’数据集
#查看数据结构
str(mtcars)
head(mtcars[,1:10])



################################################################################
#Data Preparation and Preprocessing数据预处理

#1、splitting for training and test datasets
set.seed(100)
#创建训练数据集
trainRowNumbers <- createDataPartition(mtcars$vs, p=0.8, list=FALSE)
trainData <- mtcars[trainRowNumbers,]
#创建测试数据集
testData <- mtcars[-trainRowNumbers,]
#建立模型中X和Y值
x = trainData[, 2:11]
y = trainData$vs

#2、check and impute missing data
#使用skimmed描述列，查看是否有缺失值
library(skimr)
skimmed <- skim(trainData)
skimmed   #查看数据分布状态
#使用preProcess计算缺失值（mtcars无缺失值）
#Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model   #无缺失值

#Use the imputation model to predict the values of missing data points
library(RANN)
trainData <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)   #无缺失值

#3、create One-Hot Encoding
#创建dummy variables
dummies_model <- dummyVars(vs ~ ., data=trainData)
#使用predict创建dummy ， vs不出现在trainData_mat中
trainData_mat <- predict(dummies_model, newdata = trainData)
#转换为ataframe
trainData <- data.frame(trainData_mat)
#查看新的数据集结构
str(trainData)

#4、convert numertic variables “0-1”
#使用range将数值变量转换为0和1之间
preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)
#y变量
trainData$vs <- y
apply(trainData[, 1:6], 2, FUN=function(x)
  {c('min'=min(x), 'max'=max(x))})  # 2表示按列操作应用指定的函数
str(trainData)



################################################################################
#1、回归树模型 (rpart)
library(rpart) 
library(rpart.plot)
#使用rpart包创建决策树
dt_tree <- rpart(vs ~ ., data=trainData, method = 'anova')
rpart.plot(dt_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# Variable importance变量重要性分析
library(caret)# 使用caret包评估变量的重要性
(varimp.dt_tree <- varImp(dt_tree))
# evaluate model using test dataset模型评估
testData$dt_tree.pred <- predict(dt_tree, newdata = testData)
head(testData[c("vs","dt_tree.pred")], n=nrow(testData))
Metrics::rmse(testData$vs,testData$dt_tree.pred) #计算均方根误差(RMSE)0.5773503



################################################################################
#2、随机森林回归
library(randomForest)
#创建随机森林模型：使用randomForest包，设置树的数量为1000
rf_tree <- randomForest(vs ~ .,data=trainData, 
                        proximity=TRUE,
                        ntree=1000)

# Variable importance变量重要性分析
(varimp.rf_tree <- caret::varImp(rf_tree))
# evaluate the model模型评估
testData$rf_tree.pred <- predict(rf_tree, newdata = testData)
Metrics::rmse(testData$vs, testData$rf_tree.pred) #计算均方根误差(RMSE)0.508112



################################################################################
#3、提升回归树
library(gbm)
#创建提升树模型：使用gbm包，设置树的数量为5000，学习率为0.01
gbm_tree <- gbm(vs ~ ., data = trainData,
                distribution = "gaussian",
                n.trees = 5000, shrinkage = 0.01,
                interaction.depth = 4,
                bag.fraction = 0.7,  #取70%
                n.minobsinnode = 5)   
# evaluate the model 评估模型
testData$gbm_tree.pred <- predict(gbm_tree, newdata = testData)
Metrics::rmse(testData$vs, testData$gbm_tree.pred) #计算均方根误差(RMSE)0.5660953



################################################################################
#4、使用caret包同时构建多个树模型
#查看可用的算法模型
modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames
modelLookup("rpart") #决策树模型
modelLookup("rf")  #随机森林
modelLookup("gbm") #提升回归树
#加载数据（测试数据集和训练数据集）
trainData
testData

#training a tree regression 训练树模型
#定义数据预处理步骤和重采样控制
fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds 每次采样五
                           repeats = 2)    # repeated ten times 重复采样两次
#训练模型
model_rpart <- train(vs ~ ., data = trainData, 
                     method = "rpart",
                     trControl = fitControl,
                     preProcess = c('scale', 'center'),
                     tuneLength = 5,# find an optimal cp based on its 5 values
                     metric="RMSE") 
# Predict on the test data进行预测
predictions_rpart <- predict(model_rpart, newdata = testData)
# 评估模型性能(计算RMSE)
Metrics::rmse(testData$vs, predictions_rpart) #0.5773503

#training a rf regression随机森林模型
model_rf <- train(vs ~ ., data = trainData, 
                  method = "rf", #方法改为随机森林
                  trControl = fitControl,
                  preProcess = c('scale', 'center'),
                  tuneLength = 5,
                  metric="RMSE") 
#进行预测
predictions_rf <- predict(model_rf, newdata = testData)
#评估模型性能（计算RMSE）
Metrics::rmse(testData$vs, predictions_rf) #0.5750501

#training a boosting regression提升树模型
model_gbm <- train(vs ~ ., data = trainData, 
                   method = "gbm",
                   trControl = fitControl,
                   preProcess = c('scale', 'center'),
                   tuneLength = 5,
                   metric="RMSE")  
#进行预测
predictions_gbm <- predict(model_gbm, newdata = testData)
#评估模型性能（计算RMSE）
Metrics::rmse(testData$vs, predictions_gbm) #计算错误



################################################################################
# Compare the models' performances for final picking 比较不同模型
models_compare <- resamples(list(TREE=model_rpart, 
                                 RF=model_rf))
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free"))
bwplot(models_compare, scales=scales)

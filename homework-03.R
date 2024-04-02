##HEADER --------------------------------------------------------------------
#
#Scrip name:   "homework-03"
#Purpose:      "create a machine learning model using caret-pakage"
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/4/2        
#
#SETUP ---------------------------------------------------------------------

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




#Data Preparation and Preprocessing

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




#Visualize The Importance of variables and RFE
featurePlot(x = trainData[,1:10], 
            y = as.factor(trainData[,11]), 
            plot = "box",  
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
#箱线图boxplot，用于展示每个特征的分布情况；cex =.7调整了标签文本的大小
featurePlot(x = trainData[, 1:10], 
            y = as.factor(trainData[,11]), 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
#设置密度图，用于展示数据分布的连续估计，通常用于了解数据的形状、中心趋势和变异

#for recursive feature elimination递归特征消除
set.seed(100)   #设置随机数种子，确保结果的可重复性
options(warn=-1)   #临时关闭R中的警告消息，使输出更清晰
subsets <- c(1:11)#定义了一系列的子集大小
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,      #重复交叉验证5次
                   verbose = FALSE)  #关闭详细输出
#rfFuncs用于特征选择的函数，rfFuncs通常指随机森林方法
lmProfile <- rfe(x=trainData[, 1:10], y=trainData$vs,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile  #The top 1 variables (out of 1):qsec




#Traning and Tuning the model

#查看可用算法
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames
modelLookup('earth')   #MARS算法在R中被命名为"earth"

set.seed(100)
#Train the model using randomForest and predict on the training data itself.
model_mars = train(vs ~ ., data=trainData, method='earth')
fitted <- predict(model_mars)
model_mars
plot(model_mars,main="Model Accuracies with MARS")

#computer variable importance
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")




#Evaluating the performance of the model

#Impute missing values 
testData2 <- predict(preProcess_missingdata_model, testData)  
#Create one-hot encodings (dummy variables)
testData3 <- predict(dummies_model, testData2)
#Transform the features to range between 0 and 1
testData4 <- predict(preProcess_range_model, testData3)
#View
head(testData4[, 1:10])
#Predict on testData
predicted <- predict(model_mars, testData4)
head(predicted)
# Compute the confusion matrix
confusionMatrix(reference = testData$vs, 
                data = predicted, 
                mode ='everything',
                positive='1')
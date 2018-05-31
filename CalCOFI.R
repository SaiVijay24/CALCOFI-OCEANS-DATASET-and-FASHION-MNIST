library(DataExplorer)
library(Hmisc)
library(dplyr)
library(grnn)
library(doParallel)
library(foreach)
library(doSNOW)
library(nnet)
library(NeuralNetTools)
library(RSNNS) 
library(caret) 
library(Metrics) 

raw<-read.csv("ocean_2016.csv",header=T,na.strings=c(""," ","NA"))

plot_str(raw)
#Some analysis
par(mfrow=c(2,3))
plot(raw$R_Depth, raw$R_TEMP, xlab = "Depth", ylab = "Temperature")
plot(raw$R_SALINITY, raw$R_TEMP, xlab = "Salinity", ylab = "Temperature")
plot(raw$R_O2Sat, raw$R_TEMP, xlab = "Oxygen Saturation", ylab = "Temperature")
plot(raw$R_SIGMA, raw$R_TEMP, xlab = "Density", ylab = "Temperature")
plot(raw$R_PRES, raw$R_TEMP, xlab = "Pressure", ylab = "Temperature")
dev.off()

plot_missing(raw)
jpeg('MissingData_plot.jpeg', width = 1000, height = 1000, quality = 100) 
plot_missing(raw)
dev.off()

#BtlNum and R_SAMP will not have any effect on target temperature. So missing values in these columns are filled as 0.
for(i in c(12,67))
  for(j in 1:nrow(raw))
    if(is.na(raw[j,i]))
      raw[j,i] <- 0

#Blanks in Quality columns means the data is Okay/good. So convert blanks to code 1 for convenience.
for(i in c(15,17,18,19,20,21,23,25,27,29,31,33,35,38,41,44,47))
  for(j in 1:nrow(raw))
    if(is.na(raw[j,i]))
      raw[j,i] <- 1

#Remove colums with 95% to 100% missing data
modifiedData<-raw[,-c(36,37,39,40,42,43,45,46,48,49,68:74)]

plot(modifiedData$Salnty,modifiedData$O2ml_L)

#imputing missing value for O2ml_L
l_m1<-lm(formula=as.formula('O2ml_L~Salnty'),data=modifiedData)
print(l_m1)
count1 <- 0
for (row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'O2ml_L'])) {
    #print(predict(l_m1,newdata = modifiedData[row,]))
    modifiedData[row,'O2ml_L']<-predict(l_m1,newdata = modifiedData[row,])
    count1 <- count1+1
  }
}
print(count1)

plot(modifiedData$O2ml_L,modifiedData$O2Sat)

#imputing missing value for O2Sat
l_m2<-lm(formula=as.formula('O2Sat~O2ml_L'),data=modifiedData)
print(l_m2)
count2 <- 0
for (row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'O2Sat'])) {
    #print(predict(l_m2,newdata = modifiedData[row,]))
    modifiedData[row,'O2Sat']<-predict(l_m2,newdata = modifiedData[row,])
    count2 <- count2+1
  }
}
print(count2)

plot(modifiedData$O2ml_L,modifiedData$Oxy_µmol.Kg)

#imputing missing value for Oxy_µmol.Kg
l_m3<-lm(formula=as.formula('Oxy_µmol.Kg~O2ml_L'),data=modifiedData)
print(l_m3)
count3 <- 0
for (row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'Oxy_µmol.Kg'])) {
    #print(predict(l_m3,newdata = modifiedData[row,]))
    modifiedData[row,'Oxy_µmol.Kg']<-predict(l_m3,newdata = modifiedData[row,])
    count3 <- count3+1
  }
}
print(count3)

#imputing missing value for R_O2
count4 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_O2'])) {
    modifiedData[row,'R_O2']<-round(modifiedData[row,'O2ml_L'],2)
    count4 <- count4+1
  }
}
print(count4)

#imputing missing value for R_O2Sat
count5 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_O2Sat'])) {
    modifiedData[row,'R_O2Sat']<-round(modifiedData[row,'O2Sat'],1)
    count5 <- count5+1
  }
}
print(count5)

plot_missing(modifiedData)

#do not select columns with constant/limited values
selectedColsData <- select(modifiedData, -c(41,3,4,13,14,15,16,18,19,21,29,31,33,35))
formulaString<-paste("~ R_TEMP +", paste(colnames(selectedColsData), collapse=" + "))
imputedData <- aregImpute(as.formula(formulaString), data = modifiedData, n.impute = 5)
completedData <- impute.transcan(imputedData, imputation=1, data=modifiedData, list.out=TRUE,pr=FALSE, check=FALSE) 
summary(completedData)

modifiedData$NO3uM <- completedData$NO3uM
modifiedData$NO2uM <- completedData$NO2uM
modifiedData$SiO3uM <- completedData$SiO3uM
modifiedData$NH3uM <- completedData$NH3uM
modifiedData$PO4uM <- completedData$PO4uM
modifiedData$Phaeop <- completedData$Phaeop
modifiedData$ChlorA <- completedData$ChlorA

#imputing missing value for R_SIO3
count6 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_SIO3'])) {
    modifiedData[row,'R_SIO3']<-round(modifiedData[row,'SiO3uM'],1)
    count6 <- count6+1
  }
}
print(count6)

#imputing missing value for R_PO4
count7 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_PO4'])) {
    modifiedData[row,'R_PO4']<-round(modifiedData[row,'PO4uM'],2)
    count7 <- count7+1
  }
}
print(count7)

#imputing missing value for R_NO3
count8 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_NO3'])) {
    modifiedData[row,'R_NO3']<-round(modifiedData[row,'NO3uM'],1)
    count8 <- count8+1
  }
}
print(count8)

#imputing missing value for R_NO2
count9 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_NO2'])) {
    modifiedData[row,'R_NO2']<-round(modifiedData[row,'NO2uM'],2)
    count9 <- count9+1
  }
}
print(count9)

#imputing missing value for R_NH4
count10 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_NH4'])) {
    modifiedData[row,'R_NH4']<-round(modifiedData[row,'NH3uM'],2)
    count10 <- count10+1
  }
}
print(count10)

#imputing missing value for R_CHLA
count11 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_CHLA'])) {
    modifiedData[row,'R_CHLA']<-round(modifiedData[row,'ChlorA'],2)
    count11 <- count11+1
  }
}
print(count11)

#imputing missing value for R_PHAEO
count12 <- 0
for(row in 1:nrow(modifiedData)) {
  if(is.na(modifiedData[row,'R_PHAEO'])) {
    modifiedData[row,'R_PHAEO']<-round(modifiedData[row,'Phaeop'],2)
    count12 <- count12+1
  }
}
print(count12)

plot_missing(modifiedData)

#Outlier Analysis in Target
boxplot(modifiedData$R_TEMP, main = "Temperature")

write.csv(modifiedData,file="cleanedData.csv")

#feature selection
X <- modifiedData[c(40,43:56)]
Y <- modifiedData[41]
finalData <- data.frame(X, Y)

#min-max normalization
min_max_normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
  }

finalData <- as.data.frame(lapply(finalData, min_max_normalize))

#correlation plot
corrplot(cor(finalData))

#splitting into training and testing data.
index <- sample(nrow(finalData), 0.8 * nrow(finalData))
trainingData <- finalData[index, ]
testingData <- finalData[-index, ]

#neuralnet using caret
mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))
nn_model <- caret::train(R_TEMP~., data=trainingData, method="nnet", maxit=500, tuneGrid=mygrid, trace=F, linOut = TRUE) 

plotnet(nn_model, pos_col = "green", neg_col = "red")
jpeg('NeuralNetArchitecture_plot.jpeg', width = 800, height = 400, quality = 100) 
plotnet(nn_model, pos_col = "green", neg_col = "red")
dev.off()

pred_train_nn <- predict(nn_model,trainingData[1:15])
pred_test_nn <- predict(nn_model,testingData[1:15])
print(nn_model)

plot(nn_model)
jpeg('NeuralNetTuning_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(nn_model)
dev.off()

rmse_nn <- rmse(testingData$R_TEMP, pred_test_nn)
print(rmse_nn)

plot(testingData$R_TEMP, pred_test_nn, xlab = "Actual temp", ylab = "Predicted temp", main = "Neural Net predictions")
jpeg('NeuralNetPredictions_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(testingData$R_TEMP, pred_test_nn, xlab = "Actual temp", ylab = "Predicted temp", main = "Neural Net predictions")
dev.off()

#rbf network
set.seed(21)
rbf_model <- rbf(trainingData[1:15], trainingData[16], size = 10, maxit = 500, linOut = TRUE, inputsTest = testingData[1:15], targetsTest = testingData[16])
#training data's target value predictions
pred_train_rbf <- fitted(rbf_model)
#testing data's target value predictions
pred_test_rbf <- predict(rbf_model,testingData[1:15])  
plot(pred_test_rbf,type="l")  
lines(testingData[16],col="green")    

#Another way to get Predicted training set values
rbf_model$fitted.values

#Another way to get Predicted testing set values
rbf_model$fittedTestValues

rmse_rbf <- rmse(testingData$R_TEMP, pred_test_rbf)
print(rmse_rbf)

plot(testingData$R_TEMP, pred_test_rbf, xlab = "Actual temp", ylab = "Predicted temp", main = "RBF predictions")
jpeg('RBFPredictions_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(testingData$R_TEMP, pred_test_rbf, xlab = "Actual temp", ylab = "Predicted temp", main = "RBF predictions")
dev.off()
plotIterativeError(rbf_model)
jpeg('RBFIterativeError_plot.jpeg', width = 800, height = 400, quality = 100) 
plotIterativeError(rbf_model)
dev.off()
plotRegressionError(trainingData$R_TEMP, rbf_model$fitted.values, main = "Regression Error plot for training data") 
plotRegressionError(testingData$R_TEMP, rbf_model$fittedTestValues, main = "Regression Error plot for testing data")
hist(rbf_model$fitted.values - trainingData$R_TEMP, xlab = "Errors", main = "Error histogram of training error")
hist(rbf_model$fittedTestValues - testingData$R_TEMP, xlab = "Errors", main = "Error histogram of testing error")

#grnn
registerDoParallel()
getDoParWorkers()

pred_grnn <- function(x, nn){
  xlst <- split(x, 1:nrow(x))
  pred <- foreach(i = xlst, .combine = rbind, .packages = c("grnn", "doParallel")) %dopar% {
    data.frame(pred = guess(nn, as.matrix(i)), row.names = NULL)
  }
}

#finding optimal sigma
cv <- foreach(s = seq(0.25, 1, 0.05), .combine = rbind, .packages = c("grnn", "doParallel")) %dopar% {
  grnn <- smooth(learn(trainingData, variable.column = ncol(trainingData)), sigma = s)  
  pred <- pred_grnn(testingData[, -ncol(testingData)], grnn)    
  #print(pred)
  test_rse <- sum((testingData[, ncol(testingData)] - pred$pred)^2)  #residual sum of squares
  data.frame(s, rse = test_rse)
}

print(cv)

plot(cv$s, cv$rse, xlab = "Sigma", ylab = "Residual Sum of Squares (Error)", type = 'b')
jpeg('GRNNTuning_plot.jpeg', width = 800, height = 400, quality = 100)
with(cv, plot(s, rse, xlab = "Sigma", ylab = "Residual Sum of Squares (Error)", type = 'b'))
dev.off()

print(best.s <- cv[cv$rse == min(cv$rse), 1])

final_grnn <- smooth(learn(trainingData, variable.column = ncol(trainingData)), sigma = best.s)

#entire data's target value predictions
pred_all <- pred_grnn(finalData[, -ncol(finalData)], final_grnn)
plot(finalData$R_TEMP, pred_all$pred) 

#training data's target value predictions
pred_train_grnn <- pred_grnn(trainingData[, -ncol(trainingData)], final_grnn)

#testing data's target value predictions
pred_test_grnn <- pred_grnn(testingData[, -ncol(testingData)], final_grnn)
rmse_grnn <- rmse(testingData$R_TEMP, pred_test_grnn)
print(rmse_grnn)

plot(testingData$R_TEMP, pred_test_grnn$pred, xlab = "Actual temp", ylab = "Predicted temp", main = "GRNN predictions") 
jpeg('GRNNPredictions_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(testingData$R_TEMP, pred_test_grnn$pred, xlab = "Actual temp", ylab = "Predicted temp", main = "GRNN predictions") 
dev.off()

#create new colunms in training and testing data to store all predictions
trainingData$pred_nn <- pred_train_nn
trainingData$pred_rbf <- pred_train_rbf
trainingData$pred_grnn <- as.matrix(pred_train_grnn) 
testingData$pred_nn <- pred_test_nn
testingData$pred_rbf <- pred_test_rbf
testingData$pred_grnn <- as.matrix(pred_test_grnn)

#simple ensemble using Averaging : Taking average of predictions
testingData$pred_avg<-(testingData$pred_nn+testingData$pred_rbf+testingData$pred_grnn)/3
rmse_ensemble_avg <- rmse(testingData$R_TEMP, testingData$pred_avg)
print(rmse_ensemble_avg)

#Stacking ensemble using Gradient Boosting Model(gbm)
fitControl <- trainControl(method = "cv", number = 10, savePredictions = 'final')
gbm_model <- caret::train(trainingData[,c(17,18,19)],trainingData[,16], method='gbm', metric = "RMSE", trControl=fitControl,tuneLength=3)
print(gbm_model) 
plot(gbm_model)
jpeg('EnsembleTuning_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(gbm_model)
dev.off()

#predict 
testingData$pred_gbm_stacked<-predict(gbm_model,testingData[,c(17,18,19)])
rmse_ensemble_gbm <- rmse(testingData$R_TEMP,testingData$pred_gbm_stacked)
print(rmse_ensemble_gbm)

plot(testingData$R_TEMP, testingData$pred_gbm_stacked, xlab = "Actual temp", ylab = "Predicted temp", main = "Ensemble predictions") 
jpeg('EnsemblePredictions_plot.jpeg', width = 800, height = 400, quality = 100) 
plot(testingData$R_TEMP, testingData$pred_gbm_stacked, xlab = "Actual temp", ylab = "Predicted temp", main = "Ensemble predictions") 
dev.off()

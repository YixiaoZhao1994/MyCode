#################### Homework 2 ####################

setwd("C:/Users/YixiaoZhao/Desktop/Winter/R/HW/2")
library(readr)
library(reshape2)
library(leaps)
library(glmnet)
library(earth)
library(rlist)
library(class)
library(nnet)
library(kknn)

#################### Part 1 ########################

# a). Load the rating data into R and save as a data frame

Data = read.csv('Homework 2 - MKT436R Data.csv')
TestSet = read.csv('Homework 2 - Test Set.csv')
rocky1 = Data[Data$rockyID == 1,]
names(rocky1)[names(rocky1) == "rating"] <- "rocky1"
rocky1 = within(rocky1, rm(rockyID))

rocky2 = Data[Data$rockyID == 2,]
names(rocky2)[names(rocky2) == "rating"] <- "rocky2"
rocky2 = within(rocky2, rm(rockyID))

rocky3 = Data[Data$rockyID == 3,]
names(rocky3)[names(rocky3) == "rating"] <- "rocky3"
rocky3 = within(rocky3, rm(rockyID))

rocky4 = Data[Data$rockyID == 4,]
names(rocky4)[names(rocky4) == "rating"] <- "rocky4"
rocky4 = within(rocky4, rm(rockyID))

rocky5 = Data[Data$rockyID == 5,]
names(rocky5)[names(rocky5) == "rating"] <- "rocky5"
rocky5 = within(rocky5, rm(rockyID))

NewData = Reduce(function(rocky1, rocky2) merge(rocky1, rocky2, by = 'consumerID', all=TRUE), 
              list(rocky1, rocky2, rocky3, rocky4, rocky5))

#################### Part 2 ########################
# a). Compare the correlations between the ratings of each of the five movies using the cor
#     command on your new data frame. Which movies are most similar? Which movie is
#     most different from the others? 
cor(NewData, y = NULL, use = "pairwise.complete.obs")

# b). Find the mean rating of each movie. Which is the best? Which is the worst? 
colMeans(NewData, na.rm = TRUE, dims = 1)

# c). Create a subset of your data frame that only contains consumers who rated Rocky 4.
#     Find the mean rating of each movie in the data set. Compare with the results of the
#     previous section. Why do you think the mean ratings changed? 
DataRocky4 = NewData[is.na(NewData$rocky4) == FALSE, ]
#mean(DataRocky4)

# d). In the next part you will only run analysis on consumers who rated each of the Rocky
#     movies. This dataset has been posted online with the name 'Homework 2 -
#     completeDB.csv'. Why are some movie ratings missing? What kind of bias might be
#     caused by omitting incomplete data?

completeDB = read.csv('Homework 2 - completeDB(1).csv')

# Customers who have not rated all 5 rocky movies are missing.
# If you want to predict the rate of rocky 5, only customers who have watched all 5 rocky moviews and rated all of 
# them can get evalutaed. The customers are not randomly selected, so it will generate bias, the answer we get from 
# the completeDB will only be the prediction of customers who have watched all 5 rocky moviews and rated all.

#################### Part 3 ########################
# a). Use the following code to generate different orders of interactions among the predictor variables:

firstInteractions = model.matrix(~(-
                                       1+rocky1+rocky2+rocky3+rocky4),completeDB)
secondInteractions = model.matrix(~(-
                                      1+rocky1+rocky2+rocky3+rocky4)^2,completeDB)
thirdInteractions = model.matrix(~(-
                                     1+rocky1+rocky2+rocky3+rocky4)^3,completeDB)
fourthInteractions = model.matrix(~(-
                                      1+rocky1+rocky2+rocky3+rocky4)^4,completeDB)

# b). Run and store a linear regression for each of the above sets of predictor variables
lm1 = lm(rocky5~firstInteractions, data=completeDB)
summary(lm1)

lm2 = lm(rocky5~secondInteractions, data=completeDB)
summary(lm2)

lm3 = lm(rocky5~thirdInteractions, data=completeDB)
summary(lm3)

lm4 = lm(rocky5~fourthInteractions, data=completeDB)
summary(lm3)

# c). Calculate AIC, and BIC for each of these linear regressions. 

AIC1 = AIC(lm1)
AIC2 = AIC(lm2)
AIC3 = AIC(lm3)
AIC4 = AIC(lm4)

BIC1 = BIC(lm1)
BIC2 = BIC(lm2)
BIC3 = BIC(lm3)
BIC4 = BIC(lm4)

AICBIC_Tables <- matrix(c(AIC1,AIC2,AIC3,AIC4,BIC1,BIC2,BIC3,BIC4),ncol=4,byrow=TRUE)
colnames(AICBIC_Tables) <- c("firstInteractions","secondInteractions","thirdInteractions","fourthInteractions")
rownames(AICBIC_Tables) <- c("AIC","BIC")
AICBIC_Tables <- as.table(AICBIC_Tables)
AICBIC_Tables


# d). Now you will implement a lasso estimator using fourthInteractions. Estimate the
#     lasso model on the fourthInteractions data using the glmnet function. Use the
#     predict function to extract the coefficients in the case of s = 0.05 and s = 0.5. 

install.packages('glmnet')
library('glmnet')
lassoFit = glmnet(cbind(fourthInteractions), completeDB$rocky5,alpha=1)
plot(lassoFit)

#Get the coefficients when the penalty is set to 0.05 and 0.5
#RandX is 0!  But so is releventX2 :S.  Risks and rewards to using the lasso 
predict(lassoFit,s = 0.5, type = 'coefficients')
predict(lassoFit, s=.05, type='coefficients')


# e). Rather than choosing the penalty parameter yourself, you will calculate an optimal
#     penalty parameter using cross validation. This can be done with the cv.glmnet
#     function, in the glmnet package. For example, the following code would estimate the lasso
#     a particular x and y

lassoFit = cv.glmnet(fourthInteractions, completeDB$rocky5, alpha=1)
plot(lassoFit)




#################### Part 4 ####################
# a). Create at least two training and validation data sets.  
#     You will use these data sets to estimate and then evaluate the out of sample MSE of each model estimated in part d.  
#     Hint: You can make the validation set as large or as small as you want, so long as it is at 

set.seed(1)
isTraining1 = runif(nrow(completeDB)) < .7
trainingData1 = subset(completeDB,isTraining1)
validationData1= subset(completeDB,!isTraining1)

set.seed(2)
isTraining2 = runif(nrow(completeDB)) < .7
trainingData2 = subset(completeDB,isTraining2)
validationData2= subset(completeDB,!isTraining2)




# set.seed(57)
# train1 <- sample(1:nrow(completeDB), 0.66667*nrow(completeDB))
# completeDB.train1 <- completeDB[train1, ]
# completeDB.valid1 <- completeDB[-train1, ]
# 
# set.seed(68)
# train2 <- sample(1:nrow(completeDB), 0.66667*nrow(completeDB))
# completeDB.train2 <- completeDB[train2, ]
# completeDB.valid2 <- completeDB[-train2, ]

# b). In this part you will try to predict the ratings of rocky5 using a number of different models from class. 
#     i. Linear Regression, using the "lm" function 

nFold = 10
valNum = floor(runif(nrow(completeDB))*nFold)+1
#Create a matrix where we store prediction error 
modelPerformance = matrix(NA,nFold,10)
lmPerformance = matrix(NA,nFold,10)

for(fold in 1:nFold){
  trainingData = subset(completeDB, valNum != fold)
  validationData = subset(completeDB, valNum == fold)
  lm1 = lm(log(rocky5)~(log(rocky1)+log(rocky2)+log(rocky3)+log(rocky4)), data=trainingData)
  lm2 = lm(rocky5~rocky1+rocky2+rocky3+rocky4, data=trainingData)
  lm3 = lm(log(rocky5)~rocky1+rocky2+rocky3+rocky4, data=trainingData)
  lm4 = lm(rocky5~(rocky1+rocky2+rocky3+rocky4)^3, data=trainingData)
  lm5 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky2*rocky3, data=trainingData)
  lm6 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky2+rocky3*rocky4, data=trainingData)
  lm7 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky3+rocky2*rocky4, data=trainingData)
  lm8 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky4+rocky2*rocky3, data=trainingData)
  lm9 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky2*rocky3*rocky4, data=trainingData)
  lm10 = lm(rocky5~(rocky1+rocky2+rocky3+rocky4)^2, data=trainingData)
  
  valid1 = mean((validationData$rocky5 - predict(lm1,validationData))^2)^0.5
  valid2 = mean((validationData$rocky5 - predict(lm2,validationData))^2)^0.5
  valid3 = mean((validationData$rocky5 - predict(lm3,validationData))^2)^0.5
  valid4 = mean((validationData$rocky5 - predict(lm4,validationData))^2)^0.5
  valid5 = mean((validationData$rocky5 - predict(lm5,validationData))^2)^0.5
  valid6 = mean((validationData$rocky5 - predict(lm6,validationData))^2)^0.5
  valid7 = mean((validationData$rocky5 - predict(lm7,validationData))^2)^0.5
  valid8 = mean((validationData$rocky5 - predict(lm8,validationData))^2)^0.5
  valid9 = mean((validationData$rocky5 - predict(lm9,validationData))^2)^0.5
  valid10 = mean((validationData$rocky5 - predict(lm10,validationData))^2)^0.5
  
  lmPerformance[fold,] = c(valid1,valid2,valid3,valid4,valid5,valid6,valid7,valid8,valid9,valid10)
}

lmPerformance
min(colMeans(lmPerformance, na.rm = FALSE, dims = 1))   # 0.960365 model 9 has the lowest MSE

FinallmModel = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky2*rocky3*rocky4, data=completeDB)
FinalLmvalid = mean((completeDB$rocky5 - predict(lm9,completeDB))^2)^0.5
FinalLmvalid # 0.9572015
  



#     ii. MARS, using the "earth" package. 








#     iii. Neural networks, using the "nnet" package.

library("nnet")
# valNum3 = floor(runif(nrow(completeDB))*nFold)+1
#Create a matrix where we store prediction error 
nntPerformance = matrix(NA,10,10)


for (i in seq(10000, 100000, 10000)){
  for (h in (1:10)){
    for(fold in 1:nFold){
      validfold3 <- c()
      trainingData = subset(completeDB, valNum != fold)
      validationData = subset(completeDB, valNum == fold)
      NeuNet = nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,data = trainingData, linout=1,size = h,maxit = i)
      valid = mean((validationData$rocky5 - predict(NeuNet,validationData))^2)^.5
      validfold3 = append(validfold3, valid)
      TenFoldMean3 = mean(validfold3)
      r = i/10000
      nntPerformance[r,h] = TenFoldMean3
      
    }
  }
  
}
min(nntPerformance) # 0.9969919, in row 4 column 4, which is a model with size = 4, maxit = 4000
FinalNeuNetModel = nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,data = completeDB, linout=1,size = 9,maxit = 7000)
FinalNeuNetvalid = mean((completeDB$rocky5 - predict(FinalNeuNetModel,completeDB))^2)^.5
FinalNeuNetvalid  # 0.9427697



#     iv. K-Nearest Neighbour, using the "class" package. Hint: The syntax for this package is a bit different than the others. 

library(kknn)
knnPerformance1 = matrix(NA,10,10)
knnPerformance2 = matrix(NA,10,10)
knnPerformance3 = matrix(NA,10,10)
knnPerformance4 = matrix(NA,10,10)
knnPerformance5 = matrix(NA,10,10)


for (i in (1:10)) {
  for (d in (1:10)) {
    for(fold in 1:nFold){
      validfold4 <- c()
      trainingData = subset(completeDB,valNum!=fold)
      validationData = subset(completeDB,valNum==fold)
      kknnModel1 = kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,trainingData, validationData, k = i, distance = d)
      kknnModel2 = kknn(rocky5 ~ (rocky1 + rocky2 + rocky3 + rocky4)^2,trainingData, validationData, k = i, distance = d)
      kknnModel3 = kknn(rocky5 ~ (rocky1 + rocky2 + rocky3 + rocky4)^3,trainingData, validationData, k = i, distance = d)
      kknnModel4 = kknn(rocky5 ~ (rocky1 + rocky2 + rocky3 + rocky4)^4,trainingData, validationData, k = i, distance = d)
      kknnModel5 = kknn(log(rocky5)~(log(rocky1)+log(rocky2)+log(rocky3)+log(rocky4)),trainingData, validationData, k = i, distance = d)
      
      valid1 = mean((validationData$rocky5 - kknnModel1$fitted.values)^2)^.5
      validfold41 <- append(validfold4, valid1)
      TenFoldMean41 = mean(validfold41)
      
      valid2 = mean((validationData$rocky5 - kknnModel2$fitted.values)^2)^.5
      validfold42 <- append(validfold4, valid2)
      TenFoldMean42 = mean(validfold42)
      
      valid3 = mean((validationData$rocky5 - kknnModel3$fitted.values)^2)^.5
      validfold43 <- append(validfold4, valid3)
      TenFoldMean43 = mean(validfold43)
      
      valid4 = mean((validationData$rocky5 - kknnModel4$fitted.values)^2)^.5
      validfold44 <- append(validfold4, valid4)
      TenFoldMean44 = mean(validfold44)
      
      valid5 = mean((validationData$rocky5 - kknnModel5$fitted.values)^2)^.5
      validfold45 <- append(validfold4, valid5)
      TenFoldMean45 = mean(validfold45)
      
      knnPerformance1[i,d] = TenFoldMean41
      knnPerformance2[i,d] = TenFoldMean42
      knnPerformance3[i,d] = TenFoldMean43
      knnPerformance4[i,d] = TenFoldMean44
      knnPerformance5[i,d] = TenFoldMean45
    }
  }
}

min(knnPerformance1)
min(knnPerformance2)
min(knnPerformance3)
min(knnPerformance4)
min(knnPerformance5)

knnPerformance
min(knnPerformance) # Min 1.079522: row 10, column 4, which is a model with k = 10, distance = 4
FinalkknnModel = kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,trainingData, validationData, k = 10, distance = 4)
FinalKnnvalid = mean((completeDB$rocky5 - FinalkknnModel$fitted.values)^2)^.5
FinalKnnvalid # 1.647053

# lastOne
TestData = read.csv('Homework 2 - Test Set.csv')

lm9 = lm(rocky5~rocky1+rocky2+rocky3+rocky4+rocky1*rocky2*rocky3*rocky4, data=trainingData)
TestData$LM_rocky5 = round(predict(lm9,TestData), digits = 0)

# NeuNet = nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,data = trainingData, linout=1,size = 9,maxit = 7000)
# TestData$NNT_rocky5 = round(predict(NeuNet,TestData), digits = 0)
# 
# kknnModel = kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,trainingData, TestData, k = 10, distance = 4)
# TestData$KNN_rocky5 = round(kknnModel$fitted.values, digits = 0)

write.csv(TestData, file = "bestPredictions10.csv")
bestPredictions10 = read.csv("bestPredictions10.csv")
save(bestPredictions10, file = "bestPredictions10.rda")





completeDB = read.csv('Homework 2 - completeDB(1).csv')
testData =  read.csv('Homework 2 - Test Set.csv')
lmBest = lm(rocky5~rocky1+rocky2+rocky4+rocky3++rocky1*rocky2+rocky1*rocky4+rocky2*rocky4+ 
              rocky3*rocky4 +rocky1*rocky2*rocky4+rocky1*rocky2*rocky3*rocky4+rocky1*rocky3*rocky4+rocky2*rocky3*rocky4 +
              log(rocky3)+log(rocky4)+log(rocky2),data=completeDB)
testData$LM_rocky5 = round(predict(lmBest,testData), digits = 0)

mars = earth(rocky5~rocky1+rocky2+rocky3+rocky4, data = completeDB,degree=3,trace=0,thresh=0.001)
testData$MARS_rocky5 = round(predict(mars,testData), digits = 0)

NeuNet = nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,data = completeDB, linout=1,size = 10,maxit = 7000)
testData$NNT_rocky5 = round(predict(NeuNet,testData), digits = 0)

kknnModel = kknn(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4,completeDB, testData, k = 10, distance = 1)
testData$KNN_rocky5 = round(kknnModel$fitted.values, digits = 0)

write.csv(testData, file = "bestPredictions10.csv")
bestPredictions10 = read.csv("bestPredictions10.csv")
save(bestPredictions10, file = "bestPredictions10.rda")
View(testData)

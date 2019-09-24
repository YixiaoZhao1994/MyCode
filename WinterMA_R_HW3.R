############################### HW 3 #####################################

setwd("C:/Users/YixiaoZhao/Desktop/Winter/R/HW/3")
hw3Data <- read.csv('Homework 3 Student Data.csv')



############################### PART 1 ###################################

# a). Load the data
hw3Data <- read.csv('Homework 3 Student Data.csv')

# b). 
library('forecast')
TotalSalesByWeek = aggregate(totalCost~overallWeekNum, data = hw3Data, FUN = sum)

Train_TotalSalesByWeek = head(TotalSalesByWeek, n=nrow(TotalSalesByWeek)-52 )

HoldOutSample = tail(TotalSalesByWeek,n=52)

tsTotalSale = ts(Train_TotalSalesByWeek$totalCost, frequency = 52)

# c). Plot the time series 
plot(tsTotalSale)
acf(tsTotalSale)
  
# d). Using the Arima function, to estimate an ARMA model with degree (3, 2) and a drift term
Arima(tsTotalSale,order=c(3,0,2),include.drift=TRUE)  


# e). Using the auto.arima function, estimate a non-seasonal ARMA model by setting the
#     non-seasonal differencing D=0, seasonal=FALSE, approximation=FALSE. Repeat
#     this process for a seasonal model by setting seasonal=TRUE,
#     approximation=FALSE.
#     Hint: This will take a large amount of computational time
Auto_1 = auto.arima(tsTotalSale,approximation=FALSE,D=0,seasonal=FALSE,stepwise=FALSE)

Auto_2 = auto.arima(tsTotalSale,approximation=FALSE,D=0,seasonal=TRUE,stepwise=FALSE)

Auto_1
Auto_2

# f). Compare the performance in 1e by:
#     - AIC for each.
AIC(Auto_1)
AIC(Auto_2)

#     - Use the hold out sample, and calculate the sample mean squared error
SampleMeanSE = forecast(Auto_1)

predictedValues1 =forecast(Auto_1,h=52)$mean
predictedValues2 =forecast(Auto_2,h=52)$mean

mean(((HoldOutSample$totalCost-predictedValues1))^2)
mean(((HoldOutSample$totalCost-predictedValues2))^2)


#     - Plotting the forecasr for one year for each model using the ploy and forecast fuction. 
#       Hint: Set h =52 to forecast one year in advance.
plot(forecast(Auto_1,h=52))
plot(forecast(Auto_2,h=52))






############################### PART 2 ###########################################

# a). Calculate Price per can

hw3Data$PricePerCan = hw3Data$totalCost/hw3Data$units

# b). Regress Log(Units), and Price Per Can

lm1 = lm(log(units)~PricePerCan,data=hw3Data)
summary(lm1)
confint(lm1, level = 0.95) # CI: PricePerCan  -0.3388219 -0.3198925

# c). Aggregate units and totalCost to the productNum+overallWeekNum level using the aggregate function, 
#     with FUN = mean. Repeat the regression in 2b using this new aggregated dataset.

AvgByProdWeek = aggregate(cbind(units,totalCost)~productNum + overallWeekNum, data = hw3Data, FUN = mean)
AvgByProdWeek$PricePerCan = AvgByProdWeek$totalCost/AvgByProdWeek$units
lm2 = lm(log(units)~PricePerCan,data=AvgByProdWeek)
summary(lm2)
confint(lm2, level = 0.95) # CI: PricePerCan -0.2364645 -0.2067561

# d). Aggregate units and totalCost to the overallWeekNum level using the aggregate function, with FUN = mean. 
#     Repeat the regression in 2b using this new aggregated dataset.

AvgByWeek = aggregate(cbind(units,totalCost)~overallWeekNum, data = hw3Data, FUN = mean)
AvgByWeek$PricePerCan = AvgByWeek$totalCost/AvgByWeek$units
lm3 = lm(log(units)~PricePerCan,data=AvgByWeek)
summary(lm3)
confint(lm3, level = 0.95) # CI: PricePerCan -0.2660338 -0.07091944


############################# PART 3 ###############################################

# a). Calculate the Price per Can by dividing totalCost by units.
# b). Run a regression where log(units) is the dependent variable, and pricePerCan is the independent variable.

lm1 = lm(log(units)~PricePerCan,data=hw3Data)
summary(lm1)
confint(lm1, level = 0.95) # CI: PricePerCan -0.3388219 -0.3198925


# c). Repeat the regression in part 3b, but control for isFeature.

lm4 = lm(log(units)~PricePerCan+factor(isFeature),data=hw3Data)
summary(lm4)
confint(lm4, level = 0.95) # CI: PricePerCan -0.3135609 -0.2944758
                     

# d). Repeat the regression in part 3c, but control for isDisplay.

lm5 = lm(log(units)~PricePerCan+factor(isFeature)+factor(isDisplay),data=hw3Data)
summary(lm5)
confint(lm5, level = 0.95) # CI: PricePerCan -0.29427402 -0.2749100

# e). Repeat the regression in part 3d, but control for the storeNum. Hint: Store should be treated as a categorical variable.

lm6 = lm(log(units)~PricePerCan+factor(isFeature)+factor(isDisplay)+factor(storeNum),data=hw3Data)
summary(lm6)
confint(lm6, level = 0.95) # CI: PricePerCan -0.3254184734 -0.30452984

# f). Repeat the regression in part 3e, but control for the productNum. Hint: Product should be treated as a categorical variable.

lm7 = lm(log(units)~PricePerCan+factor(isFeature)+factor(isDisplay)+factor(storeNum)+factor(productNum),data=hw3Data)
summary(lm7)
confint(lm7, level = 0.95) # CI: PricePerCan -0.152749953 -0.1101675565

# g). Repeat the regression in part 3f, but control for the weekInYearNum as a factor variable. Hint: Product should be treated as a categorical variable.

lm8 = lm(log(units)~PricePerCan+factor(isFeature)+factor(isDisplay)+factor(storeNum)+factor(productNum)+factor(weekInYearNum),data=hw3Data)
summary(lm8)
confint(lm8, level = 0.95) # CI: PricePerCan -0.149948079 -0.107210640


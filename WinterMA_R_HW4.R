############################### HW 4 #####################################

######################### By: Yixiao Zhao ################################


setwd("C:/Users/YixiaoZhao/Desktop/Winter/R/HW/4")
hw4Data <- read.csv('Homework 4 Student Data.csv')

############################### PART 1 ###################################

# a). Figure out the most popular productNum in the dataset by units sold. Hint: Aggregating
#     units to the productNum level and using the which.max function can help you figure out
#     the most popular productNum.

UnitSaleByPrd = aggregate(units~productNum, data = hw4Data, FUN = sum)
which.max(UnitSaleByPrd$units) # productNum = 89



# b). Take a subset of the initial data file only keep data from the upc you found in part a and
#     name it upcFile.

upcFile = hw4Data[hw4Data$productNum == which.max(UnitSaleByPrd$units), ]



# c). Using the data in part 1b, use the following code to run this regression: 

aggUPCFile = aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)

aggUPCFile$pricePerCan = aggUPCFile$totalCost/aggUPCFile$units

model1 = lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile)

model1

# d). You will use the predict function to predict demand at a wide range of prices. Create a new dataframe as follows

possiblePrices = data.frame(price = seq(0,10,.01))  # Create df with column price with value range from 1~10, step 0.01
possiblePrices$demand = NA
newData = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)
# Randomly Generate a dataset



# e). Use this data frame estimated in part 1c to get predicted demand for each of the possible prices. Hint: Use the predict function.

possiblePrices$demand = exp(predict(model1, newData)) # We predict the units sold to make it out damand


# f). Using the estimate of demand in part 1e, calculate expected profit by multiplying demand
#     by the profit margin for each price. Assume a marginal cost of 30 cents.

possiblePrices$profit = (possiblePrices$price - 0.3) * possiblePrices$demand


# g). Use the which.max function to find the optimal price, and the expected profit at that
#     price. Hint: Carefully consider what y is in this case. You might have to transform the
#     predicted y to get the predicted demand.

which.max(possiblePrices$profit)
MaxPriceRow = possiblePrices[which.max(possiblePrices$profit),]
MaxPriceRow

#         price    demand      profit
# 107     1.05     2.777803    2.083353     


# h). Repeat parts 1c-1g but using the following model, which omits features and displays:

model2 = lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)
model2
possiblePrices$demandModel2 = exp(predict(model2, newData))
possiblePrices$profitModel2 = (possiblePrices$price - 0.3) * possiblePrices$demandModel2

which.max(possiblePrices$profitModel2)
MaxPriceRow2 = possiblePrices[which.max(possiblePrices$profitModel2),]
MaxPriceRow2

#        price    demandModel2      profitModel2
# 58     0.57     11.79982          3.185952





############################### PART 2 ###################################

# a). Use the following code to estimate two neural networks based on the same data 
#     (this is similar to the code in topic 4, for simplicity no need for cross validation)

library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+
                        factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)

# b). Using the predict function, and newData from part 1, use both nnet1 and nnet2 to predict demand when the price is 50 cents. 
#     Then, use nnet1 and nnet2 to predict demand when the price is 1 dollar. By how much did nnet1 and nnet2 predict that
#     demand would change when price increases from 50 cents to 1 dollar? Why does one model predict a larger reaction than the other?
#     Answer in 3-6 sentences in the pdf. (15 marks)
#     Note: Keep all other variables fixed at the values you set in part 1d.

possiblePrices$demandNnet1 = exp(predict(nnet1, newData))
possiblePrices$demandNnet2 = exp(predict(nnet2, newData))

DemandChangeNnet1 = possiblePrices[possiblePrices$price == 1,]$demandNnet1 - possiblePrices[possiblePrices$price == 0.5,]$demandNnet1
DemandChangeNnet1
# Demand decreases 5.167241

DemandChangeNnet2 = possiblePrices[possiblePrices$price == 1,]$demandNnet2 - possiblePrices[possiblePrices$price == 0.5,]$demandNnet2
DemandChangeNnet2
# Demand decreases 9.872922




# c). Use part 1d through 1g to get an 'optimal' price using the neural networks nnet1 and nnet2. 
#     Report your calculated optimal prices in the pdf. nnet1, despite having some nicer properties, 
#     should give you a very strange answer. (5 marks)

possiblePrices$profitNnet1 = (possiblePrices$price - 0.3) * possiblePrices$demandNnet1
possiblePrices$profitNnet2 = (possiblePrices$price - 0.3) * possiblePrices$demandNnet2


which.max(possiblePrices$profitNnet1)
MaxPriceRowNnet1 = possiblePrices[which.max(possiblePrices$profitNnet1),][, c("price", "demandNnet1", "profitNnet1")]
MaxPriceRowNnet1
#         price    demandNnet1    profitNnet1
# 1001    10       75850.83       735753


which.max(possiblePrices$profitNnet2)
MaxPriceRowNnet2 = possiblePrices[which.max(possiblePrices$profitNnet2),][, c("price", "demandNnet2", "profitNnet2")]
MaxPriceRowNnet2
#         price    demandNnet2    profitNnet2
# 72      0.71     11.03335       4.523674




# d). Why did nnet1 give you such a strange answer? Figure out what went wrong, and fix it to get a reasonable optimal price using nnet1. 
#     Answer in 3-6 sentences in the pdf. (20marks) 
#     Important Hints: Check 'practical issues' in Topic 4: Predictive Methods. 
#     Plot predicted profit versus price. Check the min and max of observed prices in the dataset

# Answer: The optimal price of Nnet 1 is 10, which is out of the price range of our sample (aggUPCFile).
SampleMaxPrice = aggUPCFile[which.max(aggUPCFile$pricePerCan), ]$pricePerCan
SampleMinPrice = aggUPCFile[which.min(aggUPCFile$pricePerCan), ]$pricePerCan


InRangePrediction = possiblePrices[possiblePrices$price >= SampleMinPrice & possiblePrices$price <= SampleMaxPrice, ]
which.max(InRangePrediction$profitNnet1)
MaxPriceRowNnet1_2 = InRangePrediction[which.max(InRangePrediction$profitNnet1),][, c("price", "demandNnet1", "profitNnet1")]
MaxPriceRowNnet1_2
#         price    demandNnet1    profitNnet1
# 68      0.67     9.270579       3.430114

# Or Another Way to do it but with same answer
InRangePredictionSamle = newData[newData$pricePerCan >= SampleMinPrice & newData$pricePerCan <= SampleMaxPrice, ]
InRangePredictionSamle$Demand = exp(predict(nnet1, InRangePredictionSamle))
InRangePredictionSamle$profitNnet1 = (InRangePredictionSamle$price - 0.3) * InRangePredictionSamle$Demand
which.max(InRangePredictionSamle$profitNnet1)
MaxPriceRowNnet1_21 = InRangePrediction[which.max(InRangePredictionSamle$profitNnet1),][, c("price", "demandNnet1", "profitNnet1")]
MaxPriceRowNnet1_21
#         price    demandNnet1    profitNnet1
# 120     0.67     9.270579       3.430114

plot(InRangePrediction$price, InRangePrediction$profitNnet1)

plot(possiblePrices$price, possiblePrices$profitNnet1)
plot(possiblePrices$price, possiblePrices$profitNnet2)

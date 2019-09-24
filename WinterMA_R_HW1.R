
######### HW 1 ##########

setwd("C:/Users/YixiaoZhao/Desktop/Winter/R/HW/1")

##### Part 1 #####

# a). Load the dataset in R
Dataset <- read.table("Homework 1 Data - 436R.csv", header=TRUE, sep=",")





# b). Create a new column called 'rDate' convert the 'date' column into the 'date' datatype. 
Dataset['rDate'] = as.Date(Dataset$date,'%m/%d/%Y')





# c). Determine the date that started the treatment period. That is, write code to determine
#    the earliest date in the treatment period. 
TreatmentPeriodDate <- Dataset$rDate[Dataset$isTreatmentPeriod== 1]
print(min(TreatmentPeriodDate))





# d). The data contains a control group, which are shown search ads throughout the data,
#     and a treatment group, which are only shown search ads before the treatment period.

#     i. Take a subset of all the data from the treatment group.
TreatmentGroup <- Dataset[Dataset$isTreatmentGroup== 1,]

#     ii. Run a regression that compares log(revenue) of the treatment group in the
#         pre-treatment period and in the treatment period. Hint: the independent
#         variable should be isTreatmentPeriod
lm = lm(log(revenue)~isTreatmentPeriod, data=TreatmentGroup)
summary(lm)

#     iii. Paste a summary of the regression into your pdf 




# e). Now we will use the control group for a true experimental approach. First, we will
#     check to make sure that the randomization was done properly. 10 marks

#     i. Take a subset of all the data from the pre-treatment period
ControlGroup <- Dataset[Dataset$isTreatmentGroup== 0,]
PreTreatmentPeriod <- Dataset[Dataset$isTreatmentPeriod== 0,]
#     ii. Run a regression that compares log(revenue) of the treatment group and the
#         control group in the pre-treatment period.
lm2 = lm(log(revenue)~isTreatmentGroup, data=PreTreatmentPeriod)
summary(lm2)

#     iii. Paste a summary of the regression into your pdf 




# f). Now, using the post treatment data, determine the effectiveness of eBay ads. Run a
#     regression with log(revenue) as the dependent variable, and control for whether the
#     DMA is the treatment group. Paste a summary of this regression into your pdf. 10
#     marks
PostTreatmentPeriod <- Dataset[Dataset$isTreatmentPeriod== 1,]
lm3 = lm(log(revenue)~isTreatmentGroup, data=PostTreatmentPeriod)

summary(lm3)





# g). Check whether the effectiveness of eBay advertising changes over time. Create a
#     new variable called month, which contains a factor variable of the month that the date
#     falls in. In a new regression on the post treatment data, interact whether the DMA is in
#     the treatment group with this month variable. Paste a summary of this regression into
#     your pdf. 10 marks
#     Hint: Creating the month variable can be done in a least two ways. One method
#           would use the paste and the substr function. Another would use the lubridate
#           package. Any approach that works is acceptable.

PostTreatmentPeriod$month = as.factor(format(PostTreatmentPeriod$rDate, '%m'))
lm4 = lm(log(revenue)~isTreatmentGroup*month, data=PostTreatmentPeriod)

summary(lm4)

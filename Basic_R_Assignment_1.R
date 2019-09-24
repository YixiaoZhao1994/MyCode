# # ===================================================
# GBA464: Assignment 1
# Author: Yufeng Huang
# Description: location choice of Belgium ATMs
# Data: Belgium ATM distribution in 1994
# Source: will reveal later
# ===================================================
# INSTRUCTIONS
# 1. this assignment is individual and you cannot look at others' code
# 2. this assignment is graded mostly on correctness of the results but please do 
#       try to maintain readability; please also follow the variable naming instructions
# 3. variable definitions are downloadable at 
#   https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_1/variable_definition.txt


# clear everything
rm(list = rm())
setwd("D:/5_Study/UR/Study/1-Prefall/GBA 464 Programming for Ananlytics/Week 1/Assignment")

# load data
fileloc <- "D:\5_Study\UR\Study\1-Prefall\GBA 464 Programming for Ananlytics\Week 1\Assignment\belgium_atm"

df <- read.csv("belgium_atm.csv", stringsAsFactors = F)

# we can check the structure of the data by running

head(df)

# ==== question 1 ====

# Q1. First, recall that df is a data frame which is like a spreadsheet in Excel. 
#   Let's convert every column into a separate variable using '$'; for example:
population <- df$population
numATMs <- df$numATMs

# do the same for the other columns
ATMwithdr <- df$ATMwithdr
withdrvalue <- df$withdrvalue
unemprate <- df$unemprate
numbranches <- df$numbranches




# ==== question 2 ====

# Q2a. Do the necessary conversion for all variables so that you can apply numeric operations on them
#   replace the original value in case of conversion
is.numeric(population)
population <- as.numeric(population)
is.numeric(population)                 # check

is.numeric(numATMs)

is.numeric(ATMwithdr)
ATMwithdr <- as.numeric(ATMwithdr)
is.numeric(ATMwithdr)                  # check

is.numeric(withdrvalue)
withdrvalue <- as.numeric(withdrvalue)
is.numeric(withdrvalue)                # check

is.numeric(unemprate)

is.numeric(numbranches)



# Q2b. population is in a very different scale. Rescale it into thousands, i.e., divide population by 1000
#   and replace the variable
population <- population/1000
table(population) # check
population        # double check the data


# ==== question 3 ====

# You want to take average for all variables but you realized that some variables have missing value
#   before taking averages, you need to make sure that all observations are taken from the same sets of 
#   observations (i.e. rows) where no variable is missing 

# Q3a. let's define a logical vector for non-missing rows, i.e. rows without any missing values, name it 'nm'

#My thinking Process Note:nmATMwithdr <- as.logical(na.omit(ATMwithdr,with.na))
#nmATMwithdr
#nmwithdrvalue <- na.omit(withdrvalue,with.na)

# Thinking Proocess (This is not correct): nm <- as.logical(which(!is.na(ATMwithdr) & !is.na(withdrvalue)))
nm <- !is.na(ATMwithdr)
nm # check

table(nm) # check
is.logical(nm) # check

# Q3b. count the number of non-missing rows, name it 'count_nm'

count_nm <- length(nm[nm==TRUE])
count_nm #check



# ==== question 4 ====

# Q4. Summarize the average of number of ATM, number of branches, population, 
#   unemployment rate, number of withdraw per resident and amount per withdrawl.
#   In particular, notice that certain variables have missing values and you might want to  
#   only calculate means for the rows without missing values of any variable
#   Finally, collect results in a vector called 'mean_nm', name elements in the vector by the original variable name


#mean_nm <- c(numATMs=mean(numATMs[!is.na(ATMwithdr) & !is.na(withdrvalue)]), 
#             numbranches=mean(numbranches[!is.na(ATMwithdr) & !is.na(withdrvalue)]), 
#             population=mean(population[!is.na(ATMwithdr) & !is.na(withdrvalue)]),
#             unemprate=mean(unemprate[!is.na(ATMwithdr) & !is.na(withdrvalue)]),
#             withdrvalue=mean(withdrvalue[!is.na(ATMwithdr) & !is.na(withdrvalue)]),
#             ATMwithdr=mean(ATMwithdr[!is.na(ATMwithdr) & !is.na(withdrvalue)])
#            )

mean_nm <- c(numATMs= mean(numATMs[nm]), 
             numbranches = mean(numbranches[nm]), 
             population = mean(population[nm]),
             unemprate = mean(unemprate[nm]),
             withdrvalue = mean(withdrvalue[nm]),
             ATMwithdr = mean(ATMwithdr[nm])
              )

mean_nm #check





# ==== question 5 ====

# Q5. You realize that the reason for missing values in the original data is that there are no ATMs.
#   So in that regard you could have defined the missing values to zero
#   Re-define the missings to zero and assign it to the original variable,
#   find the total number of observations in the dataset (call it 'count_all'), 
#   and re-calculate means for the same set of variables and collect results in 'mean_all'

# ATMwithdr[is.na(ATMwithdr)] <- 0 Websolution
ATMwithdr[is.na(ATMwithdr)]<- 0
ATMwithdr #check

withdrvalue[is.na(withdrvalue)]<- 0
withdrvalue #check


count_all <- nrow(df)
count_all   #check

mean_all <- c(numATMs = mean(numATMs), 
              numbranches = mean(numbranches), 
              population = mean(population),
              unemprate = mean(unemprate),
              withdrvalue = mean(withdrvalue),
              ATMwithdr = mean(ATMwithdr))

mean_all #check



# ==== question 6 ====

# You decide to investigate what's the average number of withdrawal and amount per withdrawal
#   by areas with different number of ATMs

# Q6a. Let's summarize ATMwithdr and withdrvalue by the number of atms (for range 1-4). 
#   collect results in two separate vectors and name them 'mean_a' and 'mean_w'

head(df, n=10)
#cor(ATMwithdr, numATMs)
mean_a <- c(mean(ATMwithdr[numATMs==1]),
            mean(ATMwithdr[numATMs==2]),
            mean(ATMwithdr[numATMs==3]),
            mean(ATMwithdr[numATMs==4])
                  )
mean_a     #check

mean_w <- c(mean(withdrvalue[numATMs==1]),
            mean(withdrvalue[numATMs==2]),
            mean(withdrvalue[numATMs==3]),
            mean(withdrvalue[numATMs==4])
                  )
mean_w     #check






# Q6b. Plot these by the number of ATMs; label the x axis "number of ATMs" and y axis 
#   "withdrawl per resident" and "amount per withdrawl", respectively
#   use line plot by setting type = 'l' as one of the plot function arguments

# Seperate
r <- c(1,2,3,4)
r
plot(
       r, mean_a,
       xlab = "number of ATMs", ylab = "withdrawl per residen",
       type = 'l',
       
       main = "Withdrawl per resident by the number of atms"
       
       
      )

plot(
      r, mean_w,
      xlab = "number of ATMs", ylab = "amount per withdrawl",
      type = 'l',
      
      main = "Amount per withdrawl by the number of atms"
      
    )





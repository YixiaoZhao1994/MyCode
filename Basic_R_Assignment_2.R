# # ===================================================
# GBA464: Assignment 2 
# Author: Yufeng Huang
# Description: working with data frame
# Data: European car characteristics, prices and sales, 1970-1999
# Source: 
#   cars: https://sites.google.com/site/frankverbo/data-and-software/data-set-on-the-european-car-market
#   crude oil price: OPEC; IEA; extracted from 
#   http://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
# Optionally, you can also try plotting against UK gasoline price from www.theaa.com 
#   link: https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_2/ukgas.csv
#   (https://www.theaa.com/public_affairs/reports/Petrol_Prices_1896_todate_gallons.pdf)
# Acknowledgement: Frank Verboven has contributed significant effort
#   making the car sales dataset publically available
# ===================================================

# ===== Step 0: load data and required packages =====

# download file to local folder if file does not exist
url <- 'https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_2/cars.csv'
if (!file.exists('cars.csv')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'cars.csv', mode = 'wb')
}

setwd("D:/5_Study/UR/Study/1-Prefall/GBA 464 Programming for Ananlytics/Week 2/Assignment 2")
df <- read.csv('cars.csv')  # load data
oil <- read.csv('oil.csv')  # load data

# ===== Question 0: what are the keys of the data frames? =====
# Before we start, let's think about what are the keys in the data frame
# You don't have to do anything now; just think about it

df
oil

# ===== Question 1: cleanup data =====
# 1) Take a subset of data where class ($cla) is "standard" or "intermediate"
#   or "luxury", store it in place of the original variable df

head(df)
str(df)
cla <- df$cla
df <- df[ df$cla == "standard" | df$cla == "intermediate" | df$cla == "luxury",]
df # Check

# 2) Generate a column in df, $mpg, that measures fuel efficiency in mile per gallon
#   note that 1 mile per gallon = 235.215 / (1 liter per 100km). In other words, mpg is not 
#   proportional to liter per 100km, but is proportional to 1/(liter per 100). 
#   To check your answers, keep in mind that we usually see mpgs between 10 and 40. 
#   mpg =   235.215 * (1/ (100*li)
#   mpg = (km/lt) * 2.352
#   km/lt = mpg * 0.425

df$mpg <- c(235.215 * (1 / df$li))
 
  
# 3) Find a way to replace year in dataframe oil ($ye) into 2 digit (e.g. 1990 becomes 90, etc.)


oil$ye <- format(as.Date(as.character(oil$ye), format = '%Y'), '%y')
oil$ye <- as.numeric(oil$ye)

# ===== Question 2: summarize fuel efficiency by year and manufacturer =====
# Take average of fuel efficiency $mpg for given the firm ($frm) and year ($ye)
#   You could use the function aggregate()
# Then, plot the average $mpg for firm ($frm) Volkswagen ("VW") across all years.
#   Set your axis label to "year" and "mile per gallon" and put up a title
#   "Volkswagen fuel efficiency"

avg_mpg <- aggregate(x =  list("Avg_mpg" = df$mpg ),
                     by = list("firm" = df$frm,
                                 "ye" = df$ye),
                     FUN = mean)

plot(
      avg_mpg$ye[avg_mpg$firm == "VW"], avg_mpg$Avg_mpg[avg_mpg$firm == "VW"],
      xlab = "year", ylab = "mile per gallon",
      main = "Volkswagen fuel efficiency")




# ===== Question 3: merge with gasoline price data =====
# 1) Merge the average fuel efficiency data with crude oil price data, 
head(oil)
head(avg_mpg)
head(df$ye)
typeof(df$ye)
typeof(oil$ye)
typeof(avg_mpg$ye)
avg_mpg$
length(df$ye)
#VM
#vm <- data.frame(avg_mpg[avg_mpg$firm == "VW",,])
avg_mpg$ye <- as.numeric(avg_mpg$ye)
mg <- merge(avg_mpg, oil, by = "ye") 

head(mg)



# 2) Create the same plot as above (also for VW) but add a plot of crude oil price over time
#   when doing so:  a) set xlab and ylab to "" in order to not generate any label,
#                   b) generate an axis on the right by using axis(side = 4, ...)



plot(
      mg$ye[mg$firm == "VW"], mg$Avg_mpg[mg$firm == "VW"],
      xlab = "", ylab = "",
      main = "Volkswagen fuel efficiency 2"
       )

#main = "Volkswagen fuel efficiency 2"

par(new = T)

plot(
      mg$ye[mg$firm == "VW"], mg$oilpr[mg$firm == "VW"],
      xlab = "", ylab = "",
      type = 'l',axes=FALSE
    )
axis(side = 4)

par(new = F)

# 3) 1985 was the start of the new US regulation on fuel efficiency (which was announced in 1975).
#   Add a vertical line in the year 1985 to indicate this (search help lty or col for line type or 
#   color; adjust them so that the graph does not look that busy)

abline (v=85, lty = 1, col = "pink", lwd = 4)
      





# ===== Question 4: find new cars models =====
# 1) Find the first year where a specific car model (indicated by variable $type) 
#   has positive sales in a given market ($ma); i.e. that's the year when the model 
#   started to sell at all
#   Think of this as the year of introduction; consider using aggregate()

# Note: You might want to construct a data frame for this, but in the end merge it with 
#   to the original data frame and assign the merge result to df.augment

head(df)
agument <- aggregate(x =  list("First year" = df$ye ),
                     by = list("type" = df$type,
                               "ma" = df$ma),
                     FUN = min)

df.augment <- merge(df, agument, by = c("type", "ma" ), all = TRUE)


# 2) Generate a sub-data frame where each car model just started selling for the first/second year;
#   that is, year <= year of introduction + 1; assign the data frame df.new

df.new <- df.augment [df.augment$ye <= (df.augment$First.year + 1),]


# 3) Generate average $mpg for all cars that started selling for the first/second year; 
#   use aggregate()
# Note: in this step, keep focusing on Volkswagen (VW)

vw_mpg <- aggregate(x =  list("Avg_mpg" = df.new$mpg [df.new$frm == "VW"]),
                     by = list("type" = df.new$type [df.new$frm == "VW"],
                               "year" =  df.new$First.year [df.new$frm == "VW"]),
                    
                     FUN = mean)


vw_mpg <- aggregate(x =  list("Avg_mpg" = df.new$mpg),
                    by = list("type" = df.new$type [df.new$frm == "VW"]),
                    FUN = mean)





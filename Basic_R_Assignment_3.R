# # ===================================================
# GBA464: RFM analysis on CDNOW data
# Author: Yufeng Huang
# Description: Lab on functions and loops
# Data: CDNOW customer data (this time full data)
# Source: provided by Professor Bruce Hardie on
#   http://www.brucehardie.com/datasets/CDNOW_sample.zip
# ===================================================

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_3/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month

typeof(df.raw$date)
year <- as.character(df.raw$date)
df.raw$year <- substr(year, start = 1, stop = 4)

month <- as.character(df.raw$date)
df.raw$month <- substr(month, start = 5, stop = 6)

head(df.raw$year)
head(df.raw$month)



# b) aggregate into monthly data with number of trips and total expenditure

df.raw$trips <- 0



length(df.raw$id)
t <- 1
i <- -1

for(id in 1:1000){
  for(t in 1: 2891){
              if (id == df.raw$id[t]){  i <- i + 1
                                        df.raw$trips[t] <- i
                                      
                                      }
              else {i <- -1}
                  }}


df.raw$ntrip <- 1


Monthly <- aggregate(x =  list("Total_quantity" = df.raw$qty, 
                               "Total_expenditure" = df.raw$expd, 
                               "Number_of_trip" = df.raw$ntrip ),
                                by = list("ID" = df.raw$id,
                                          "Year" = df.raw$year,
                                          "Month" = df.raw$month
                                          ),
                                FUN = sum)


# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calcualte RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.

unique(df.raw$year)
unique(df.raw$month)

#month <- data.frame("Month" = c ("01", "02", "03", "04", "05", "06", 
#                                 "07", "08", "09", "10", "11", "12"))

#year <- data.frame("Year" = sort (rep(c("1997","1998"), length.out = 24)))

year_month <- data.frame("id" =  sort (rep(c(1:1000), length.out = 24000)),
                         "year" = sort (rep(c("1997","1998"), length.out = 24)),
                         "month" = c ("01", "02", "03", "04", "05", "06", 
                                      "07", "08", "09", "10", "11", "12")
                         )
length(unique(df.raw$id))

ndf.raw <- merge(df.raw, year_month, by = c("id","year", "month"), all = TRUE)
ndf.raw[is.na(ndf.raw)]  <- 0

nMonthly <- aggregate(x =  list("Total_quantity" = ndf.raw$qty, 
                               "Total_expenditure" = ndf.raw$expd, 
                               "Number_of_trip" = ndf.raw$ntrip ),
                     by = list("Month" = ndf.raw$month,
                               "Year" = ndf.raw$year,
                               "ID" = ndf.raw$id
                               
                               
                     ),
                     FUN = sum)


# df<- expand.grid <- (id = seq(1,1000)
#                      year = seq(1997,1998)
#                      month = seq(1,12))






# Check the data by using length(ndf.raw$date[ndf.raw$ntrip==0])

# now we should have the dataset we need; double check to make sure that every consumer is in every period


# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency

nMonthly$recency <- 0
nMonthly$Month <- as.numeric(nMonthly$Month)
nMonthly$Year <- as.numeric(nMonthly$Year)

r <- 1
m1 <- 1




for(i in 1:1000){
  for(y in 1997:1998) {
    for(m in 2:12){
                          if (nMonthly$Number_of_trip[nMonthly$ID == i & 
                                                      nMonthly$Year == y & 
                                                      nMonthly$Month == m - 1] == 0)
                              {r <- r + 1
                               nMonthly$recency[nMonthly$ID == i & 
                                                nMonthly$Year == y & 
                                                nMonthly$Month == m] <- r}
                                                      
                          else{r <- 1
                               nMonthly$recency[nMonthly$ID == i & 
                                                nMonthly$Year == y & 
                                                nMonthly$Month == m] <- r}
                            
    }
  }
                    r <- 1
                  }



  

for(i in 1:1000){
  
  if(nMonthly$Number_of_trip[nMonthly$ID == i & 
                      nMonthly$Year == 1997 & 
                      nMonthly$Month == 12]== 0 )
  
  
  {nMonthly$recency[nMonthly$ID == i & 
                    nMonthly$Year == 1998 & 
                    nMonthly$Month == 1] <- nMonthly$recency[nMonthly$ID == i & 
                                                             nMonthly$Year == 1997 & 
                                                             nMonthly$Month == 12] + 1} 
  else {nMonthly$recency[nMonthly$ID == i & 
                           nMonthly$Year == 1998 & 
                           nMonthly$Month == 1] <- 1}
 
}



for(i in 1:1000){
  for(y in 1998:1998) {
    for(m in 2:12){
      if (nMonthly$Number_of_trip[nMonthly$ID == i & 
                                  nMonthly$Year == y & 
                                  nMonthly$Month == m - 1] == 0)
      {r <- nMonthly$recency[nMonthly$ID == i & 
                               nMonthly$Year == y & 
                               nMonthly$Month == m-1]
       r <- r + 1
      nMonthly$recency[nMonthly$ID == i & 
                         nMonthly$Year == y & 
                         nMonthly$Month == m] <- r}
      
      else{r <- 1
      nMonthly$recency[nMonthly$ID == i & 
                         nMonthly$Year == y & 
                         nMonthly$Month == m] <- r}
      
    }
  }
  r <- 1
}





for(i in 1:1000){
  nMonthly$recency[nMonthly$ID == i & 
                   nMonthly$Year == 1997 & 
                   nMonthly$Month == 1] <- "NA"}


# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 8 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency


nMonthly$Quarter <- 1
qn <- 1
for (qn in 1:24000){
if (nMonthly$Year[qn] ==1997)
 {if (nMonthly$Month[qn] == 1|
      nMonthly$Month[qn] == 2|
      nMonthly$Month[qn] == 3) {nMonthly$Quarter[qn] <- 1}
  else{ 
        if (nMonthly$Month[qn] == 4|
            nMonthly$Month[qn] == 5|
            nMonthly$Month[qn] == 6) {nMonthly$Quarter[qn] <- 2}
    else {if (nMonthly$Month[qn] == 7|
              nMonthly$Month[qn] == 8|
              nMonthly$Month[qn] == 9) {nMonthly$Quarter[qn] <- 3}
      else{nMonthly$Quarter[qn]<- 4}}}}
  
  else{ if (nMonthly$Month[qn] == 1|
            nMonthly$Month[qn] == 2|
            nMonthly$Month[qn] == 3) {nMonthly$Quarter[qn] <- 5}
    else{ if (nMonthly$Month[qn] == 4|
              nMonthly$Month[qn] == 5|
              nMonthly$Month[qn] == 6) {nMonthly$Quarter[qn] <- 6}
      else {if (nMonthly$Month[qn] == 7|
                nMonthly$Month[qn] == 8|
                nMonthly$Month[qn] == 9) {nMonthly$Quarter[qn] <- 7}
        else{nMonthly$Quarter[qn]<- 8}}}}
  }

odf <- aggregate(x = list("Total_number_of_trip" = nMonthly$Number_of_trip),
                by = list("Quarter" = nMonthly$Quarter,
                          "ID" = nMonthly$ID),
                FUN = sum)


odf$frequency <- 0
for(i in 1:1000){
  for(q in 2:8){
    
      odf$frequency[odf$Quarter==q & odf$ID==i] <- 
                                            odf$Total_number_of_trip[odf$Quarter==q-1 & odf$ID==i]
  }
}

for(i in 1:1000){
  for(q in 1:1){odf$frequency[odf$Quarter==q & odf$ID==i] <- "NA"
  }
}

nMonthly <- merge(nMonthly, odf, by = c("ID","Quarter"), all = TRUE)

# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the beginning to 
#   the PAST MONTH. Call this df$monvalue

nMonthly$monvalue <- 0
nMonthly$CTE <- 0

#for(i in 1:1000){
 # for(y in 1997:1997) {
  #  for(m in 1:1){
   # nMonthly$CTE [nMonthly$ID == i & 
    #             nMonthly$Year == y & 
     #            nMonthly$Month == 1] <- #nMonthly$Total_expenditure[nMonthly$ID == i & 
                                                                    #nMonthly$Year == y & 
                                                                    #nMonthly$Month == 1]
#    }
#    }}
for(i in 1:1000){
  for(y in 1997:1997) {    
    for(m in 2:12){
      nMonthly$CTE [nMonthly$ID == i & 
                   nMonthly$Year == y & 
                   nMonthly$Month == m] <- 
        nMonthly$CTE[nMonthly$ID == i & 
                     nMonthly$Year == y & 
                     nMonthly$Month == m-1] + nMonthly$Total_expenditure[nMonthly$ID == i & 
                                                                         nMonthly$Year == y & 
                                                                         nMonthly$Month == m-1]}}}

for(i in 1:1000){
  for(y in 1998:1998) {
    for(m in 1:1){
      nMonthly$CTE [nMonthly$ID == i & 
                    nMonthly$Year == y & 
                    nMonthly$Month == 1] <- 
        nMonthly$CTE[nMonthly$ID == i & 
                     nMonthly$Year == 1997 & 
                     nMonthly$Month == 12]+ nMonthly$Total_expenditure[nMonthly$ID == i & 
                                                                       nMonthly$Year == 1997 & 
                                                                       nMonthly$Month == 12]}}}
for(i in 1:1000){
  for(y in 1998:1998) {    
    for(m in 2:12){
      nMonthly$CTE [nMonthly$ID == i & 
                      nMonthly$Year == y & 
                      nMonthly$Month == m] <- 
        nMonthly$CTE[nMonthly$ID == i & 
                       nMonthly$Year == y & 
                       nMonthly$Month == m-1] + nMonthly$Total_expenditure[nMonthly$ID == i & 
                                                                           nMonthly$Year == y & 
                                                                           nMonthly$Month == m-1]}}}
                                                                             
                                                                             
                                                                             

#nMonthly$Number_of_trip[nMonthly$ID == 1 & nMonthly$Quarter == 4 & nMonthly$Month == 12]

nMonthly$Count <- 0

#for(i in 1:1000){
 # for(y in 1997:1997) {
  #  for(m in 1:1){
   #   if(nMonthly$Number_of_trip [nMonthly$ID == i & 
    #                     nMonthly$Year == y & 
     #                    nMonthly$Month == 1]== 0){
      #nMonthly$Count [nMonthly$ID == i & 
       #               nMonthly$Year == y & 
        #              nMonthly$Month == 1] <- 0}
      #
      #else {nMonthly$Count [nMonthly$ID == i & 
       #                     nMonthly$Year == y & 
        #                    nMonthly$Month == 1] <- 1}}}}

for(i in 1:1000){
  for(y in 1997:1997) {
    for(m in 2:12){
      if(nMonthly$Number_of_trip [nMonthly$ID == i & 
                                  nMonthly$Year == y & 
                                  nMonthly$Month == m-1]== 0){
        nMonthly$Count [nMonthly$ID == i & 
                          nMonthly$Year == y & 
                          nMonthly$Month == m] <- nMonthly$Count [nMonthly$ID == i & 
                                                                    nMonthly$Year == y & 
                                                                    nMonthly$Month == m-1]}
      
      else {nMonthly$Count [nMonthly$ID == i & 
                              nMonthly$Year == y & 
                              nMonthly$Month == m] <- nMonthly$Count [nMonthly$ID == i & 
                                                                         nMonthly$Year == y & 
                                                                         nMonthly$Month == m-1]+1}
    }}}

for(i in 1:1000){
  for(y in 1998:1998) {
    for(m in 1:1){
      if(nMonthly$Number_of_trip [nMonthly$ID == i & 
                                  nMonthly$Year == 1997 & 
                                  nMonthly$Month == 12]== 0){
        nMonthly$Count [nMonthly$ID == i & 
                          nMonthly$Year == y & 
                          nMonthly$Month == 1] <- nMonthly$Count [nMonthly$ID == i & 
                                                                    nMonthly$Year == 1997& 
                                                                    nMonthly$Month == 12]}
      
      else {nMonthly$Count [nMonthly$ID == i & 
                            nMonthly$Year == y & 
                            nMonthly$Month == 1] <- nMonthly$Count [nMonthly$ID == i & 
                                                                    nMonthly$Year == 1997& 
                                                                    nMonthly$Month == 12]+1}
      }}}


for(i in 1:1000){
  for(y in 1998:1998) {
    for(m in 2:12){
      if(nMonthly$Number_of_trip [nMonthly$ID == i & 
                                  nMonthly$Year == y & 
                                  nMonthly$Month == m-1]== 0){
        nMonthly$Count [nMonthly$ID == i & 
                          nMonthly$Year == y & 
                          nMonthly$Month == m] <- nMonthly$Count [nMonthly$ID == i & 
                                                                    nMonthly$Year == y & 
                                                                    nMonthly$Month == m-1]}
      
      else {nMonthly$Count [nMonthly$ID == i & 
                              nMonthly$Year == y & 
                              nMonthly$Month == m] <- nMonthly$Count [nMonthly$ID == i & 
                                                                        nMonthly$Year == y & 
                                                                        nMonthly$Month == m-1]+1}
    }}}

nMonthly$monvalue <- nMonthly$CTE/nMonthly$Count





# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

b1 <- -0.1
b2 <- 3.5
b3 <- 0.2

nMonthly$index <- b1*as.numeric(nMonthly$recency) + 
            b2*as.numeric(nMonthly$frequency) + 
            b3*as.numeric(nMonthly$monvalue)


# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-trip revenue that these consumers generate and comment on 
# whether the RFM index help you segment which set of customers are "more valuable"

quantile(nMonthly$index, probs = seq(0.1, 1, 0.1), na.rm = TRUE,
         names = TRUE, type = 7)






sortindex<-!is.na(nMonthly$index)


sortindex <- data.frame("index" = nMonthly$index[sortindex],
                        "consumer" = nMonthly$ID[sortindex],
                        "year" = nMonthly$Year[sortindex],
                        "month" = nMonthly$Month[sortindex],
                        "Expenditure" = nMonthly$Total_expenditure[sortindex],
                        "Number_of_Trips" = nMonthly$Number_of_trip[sortindex])


#sortindex <- data.frame ("index" = sort(nMonthly$index))

#df[order(-df$index), ]

sortindex$Group <- 1
idx <- 1

for(idx in 1:21000){
  if(sortindex$index[idx] <= 1.172){sortindex$Group[idx] <- 1}
  else{if(sortindex$index[idx] > 1.172 & sortindex$index[idx]<=2.0432){sortindex$Group[idx] <- 2}
      else{if(sortindex$index[idx] > 2.0432 & sortindex$index[idx]<=3.1980){sortindex$Group[idx] <- 3}
        else{if(sortindex$index[idx] > 3.1980 & sortindex$index[idx]<=4.4280){sortindex$Group[idx] <- 4}
          else{if(sortindex$index[idx] > 4.4280 & sortindex$index[idx]<=5.6960){sortindex$Group[idx] <- 5}
            else{if(sortindex$index[idx] > 5.6960 & sortindex$index[idx]<=7.1400){sortindex$Group[idx] <- 6}
              else{if(sortindex$index[idx] > 7.1400 & sortindex$index[idx]<=9.1983){sortindex$Group[idx] <- 7}
                else{if(sortindex$index[idx] > 9.1983 & sortindex$index[idx]<=12.0318){sortindex$Group[idx] <- 8}
                  else{if(sortindex$index[idx] > 12.0318& sortindex$index[idx]<=17.8582){sortindex$Group[idx] <- 9}
                    else{sortindex$Group[idx] <- 10}
                    
                }}}}
              
              }
          }
        }
    }
}

summaryindex <- aggregate(x = list("TE_by_group" = sortindex$Expenditure,
                                   "Trips_by_group" = sortindex$Number_of_Trips),
                          by = list("Group" = sortindex$Group),
                          FUN = sum
                          )

summaryindex$Revenue_per_trip <- summaryindex$TE_by_group/summaryindex$Trips_by_group

bar <- table(summaryindex$Revenue_per_trip, summaryindex$Group)

barplot(
          summaryindex$Revenue_per_trip,
          names.arg = summaryindex$Group,
          xlab = "deciles in the RFM index", ylab = "average expenditure",
          col = "pink",
          font.lab = 3,
          font.main = 4,
          main = "Average expenditure by each index group"
  
)

# # ===================================================
# GBA464: Assignment 4
# Author: Yufeng Huang
# Description: write a function that plots crimes 
#              occurance in Baltimore city
# Data: Baltimore crime data
# Source: https://data.baltimorecity.gov/
# ===================================================

# libraries
library(maps)
library(maptools)

# load data

url <- 'https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_4/baltimore_crime.csv'
if (!file.exists('baltimore_crime.csv')) download.file(url, 'baltimore_crime.csv')	# check and download
df <- read.csv('baltimore_crime.csv', stringsAsFactors = F)

# read the shape file
url_zip <- 'https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_4/school_distr/school_distr.zip'
if(!file.exists('school_distr.zip')) download.file(url_zip, 'school_distr.zip')     # download file as zip
unzip('school_distr.zip')   # unzip in the default folder
schdstr_shp <- readShapePoly('school.shp')  # read shape file
xlim <- schdstr_shp@bbox[1,]
ylim <- schdstr_shp@bbox[2,]

# transform dates and time, date into date variable, time into hours (24, minutes as fraction of hours)
df$date <- as.Date(df$CrimeDate, "%m/%d/%Y")
df$crimetime <- as.POSIXlt(strptime(df$CrimeTime, "%H:%M:%S"))
df$time <- df$crimetime$hour + df$crimetime$min/60
df$crimetime <- NULL
df$year <- as.numeric(format(df$date, "%Y"))
df$month <- as.numeric(format(df$date, "%m"))
df$day <- as.numeric(format(df$date, "%d"))

# split coordinates
get.coord <- function(var, pos) {
    coord <- as.numeric(
        sub('[\\(\\)]', '', 
            unlist(lapply(split.str, `[`, pos)) # equivalent: lapply(split.str, function(x) x[pos])
        )
    )
    coord
}
split.str <- strsplit(df$Location1, split = '\\,\\s')
df$latitude <- get.coord(split.str, 1)
df$longitude <- get.coord(split.str, 2)

# construct a plot function
#   you do not have to do this, but you could if we need to plot multiple things

plot.crime <- function(data, typelist = unique(data$Description), yearlist = (2011:2016), monthlist = (1:12), hourlist = (1:24), rgb.par = c(0, 0, 0), transparency = 1, ...) {
    subvec <- data$Description %in% typelist & data$year %in% yearlist & data$month %in% monthlist & floor(data$time) %in% hourlist 
    print(table(subvec))            # (optional) return this, so can export summary stats
    plot(schdstr_shp, xlim = xlim, ylim = ylim, axes = T, ...)    # first plot the outline 
    points(data$longitude[subvec], data$latitude[subvec], 
           pch = 16, cex = .5, col = rgb(rgb.par[1], rgb.par[2], rgb.par[3], transparency))   # marker look, size and color
}

# plot ASSAULT patterns, geographically and temporally
pdf("./assault.pdf",      # directory relative to the work directory
    width = 12, height = 16)      # size of figure (inch)
par(mfrow = c(2, 2))
for (i in seq(0, 18, by = 6)) {
    plot.crime(data = df, typelist = grep("ASSAULT", unique(df$Description), value = T), hourlist = i:(i+6), rgb.par = c(0.8, 0, 0), transparency = 0.1, main = paste("hour:", i, "-", i+6, sep = " "))
}
dev.off()

# Created by Lon Lieberman
# For Coursera, Course Optional Plotting Practice, Week 1, Reproducible Research
# Created on September 15th, 2016
# Created on a MAC OS 10.11.16 | RStudio v 0.99.902
# File name = **plottingPracticeR**
###############################################################################
# You should do the following:
# 
# Make a plot that answers the question:

# what is the relationship between mean covered 
# charges (Average.Covered.Charges) and mean total 
# payments (Average.Total.Payments) in New York?

# Make a plot (possibly multi-panel) that answers the question:

# how does the relationship between mean covered 
# charges (Average.Covered.Charges) and mean total 
# payments (Average.Total.Payments) vary by 
# medical condition (DRG.Definition) and the state in 
# which care was received (Provider.State)?

# Use only the base R graphics system (not ggplot2 or lattice) 
# to make your figure.
###############################################################################
# Library
library(dplyr)

# Set wd
setwd("~/Documents/R/Assignments/Data_Cleaning_Coursera/Week 4/
      Course Project 2/Reproducible Research")

# Import the preprocessed data.
# Raw data available at:
# https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
payments <- read.csv(file = "https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1474243200&Signature=JGgQQUivp~bo4uDUV9zIIyynJhgZVKTDqVbuFjHvCjlZrrluW3Tx96mRtWhn2iuIzl0fO541VBwinm69r0O~j-twUlaKyny~C6vYyc3cEczOGXg3J9Kl94LjXzZEm6FBc7aDGh~tyJDNOweEJKH9ZI85hXa0XaauH-QhlEaL4-s_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A")
###############################################################################
#                               PROBLEM 1
###############################################################################

# Look at the data & use dplyr to make life a bit easier
str(payments)
payments_df <- tbl_df(payments)

# Create New York subsets...
NY_payments_df <- payments_df %>%
        select(Hospital.Referral.Region.Description,Average.Covered.Charges, 
               Average.Total.Payments) %>%
        filter(grepl('NY', Hospital.Referral.Region.Description))

# summary(NY_payments_df$Average.Covered.Charges)  # Get mins and max for x axis
# summary(NY_payments_df$Average.Total.Payments)  # Get mins and max for y axis 

# Launch pdf and create file
pdf(file = "plot1.pdf")

plot(x = NY_payments_df$Average.Covered.Charges, 
     y = NY_payments_df$Average.Total.Payments,
     xlab = "Average Covered Charges",
     ylab = "Average Total Payments",
     xlim = c(4879, 134000),
     ylim = c(3330, 33050),
     main = "Relationship between Average Covered Charges & 
     Average Total  in New York State",
    col = 2,
    pch = 20,
    cex = 0.5)
M.Loess <- loess(Average.Total.Payments ~ Average.Covered.Charges,
                 data = NY_payments_df)
FIT <-fitted(M.Loess)
Ord1 <- order(NY_payments_df$Average.Covered.Charges)
lines(NY_payments_df$Average.Covered.Charges[Ord1], FIT[Ord1], 
      lwd = 2,
      lty = 2)

# Shut graphics device off
dev.off()

###############################################################################
#                               PROBLEM 2
###############################################################################
# Create a data set summarizing both charges and payments and grouping them by
# 'DRG.Definition (condition)' & state.

# Import data set  # set stringsAsFactors to FALSE for abbr DRG.Definition
pay<- read.csv(file = "https://d18ky98rnyall9.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1474243200&Signature=JGgQQUivp~bo4uDUV9zIIyynJhgZVKTDqVbuFjHvCjlZrrluW3Tx96mRtWhn2iuIzl0fO541VBwinm69r0O~j-twUlaKyny~C6vYyc3cEczOGXg3J9Kl94LjXzZEm6FBc7aDGh~tyJDNOweEJKH9ZI85hXa0XaauH-QhlEaL4-s_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A",
               stringsAsFactors = TRUE) 

# Import libraries
library(stringr)
library(dplyr)
library(reshape2)

# Modify data set for easier charting
pay_df <- tbl_df(pay)
DRG.melt <- melt(pay_df, id = c("DRG.Definition","Provider.State"),
                         measure.vars = c("Average.Covered.Charges", 
                                          "Average.Total.Payments"),
                        factorsAsStrings = TRUE)

ByState <- dcast(DRG.melt, Provider.State ~ variable, fun.aggregate = mean)

ByCondition <-dcast(DRG.melt, DRG.Definition ~ variable, fun.aggregate = mean)

# Launch pdf and create file
pdf(file = "plot2.pdf")

# Create 6 seperate plots showing medical conditions by state...
par(mfrow = c(2, 2), pty = "s")
barplot(ByState$Average.Total.Payments, 
        main = "Mean Payments by State",
        xlab = "STATE",
        ylab = "Mean Payments",
        names.arg = c("CA", "FL", "IL", "NY", "PA", "TX"),
        border = "blue",
        col = rainbow(6),
        density = c(10, 20, 30, 40, 50, 60))
                 
barplot(ByState$Average.Covered.Charges, 
        main = "Mean Covered Charges by State",
        xlab = "STATE",
        ylab = "Mean Covered Charges",
        names.arg = c("CA", "FL", "IL", "NY", "PA", "TX"),
        border = "blue",
        col = rainbow(6),
        density = c(10, 20, 30, 40, 50, 60))

barplot(ByCondition$Average.Total.Payments, 
        main = "Mean Payments by Condition",
        xlab = "Condition",
        ylab = "Mean Payments",
        names.arg = c("PNMA", "HRT", "DGST", "MISC",
                      "KDNY", "SPSS"),
        border = "blue",
        col = rainbow(6),
        density = c(10, 20, 30, 40, 50, 60))

barplot(ByCondition$Average.Covered.Charges, 
        main = "Mean Charges by Condition",
        xlab = "Condition",
        ylab = "Mean Covered Charges",
        names.arg = c("PNMA", "HRT", "DGST", "MISC",
                      "KDNY", "SPSS"),
        border = "blue",
        col = rainbow(6),
        density = c(10, 20, 30, 40, 50, 60))
# Shut Graphics Device
dev.off()






         
#----------------------------------------------
# test_code.R: Introdcution to some basic R commands
#----------------------------------------------

#-------------------------------
# simple calculation
#-------------------------------
5 + 3  # space does not matter
5-3
5/3
2^3
5*(10-3)
sqrt(4)

#-------------------------------
# defining objects
#-------------------------------
result <- 5+3 # "<-" is an assignment operator
result2 = 5+3 # "=" is also an assignment operator (both are fine)
ls()          # list all of the objects

## show `result`
result
print(result)
result/5

## R is case sensitive, so the following gives you an error
Result

## We can assign letter characters to an object with " "
instructor <- "Nobu"
instructor

instructor <- "Nobuyuki Kanazawa" 
instructor # when you use " ", space is preserved

Result <- "5"
Result   # here, 5 is recognized as a character, not a number
Result/5

class(result)
class(Result)

#----------------------------------------------
# Working with Data
#----------------------------------------------
## setting up the working directory
getwd() # get working directory
setwd("") # set working directory

## import dataset, analyze the data
resume = read.csv("resume.csv")  # read csv dataset

names(resume)  # name of variables in the dataset
nrow(resume)   # number of observation in the dataset
ncol(resume)   # number of variable in the dataset
dim(resume)    # nrow and ncol
summary(resume) # summarize the dataset

summary(resume$call) # using "$", you can summarize one variable

attach(resume) # after you attach a dataset, you don't have to specify the dataset.
summary(call) # you can directly name variable that you want to summarize after you attach a dataset

mean(call) # mean of a variable
var(call)  # variance of a variable
sd(call)   # standard deviation
cov(call,black)  # covariance
cor(call,black)  # correlation

## conditional mean
mean(call[black == 1]) # mean of callback if black == 1
mean(call[black == 0]) # mean of callback if black == 0

## Let's construct t-stat for difference in means
b_mean <- mean(call[black == 1]) # mean for black-sounding names
b_sd   <- sd(call[black == 1]) # mean for black-sounding names
b_n    <- sum(black==1)
w_mean <- mean(call[black == 0]) # mean for white-sounding names
w_sd   <- sd(call[black == 0]) # mean for black-sounding names
w_n    <- sum(black==0)

t <- (b_mean-w_mean)/sqrt(b_sd^2/b_n+w_sd^2/w_n)
t
abs(t)


#----------------------------------------------
# plotting results
#----------------------------------------------
plot(yearsexp, ofjobs)
plot(yearsexp, ofjobs,
     xlab = "years of experience",
     ylab = "number of jobs",
     main = "experience and jobs")
dev.off()
jpeg('exp_jobs.jpg') # you can also save the plot in jpeg or png
# png('exp_jobs.png')

#----------------------------------------------
# creating tables
#----------------------------------------------
table(black)     # create table
table(black,education) # cross tabulation
table(black = black, educ = education) # you can label the variables on table
tab.black.educ <- table(black = black, educ = education)
tab.black.educ
# addmargins(tab.black.educ)


#----------------------------------------------
# some other useful commands:
#----------------------------------------------
rm(list = ls())  # erase all objects from the current workplace
source(test_code.R) # run all the commands in the code "test_code.R" 
                    # without showing the commands in the consoles
install.packages("AER") # install package for later
install.packages("systemfit") # install package for later
install.packages("devtools")
install.packages("tidyverse")

library(AER)
library(systemfit)


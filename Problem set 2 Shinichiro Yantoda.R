setwd("C:/Users/Asus/Documents/Econometric File R")
data = read.csv("wage2.csv")
attach(data)

#Problem set 2 Question 1 
#1.A
mean(IQ)
mean(wage)
sd(IQ)
#1.B
ols_result = lm(wage ~ IQ, data = data)  
summary(ols_result)
#1.C
ols_result2 = lm(log(wage) ~ IQ, data = data)  
summary(ols_result2)

#Problem set 2 Question 
setwd("C:/Users/Asus/Documents/Econometric File R")
meap93 = read.csv("meap93.csv")
attach(meap93)

#2(A)
ols_result1 = lm(math10 ~ expend,data = meap93)
summary(ols_result1)

#2(d)
ols_result2 = lm(math10 ~ log(expend),data = meap93)
summary(ols_result2)

#2(g)
plot(log(expend),math10)
abline(ols_result2) 

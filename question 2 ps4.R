setwd("C:/Users/Asus/Documents/Econometric File R")
data2 = read.csv('crime1.csv')
attach(data2)

#2.A 
data2$arr86 = as.numeric(narr86 >= 1)

ols_ques2 = lm(arr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86, data = data2)
summary(ols_ques2)

#robust 
library(sandwich)
library(AER)

vcv <- vcovHC(ols_ques2, type = "HC1")
coeftest(ols_ques2, vcv)  # robust-inference

u2 <- resid(ols_ques2)^2
test_het <- lm(u2 ~ pcnv + avgsen + tottime + ptime86 + qemp86)
test_het_r<- lm(u2 ~ 1)
anova(test_het,test_het_r)

#Conclusion = because the we can reject the null, the model is heteroskedacity 

#2.C
fval <- fitted(ols_ques2)
fval_sqre <- fitted(ols_ques2)^2

test_het_f <- lm(fval_sqre ~ pcnv + avgsen + tottime + ptime86 + qemp86, data = data2)
test_het_r <- lm(fval_sqre ~ 1, data = data2)
anova(test_het_f,test_het_r)


#3
setwd("C:/Users/Asus/Documents/Econometric File R")
data3 = read.csv('wage2.csv')
attach(data3)

#3.A
ols_ques3 = lm(wage ~ educ + IQ, data = data3)
summary(ols_ques3)

#3.B
n <- nrow(data3)
nu <- rnorm(n,0,1)
v <- rnorm(n,0,1)

#3.C

ols_ques3_c = lm(wage_star ~ educ + IQ, data = data3)
summary(ols_ques3_c)

#regress on y classical error, so not many changes

#3.D
data3$educ_star = educ + v 
ols_ques3_d = lm(wage ~ educ_star + IQ, data = data3)
summary(ols_ques3_d)

#regress on x even with classical eror there is bias
# the coeficient will be incosist ent
#because both b1 and b2 is plus, it will be a downward bias toward 0


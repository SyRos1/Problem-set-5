#Shinichiro Yantoda 1906902
#Question 1
setwd("C:/Users/Asus/Documents/Econometric File R")
data1= read.csv("injury.csv")
attach(data1)

#Question 1.A
ols_q1A <- lm(ldurat ~ afchnge, data=subset(data1, ky==1 & highearn==1))
summary(ols_q1A)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.38209    0.03698  37.373  < 2e-16 ***
#afchnge      0.19826    0.05310   3.733 0.000193 ***

#Multiple R-squared:  0.005793
# the assumption needed is that in the absence of policy there will be no changes in the duration for high income group and low income group

#Question 1.B
ols_q1B <- lm(ldurat ~ highearn + afchnge + highearn*afchnge, data=subset(data1, ky==1))
summary(ols_q1B)


#Coefficients:
#                  Estimate Std. Error   t-value Pr(>|t|)    
#(Intercept)       1.125615   0.030737  36.621  < 2e-16 ***
# highearn         0.256479   0.047446   5.406 6.72e-08 ***
# afchnge          0.007657   0.044717   0.171  0.86404    
#highearn:afchnge  0.190601   0.068509   2.782  0.00542 ** 

#For the estimator to be unbiased, we need to have the parallel trend assumption,
#which means that in the absence of policy the change in duration of move on a same direction for high earner and low earner people. 

# b0(a)= b0(b)+b1(b)
# 1.38209 = 1.125615 + 0.256479
# b1(a)= b2(b)+b3(b)
# 0.19826 = 0.007657 + 0.190601

#Question 1.c
ols_q1C = lm(ldurat ~ highearn + afchnge + highearn*afchnge + male + married + factor(indust) + factor(injtype), data=subset(data1, ky==1))
summary(ols_q1C)

#                   Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)         0.57135    0.10266   5.565 2.74e-08 ***
#  highearn          0.17576    0.05175   3.397 0.000687 ***
#  afchnge           0.01063    0.04492   0.237 0.812973    
#  male             -0.09794    0.04455  -2.198 0.027959 *  
#  married           0.12210    0.03912   3.121 0.001812 ** 
#  factor(indust)2   0.27087    0.05867   4.617 3.98e-06 ***
#  factor(indust)3   0.16067    0.04090   3.928 8.67e-05 ***
#  factor(injtype)2  0.78381    0.15617   5.019 5.36e-07 ***
#  factor(injtype)3  0.33536    0.09234   3.632 0.000284 ***
#  factor(injtype)4  0.64035    0.10087   6.348 2.36e-10 ***
#  factor(injtype)5  0.50530    0.09281   5.445 5.42e-08 ***
#  factor(injtype)6  0.39361    0.09356   4.207 2.63e-05 ***
#  factor(injtype)7  0.78661    0.20703   3.800 0.000147 ***
#  factor(injtype)8  0.51390    0.12928   3.975 7.13e-05 ***
#  highearn:afchnge  0.23088    0.06952   3.321 0.000904 ***


#when factor is controlled, the coefficient increases. (highern*afchnge increase)

#Yes, it is statistically significant

#Question 1.D
#small r-squared means that the independent variable is not explaining much in the variation of the dependent variable 
 #  The small r-squared does not means that the model is completely useless, it just that the model could not explain everything that will affect on y. 
 #  and for this case, this small part that is able to explain the y is still a very important thing to understand.

#Question 1.E
ols_q1E <- lm(ldurat ~ highearn + afchnge + highearn*afchnge, data=subset(data1, mi==1 ))
summary(ols_q1E)
#                   Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)        1.41274    0.05672  24.908   <2e-16 ***
#highearn           0.16914    0.10557   1.602    0.109    
#afchnge            0.09738    0.08479   1.149    0.251    
#highearn:afchange  0.19199    0.15417   1.245    0.213    


# Michigan is not statistically significant because we fail to reject highearn*afchnge
# Yes, it is geographically significant

#Question 2
data2 = read.csv("scholarship.csv")
attach(data2)
 
#Quedtion 2.A
data2$D <- as.numeric(inc<=400)

#Question 2.B
ols_q2B = lm(GPA ~ inc + D, data = data2)
summary(ols_q2B)

#Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)   1.0490020  0.1198762   8.751  < 2e-16 ***
# inc          0.0016115  0.0001742   9.251  < 2e-16 ***
# D            0.3826289  0.0842328   4.543 6.98e-06 ***

#t = 0.3626289
#standard errors: 0.0842328
#interpretation of t = capture the jump in the policy effect before and after the cutoff line (cutoff line = 4 million)

#Question 2.C
fitted_values <-fitted(ols_q2B)
cutoff_value_for_scholarship = 400
plot(inc, GPA)
lines(inc [inc < 400],fitted_values[inc<400], col="blue", lwd = 2)
lines(inc [inc > 400],fitted_values[inc> 400], col="blue", lwd = 2) 
abline(v =cutoff_value_for_scholarship, lwd = 2, lty = "dashed") 

#Question 2.D
ols_q2D = lm(GPA ~  inc*(1-D) + inc*D + D, data = data2)
summary(ols_q2D)

#Coefficients:
#              Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)  1.0753116  0.1208152   8.900   <2e-16 ***
#  inc        0.0015721  0.0001756   8.950   <2e-16 ***
#  D         -0.2741472  0.4193488  -0.654    0.514    
# inc:D       0.0020036  0.0012533   1.599    0.111    

#t: -0.2741472 (the tau is different)

#Question 2.D
fitted_values2 <-fitted(ols_q2D)
plot(inc, GPA)
lines(inc [inc < 400],fitted_values2[inc<400], col="blue", lwd = 2)
lines(inc [inc > 400],fitted_values2[inc>400], col="blue", lwd = 2)
abline(v = cutoff_value_for_scholarship, lwd = 2, lty = "dashed") 


#Q2e
inc_squared <-inc^2
ols_q2E = lm(GPA ~  inc*(1-D) + inc_squared*(1-D) + inc*D + inc_squared*D+ D, data = data2)
summary(ols_q2E)

#Coefficient
#                 Estimate  Std. Error t value Pr(>|t|)  
#(Intercept)      4.113e-01  5.680e-01   0.724   0.4693  
#inc              3.669e-03  1.761e-03   2.083   0.0378 *
#inc_squared     -1.560e-06  1.303e-06  -1.196   0.2321  
#D               -1.327e+00  3.244e+00  -0.409   0.6826  
#inc:D            1.087e-02  2.035e-02   0.534   0.5934  
#D:inc_squared   -1.566e-05  3.180e-05  -0.492   0.6226  

#t = -1.327e+00 

#Question 2.F
fitted_values3 <-fitted(ols_q2E)
plot(inc, GPA)
lines(inc [inc < 400],fitted_values3[inc<400], col="blue", lwd = 2)
lines(inc [inc > 400],fitted_values3[inc>400], col="blue", lwd = 2)
abline(v = cutoff_value_for_scholarship, lwd = 2, lty = "dashed") 

#Question 2G
hist(inc, breaks = 50) 
# by seeing from the histogram, there is not much changes people that goes after 400 line to before 400 line. 
# If the 2nd assumption of RDD is violated, there will be a situation where the people in income a
# above 400 can change into the treatment group. so the bins before 400 should have a higher box. and if that happen means that the bins is higher. 



setwd("C:/Users/Asus/Documents/Econometric File R")
data1 = read.csv("ps4.csv")
attach(data1)

#1.A
data1$exper = age - years_ed - 6

#1.B
data1$White <- as.numeric(race == 1)
data1$Black <- as.numeric(race == 2)
data1$American_Indian <- as.numeric (race == 3)
data1$Asian_or_Pasific_Islander <- as.numeric (race == 4)
data1$Other <- as.numeric(race == 5)

#1.C
data1$male <- as.numeric(gender == 1)
data1$female <- as.numeric(gender == 2)
data1$exper_sqre <- as.numeric( exper ^ 2)
ols_result1 = lm(ln_ahe ~ female + White + Black + American_Indian + 
                   Asian_or_Pasific_Islander +Other + exper + exper_sqre + years_ed, data = data1)
summary(ols_result1)

#model
# log hourly wage = 7.473e-01 -3.011e-01female +8.193e-02white -4.043e-02Black -1.571e-02American_Indian +6.969e-02Asian_pacific_islander +2.712e-02exper -4.112e-04exper_sqre + 9.699e-02years_ed
#rsquared = 0.191
#1.c part 2
exper[which.max(yhat)]

#highest number is 33
#thus exper = age - year_ed -6, 33 = age - 12 -6
#age = 51 years old

#1.D 
yhat = coef(ols_result1)[1] + coef(ols_result1)[2]*1 + coef(ols_result1)[3]*1 + coef(ols_result1)[8]*exper + coef(ols_result1)[9]*exper_sqre + coef(ols_result1)[10]*12 
plot(exper,yhat) 

#1.E
#unrestricted 
ols_result2 = lm(ln_ahe ~ female + White + Black + American_Indian + 
                   Asian_or_Pasific_Islander + Other + exper_sqre + exper + years_ed + female*exper + female*exper_sqre, data = data1)
summary(ols_result2)

#restricted c
ols_result1

#ftest 
anova(ols_result1,ols_result2)

#ftest = 45.444
#pvalue = 2.2e-16, we can reject the null

library(sandwich)
library(AER)

#1.F
#unrestricted
ols_result3 = lm(ln_ahe ~ female + White + Black + American_Indian + Asian_or_Pasific_Islander + 
                   exper + exper_sqre + years_ed + White*years_ed + Black*years_ed + American_Indian*years_ed + 
                               Asian_or_Pasific_Islander*years_ed, data = data1)
summary(ols_result3)

#restricted 
ols_result1

#ftest
anova(ols_result3,ols_result1)

#ftest = 2.4231 
#pvalue = 0.04599, we can reject the null

#1.G
data1$fe_whi<-White*female
data1$fe_bla <- female * Black
data1$fe_ameri <- female * American_Indian
data1$fe_asia <-  female * Asian_or_Pasific_Islander

#there will 4 new variable added into the model

ols_5 = lm(ln_ahe ~ female + White + Black + American_Indian + Asian_or_Pasific_Islander + exper + exper_sqre + years_ed +  female*White + female*Black + female*American_Indian + female*Asian_or_Pasific_Islander , data = data1)
summary(ols_5)
# ln_ahe = 8.352e-01 -5.002e-01female +5.477e-04white  -1.973e-01Black -1.951e-02Asian_Indian -6.709e-02_Asianorpasicificislander +2.705e-02 exper -4.084e04exper_sqre +9.700e-02years_ed +1.856e-01femalewhite + 3.227e-01femaleblack + 3.947e-02femaleamericanindian + 3.042e-01femaleasianoracificislander

anova(ols_result1, ols_5)
# ftest = 5.162, plavue = 0.0003741

white_male = exp(coef(ols_5)[1]*1+coef(ols_5)[3]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*49 + coef(ols_5)[9]*12) 
#white_male = 8.750657 
white_female = exp(coef(ols_5)[1]*1 + coef(ols_5)[2]*1 +  coef(ols_5)[3]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*7^2 + coef(ols_5)[9]*12 + coef(ols_5)[10]*1) 
#white_female = 6.388717 
black_male = exp(coef(ols_5)[1]*1+coef(ols_5)[4]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*49 + coef(ols_5)[9]*12) 
#black_male = 7.180174 
black_female = exp(coef(ols_5)[1]*1 + coef(ols_5)[2]*1 +  coef(ols_5)[4]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*7^2 + coef(ols_5)[9]*12 + coef(ols_5)[11]*1) 
#black_female = 6.012063 
AmericanIndian_male = exp(coef(ols_5)[1]*1+coef(ols_5)[5]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*49 + coef(ols_5)[9]*12) 
#American or Indian male = 8.576913 
AmericanIndian_female =  exp(coef(ols_5)[1]*1 + coef(ols_5)[2]*1 +  coef(ols_5)[5]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*7^2 + coef(ols_5)[9]*12 + coef(ols_5)[12]*1) 
#AMerican or Indian female = 5.410453 
AsianPacific_male =exp(coef(ols_5)[1]*1+coef(ols_5)[6]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*49 + coef(ols_5)[9]*12) 
#Asian pacific male = 8.178316 
AsianPacific_female = exp(coef(ols_5)[1]*1 + coef(ols_5)[2]*1 +  coef(ols_5)[6]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*7^2 + coef(ols_5)[9]*12 + coef(ols_5)[13]*1)
#Asian pacific female = 6.722934 
Other_male = exp(coef(ols_5)[1]*1+coef(ols_5)[7]*7+ coef(ols_5)[8]*49 + coef(ols_5)[9]*12) 
#other_male = 8.745865 
Other_female = exp(coef(ols_5)[1]*1 + coef(ols_5)[2]*1 + coef(ols_5)[7]*7+ coef(ols_5)[8]*7^2 + coef(ols_5)[9]*12) 
#other female = 5.303525
  

white_male
white_female
black_male
black_female
AmericanIndian_male
AmericanIndian_female
AsianPacific_male
AsianPacific_female
Other_male
Other_female

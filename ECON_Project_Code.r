setwd("D:/D/UTA Studies/UTA_Sem_4/ECON-5336/Project")

#install.packages("dummies")
library("dummies")
library("e1071")

#install.packages("fmsb")
library("fmsb")
library("lmtest")
library("het.test")

#install.packages("het.test")


options(scipen=999)

data = read.csv("HR_comma_sep.csv")


plot(data)

summary(data)

hist(data$satisfaction_level) # Dependent Variable 

# Independent Variable
hist(data$last_evaluation)
hist(data$number_project)
hist(data$average_montly_hours)
hist(data$time_spend_company)
hist(data$Work_accident)
hist(data$left)
hist(data$promotion_last_5years)
hist(data$sales) 
hist(dummy(data$salary))


#Checking skewness of Data

skewness(data$last_evaluation)
skewness(data$number_project)
skewness(data$average_montly_hours)
skewness(data$time_spend_company)
skewness(data$Work_accident)
skewness(data$left)
skewness(data$promotion_last_5years)
skewness(dummy(data$sales) )
skewness(dummy(data$salary))


#Dependent Variable
y = data$satisfaction_level
y[1:10]

colnames(data)
x1  = data$last_evaluation
x2 = data$number_project
x3 = data$average_montly_hours
x4 = data$time_spend_company
x5 = data$Work_accident
x6 = data$left
x7 = data$promotion_last_5years

#Create Dummies
x8 = data$sales
x8 = dummy(x8)
x8

x9 = data$salary
x9 = dummy(x9)
x9
corre = cor(data.frame(x1 , x2 , x3, x4 , x5 , x6, x7,x8,x9,y))

heatmap(cor(x7))
heatmap(cor(x8))
heatmap(corre)



#Running Regression For Different Models
res = lm(y~x1 + x2 + x3 + x4 + x5 + x6+ x7+x8+x9) # Selected
summary(res)
VIF(res)  # 
bptest(res)
plot(res)


res2 = lm(log(y)~x1 + x2 + x3 + x4 + x5 + x6+ x7+x8+x9) # Selected
summary(res2)
VIF(res2)
bptest(res2)
plot(res2)


res3 = lm(y~log(x1) + x2 + x3 + x4 + x5 + x6+ x7+x8+x9)
summary(res3)
VIF(res3)
plot(res3)

res4 = lm(log(y)~log(x1) + x2 + x3 + x4 + x5 + x6+ x7+x8+x9)
summary(res4)
VIF(res4)
plot(res4)


res5 = lm(log(y)~x1 + x2 + x3 + log(x4) + x5 + x6+ x7+x8+x9)
summary(res5)
VIF(res5)
plot(res5)



res6 = lm(log(y)~x1 + x2 + x3 + x4 + x5 + x6 )
summary(res6)
VIF(res6)
plot(res6)


x1_sq = x1^2
res7 = lm(log(y)~x1 + x1_sq + x2 + x3 + x4 + x5 + x6+ x7+x8+x9) # Selected
summary(res7)
VIF(res7)
bptest(res7)
plot(res7)

x4_sq = x4^2
res11 = lm(log(y)~x1 + x1_sq + x2 + x3 + x3_sq + x4+ x4_sq + x5 + x6+ x7+x8+x9) # Selected
summary(res11)
VIF(res11)
bptest(res11)
plot(res11)


res12 = lm(y~x1 + x1_sq + x2 + x3 + x3_sq + x4+ x4_sq + x5 + x6+ x7+x8+x9) # Selected
summary(res12)
VIF(res12)
bptest(res12)
plot(res12)

x3_sq = x3^2
res8 = lm(log(y)~x1 + x1_sq + x2 + x3 + x3_sq + x4 + x5 + x6+ x7+x8+x9) # Selected
summary(res8)
VIF(res8)
bptest(res8)
plot(res8)



res9 = lm(log(y)~log(x1) + x2 + x3 + x3_sq + x4 + x5 + x6+ x7+x8+x9)
summary(res9)
VIF(res9)
plot(res9)

res10 = lm(log(y)~log(x1) + x2 + log(x3) + x4 + x5 + x6+ x7+x8+x9)
summary(res10)
VIF(res9)
plot(res10)


x3_sq = x3^2
res12 = lm(log(y)~x1 + x1_sq + x2 + x3 + x3_sq + x4 +x4_sq + x7+x8+x9) # Selected
summary(res12)
VIF(res12)
bptest(res12)

res13 = lm(log(y)~x1 + x1_sq + x2 + x3 + x3_sq + x4 +x4_sq + x7+x8) 
summary(res13)
VIF(res13)
bptest(res13)



bptest(res)
bptest(res2)
bptest(res3)
bptest(res4)

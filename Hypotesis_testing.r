install.packages("readr")
installed.packages("psych")
Test=read.csv(file.choose())
head(Test)
library(readr)
# 1 sample test
t.test(Test$BP, mu=120)
t.test(Test$Age,mu=35)
t.test(Test$Age,mu=45)
t.test(Test$Chlstrl,mu = 150,alternative="less")
t.test(Test$Chlstrl,mu = 200,alternative="less", conf.int=0.99, conf.level = 0.99)
t.test(Test$Age,mu = 40,alternative="greater")

# 2 sample test
#independent sample test
t.test(Test$BP~Test$AnxtyLH)
head(Test)
t.test(Test$BP~Test$DrugR, alternative  = "less")
t.test(Test$Age~Test$Prgnt, alternative = "greater")

#Paired Sample  t test
Grades = read.csv(file.choose())
head(Grades)
t.test(Grades$quiz1)
t.test(x = Grades$quiz1, y = Grades$quiz2, paired = 1)
mean(Grades$quiz1)
mean(Grades$quiz2)

mean(Test$BP[Test$Prgnt=="0"])
mean(Test$BP[Test$Prgnt=="1"])

sd(Test$Age[Test$Prgnt=="0"])
sd(Test$Age[Test$Prgnt=="1"])

mean(Test$Age[Test$Prgnt=="1"])
mean(Test$Age[Test$Prgnt=="0"])


#Chi Square test
head(Test)
chisq.test(Test$Prgnt,Test$AnxtyLH)

#ANOVA
Sales=read.csv(file.choose())
head(Sales)
mean(Sales$sales)

A1=aov(Sales$sales~Sales$manufact)
A1
summary(A1)

A1=aov(Sales$width~Sales$manufact)
summary(A1)
mean(Sales$width)
A1

#Multiple Linear Regression
install.packages("usdm")
library(usdm)
library(sp)
library(raster)
DF1=data.frame(Sales$price,Sales$engine_s,Sales$horsepow,Sales$wheelbas,
               Sales$width,Sales$length,Sales$curb_wgt,Sales$fuel_cap,
               Sales$mpg,Sales$resale)
cor(DF1)

vif(DF1[,1:9])
ResultsLM = lm(resale ~ price + wheelbas + width + length + fuel_cap + mpg, 
               data=Sales)
summary(ResultsLM)
ResultsLM

ResultsLM = lm(resale ~ price + length, data=Sales)
ResultsLM
summary(ResultsLM)

#######################################3
newSales = read.csv(file.choose())
head(newSales)
cor(newSales)

ResultsLM2 = lm(Sales ~ Promotion + Demand, data=newSales)
ResultsLM2
summary(newSales)
summary(ResultsLM2)

ResultsLM3 = lm(Sales ~ Demand + Price_lakhs, data=newSales)
summary(ResultsLM3)

ResultsLM4 = lm(Sales ~ Promotion + Demand + Price_lakhs + Demand:Price_lakhs, data=newSales)
summary(ResultsLM4)
vif(newSales[,2:4])


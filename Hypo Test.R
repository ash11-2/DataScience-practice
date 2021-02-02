# One sample t test
library(readr)
cs2m=read.csv(file.choose())
#("C:/Users/hp/Desktop/R Intro/Hypothesis_testing/cs2m.csv")
View(cs2m)
# Ho: mu age = 40
t.test(cs2m$BP, mu = 115) # accept Ho
t.test(cs2m$Chlstrl, mu = 172) 
#--------Paired sample t test

library(readr)
grades=read.csv(file.choose())
View(grades)
t.test(x=grades$quiz1, y=grades$quiz2, alternative = "two.sided", mu =0, paired = T)
t.test(grades$quiz1, grades$quiz2, paired = T) # THIS IS MORE EASY
boxplot(grades$quiz1, col = "red", main = "Quiz 1")
boxplot(grades$quiz2, col = "blue", main = "Quiz 2")
#----------------Independent samples t test

t.test(cs2m$BP~cs2m$AnxtyLH) # assuming unequal variance
mean(cs2m$BP[cs2m$AnxtyLH == "0"]) # 117.8125
mean(cs2m$BP[cs2m$AnxtyLH == "1"]) # 138.2143
tapply(cs2m$BP, cs2m$AnxtyLH, mean)
sd(cs2m$BP[cs2m$AnxtyLH == "0"]) # 19.83001
sd(cs2m$BP[cs2m$AnxtyLH == "1"]) # 21.71557
tapply(cs2m$BP, cs2m$AnxtyLH, sd)
install.packages("psych")
library(psych)
tapply(cs2m$BP, cs2m$AnxtyLH, skew)
boxplot(cs2m$BP~cs2m$AnxtyLH, col = heat.colors(2), main = "Boxplot of BP")
str(cs2m) # AnxtyLH is integer
boxplot(cs2m$BP~cs2m$AnxtyLH, col = topo.colors(2))

boxplot(cs2m$BP~cs2m$Prgnt, col = topo.colors(2))
tapply(cs2m$BP, cs2m$Prgnt, mean)
tapply(cs2m$BP, cs2m$Prgnt, median)
boxplot(cs2m$Age~cs2m$Prgnt, col = heat.colors(2))
boxplot(cs2m$BP~cs2m$DrugR, col = heat.colors(2), main = "BP VS dRUGr")
boxplot(cs2m$BP~cs2m$AnxtyLH, col = topo.colors(2))

#-----------------One way ANOVA

library(readr)
salescity <- read_csv("C:/Users/hp/Desktop/R Intro/Hypothesis_testing/salescity.csv")
View(salescity)
str(salescity)
dim(salescity)
summary(salescity)
library(psych)
describe(salescity)
hist(salescity$Sales, col = "red")
boxplot(salescity$Sales, col = "red", horizontal = T, main = "Boxplot of Sales")
boxplot(Sales~City, data = salescity, col= heat.colors(4))
boxplot(Sales~City, data = salescity, col= topo.colors(4))

mod<- anova(lm(Sales~City, data = salescity))
mod
k<- aov(Sales~City, data = salescity)
k
summary(k)


tapply(salescity$Sales, salescity$City, mean)
# A = 22, B = 19.58, C = 17.67, D = 20.58
GrandMean<- mean(tapply(salescity$sales, salescity$city, mean))
GrandMean # 19.96
A<-6*(22-19.96)^2
A
B<-6*(19.58-19.96)^2
B
C<-6*(17.67-19.96)^2
C
D<-6*(20.58-19.96)^2
D
SSC<- sum(A,B,C,D)
SSC # 59.607


#-----ethnicity vs final
anovaFinal<- anova(lm(final~ethnicity, data = grades))
anovaFinal
boxplot(final~ethnicity, data = grades, col= heat.colors(6))
boxplot(final~ethnicity, data = grades, col= topo.colors(6))
is.factor(grades$ethnicity)
str(grades$ethnicity)

boxplot(gpa~ethnicity, data = grades, col= heat.colors(6))
anovaGPA<- anova(lm(gpa~ethnicity, data = grades))
anovaGPA

boxplot(gpa~section, data = grades, col= heat.colors(6))
anovaSECTION<- anova(lm(gpa~section, data = grades))
anovaSECTION

boxplot(final~section, data = grades, col= topo.colors(6))
anovaFINAL_section<- anova(lm(final~section, data = grades))
anovaFINAL_section

which(grades$passfail == "O") # O Should be capital;case=82, DON PRADO
#----------cHI SQUARE

chisq.test(cs2m$AnxtyLH, cs2m$DrugR)
# CHIINV(0.05,1) = 3.84


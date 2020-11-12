carsales=read.csv(file.choose())
head(carsales)
library(usdm)
library(sp)
library(raster)
variables = data.frame(carsales[,7:14])
head(variables)
cor(variables)
vif(variables)
vif(variables[,2:7])
variables = data.frame(variables$horsepow,
                       variables$wheelbas,
                       variables$width,
                       variables$curb_wgt,
                       variables$fuel_cap,
                       variables$mpg)
vif(variables)
variables1 = data.frame(variables$variables.horsepow,
                       variables$variables.wheelbas,
                       variables$variables.width,
                       variables$variables.fuel_cap,
                       variables$variables.mpg)


vif(variables[,1:4])
head(variables1)
Results = lm(variables1$variables.variables.mpg ~
               variables1$variables.variables.horsepow +
               variables1$variables.variables.wheelbas +
               variables1$variables.variables.width +
               variables1$variables.variables.fuel_cap)
summary(Results)          

Results1 = lm(variables1$variables.variables.mpg ~
                variables1$variables.variables.horsepow +
                variables1$variables.variables.fuel_cap +
                factor(carsales$type))             
summary(Results1)             
             
#Logistic Regression
#########################################
binary = read.csv(file.choose())
head(binary)
Res=glm(binary$admit~binary$gre+
          binary$gpa+
          binary$rank, family="binomial")       
summary(Res)             

head(carsales)
Res1 = glm(factor(carsales$type) ~ carsales$mpg +
             carsales$horsepow +
             carsales$fuel_cap, family = "binomial")
summary(Res1)

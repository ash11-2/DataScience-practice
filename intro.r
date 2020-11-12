print("Hello World")
4+5
4*4+log(125)
print.Date(5,max = NULL)
print.Date(5,max = 10)
x = c(1,2,3,4,5)
y = c("a","b","c","d","e")
table = data.frame(x,y)
z = c(1,2,3,4,5)
table1 = data.frame(x,y,z)
table[1:3,1]
table[1:4,1]
table[1:3,1:2]
table[2:3,1:2]
table[,2]
iris = read.csv(file.choose())
head(iris)
tail(iris)
View(iris)
mean(iris$Sepal.Length)
sd(iris,FALSE)
sd(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
var(iris$Sepal.Length)
range(iris$Sepal.Length)
IQR(iris$Sepal.Length)
mean(iris$Sepal.Width)
sd(iris$Sepal.Width)
min(iris$Sepal.Width)
var(iris$Sepal.Width)
range(iris$Sepal.Width)
IQR(iris$Sepal.Width)
summary(iris$Sepal.Length)
iris[1:10,1:4]
dim(iris)

#Ejemplo 1
data.Ex1<- read.table("http://waveland.com/Glover-Mitchell/Example10-1.txt", header = TRUE)
tail(data.Ex1)
plot(data.Ex1$Temperature,data.Ex10.1$HrtRt, main = "Heart Rate
     vs Temperature", xlab = "Temperature(C)", ylab = "BPM")
lm.Ex1 <- lm(HrtRt~Temperature,data = data.Ex1)
lm.Ex1 #Imprimir los resultados 

plot(data.Ex1$Temperature,data.Ex10.1$HrtRt, main = "Heart Rate
     vs Temperature", xlab = "Temperature(C)", ylab = "BPM")
abline(lm.Ex1,col= "red")

aov.Ex1<-aov(HrtRt~Temperature,data = data.Ex1)
summary(aov.Ex1)

#Ejemplo 3

data.Ex3 <- read.table("http://waveland.com/Glover-Mitchell/Example10-5.txt", header = TRUE)
cor.test(data.Ex3$AfterCutOff, data.Ex3$Students, method = "kendall",exact = FALSE , conf.level = 0.95)

#Ejemplo 5
binom.test(x=15,n=20, alternative = "greater")

#Ejemplo 7
binom.test(x=169,n=480, p = 0.304, alternative = "greater")

#Ejemplo 9
x <- c(171,179)
n <- c(1082,1084)
source("http://waveland.com/Glover-Mitchell/z.prop.txt")

z.prop.test(x,n, alternative = "less")

#Ejemplo 11
mu.est <- round(159/98, digits=3)
mu.est

dpois(2,mu.est)

dpois(c(0:5),mu.est)

ppois(5,mu.est, lower.tail = FALSE)

expected.proportions <- c(dpois(c(0:5),mu.est),ppois(5,mu.est, lower.tail = FALSE))
expected.proportions

expected.values <- 98*expected.proportions
expected.values

expected <- c(dpois(c(0:3),mu.est), ppois(3,mu.est, lower.tail = FALSE))
expected

observed <- c(18,34,24,16,6)

source("http://waveland.com/Glover-Mitchell/intrinsic.chisq.txt")

intrinsic.chisq.test(x = observed, p = expected, est.params = 1)
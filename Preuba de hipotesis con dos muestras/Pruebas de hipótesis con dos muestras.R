## Pruebas de hipótesis con dos muestras
## Ejemplo 1
data.Ex1 <- read.table("http://waveland.com/Glover-Mitchell/Example07-1.txt", header = TRUE)
var.test(data.Ex1$A, data.Ex1$B)

## Ejemplo 3
t.test(data.Ex1$A, data.Ex1$B, var.equal = TRUE)

## Ejemplo 5
source("http://waveland.com/Glover-Mitchell/t.test2.txt")
source("http://waveland.com/Glover-Mitchell/f.test2.txt")
f.test2(sx = 40,nx = 15, sy=15,ny = 10)
t.test2(mx = 115, sx = 40, nx = 15, my = 135, sy = 15 ,ny =10, 
        alternative = "less", var.equal = FALSE)

##Ejemplo 7
data.Ex7 <- read.table("http://waveland.com/Glover-Mitchell/Example08-1.txt", header = TRUE)
data.Ex7

aov.Ex7 <- aov(Lifespan ~ Diet, data = data.Ex7)
summary(aov.Ex7)

tapply(data.Ex7$Lifespan, data.Ex7$Diet, mean)

boxplot(Lifespan ~ Diet, data = data.Ex7)

pairwise.t.test(data.Ex7$Lifespan, data.Ex7$Diet, p.adj = "holm")

##Ejemplo 9
data.Ex9 <- read.table("http://waveland.com/Glover-Mitchell/Example08-4.txt", header = TRUE)
tail(data.Ex9)

aov.Ex9 <- aov(Breadth~Era, data = data.Ex9)
summary(aov.Ex9)

tapply(data.Ex9$Breadth,data.Ex9$Era, mean)

boxplot(Breadth ~ Era, data = data.Ex9)

pairwise.t.test(data.Ex9$Breadth, data.Ex9$Era, p.adj = "holm")

TukeyHSD(aov.Ex9, ordered = TRUE)

## Ejemplo 11
data.Ex11 <- read.table("http://waveland.com/Glover-Mitchell/Example09-1.txt", header = TRUE)
tail(data.Ex11,n = 5)

aov.Ex11 <- aov(Catch ~ Technique + Day, data = data.Ex11)
summary(aov.Ex11)

TukeyHSD(aov.Ex11, "Technique", ordered = TRUE)

##Ejemplo 13
data.Ex13 <- read.table("http://waveland.com/Glover-Mitchell/Problem09-16.txt", header = TRUE)
tail(data.Ex13, n = 3)

aov.Ex13 <- aov(Weight ~ Age * SmokingStatus, data = data.Ex13)
summary(aov.Ex13)

tapply(data.Ex13$Weight, data.Ex13$SmokingStatus, mean)

boxplot(data.Ex13$Weight ~ data.Ex13$SmokingStatus)

TukeyHSD(aov.Ex13, "SmokingStatus", ordered = TRUE)

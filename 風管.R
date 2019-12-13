#AVNOA
X = rnorm(40, 5, 1)
X
A = factor(rep(1:4, each=10))
A
XA = data.frame(X, A)
aov.XA = aov(X~A, data=XA)
summary(aov.XA)
plot(XA$X~XA$A)

#ANOVA
x1 <- c(5, 6, 8, 9,10, 11, 11, 12, 6, 7, 9, 10)
x2 <- factor(rep(1:3, each=4))
X1 <- data.frame(x1, x2)
X1$x2 <- factor(X1$x2)
aov.X1 <- aov(x1~x2, data = X1)
summary(aov.X1)
plot(X1$x1~X1$x2)

tapply(x1, x2, mean)
TukeyHSD(aov.X1)
plot(TukeyHSD((aov.X1), conf.level = 0.95))

library(ggplot2)
ggplot(X1, aes(x = x2, y = x1)) +
  geom_boxplot(color = "red") +
  geom_jitter(position = position_jitter(0.05))

#ANOVA
y1 <- c(45.2, 60.1, 52.8, 31.7, 33.6, 39.4, 48.2, 51.6, 63.7, 46.8, 49.2, 50.7, 71.6, 61.3, 49.8)
y2 <- c(rep(1,6), rep(2,5), rep(3,4))
y <- data.frame(y1, y2)
y$y2 <- factor(y$y2, level = rep(1:3), labels = c("under", "interval", "over"))
result <- aov(y1~y2, data = y)
summary(result)
plot(y$y1 ~ y$y2)

#Tukey多重比較檢定
TukeyHSD(result)
plot(TukeyHSD((result), conf.level = 0.95))

#ggplot
ggplot(y, aes(x = y2, y = y1)) +
  geom_boxplot(color = "red") +
  geom_jitter(position = position_jitter(0.05))

#K-W檢定
kruskal.test(y1~y2, data = y)

#Dunn多重比較檢定
#install.packages("FSA")
library(FSA)
dunnTest(y1 ~ y2, data = y) 

#remove na
library(readxl)
data <- read_excel("C:/Users/John/Desktop/a.xlsx")
data1 <- subset(data, !is.na(本期綜合損益))

write.csv(data1, "data1.csv")

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

#write
write.csv(data1, "data1.csv")


##
data <- read.csv("C:/Users/John/Desktop/dummyreg2.csv")
data$ind <- factor(data$ind, labels = c("銀行", "金控", "證券期貨", "保險"))
data$ret <- as.numeric(as.character(data$ret))
data$pb <- as.numeric(as.character(data$pb))
data$roe <- as.numeric(as.character(data$roe))
data$bis <- as.numeric(as.character(data$bis))
data$loan <- as.numeric(as.character(data$loan))
data$baddebt <- as.numeric(as.character(data$baddebt))
data$current <- as.numeric(as.character(data$current))
data$ratio <- as.numeric(as.character(data$ratio))
data$eps <- as.numeric(as.character(data$eps))
data$beta <- as.numeric(as.character(data$beta))
data$大盤報酬率 <- as.numeric(as.character(data$大盤報酬率))
data$台灣十年期殖利率 <- as.numeric(as.character(data$台灣十年期殖利率))
data$jc <- as.numeric(as.character(data$jc))
data$產業別_銀行 <- as.numeric(as.character(data$產業別_銀行))
data$產業別_金控 <- as.numeric(as.character(data$產業別_金控))
data$產業別_保險 <- as.numeric(as.character(data$產業別_保險))
data$所有權類型_1 <- as.numeric(as.character(data$所有權類型_1))
data$所有權類型_2 <- as.numeric(as.character(data$所有權類型_2))
data$所有權類型_3 <- as.numeric(as.character(data$所有權類型_3))
data$金融海嘯 <- as.numeric(as.character(data$金融海嘯))

summary(data)

library(FSA)

#1.1
result <- aov(data$ret ~ data$ind, data = data)
summary(result)
plot(data$ret ~ data$ind)

TukeyHSD(result)
plot(TukeyHSD((result), conf.level = 0.95))

kruskal.test(data$ret ~ data$ind, data = data)
dunnTest(data$ret ~ data$ind, data = data)





#regressiom
reg <- lm(ret ~ bis+loan+baddebt+current+ratio+eps+beta+大盤報酬率+台灣十年期殖利率+jc+產業別_銀行+產業別_金控+產業別_保險+
            所有權類型_1+所有權類型_2+所有權類型_3+金融海嘯, data = data)
summary(reg)

reg <- lm(ret ~ bis+eps+beta+大盤報酬率+台灣十年期殖利率+jc+產業別_銀行+產業別_金控+產業別_保險+
            所有權類型_1+所有權類型_2+所有權類型_3+金融海嘯, data = data)
summary(reg)



#install.packages("PerformanceAnalytics") # needed for chart correlation
#install.packages("Hmisc") # needed for correlation
library(Hmisc)

library("PerformanceAnalytics")
getwd()

d <- read.csv("Fall 2019 Courses/DSCI 429/Project1B.csv")

summary(d) # check the 'five' (six) number summary
# correlation for the entire set
chart.Correlation(d, histogram=TRUE, pch=19)

cor(d)

# remove the columns that add nothing

d1 <- d[,-1] # remove Region
summary(d2)

d2 <- d1[,-8] # remove Rain
summary(d1)
cor(d2)

chart.Correlation(d2, histogram=TRUE, pch="+")

cor.data <- cor(d2)
#Show the correlation matrix
round(cor.data, 2)
#Round correlation matrix to two places
# as.dist(): output second 1/2 only
print(as.dist(round(cor(d2),2)))

# correlations from Hmisc
rcorr(as.matrix(d2), type="pearson")

d3 <- d2[,c(5,8,9)]
summary(d3)
print(as.dist(round(cor(d3),2)))

chart.Correlation(d3, histogram=TRUE)
chart.Correlation(d3, histogram=FALSE)

m1 <- lm( Profit.per.capita ~ Avg.Age, data=d3 )
summary(m1)
par(mfrow=c(2,2))
plot(m1)

m2 <- lm( Profit.per.capita ~ Price, data=d3 )
summary(m2)
plot(m2)

windows()

m3 <- lm( Profit.per.capita ~ Avg.Age + Price, data=d3 )
summary(m3)
plot(m3)


m4 <- lm( Profit.per.capita ~ ., data=d )
summary(m4)
plot(m4)

#lng <- length(d3[,1]) # use the first column to get the row count
#lng
#lng.75 <- lng * 0.75 # keep 75% for training
#lng.75
#lng - lng.75 # 25% for testing
#lng.75 + (lng - lng.75)

# set a seed so that the results are reproducible
#set.seed(1)
#train <- sample(lng, lng.75)
#train
#(sorttrain <- sort(train))

#traindata <- d3[sorttrain,] # select a subset out of d2

#head(traindata)
#tail(traindata)

#testdata <- d3[-sorttrain,]

#head(traindata)
#head(testdata, 10)
#tail(traindata)
#tail(testdata)

#alltrain <- lm(Profit.per.capita ~ ., data = traindata)
#summary(alltrain)

#par(mfrow=c(2,2))
#plot(alltrain)

#pred1 <- predict.lm(alltrain, testdata)
#summary(pred1) # no help here

#par(mfrow=c(1,1))
#plot( pred1, testdata$Profit.per.capita, type="p", xlab="Predicted", ylab = "Observed" )
#abline(coef = c(0,1), col="darkorange")

#par(mfrow=c(2,1))
#plot( alltrain$fitted.values, traindata$RevPerCapita, type="p", xlab="Predicted", ylab = "Observed" )
#abline(coef = c(0,1), col="darkorange")

#plot( pred1, testdata$Profit.per.capita, type="p", xlab="Predicted", ylab = "Observed" )
#abline(coef = c(0,1), col="darkorange")

#install.packages("caret")
#library(caret)
#results <- data.frame(obs = testdata$Profit.per.capita, pred = pred1)
#defaultSummary(results)
#summary(alltrain)

#1-(1-0.6104784)*(291-1)/(291-9-1)



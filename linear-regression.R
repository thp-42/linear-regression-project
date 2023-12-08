df <- read.csv('student-mat.csv', sep=';')
print(summary(df))
print(head(df))

# check na
any(is.na(df))

# import libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

#num only
num.cols <- sapply(df, is.numeric)
#filter 
cor.data <- cor(df[,num.cols])

#correlation stuff
corrplot(cor.data, method="color")
corrgram(df)

# seperate into training and test
library(caTools)
set.seed(101)
sample <- sample.split(df$G3, SplitRatio = 0.7)

#seperate into train and test
train <- subset(df, sample==TRUE)
test <- subset(df, sample==FALSE)

#train and build model
model <- lm(G3 ~ ., data = train)
print(summary(model))

#residuals
res <- residuals(model)
res <- as.data.frame(res)

ggplot(res, aes(res)) + geom_histogram(fill='blue', alpha=0.5)
plot(model)

# predictions
G3.predictions <- predict(model, test)

print(G3.predictions)
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)
head(results)

#take care of negative values
to_zero <- function (x) {
  if (x < 0) {
    return(0)
  } else {
    return (x)
  }
}

results$predicted <- sapply(results$predicted, to_zero) 

#mean squared error 

mse <- mean((results$actual - results$predicted)^2)
print(mse)

rmse <- mse^0.5
print(rmse)

## sse 
sse <- sum((results$actual - results$predicted)^2)
sst <- sum((mean(df$G3) - results$actual)^2)

r2 <- 1 - sse/sst
print(r2)

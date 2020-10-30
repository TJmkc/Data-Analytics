library(ISLR)
library(MASS)
library(boot)
set.seed(1)

data("Titanic")

train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto,subset = train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2),data = Auto,subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train = sample(392,196)

lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2),data = Auto,subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

## Random Forest 

install.packages("randomForest")

library(randomForest)

data1 <- read.csv(file.choose(),header=TRUE)

head(data1)

colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","BootSpace","Safety","Condition")
head(data1)
str(data1)


levels(data1$Condition)
summary(data1)

set.seed(100)
train <- sample(nrow(data1),0.7*nrow(data1),replace = FALSE)
Trainset <- data1[train,]
Validset <- data1[-train]    
summary(Trainset)
summary(Validset)

help("randomForest")

model1 <- randomForest(Condition ~.,data = Trainset, importance = TRUE)

model2 <- randomForest(Condition ~.,data = Trainset, ntree = 500,mtry = 6, importance = TRUE)

predTrain <- predict(model2,Trainset,typle="class")
table(predTrain,Trainset$Condition)
preValid <- predict(model2,Validset,type = "class")
table(preValid,VValidset$Condition)

importance(model2)
varImpPlot(model2)

a = c()
i = 5

for (i in 3:8){
  model3 <- randomForest(Condition ~. , data = Trainset,ntree = 500,mtry = i, importance = TRUE)
  preValid <- predict(model3, Validset,type = "class")
  a[i-2] = mean(preValid == Validset$Condition)
}

a 
plot(3:8,a)

library(rpart)
library(caret)
library(e1071)


model_dt <- train(Condition ~., data = Trainset,method = "rpart")

model_dt_1 = predict(model_dt, data = Trainset)

table(model_dt_1,Trainset$Condition)
mean(model_dt_1 == Trainset$Condition)

model_dt_vs = predict(model_dt, data = Validset)
table(model_dt_vs,Validset$Condition)
mean(model_dt_vs == Validset$Condition)




require(ggplot2)
library(ggpubr)
library(dplyr)
theme_set(theme_pubr())
ggplot(data2,aes(x=data2$`Student Attendance Rate`, y=data2$`Teacher Attendance Rate`, fill=`School Type`,group = `School Type`))+geom_boxplot()





my_screen_step1 <- split.screen(c(2, 1))
screen(my_screen_step1[1])
plot( data2$`Student Attendance Rate`,data2$`Teacher Attendance Rate` , pch=20 , xlab="value of Student Attendance" , cex=3 , col=rgb(0.4,0.9,0.8,0.5) )
my_screen_step2 <- split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(data2$`Student Attendance Rate`, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="distribution of Student Attendance Rate")
screen(my_screen_step2[2])
hist(data2$`Teacher Attendance Rate`, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="distribution of Teacher Attendance Rate")



a <-
  ggplot(data2, aes(x=as.factor(data2$`School Type`), y=data2$`Percent White`,fill = data2$`School Type`)) + 
  geom_boxplot() + 
  xlab("School Type") + ylab(ylab("Percent White"))


b <- ggplot(data2, aes(x=as.factor(data2$`School Type`), y=data2$`Percent Black`,fill = data2$`School Type`)) + 
  geom_boxplot() + 
  xlab("School Type") + ylab("Percent Black")


c <- ggplot(data2, aes(x=as.factor(data2$`School Type`), y=data2$`Percent Hispanic`,fill = data2$`School Type`)) + 
  geom_boxplot() + 
  xlab("School Type") + ylab("Percent Hispanic")
  

d<- ggplot(data2, aes(x=as.factor(data2$`School Type`), y=data2$`Percent Asian`,fill = data2$`School Type`)) + 
  geom_boxplot() + 
  xlab("School Type") + ylab("Percent Asian") + title()
  
 ggarrange(a,b,c,d,
                ncol = 2,nrow = 2)

hist(data1$`School Type`)bar_age <- barplot(pack,col="lightblue")

df <- data1 %>%
  group_by(`School Type`) %>%
  summarise(counts = n())
df

library(tidyverse)
library(hrbrthemes)
library(viridis)

ggplot(data1,aes(x=`Percent Black`, y=`Percent White`, fill=`School Type`,groups = `School Type`))+geom_boxplot()



ggplot(df, aes(x = `School Type`, y = counts,fill = `School Type`)) + geom_bar(color = 'white', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 


#k3
k3 <- read_csv('k3.csv')

# `Collaborative Teachers Rating`
kf <- k3 %>%
  group_by(`Collaborative Teachers Rating`) %>%
  summarise(counts = n())

kf

ggplot(kf, aes(x = `Collaborative Teachers Rating`, y = counts,fill = `Collaborative Teachers Rating`)) + geom_bar(color = 'white', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 


#`Effective School Leadership Rating`
kf_envi <- k3 %>%
  group_by(`Effective School Leadership Rating`) %>%
  summarise(counts = n())



ggplot(kf_envi, aes(x = `Effective School Leadership Rating`, y = counts,fill = `Effective School Leadership Rating`)) + geom_bar(color = 'red', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 

boxplot(k3$`Percent White`,k2$`Percent White`)


# k2 
k2 <- read_csv('k2.csv')

# collaborative 
k2f <- k2 %>%
  group_by(`Collaborative Teachers Rating`) %>%
  summarise(counts = n())
ggplot(k2f, aes(x = `Collaborative Teachers Rating`, y = counts,fill = `Collaborative Teachers Rating`)) + geom_bar(color = 'white', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 


#supportive 

k2f_envi <- k2 %>%
  group_by(`Effective School Leadership Rating`) %>%
  summarise(counts = n())



ggplot(k2f_envi, aes(x = `Effective School Leadership Rating`, y = counts,fill = `Effective School Leadership Rating`)) + geom_bar(color = 'red', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 




ggplot(data1, aes(`School Type`)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()



hist(k3$`Percent White`,col=rgb(1,0,0,0.5), xlab="Percent Of White", 
     ylab="nbr of plants", main="distribution of Percent of White of 2 Type of Schools" )

hist(k2$`Percent White`, col=rgb(0,0,1,0.5), add=T)

legend("topright", legend=c("k3$`Percent White`","k2$`Percent White`"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )



hist(k3$`Rigorous Instruction Rating`,col=rgb(1,0,0,0.5), xlab="Percent Of White", 
     ylab="nbr of plants", main="distribution of Percent of White of 2 Type of Schools" )

hist(k2$`Rigorous Instruction Rating`, col=rgb(0,0,1,0.5), add=T)


ggplot(data1, aes(x = , y = `Rigorous Instruction Rating`,gruop = `School Type`)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = 'School Type', y = 'Rigorous Instruction Rating')



## Dataset 2




d2 <- data3 %>%
  group_by(`Program Type`) %>%
  summarise(counts = n())

d2

ggplot(d2, aes(x = `Program Type`, y = counts,fill = `Program Type`)) + geom_bar(color = 'white', stat = "identity") +geom_text(aes(label = counts), vjust = -0.3) 


ggplot(data3, aes(x=as.factor(data3$`Program Type`), y=data3$Enrollment,fill = data3$`Program Type`)) + 
  geom_boxplot() + 
  xlab("Program Type") + ylab("Enrollment")



## Model - dataset 2
 
l <- read_csv('final.csv')

## KNN 

## Train test spilt 


library(e1071)
library(class)

l <- read_csv('final.csv')

l2 <- l[complete.cases(l),]

l3 <- l2

l3 <- sapply(l2[,],as.numeric)

nrow(l3)


ind <- sample(2, nrow(l3), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- l3[ind==1,]
KNNtest <- l3[ind==2,]



knn4 = knn(train = KNNtrain[,-32], test = KNNtest[,-32], cl = KNNtrain$`Trust - Survey Percent Positive`, k = 4)

knn8 = knn(train = KNNtrain[,-32], test = KNNtest[,-32], cl = KNNtrain$`Trust - Survey Percent Positive`, k = 8)





KNN4pred <- knn(train = KNNtrain[,-32], test = KNNtest[,-32], cl = KNNtrain$`Trust - Survey Percent Positive`, k = 4)

KNN8pred <- knn(train = KNNtrain[,-32], test = KNNtest[,-32], cl = KNNtrain$`Trust - Survey Percent Positive`, k = 8)



help("plot")

plot(KNNtest$`Program Type`,KNNtest$`Trust - Survey Percent Positive`)
points(KNNtest$`Program Type`,as.numeric(levels(KNN4pred))[KNN4pred], col = "red",pch = 14)

points(KNNtest$`Program Type`,as.numeric(levels(KNN8pred))[KNN8pred],col = "blue", pch =22)

ggplot(KNNtest, aes(x=`Program Type`, y=`Trust - Survey Percent Positive`,color = `Program Type`)) + 
  geom_point() + 
  xlab("Program Type") + ylab("Trust")

k4 <- KNNtest


k4$k4pred <- as.numeric(levels(KNN4pred))[KNN4pred]

ggplot(k4, aes(x=`Program Type`, y= k4pred,color = `Program Type`)) + 
  geom_point() + 
  xlab("Program Type") + ylab("Trust")




k8 <- KNNtest


k8$k8pred <- as.numeric(levels(KNN8pred))[KNN8pred]

ggplot(k8, aes(x=`Program Type`, y= k8pred,color = `Program Type`)) + 
  geom_point() + 
  xlab("Program Type") + ylab("Trust")
help("points")



l3$Perfect <- l3$`Trust - Survey Percent Positive` == 1
l3$Good <- l3$`Trust - Survey Percent Positive` >= 0.96 & l3$`Trust - Survey Percent Positive` <= 0.99
l3$Need_Improvement <- l3$`Trust - Survey Percent Positive` <= 0.95



ind <- sample(2, nrow(l3), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- l3[ind==1,]
KNNtest <- l3[ind==2,]

knn4_class <- knn(train = KNNtrain[1:31], test = KNNtest[1:31], cl = KNNtrain$Perfect, k = 4)
table(KNNtest$Perfect, knn4_class)


knn8_class <- knn(train = KNNtrain[1:31], test = KNNtest[1:31], cl = KNNtrain$Perfect, k = 8)
table(KNNtest$Perfect, knn8_class)


help("table")



#Decision Tree 

library(rpart)
library(rpart.plot)
library(plyr)

q <- read_csv('q.csv',skip = 1)

q2 <- q[complete.cases(q),]

ind2 <- sample(2, nrow(q2), replace=TRUE, prob=c(0.7, 0.3))
catrain <- q2[ind2==1,]
catest <- q2[ind2==2,]

q2[,32]


dtm1 <- rpart(Label ~., data = catrain, method = 'class')

dtm1

rpart.plot(dtm1)

complete.cases(fit1)

fit1 <- predict(dtm1, catest[-32],type='class')



tree.result <- as.character(fit1)

true.result <- catest[32]

tree <- as.data.frame(cbind(true.result,tree.result))

count(tree[which(tree$Label == 'Good'),])

accuracy <- length(which(tree$Label == tree$tree.result)) / nrow(tree)
accuracy*100

table(tree$Label,tree$tree.result)

accuracyk4 <- (330+65) / (330+65+95+117)
accuracyk4

accuracyk8 <- (343+64)/(343+64+82+118)
accuracyk8

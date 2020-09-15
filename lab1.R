temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)  ### create columns 
snowed <- c('T','T','T','F','F','T','F')
#help("data.frame")
days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
RPI_Weather_Week <- data.frame(days,temp,snowed)  ## create new dataframe
RPI_Weather_Week
head(RPI_Weather_Week)
summary(RPI_Weather_Week)
RPI_Weather_Week[1,]  ## first row and all columns 
RPI_Weather_Week[,1]  ## first column and all rows 
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
subset(RPI_Weather_Week,subset = snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])  ## order
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)   ### order 
dec.snow



### create dataframes 

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df
write.csv(df,file='saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

GPW3 <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')
head(GPW3)
EPI_data <- read.csv('2010EPI_data.csv', skip = 1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI)
stem(EPI)   # stem and leaf plot
fivenum(EPI,na.rm = TRUE)  # minimum, lower quantile, Median, upper quantile, maximum 
hist(EPI)
hist(EPI,seq(30.,95.,1.0),probability = TRUE)
#lines(density(EPI, na.rm = TRUE, bw =1 ))
lines(density(EPI, na.rm = TRUE, bw = "SJ" ))
rug(EPI)
help(rug)

plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)  ## cumulative density function 

par(pty = "s") ## quantile - quantile 
qqnorm(EPI)
qqline(EPI)

# make a Q-Q plot against the generating ditribution by : 
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df =5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
## exercise1 
plot(ecdf(WATER_E), do.points = FALSE, verticals = TRUE) 

boxplot(EPI,DALY)
qqplot(EPI,DALY)


EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30.,95.,1.0), probability = TRUE)
plot(ecdf(Eland), do.points = FALSE, verticals = TRUE)
qqplot(qt(ppoints(250),df =5), Eland, xlab = "Q-Q plot for Eland")

EPIDesert <- EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
boxplot(EDesert)
hist(EDesert, seq(30.,95.,1.0), probability = TRUE)
plot(ecdf(EDesert), do.points = FALSE, verticals = TRUE)
qqplot(qt(ppoints(250),df =5), EDesert, xlab = "Q-Q plot for Desert")


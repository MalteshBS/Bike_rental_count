#Remove All The Stored Objects
rm(list = ls())

#Get Working Directory
setwd("D:/edwisor")

getwd()

#Set Current Working Directory
day <- read.csv("D:/edwisor/day.csv", header = TRUE)

##Install Required Packages and libraries
#Load Libraries
x = c ("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm",
       "corrgram","DataCombine")


lapply(x, require, character.only = TRUE)
rm(x)

##explore the data 

#Structure Of Variables
str(day)

#Verify First Six Rows of Data
head(day)

#Column Names
names(day)

## count of number of rows and columns 
nrow(day)
ncol(day)


#Target Variable Is 'cnt' And Other Variable Are Indepentent Variable

#varify Summary of Data
summary(day)

## rename the columns to give the proper meaning 

names(day)[names(day)=="hum"] = "humidity"
names(day)[names(day)=="cnt"] = "count"
names(day)[names(day)=="yr"] = "year"
names(day)[names(day)=="mnth"] = "month"


#Check The Column Names
names(day)


#check the relationship between 'temp' and 'atemp' variable
ggplot(day, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#This  graph explians that there is strong relationship  between 'temp' and 'atemp'

#lets Check the relationship between 'temp' and 'hum' variable
ggplot(day, aes(x= temp,y=humidity)) +
  geom_point()+
  geom_smooth()
##  Humidity is increasing  till temparature is at point 0.7 and then decreasing  gradually

#Check the relationship between 'temp' and 'windspeed' variable
ggplot(day, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()
# it is showing that very less negative correlation between  temp and windspeed

##FEATURE SELECTION

#create new columns with constant multiplied as the actual values have less range and its hard to analyse

day$real_temp = day$temp * 30
day$real_atemp = day$atemp * 50
day$real_windspeed = day$windspeed *70
day$real_humidity = day$humidity * 100

day$real_season = factor(x = day$season, levels = c (1,2,3,4), labels = c('spring', 'summer', 'fall', 'winter'))
day$real_year = factor(x = day$year, levels = c(0,1), labels = c ('2011', '2012'))
day$real_holiday = factor(x = day$holiday, levels = c (0,1), labels = c ('workingday','holiday'))
day$real_weathersit = factor(x = day$weathersit, levels = c (1,2,3,4), labels = c ('clear','cloudy', 'rainy', 'heavy rainy'))

day$weathersit = as.factor(day$weathersit)
day$month = as.factor(day$month)
day$season = as.factor(day$season)
day$dteday = as.factor(day$dteday)
day$workingday = as.factor(as.character(day$workingday))
day$weekday = as.factor(as.character(day$weekday))
day$holiday =as.factor(day$holiday)
day$year = as.factor(day$year)


##MISSING VALUE ANALYSIS :


missing_values = sapply(day,function(x){sum(is.na(x))})
missing_values
## we could see that there are no missing fileds in any of the variables.

## let us plot the graphs and explore the data and analyse the relationship btw variables 

#Check the distribution of categorical Data using bar graph
bargraph_season = ggplot(data = day, aes(x = real_season)) + geom_bar() + ggtitle('count of season')
bargraph_weather = ggplot(data = day, aes(x = real_weathersit)) + geom_bar() + ggtitle('count of weathersit')
bargraph_holiday = ggplot(data = day, aes(x = real_holiday)) + geom_bar() + ggtitle('count of holiday')
bargraph_workday = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle('count of workingday')


#Ploting the all data together
gridExtra::grid.arrange(bargraph_season, bargraph_weather, bargraph_holiday, bargraph_workday, ncol=2)


#Check the distribution of numerical data using histogram

histogram_temp = ggplot(data = day, aes(x = real_temp)) + ggtitle('Distribution Of Temprature') + geom_histogram(bins = 20)
histogram_atemp = ggplot(data = day, aes(x = real_atemp)) + ggtitle('Distribution Of atemp') + geom_histogram(bins = 20)
histogram_hum = ggplot(data = day, aes(x = real_humidity)) + ggtitle('Distribution Of humidity') + geom_histogram(bins = 20)
histogram_wind = ggplot(data = day, aes(x = real_windspeed)) + ggtitle('Distribution Of windspeed') + geom_histogram(bins = 20)

#Plotting the all data together using histogram
gridExtra::grid.arrange(histogram_temp, histogram_atemp, histogram_hum, histogram_wind, ncol=2)


#Check the distribution of numerical data using scatterplot
scatter_temp = ggplot(data = day, aes(x = real_temp, y = count)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
                   
scatter_atemp = ggplot(data = day, aes(x = real_atemp, y = count)) + ggtitle("Distribution of aTemp") + geom_point() + xlab("aTemp") + ylab("Bike COunt")
                   
scatter_hum = ggplot(data = day, aes(x = real_humidity, y = count)) + ggtitle("Distribution of Humidity") + geom_point(color ="red") + xlab("Humidity") + ylab("Bike COunt")
                   
scatter_windspeed = ggplot(data = day, aes(x = real_windspeed, y = count)) + ggtitle("Distribution of Windspeed") + geom_point(color = "red") + xlab("windspeed") + ylab("Bike COunt")

#Plotting the all data together using Scatterplot
gridExtra::grid.arrange(scatter_temp, scatter_atemp, scatter_hum, scatter_windspeed, ncol=2)

#Checking for OUTLIERS in data using boxplot
cnames = colnames(day [, c('real_temp', 'real_atemp', 'real_windspeed', 'real_humidity')])
for (i in 1:length(cnames)) 
  {
  assign(paste0('gn', i), ggplot(aes_string(y = cnames[i]),data = day) + 
           stat_boxplot(geom = 'errorbar', width = 0.5) + 
           geom_boxplot(outlier.colour = 'blue',fill = 'red', outlier.shape = 20, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = 'bottom') + 
           labs(y =cnames[i]) + 
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1, gn2, gn3, gn4, ncol = 2)

#There is an outlier in windspeed

##Remove outliers in Windspeed
outlier_analysis = day [, 20][day[, 20] %in% boxplot.stats(day[, 20])$out]
day = day [ which(!day[, 20] %in% outlier_analysis),]

#Boxplot after removing outliers 
#Boxplot for casual variable
ggplot(data = day, aes(x="", y= windspeed)) + geom_boxplot()

#Check for multicollinearity using VIF
df = day [, c( "instant","temp","atemp","humidity","windspeed" )]
vifcor(df)


#Check for collinearity using corelation graph
corrgram(day, order = F, upper.panel = panel.pie, text.panel = panel.txt, main = 'correlation plot')

#Removing the unwanted variables

day = subset(day, select = -c(holiday,instant,dteday,atemp,casual,registered,real_temp,real_atemp,real_windspeed,
                               real_humidity,real_season,real_year,real_holiday,real_weathersit))

## remove all the objects except the data set (day) to build the model on top of it 
rmExcept(keepers = 'day')



##Model development on the cleaned data set 

##DECISION TREE
#Divide the data into train and test
set.seed(1234)

# train_index stores the index of the 80% of the data  
train_index = sample(1:nrow(day), 0.8* nrow(day))
#train stores the 80% of the data 
train = day[train_index,]
# test stores the remaining 20 % of the data
test = day[-train_index,]

##rpart for regression
model_dectree = rpart(count ~ . , data = train, method = 'anova')

#Predict for new test cases
prediction_dectree = predict(model_dectree, test[,-15])

print(model_dectree)


####Graphical Representation of Decision tree

par(cex= 0.8)
plot(model_dectree)
text(model_dectree)

#Prediction of the test cases
prediction_dectree = predict(model_dectree, test[, -10])

#Create dataframe for actual and predicted values
df = data.frame('actual' = test [,10], 'predict' = prediction_dectree)
head(df)

#Calculation of MAPE
regr.eval(trues = test[, 10], preds = prediction_dectree, stats = c('mae', 'mse', 'rmse', 'mape'))


MAPE = function(real, pred){
  print(mean(abs((real - pred)/real)) * 100)
}
MAPE(test[,10], prediction_dectree)

#MAPE = 18.91265 %
#MAE = 623.25
#RMSE = 813.58
#ACCURACY = 81.08%

#LINEAR REGRESSION CLASSIFICATION


#Train the data using linear regression 
model_linreg = lm (formula = count ~ . , data = train)

#Summary of the model
summary(model_linreg)

#Predict the test cases
prediction_linreg =predict(model_linreg, test[, -10])

#Create dataframe for actual and predicted values
df = cbind(df, prediction_linreg)
head(df)


#calculate MAPE
regr.eval(trues = test[, 10], preds = prediction_linreg, stats = c ('mae', 'mse', 'rmse', 'mape'))
MAPE(test[, 10], prediction_linreg)

#MAPE: 16.24673%
#RMSE: 706.56
#MAE: 514.16
#Accuracy: 83.75%
#Adjusted R squared: 0.8362
#F-statistic: 111.1


#Plot the graph real vs predicted values

plot(test$count, lty = 2, col = 'blue')
lines(prediction_linreg, col = 'black')

##RANDOM FOREST CLASSIFICATION
#Train the data using RANDOM FOREST

model_RF = randomForest(count ~ . , data = train, ntree = 500)

#predict the test cases
prediction_RF = predict(model_RF, test [, -10])

#create dataframe for real and predicted values
df = cbind(df, prediction_RF)
head(df)

#calculation of MAPE
regr.eval(trues = test[, 10], preds = prediction_RF, stats = c ('mae', 'mse', 'rmse', 'mape'))
MAPE(test[, 10], prediction_RF)


#MAPE = 13.93596 %
#MAE = 445.34
#RMSE = 612.54
#ACCURACY = 86.07% 

#Random Forest is 86.07% accurate andd hence chosen as the model for prediction of bike rental count.
#and RMSE Is 6.125429e+02
---------------------------------------------------------------------------------------------

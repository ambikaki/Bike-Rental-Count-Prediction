rm(list=ls(all=T))
setwd("C:/Users/Shriyan/Desktop")
getwd()

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", 
      "dummies", "e1071", "Information","MASS", "rpart", "gbm", "ROSE", 'sampling', 
      'DataCombine', 'inTrees','fastDummies')
lapply(x, require, character.only = TRUE)
rm(x)

data_train = read.csv("day.csv")

###########  Exploratory Data Analysis    ###############

data_train$season=as.factor(data_train$season)
data_train$mnth=as.factor(data_train$mnth)
data_train$yr=as.factor(data_train$yr)
data_train$holiday=as.factor(data_train$holiday)
data_train$weekday=as.factor(data_train$weekday)
data_train$workingday=as.factor(data_train$workingday)
data_train$weathersit=as.factor(data_train$weathersit)

# dropping the variables which do not give much value to the model
data_train=subset(data_train, select = -c(instant,casual,registered))

d1=unique(data_train$dteday)
df=data.frame(d1)
data_train$dteday=as.Date(df$d1,format="%Y-%m-%d")

df$d1=as.Date(df$d1,format="%Y-%m-%d")
data_train$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
data_train$dteday=as.factor(data_train$dteday)

#checking the structure and summary of the data
str(data_train)
summary(data_train)

head(data_train, 20)

#missing value analysis  #### there are no missing values

sum(is.na(data_train))

# Analyzing the variables by visualization

# analyze the distribution of  target variable 'cnt'  
ggplot(data_train)+
    geom_histogram(aes(x= data_train$cnt, y=..density..),
                   fill= "blue")+
    geom_density(aes(x= data_train$cnt, y=..density..))
  
# analyse the distrubution of  independence variable 'temp'
ggplot(data_train)+
  geom_histogram(aes(x= data_train$temp, y=..density..),
                 fill= "blue")+
  geom_density(aes(x= data_train$temp, y=..density..))

# analyse the distrubution of  independence variable 'atemp'
ggplot(data_train)+
  geom_histogram(aes(x= data_train$atemp, y=..density..),
                 fill= "blue")+
  geom_density(aes(x= data_train$atemp, y=..density..))

# analyse the distrubution of  independence variable 'hum'
ggplot(data_train)+
  geom_histogram(aes(x= data_train$hum, y=..density..),
                 fill= "blue")+
  geom_density(aes(x= data_train$hum, y=..density..))

# analyse the distrubution of  independence variable 'windspeed'
ggplot(data_train)+
  geom_histogram(aes(x= data_train$windspeed, y=..density..),
                 fill= "blue")+
  geom_density(aes(x= data_train$windspeed, y=..density..))

# Visualize categorical Variable 'mnth' with target variable 'cnt'

ggplot(data_train, aes(x= mnth, y=cnt),fill="blue") + 
  stat_summary(fun.y="mean", geom="bar")

# Visualize categorical Variable 'season' with target variable 'cnt'
ggplot(data_train, aes(x=season, y=cnt), color = 'b')+
  stat_summary(fun.y='mean', geom = 'bar')

# Visualize categorical Variable 'yr' with target variable 'cnt'
ggplot(data_train, aes(x=yr, y=cnt), fill = 'blue')+
  stat_summary(fun.y='mean', geom = 'bar')
  
# Visualize categorical Variable 'weathersit' with target variable 'cnt'
ggplot(data_train, aes(x=weathersit, y=cnt), fill = 'blue')+
  stat_summary(fun.y='mean', geom = 'bar')

# Visualize categorical Variable 'holiday' with target variable 'cnt'
ggplot(data_train, aes(x=holiday, y=cnt), fill = 'blue')+
  stat_summary(fun.y='mean', geom = 'bar')

# Visualize categorical Variable 'workingday' with target variable 'cnt'
ggplot(data_train, aes(x=workingday, y=cnt), fill = 'blue')+
  stat_summary(fun.y='mean', geom = 'bar')

# Visualize categorical Variable 'weekday' with target variable 'cnt'
ggplot(data_train, aes(x=weekday, y=cnt), fill = 'blue')+
  stat_summary(fun.y='mean', geom = 'bar')

############   Outlier Analysis   ###############

# BoxPlots - Distribution and Outlier Check

numeric_index = sapply(data_train,is.numeric)    #selecting only numeric

numeric_data = data_train[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), 
                                data = subset(data_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

#Remove outliers using boxplot method
df = data_train
#data_train = df

#remove outlier in windspeed
 val = df$windspeed[df$windspeed %in% boxplot.stats(df$windspeed)$out]
 
 df = df[which(!df$windspeed %in% val),]
 
#remove outlier in humidity
 val = df$hum[df$hum %in% boxplot.stats(df$hum)$out]
 
 df = df[which(!df$hum %in% val),]

##############  Correlation Analysis    ################
 
#checking relation between cnt and atemperature.   
 ### it shows there is relation between them

 ggplot(data_train, aes(x= cnt ,y=atemp)) +
  geom_point()+
  geom_smooth()

#checking relation between cnt and temperature.   
 ### it shows there is relation between them

 ggplot(data_train, aes(x= cnt ,y=temp)) +
  geom_point()+
  geom_smooth()

#checking relation between cnt and humidity.   
 ### it shows there is not much relation between them

 ggplot(data_train, aes(x= cnt ,y=hum)) +
  geom_point()+
  geom_smooth()

#checking relation between cnt and windspeed.   
 ### it shows there is not much relation between them

 ggplot(data_train, aes(x= cnt ,y=windspeed)) +
  geom_point()+
  geom_smooth()

# Correlation Plot
 
corrgram(data_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Dimension Reduction  #### atemp and temp are correlated to each other

data_train = subset(data_train, select = -c(atemp, dteday))
head(data_train, 10)

##################   Model Development  ###################

#Clean the environment

rmExcept("data_train")

#Divide data into train and test using stratified sampling method

train_index = sample(1:nrow(data_train), 0.7 * nrow(data_train))
train = data_train[train_index,]
test  = data_train[-train_index,]

###########   Decision tree regression  #################

# Error = 13.73%
# Accuracy = 86.27%

fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-11])

plot(fit)
summary(fit)

# Calculate MAPE

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)) * 100
}

MAPE(test[,11], predictions_DT)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(predictions_DT,col="blue")


#############    Random Forest Model     ##########################

# Error = 9.80%
# Accuracy = 90.20%

RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-11])

plot(RF_model)

# Calcuate MAPE

MAPE(test[,11], predictions_RF)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(predictions_RF,col="blue")


################    Linear Regression    #################

# Error = 10.09%
# Accuracy = 89.90%
# R2 = 0.849
# Adjusted R2 = 0.8408
# F-stats = 98.83
# p-value = < 2.2

#Linear regression model making
lm_model = lm(cnt ~., data = train)
predictions_LR = predict(lm_model,test[,1:11])

# Calculate MAPE
MAPE(test[,11],  predictions_LR)

summary(lm_model)

#Plot a graph for actual vs predicted values

plot(test$cnt,type="l",lty=2,col="green")
lines(predictions_LR,col="blue")


##########    extacting predicted values output from Random forest model    #############

results <- data.frame(test, pred_cnt = predictions_RF)

write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)



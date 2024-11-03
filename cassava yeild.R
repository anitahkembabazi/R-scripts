library(readxl)
Cassava_Yield_Data <- read_excel("kemz/Cassava_Yield_Data.xlsx")
View(Cassava_Yield_Data)
#exploring the data set

library(readxl)
library(tidyverse)
library(dplyr) #USED FOR DATA TRANSFORMATION
summary(Cassava_Yield_Data)


#Checking the dataset for missing values
is.na(Cassava_Yield_Data)
#visualising the outliers 
boxplot.stats(Cassava_Yield_Data$X1945)$out
boxplot(Cassava_Yield_Data$TotalTuberperHectare,main="Outliers in totaltuber per hectare",boxwex=0.1)
boxplot(Cassava_Yield_Data$TotalWeightperhectare,main="Outliers in totalweight per hectare",boxwex=0.1)
boxplot(Cassava_Yield_Data$Totaltuberno,main="Outliers in totaltuberno",boxwex=0.1)
boxplot(Cassava_Yield_Data$AV_tubers_Plant,main="Outliers in AV tubers plant",boxwex=0.1)
boxplot(Cassava_Yield_Data$Total_tubweight,main="Outliers in totaltuber weight",boxwex=0.1)
boxplot(Cassava_Yield_Data$Weight_smalltubers,main="Outliers in weight small tubers",boxwex=0.1)
boxplot(Cassava_Yield_Data$No_mediumtubers,main="Outliers in no mediumtubers",boxwex=0.1)
boxplot(Cassava_Yield_Data$Weigh_bigtubers,main="Outliers in weigh bigtubers",boxwex=0.1)
boxplot(Cassava_Yield_Data$Plants_harvested,main="Outliers in plants harvested",boxwex=0.1)
boxplot(Cassava_Yield_Data$No_bigtubers,main="Outliers in no big tubers ",boxwex=0.1)
box(Cassava_Yield_Data$Weight_mediumtubers,main="Outliers in weight medium tubers",boxwex=o.1)
summary(Cassava_Yield_Data)
#Finding outliers 
find_outlier <- function(g) {
  q1 <-quantile(g,probs=.25)
  q3 <-quantile(g,probs=.75)
  iqr =q3 -q1
  g > q3 + (iqr * 1.5) |  g < q1 - ( iqr * 1.5)
}
#removing outliers
remove_outlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[find_outlier(dataframe[[col]]), ]
  }
  print("remove_outlier")
  print(dataframe)
}     
Data2new = remove_outlier(Cassava_Yield_Data, c('Plants_harvested','No_bigtubers','No_mediumtubers','weight_smalltubers','weigh_bigtubers','Total_tubweight','weight_mediumtubers','TotalTuberperHectare','TotalWeightperhectare','AV_tubers_Plant','Totaltuberno','plotsize','No_smalltubers'))
view(Data2new)    
#The dataset has 


library(car)
library(rstatix)
library(ggpubr)
#comparing two continuous variables
#comparing No small tubers and weight of small tubers 
#visualising 
library(ggplot2)
ggplot(Cassava_Yield_Data, mapping = aes(x = No_smalltubers, y = Weight_smalltubers)) + 
  geom_point() +
  labs(x="No_smalltubers", 
       y="weight_smalltubers",
       title="Relationship between No_smalltubers and weight_smalltubers")
#performing statistical calculations
cov(Cassava_Yield_Data$No_smalltubers,Cassava_Yield_Data$Weight_smalltubers,use="complete.obs")


#The answer is 102.8278 showing that the covariance between No_smalltubers and Weight_smalltubers is positive 
#Using shapiro test because it the data set has less than 5000 observations 
shapiro.test(Cassava_Yield_Data$Weight_smalltubers)
#p value is 3.553e-06 and is not normaly distributed since p value less than 0.05
shapiro.test(Cassava_Yield_Data$No_smalltubers)
#p-value is 1.746e-08 hence the no smalltubers is not normally distributed 
view(Cassava_Yield_Data)
cor.test(Cassava_Yield_Data$No_smalltubers, Cassava_Yield_Data$Weight_smalltubers, 
         method="pearson", use="complete.obs")
#r is 0.8627615 and p value is less than 0.05 hence reject the null hypothesis


#comparing continuous and categorical variable
#comparing Sesn and Plants harvested 
Cassava_Yield_Data$Sesn = as.factor(Cassava_Yield_Data$Sesn)
ggplot(Cassava_Yield_Data,mapping =aes(x = Sesn, y = Plants_harvested)) +
  geom_boxplot() +
  labs(x="Sesn", 
       y="plants harvested",
       title="Number of plants harvested across the different seasons")
#performing some statistical calculations
cor(Cassava_Yield_Data$Sesn, Cassava_Yield_Data$Plants_harvested, use = "complete.obs")

#point biseral correlation
cor.test(Cassava_Yield_Data$Sesn, Cassava_Yield_Data$Plants_harvested)
#cor=  0.5266887 
#p value is 1.483e-09 positive correlation

#comparing two categorical variables 
#comparing locn and FerT
Cassava_Yield_Data$tillage = as.factor(Cassava_Yield_Data$tillage)
Cassava_Yield_Data$rep = as.factor(Cassava_Yield_Data$rep)
ggplot(Cassava_Yield_Data) +
  geom_count(mapping = aes(x = tillage, y = rep)) +
  labs(x="tillage", 
       y="rep",
       title="Relationship between the fertilizers and rep ")

cor.test(Cassava_Yield_Data$tillage, Cassava_Yield_Data$rep, use="complete observations")











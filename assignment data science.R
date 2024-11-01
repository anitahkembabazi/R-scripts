#Importing the dataset
Wholesale_customers_data <- read_excel("C:/Users/kemba/Downloads/Wholesale customers data.xls")
# Viewing the dataset
library(dplyr)
View(Wholesale_customers_data)
#checking to see if all continous variables are normally distributed
summary(Wholesale_customers_data)
summary(Wholesale_customers_data$Fresh)
summary(Wholesale_customers_data$Milk)
summary(Wholesale_customers_data$Frozen)
summary(Wholesale_customers_data$Grocery)
summary(Wholesale_customers_data$Detergents_Paper)
summary(Wholesale_customers_data$Delicassen)
#transforming the dataset to exclude any missing data
is.na(Wholesale_customers_data)
#There is no missing data 
#showing outliers in only the continuous variables
boxplot(Wholesale_customers_data$Fresh,main="Outliers in Fresh",boxwex=0.1)
boxplot(Wholesale_customers_data$Milk,main="Outliers in Milk", boxwex=0.1)
boxplot(Wholesale_customers_data$Grocery,main="Outliers in Grocery",boxwex=0.1)
boxplot(Wholesale_customers_data$Frozen,main="Outliers in Frozen",boxwex=0.1)
boxplot(Wholesale_customers_data$Detergents_Paper,main="Outliers in Detergents_Paper",boxwex=0.1)
boxplot(Wholesale_customers_data$Delicassen,main="Outliers in Dlicassen",boxwex=0.1)
#Transforming the dataset and handling outliers
#for Fresh
Tmin1=3128-1.5*(16934-3128)
Tmax1=16934+1.5*(16934-3128)
data_2=Wholesale_customers_data$Fresh[which(Wholesale_customers_data$Fresh > Tmin1) & Wholesale_customers_data$Fresh < Tmax1]
data_2n=data.frame(data_2)
#for milk
Tmin2=1533-1.5*(7190-1533)
Tmax2=7190+1.5*(7190+1533)
data_3=Wholesale_customers_data$Milk[which(Wholesale_customers_data$Milk > Tmin2) & Wholesale_customers_data$Milk < Tmax2]
data_3n=data.frame(data_3)
#for grocery
Tmin3=2153-1.5*(10656-2153)
Tmax3=10656-1.5*(10656+2153)
data_4=Wholesale_customers_data$Grocery[which(Wholesale_customers_data$Grocery > Tmin3) & Wholesale_customers_data$Grocery < Tmax3]
data_4n=data.frame(data_4)
#for Frozen
Tmin4=742.2-1.5*(3554.2-742.2)
Tmax4=3554.2+1.5*(3554.2-742.2)
data_5=Wholesale_customers_data$Frozen[which(Wholesale_customers_data$Frozen > Tmin4) & Wholesale_customers_data&Frozen < Tmax4]
data_5n=data.frame(data_5)
#for detergents_paper
Tmin5=256.8-1.5*(40827.0-256.8)
Tmax5=40827+1.5*(40827.0-256.8)
data_6=Wholesale_customers_data$Detergents_Paper[which(Wholesale_customers_data$Detergents_Paper > Tmin5) & Wholesale_customers_data$Detergents_Paper < Tmax5]
data_6n=data.frame(data_6)
#for delicassen
Tmin6=408.2-1.5*(47943.0-408.2)
Tmax6=47943.0+1.5*(47934.0-408.2)
data_7=Wholesale_customers_data$Delicassen[which(Wholesale_customers_data$Delicassen > Tmin6) & Wholesale_customers_data$Delicassen < Tmax6]
data_7n=data.frame(data_7)
#NUMBER 6
#import csv file
Ann <- bind_rows(data_2n,data_3n,data_4n,data_5n,data_6n,data_7n)
write.csv(Ann,file = "Kembabazi.csv")

Assignment1_Kembabazi<-read.csv("Kembabazi.csv")
View(Kembabazi)
#NUMBER 7
max(Wholesale_customers_data$Fresh)
#answer is 112151
max(Wholesale_customers_data$Milk)
#answer is 73498
max(Wholesale_customers_data$Grocery)
#answer is 92780
max(Wholesale_customers_data$Frozen)
#answer is 60869
max(Wholesale_customers_data$Detergents_Paper)
#answer is 40827
max(Wholesale_customers_data$Delicassen)
#answer is 47943
#Fresh had the highest annual spending

#number 8
getmode = function(v) {
  uniqv = unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]}
v=Wholesale_customers_data$Channel
mode_channel=getmode(v)

#Number 9
table(Wholesale_customers_data$Region)[["1"]]
#77
table(Wholesale_customers_data$Region)[["2"]]
#47
table(Wholesale_customers_data$Region)[["3"]]
#316
#Region 2 has the lowest ourchasing power

#number 10
sum(Wholesale_customers_data$Milk[which(Wholesale_customers_data$Region=="1")])
#answer is 422454
sum(Wholesale_customers_data$Milk[which(Wholesale_customers_data$Region=="2")])
#answer is 239144
sum(Wholesale_customers_data$Milk[which(Wholesale_customers_data$Region=="3")])
#answer is 1888759
#region 3 spends most on milk

na=describe(Wholesale_customers_data)







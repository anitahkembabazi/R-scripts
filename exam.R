#question 1
#Call out the necessary libraries
library(tidyverse)
library(dplyr)
library(Hmisc)
library(readxl)
Question1 <- read_excel("C:/Users/kemba/Downloads/Compressed/DSC1101_Exam_Datasets/Question1.xlsx")
View(Question1)
#Part A 
# Checking for missing values 
is.na(Question1)
#The data set does not have missing data 
#Checking the data set for outliers
find_outlier <- function(x){
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  IQR = Q3 -Q1
  x > Q3 + (IQR*1.5)| x <Q1 -(IQR*1.5)
}
names(Question1)

  
remove_outlier <- function(dataframe, columns = names(dataframe)) {
    
  
  for (col in columns) {
      
      dataframe <- dataframe[!find_outlier(dataframe[[col]]), ]
    }
    
    
    print("Remove outliers")
    print(dataframe)
  }

Data_new = remove_outlier(Question1, c('sr','pop15','pop75','dpi','dppi'))
view(Data_new)

#Part B 
#Transforming the dataset

write.csv(Data_new, file = "Kembabazi.csv")

#Part C
summary(Question1$`sr`)
shapiro.test(Question1$`sr`)
#The p value is 0.5836 which is greater than 0.05 mean is 9.671 and median is 10.510
#The mean<median implyiing that sr is not normally distributed implying left skewedness

summary(Question1$`dpi`)
shapiro.test(Question1$`dpi`)
#The pvalue is 3.28e-05 which is lest than 0.05 mean is 1106.76 and median is 695.66
#The mean>meadian hence the dpi is not normally distributed also implying right skewedness

summary(Question1$`pop15`)
shapiro.test(Question1$`pop15`)
#The p value is 8.41e-05 which is less than 0.05 mean is 35.09 and median is 32.58
#The mean>median by a small extent but still implies than pop 15 is not normally distributed 

summary(Question1$`pop75`)
shapiro.test(Question1$`pop75`)
#The p value is 0.001434 which is less than 0.05 mean is 2.293 and median is 2.175
#The mean= median hence pop75 is normally distributed 

summary(Question1$`ddpi`)
shapiro.test(Question1$`ddpi`)
#The pvalue is 1.404e-06 which is less than 0.05 mean is 3.758 and median is 3.000
#The mean>median implying that not is normally distributed


#for sr
q1 = 6.9700
q3= 12.617
iqr = print(q3-q1)
#iqr = 5.647

#for pop15
q1 =26.21
q3= 44.06
iqr = print(q3-q1)
#iqr =17.85

#for pop 75
q1 = 1.125
q3= 3.325
iqr = print(q3 - q1)
#iqr = 2.2

#for dpi
q1 = 288.21
q3 = 1795.62
iqr= print(q3 -q1 )
#iqr = 1507.41


#for ddpi
q1=2.002
q3=4.478
iqr = print(q3 -q1)
#iqr = 2.2476


#Part D
#Graph showing the distribution of population under 15
library(ggplot2)
ggplot(data = Question1, mapping = aes(y = pop15)) +
  geom_boxplot() +
  labs(y="pop15", 
       title="Distribution of Population under 15")
#The population under 15 is not normally distributed 


#Question 2 
library(readxl)
Question2 <- read_excel("C:/Users/kemba/Downloads/Compressed/DSC1101_Exam_Datasets/Question2.xlsx")
View(Question2)


#check for missing values
is.na(Question2)
#There are no missing values in the dataset
names(Question2)
#Handling Outliers
detect_outlier <- function(x){
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  IQR = Q3 -Q1
  x > Q3 + (IQR*1.5)| x <Q1 -(IQR*1.5)
}
remove_outlier <- function(dataframe, columns = names(dataframe)) {
  
  for (col in columns) {
    
    
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

Data2_new = remove_outlier(Question2, c('Sesn','tillage','Weigh_bigtubers','Weight_smalltubers','plotsize','locn','ferT','No_mediumtubers','Totaltuberno','HEC','block','Plants_harvested','Weight_mediumtubers','AV_tubers_Plant','TotalWeightperhectare','rep','No_bigtubers','No_smalltubers','Total_tubweight','TotalTuberperHectare'))
view(Data2_new)


#saving the data as a csv file 
write.csv(Data2_new, file = "Kembabazi.csv")

#Part B
max(Question2$`Totaltuberno`)
#The highest average in Totaltuberno is 443
str(Question2)

#Part C
#Visuals (count graph) 
#Relationship between Categorical variables
ggplot(data = Question2) +
  geom_count(mapping = aes(x =tillage , y = ferT)) +
  labs(x="tillage", 
       y="FerT",
       title="Relationship between Tillage and FerT")

#Relationship between Tillage and Fert
Contigeny_table = table(Question2$`tillage`, Question2$`ferT`) 
#View the table
Contigeny_table
print(chisq.test(Contigeny_table))
# The p value is 1 which is greater than 0.05 

#Part D
max(Question2$`Plants_harvested`)
#The highest number of plants harvested was 28
fert_add = filter(Question2, Question2$`Plants_harvested` == 28)
fert_add
#I would advise farmers to use  fertilizer F2150





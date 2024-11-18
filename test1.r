#Section A
library(readxl)
SectionA <- read_excel("~/BSDS test 1/SectionA.xlsx")
View(SectionA)


#number 1 
#population is the variable called Origin
#sample is the variable called variety
#Observations include; Aroma,Flavor,Slt,bitter,After Taste,Mouthfeel,Clean cups,Overall

#number 2
#The continous variables include:Fragrance/Aroma,Flavour,Salt/Acid,Bitter/Sweet,Aftertaste,Mouthfeel,Balance and Overall
#The categorical variables include:Origin, Variety,uniformity,clean cups
#The qualitative variables include:Origin and Variety

#Number 3
#question:Is there a significant difference between the coffee Flavor and Aftertaste?
cor.test(SectionA$FLAVOR,SectionA$AFTERTASTE,use="complete.obs")
#p-value = 1.401e-09 which is less than 0.05 and the correlation is 0.629962 
#thereby the null hypothesis is rejected such that the true correlation coefficient is not zero

#question:Is there a difference between the Salt/Acid and the mouth feel?
cor.test(SectionA$`SALT/ ACID`,SectionA$`MOUTH FEEL`,use="complete.obs")
#p-value = 1.687e-07which is less than 0.05 and the correlation is 0.5606237
#thereby the null hypothesis is rejected such that the true correlation coefficient is not zero

#Number4
#Performing descriptive statistics on Overall
library(Hmisc)
describe(SectionA$OVERALL )
#using the describle has enabled us to generate a summary of the descriptiv statistics for the dataset saved as SectionA and provides an overview of the data,s central tendency,dispersion and shape
#Alternatively
sapply(SectionA$OVERALL,mean)
summary(SectionA$OVERALL )
#summary gives us the min,max,qualtiles, meadian and mean of the overall variable 

#Number 5
#using graphs to display the distribution of 'Overall'
boxplot(SectionA$OVERALL, main= 'Boxplot showing the distribution of Overall')
plot(density(SectionA$OVERALL),main = 'Graph showing the distribution of Overall' )
hist(SectionA$OVERALL,main = 'Graph showing the distribution of Overall')

#Number 6
#To test for the normally distributed variables
hist(SectionA$`FRAGRANCE/AROMA`)# not normally distributed since it is scewed to the right
hist(SectionA$FLAVOR)#not normally distributed since it is scewed to the left
hist(SectionA$`SALT/ ACID`)#not normally distributed
hist(SectionA$`BITTER/ SWEET`)#not normally distributed since it is scewed to the left
hist(SectionA$AFTERTASTE)# It is normally distributed
hist(SectionA$`MOUTH FEEL`)#Not normally distributed
hist(SectionA$BALANCE)#not normally distributed 
hist(SectionA$UNIFORMITY)#not normally distributed 
hist(SectionA$`CLEAN CUPS`)#not normally distributed
hist(SectionA$OVERALL)#It is normally distributed 

#SECTION B
library(readxl)
SectionB <- read_excel("~/BSDS test 1/SectionB_1.xlsx")
View(SectionB)

#Part b
#Finding and removing outliers
library(tidyverse)
library(dplyr)

Outliers_ID<-boxplot.stats(SectionB$ID)$out
outliers_Carat<-boxplot.stats(SectionB$carat)$out
outliers_depth<-boxplot.stats(SectionB$depth)$out
outliers_Price<-boxplot.stats(SectionB$price)$out
outliers_X<-boxplot.stats(SectionB$x)$out
outliers_Y<-boxplot.stats(SectionB$y)$out

#Replacing missing values with median
SectionB_new = SectionB %>% mutate(across(where(is.numeric), ~replace_na(.,median(., na.rm=TRUE))))

#removing outliers 
#first create funtion that can detect outliers
detect_outlier <- function(x) {
  #calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  #calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  #calculate inter quantile range
  IQR = Quantile3 - Quantile1
  
  #return true or false for outlier
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 + (IQR * 1.5)
}

#functions that removes outliers
remove_outliers <- function(dataframe, columns = names(dataframe)){
  
  # for loop to transverse in columns vector
  for (col in columns) {
    
    #remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  #return data frame
  print("Remove outliers")
  print(dataframe)
}

SectionB_new2 = remove_outliers(SectionB_new, c('ID', 'carat', 'depth', 'price', 'x','y'))
#saving file as a csv
write.csv(SectionB_new2, file = "SectionB.csv")

#Part b
summary(SectionB_new$price)
hist(SectionB_new$price)#showing that it is skewed to the right
median_price=median(SectionB_new$price)
print(median_price)
#median price is 2401
t_test_result=t.test(SectionB_new$price,mu=0)
print(t_test_result)
#alternative hypothesis: true mean is not equal to 0
#the mean is 39392.8

#Part c
#showing relationship between carat and depth
ggplot(data = SectionB_new, mapping = aes(x = carat, y = depth)) +
  geom_point() + geom_smooth()
labs(x="carat",
     y="depth",
     title="Relationship between carat and depth")

cor.test(SectionB_new$carat, SectionB_new$depth,use="complete.obs")
#correlation is 0.02822431    and p-value = 5.518e-11 hence reject null hypothesis

#part d
#correlation between cut and x
ggplot(data = SectionB_new, mapping = aes(x = cut, y = x)) +
  geom_boxplot() +
  labs(x="cut",
       y="x",
       title="Relationship between cut and x")


#part e
#relationship between P and PC
ggplot(SectionB) +
  geom_count(mapping = aes(x=P, y=PC)) +
  labs(x="P",
       y="PC",
       title="Relationship between P and PC")

#number 2
#part a 
#I would use the following predictor variables
#cut,colour,clarity,depth,x,y,P,PC and ID

#part b 
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(rpart) #builds the model
library(rpart.plot) #plots the decision tree
library(ggplot2)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)
library(MASS)
library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(moments)
library(bestNormalize)
library(magrittr)
library(ggcorrplot)


#Pre-process data 
#1.Pre-process variables
SectionB$ID = as.factor(SectionB$ID)
SectionB$cut= as.factor(SectionB$cut)
SectionB$colour = as.factor(SectionB$colour)
SectionB$clarity = as.factor(SectionB$clarity)
SectionB$P = as.factor(SectionB$P)
SectionB$PC= as.factor(SectionB$PC)


SectionB2 = subset(SectionB, select = -ID)



 #Split data in to training and testing dataset
#Data can be split as 70-80% training data, and 20-30% testing data
#But remember our target variable is Revenue which is in column 10
library(caTools)
# set seed to ensure you always have same random numbers generated
set.seed(123)
# splits the data in the ratio mentioned in SplitRatio (70%. 
sample = sample.split(SectionB2,SplitRatio = 0.7)
# create a training set 
train_set =subset(SectionB2,sample ==TRUE)
# create a testing set
test_set=subset(SectionB2, sample==FALSE)

#Generate a multilinear regression model
multi_model <- lm(Price ~ ., data=train_set)
summary(multi_model)
#part c

 #Model Validation
# Used to predict the Revenue
prediction <- predict(multi_model, newdata = test_set)
prediction


                   
                   


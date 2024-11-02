#Data importation
library(readxl)

Assngmt1<-read_excel("C:/Users/kemba/Downloads/MOCK_DATA.xlsx")

View(Assngmt1)

#data exploration(Central tedency)
summary(Assngmt1)

#search for missing data
sum(is.na(Assngmt1))


  #normal distribution of data
library(ggplot2)
ggplot(Assngmt1,aes(x=weight))+geom_histogram()
                 
boxplot(Assngmt1$income, main="income",boxwex=0.1)


#caTools package is needed as it provided a collection of tools for data analysis including functions
#for splitting data

install.packages("caTools")


library(caTools)
library(ggplot2)
View(diamonds)
help("diamonds")


#Split Data
sample.split(diamonds$price,SplitRatio = 0.65) -> split_values

subset(diamonds,split_values == T) -> train_reg
subset(diamonds,split_values==F)->test_reg

#Building the Linear Model
#price being the dependent variable and the rest of the columns as independent variables
#build the model on the train set
lm(price ~., data = train_reg) -> mod_regress

#train the model on the test set
predict(mod_regress,test_reg) -> result_regress

#Bind the predicted values with the actual values
cbind(Actual = test_reg$price, predicted = result_regress)-> Final_Data

#Convert the matrix to dataframe
as.data.frame(Final_Data) -> Final_Data

#Finding Error - the lower the value of the rmse, the better the model
(Final_Data$Actual - Final_Data$predicted) -> error
cbind(Final_Data,error)-> Final_Data
rmse <- sqrt(mean(Final_Data$error^2))
rmse


#Clustering with K-Means clustering. Only works on numerical data columns--Unsupervised ML
#To use the Iris data set 
View(iris)
iris[1:4] -> iris_k
#convert the data frame into a matrix
#as.matrix(iris_k) -> iris_k

#Use kmeans to divide the data set into clusters

set.seed(123)
kmeans_result <- kmeans(iris_k, centers = 3, nstart = 25)


#print(clustered_Data)

#visualize
install.packages("factoextra")
install.packages("cluster")
library(factoextra)
library(cluster)

#fviz_cluster(list(data=iris, cluster=iris_cluster))
fviz_cluster(kmeans_result, data=iris_k,
             palette = "jco", #color palette
             ggtheme = theme_minimal(),#ggplot2 theme
             main ="K-Means Clustering of Iris Data")

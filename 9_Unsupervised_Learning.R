### ML: UNSUPERVISED LEARNING ####
#setwd
#Import libraries
library(cluster)
library(NbClust)#number of clusters
library(readr)
library(readxl)
library(clValid)
library(factoextra)
library(tidyverse)
library(magrittr)
library(fpc)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(clValid)

##DATASET: Bikesales
##Target variable: Prodcut_Category
#Import dataset
library(readxl)
data <- read_excel("C:/Users/ekihe/Downloads/Bike_Sales.xlsx")
View(Bike_Sales)##Basic exploration
#Aggregating data by the product_category
result=aggregate(data, by=list(data$Product_Category), mean) 
#Result: Mean customer age buying the three products are:
# accessories: 36.08423
# bikes: 35.34824
#clothing. 36.11196

#see the variables that ave the strongest relastionship with the target by chi square test 
##A. USL Using Principal Component Analysis (PCA)
#Remove the useless variables e.g. Date
data2 <- data[,-1]

#Remove the target variable Product_Category
data3 <- data2[,-9]

#Comparing distribution of variables
par(mfrow= c(2,5), bg = "bisque")
for (i in 1:ncol(data3)) {
  boxplot(data3[,i], main = names(data3)[i],
          col = "chocolate", 
          border = "khaki4", 
          notch = T)
}
#Result: The variable boxplots show outliers and varying sclaes
#Therefore variables should be scaled/normalised before modelling
#Would be better to use kmedoids and over kmeans algorith
#as it used median and not mean
#If the variables are correlated to the target, use the PCA
#Also ensure to only work with numeric variables
data4 <- data3 %>% select(1,4,11:16)
data_pca <- prcomp(data4, center = TRUE, scale. = TRUE)
summary(data_pca)

#Generate the number of principal components based on the proportion of variance
#PC1-PC3 have variance of 84.23%
#PC1-PC4 have variance of 95.61
data_pca$rotation[,1:4] #Four components

#View the contribution of variables to each component
fviz_pca_var(data_pca,
             col.var = "contrib", 
             gradient.cols = (("Pastel2")), 
             repel = TRUE     )

#view contribution of individual data points to principal components
fviz_pca_ind(data_pca,
             col.ind = "cos2",  
             gradient.cols = ("Pastel2"), 
             repel = TRUE)

#Use to make predictions of classification
pcadata <- predict(data_pca)[,1:4]

##B. USL Using distance measure
#Eucludian or Hamming distance 
dist_euc <- get_dist(data4, stand = TRUE)
fviz_dist(dist_euc)



##C: USL Using Kmeans clustering
#Figure out number of K or clusters
#Subsetting data3 to work with only numeric variables
data4 <- data3 %>% select(1,4,11:16)

#i. Using Elbow Method
#Set sum of mean square errors to zero
wss <- 0
# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(data4, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

## Select number of clusters
k <- 3

# Build model with k clusters: km.out
km.product <- kmeans(data4, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.product

#RESULT:
#K-means clustering with 3 clusters of sizes 13760, 6521, 92755

# Plot of Quantity vs. Revenue by cluster membership
plot(data4[, c("Order_Quantity", "Revenue")],
     col = km.product$cluster,
     main = paste("k-means clustering of Product_Category with", k, "clusters"),
     xlab = "Order_Quantity", ylab = "Revenue")



#Alternatively using result from PCA to calculate Elbow method for K
fviz_nbclust(pcadata, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow Method") 


##ii. Determining K using Silhoutte method
fviz_nbclust(pcadata, # data
             kmeans, # clustering algorithm
             method = "silhouette")
#Result: Select the K with the highest silhoutte value



##iii. Determining k using Dunn Index
# function to draw dunn plot
fviz_dunn <- function(data) {
  k <- c(2:10)
  dunnin <- c()
  for (i in 2:10) {
    dunnin[i] <- dunn(distance = dist(data), clusters = kmeans(data, i)$cluster)
  }
  dunnin <- dunnin[2:10]
  plot(k, dunnin, xlab = "Cluster number k",
       ylab = "Dunn Index",
       main = "Dunn Plot", cex.main=1,
       col = "dodgerblue1", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl = -0.2)
  abline(v=which(dunnin==max(dunnin)) + 1, lwd=1, col="red", lty="dashed")
}
#Use the function
fviz_dunn(data4)
#Result. Select the K with the highest Dunn index



###D. USL Using K-Medoids Clustering
#goal is to get number of K
#Not based on means but rather medians
#Generates a dissimilarity graph, done by the dist function
product.disimilarity <- dist(data4.1)
kmed.1 <- cluster::pam(x= product.disimilarity, k=4)
head(kmed.1$clustering)
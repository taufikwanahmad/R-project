#----------------Hierachical CLUSTERING----------------------

install.packages("readxl")
library(readxl)
getwd()
trans_data <- read_xlsx("Data source_merged_for_model.xlsx")
install.packages("plyr")
library(plyr)
View(trans_data)
#trans_data_small <- trans_data[1:1000, ]
#trans_data_small1 <- trans_data[,c(-1, -4, -11, -16, -17)]
trans_data1 <- trans_data[,c(-1, -4, -11, -16, -17)]
View(trans_data1)

trans_data2 <- cbind(trans_data1, dummy(trans_data1$Lightening, sep = "_"))
trans_data2 <- cbind(trans_data2, dummy(trans_data2$corrosive, sep = "_"))
trans_data2 <- trans_data2[, c(-4, -9)]

#trans_data_small2 <- cbind(trans_data_small1, dummy(trans_data_small1$Lightening, sep = "_"))
#trans_data_small2 <- cbind(trans_data_small2, dummy(trans_data+small2$corrosive, sep = "_"))
#trans_data_small2 <- trans_data_small2[, c(-4, -9)]

#normalize_data <- scale(trans_data_small2)
#summary(normalize_data)

normalize_data <- scale(trans_data2)
summary(normalize_data)



# Distance Matrix
d <- dist(normalize_data, method="euclidean")
#d <- dist(normalize_data, method="euclidean")

# Hierarchical Clustering using complete linkage
fit <- hclust(d, method="complete")

# Display Dendrogram
plot(fit, hang=-1)

# Cut tree into 5 clusters
groups <- cutree(fit, k=5)

# Creating 5 clusters with proper rectangular boxes
rect.hclust(fit, k=5, border="red")

# Assigning groups in a matrix format
membership <- as.matrix(groups)

# Adding membership as a column in dataframe
final <- data.frame(trans_data, membership)

# Moving membership to first column
final1 <- final[,c(ncol(final), 1:(ncol(final)-1))]

# Converting data to .csv format
write.csv(final1, "trans_data_final.csv")

##----------------------KMEANS CLUSTERING----------------------

results <- kmeans(normalize_data, 5)

# attributes of clusters
attributes(results)

# Centers of each attribute
results$centers

# Compare the orignial trip cause with the 5 clusters we created
table(trans_data$`Trip cause`, results$cluster)

# clustering is not good

##-------------------------Principal Component Analysis--------------

trans_pca <- read_xlsx("Data source_merged_for_model.xlsx")
View(trans_pca)
str(trans_pca)

summary(trans_pca)
# summary shoes the values are in different units and scales


trans_pca1 <- trans_data[,c(-1, -4, -11, -16, -17)]
View(trans_pca1)

trans_pca2 <- cbind(trans_pca1, dummy(trans_pca1$Lightening, sep = "_"))
trans_pca2 <- cbind(trans_pca2, dummy(trans_pca2$corrosive, sep = "_"))
trans_pca2 <- trans_pca2[, c(-4, -9)]

# build correlation matrix
cor(trans_pca2)

# Apply PC algorithm on my dataset, correlation matrix is true, scores is true
# covariance matrix is NULL as we dont need it
pcaobj <- princomp(trans_pca2[,1:10], cor=TRUE, scores=TRUE, covmat=NULL)

summary(pcaobj)

str(pcaobj)
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
pcaobj$scores[,1:6]

trans_pca2 <- cbind(trans_pca2, pcaobj$scores[,1:6])
View(trans_pca2)

clus_data <- trans_pca2[,19:24]
norm_clus <- scale(clus_data)
summary(norm_clus)

dist2 <- dist(norm_clus, method="euclidean")
fit2 <- hclust(dist2, method="complete")
plot(fit2, hang=-0.1)

groups <- cutree(fit2, 5)

membership_2 <- as.matrix(groups)
View(membership_2)

final_2 <- cbind(membership_2, trans_pca2)
View(final_2)
View(floor(aggregate(final_2[, -c(2:18)], by = list(membership_2), FUN = mean)))
write.csv(final_2, file = "trans_trip_clustered.csv", row.names = F, col.names = F)



dim(trans_pca)
table(trans_pca$`Trip cause`)
str(trans_pca)
trans_pca$`Trip cause` <- factor(trans_pca$`Trip cause`)
str(trans_pca)
summary(trans_pca[,1:15])
trans_pca_new <- trans_pca[, c(-4, -6, -11, -12)]
trans_pca_new <- trans_pca_new[, c(-12, -13)]


# Normalize Data
normalize_data <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_n <- as.data.frame(lapply(trans_pca_new[1:11], normalize_data))
summary(data_n)

# splitting data to train and test
data_train <- data_n[1:3076,]
data_test <- data_n[3077:4076,]

# Exclude the target variable from train and test data
data_train_labels <- data.frame(trans_pca[1:3076,16])
colnames(data_train_labels) <- "Trans_cause"
data_test_labels <- data.frame(trans_pca[3077:4076,16])
colnames(data_test_labels) <- "Trans_cause"

# package for KNN
install.packages("class")
library(class)

sqrt(178)
data_test_pred <- knn(data_train, data_test, cl=data_train_labels$Trans_cause, k=12)
class(data_test_pred)

# comparing predictions of test data with original data
table(data_test_pred, data_test_labels$Trans_cause)

# package for cross table
install.packages("gmodels")
library(gmodels)

CrossTable(x=data_test_labels$Trans_cause, y=data_test_pred, prop.chisq = FALSE)

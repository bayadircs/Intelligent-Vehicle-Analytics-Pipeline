# Libraries
library(dplyr)
library(ggplot2)
library(factoextra)
 
#Load data
data <- read.csv("car_realistic.csv")
str(data)
summary(data)
 
#Clean data
# Impute missing numeric values with the median (NOT zero, which would create fake cars)
data$Speed[is.na(data$Speed)]       <- median(data$Speed, na.rm = TRUE)
data$Distance[is.na(data$Distance)] <- median(data$Distance, na.rm = TRUE)
 
data <- distinct(data)            # drop duplicate rows
data$Status <- as.factor(data$Status)
 
summary(data)
 
#Explore: save plots as PNG
png("plot_speed_distance.png", width = 800, height = 600)
print(
  ggplot(data, aes(x = Speed, y = Distance)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red", se = FALSE) +
    ggtitle("Speed vs Distance")
)
dev.off()
 
png("plot_efficiency_distance.png", width = 800, height = 600)
print(
  ggplot(data, aes(x = Efficiency, y = Distance)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue", se = FALSE) +
    ggtitle("Efficiency vs Distance")
)
dev.off()
 
#Group summary
car_summary <- data %>%
  group_by(Status) %>%
  summarise(AvgDistance = mean(Distance))
print(car_summary)
 
#Linear regression (predict Distance)
model_reg <- lm(Distance ~ Speed + Efficiency, data = data)
summary(model_reg)
 
#Logistic regression (predict Status)
# Status has two levels (Average / Good); glm treats Good as 1, Average as 0.
model_class <- glm(Status ~ Speed + Efficiency, data = data, family = binomial)
summary(model_class)
 
data$predicted       <- predict(model_class, type = "response")
data$predicted_class <- ifelse(data$predicted > 0.5, "Good", "Average")
 
# Evaluate with a confusion matrix and accuracy
conf <- table(Actual = data$Status, Predicted = data$predicted_class)
print(conf)
accuracy <- sum(diag(conf)) / sum(conf)
cat("Classification accuracy:", round(accuracy, 3), "\n")
 
#Clustering
cluster_data   <- data[, c("Speed", "Efficiency", "Distance")]
cluster_scaled <- scale(cluster_data)
 
# Justify the number of clusters with an elbow plot
png("plot_elbow.png", width = 800, height = 600)
print(fviz_nbclust(cluster_scaled, kmeans, method = "wss"))
dev.off()
 
set.seed(123)
kmeans_result <- kmeans(cluster_scaled, centers = 3)
 
png("plot_clusters.png", width = 800, height = 600)
print(
  fviz_cluster(kmeans_result, data = cluster_scaled,
               palette = c("purple", "orange", "cyan"))
)
dev.off()
 
#Keyword tagging of reviews
# Note: this is simple keyword matching, not full sentiment analysis.
data$Sentiment <- ifelse(grepl("excellent|smooth", data$Review, ignore.case = TRUE),
                         "Positive", "Other")
print(table(data$Sentiment))
 
# Key insights
#Speed and Efficiency both relate positively to Distance.
#Logistic model classifies Good vs Average status; see accuracy above.
#K-means reveals three vehicle groups (k chosen via elbow plot).
#Review tagging flags Excellent/Smooth reviews as Positive.
 

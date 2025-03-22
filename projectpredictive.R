#using ANN 

# Load necessary libraries
install.packages("neuralnet")
library(neuralnet)
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())
str(data)

colnames(data)

#Convert target variable 'class_output' to binary numeric format
# Assuming 'Abnormal' as 1 and 'Normal' as 0 for classification
data$class_output <- ifelse(data$class_output == "Abnormal", 1, 0) 

# Normalize numeric features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to all numeric columns except the target ('class_output')
data_norm <- as.data.frame(lapply(data[, 1:6], normalize))  # Select only feature columns (1 to 6)

# Add the target variable back to the normalized data
data_norm$class_output <- data$class_output

# Split the data into training (75%) and testing (25%) sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data_norm), 0.75 * nrow(data_norm))
train_data <- data_norm[train_index, ]
test_data <- data_norm[-train_index, ]

# Build the formula dynamically based on feature columns
formula <- as.formula(paste("class_output ~", paste(names(data_norm)[1:6], collapse = " + ")))

# Train the ANN model with one hidden layer of 5 neurons
ann_model <- neuralnet(
  formula,
  data = train_data,
  hidden = 5,
  linear.output = FALSE,
  stepmax = 1e6  # To handle convergence for larger datasets
)

# Plot the trained neural network
plot(ann_model) #okk

# Predict on the test dataset
model_results <- predict(ann_model, test_data[, 1:6])  # Use only the feature columns for prediction

# Process the prediction results correctly
predicted_class <- ifelse(model_results > 0.5, 1, 0)  # Directly threshold the prediction matrix/vector

# Evaluate performance
actual_class <- test_data$class_output
confusion_matrix <- table(Predicted = predicted_class, Actual = actual_class)

print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))



#decision tree


# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Load dataset
mydata <- read.csv(file.choose())

# Case  With pelvic_incidence
features_case1 <- c("pelvic_incidence", "lumbar_lordosis_angle", "pelvic_radius", "degree_spondylolisthesis")
target <- "class_output"

# Splitting dataset into training and testing (70% training, 30% testing)
set.seed(42)
indexes <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data_case1 <- mydata[indexes, ]
test_data_case1 <- mydata[-indexes, ]

# Train Decision Tree 
tree_case1 <- rpart(class_output ~ pelvic_incidence + lumbar_lordosis_angle + pelvic_radius + degree_spondylolisthesis, 
                    data = train_data_case1, method = "class")

# Visualize the Decision Tree
rpart.plot(tree_case1, main = "Decision Tree ")

# Make predictions 
predictions_case1 <- predict(tree_case1, test_data_case1, type = "class")

# Evaluate the model 
conf_matrix_case1 <- table(test_data_case1$class_output, predictions_case1)
print("Confusion Matrix ")
print(conf_matrix_case1)
accuracy_case1 <- sum(diag(conf_matrix_case1)) / sum(conf_matrix_case1)
print(paste("Accuracy", round(accuracy_case1, 4)))






# svm 

# Set working directory and load dataset
getwd()
dataset <- read.csv(file.choose()) 
# Replace with actual dataset path
str(dataset)

# Encoding target variable as a factor
dataset$class_output <- factor(dataset$class_output, levels = unique(dataset$class_output))

# Case 1: With pelvic_incidence
features_case1 <- c("pelvic_incidence", "lumbar_lordosis_angle", "pelvic_radius", "degree_spondylolisthesis")
target <- "class_output"

dataset_case1 <- dataset[, c(features_case1, target)]

# Splitting the dataset into training and test sets
install.packages("caTools")
library(caTools)


set.seed(123)
split <- sample.split(dataset_case1$class_output, SplitRatio = 0.7)
training_set_case1 <- subset(dataset_case1, split == TRUE)
test_set_case1 <- subset(dataset_case1, split == FALSE)

# Fitting SVM to the training set 
library(e1071)
classifier_case1 <- svm(formula = class_output ~ .,
                        data = training_set_case1,
                        type = 'C-classification',
                        kernel = 'linear')

# Predicting test set results
y_pred_case1 <- predict(classifier_case1, newdata = test_set_case1[, -5])

# Making the confusion matrix 
cm_case1 <- table(test_set_case1$class_output, y_pred_case1)
print("Confusion Matrix - Case 1")
print(cm_case1)



#k means clustering 
install.packages("cluster")
library(cluster)
dataset <- read.csv(file.choose())  # Replace with actual path to your dataset
#  With pelvic_incidence
features_case1 <- c("pelvic_incidence", "lumbar_lordosis_angle", "pelvic_radius", "degree_spondylolisthesis")
dataset_case1 <- dataset[, features_case1]
# Fitting K-Means clustering model 
set.seed(240)
kmeans_case1 <- kmeans(dataset_case1, centers = 3, nstart = 20)  # centers = number of clusters, nstart = number of random initializations
# Cluster identification for each observation
print("Cluster Assignments ")
print(kmeans_case1$cluster)
print("Cluster Centers ")
print(kmeans_case1$centers)
# Confusion Matrix 
cm_case1 <- table(dataset$class_output, kmeans_case1$cluster)
print("Confusion Matrix ")
print(cm_case1)
# Plotting clusters 
plot(dataset_case1[c("pelvic_incidence", "lumbar_lordosis_angle")],
     col = kmeans_case1$cluster,
     main = "K-Means Clustering with 3 Clusters ",
     xlab = "Pelvic Incidence",
     ylab = "Lumbar Lordosis Angle")
points(kmeans_case1$centers[, c("pelvic_incidence", "lumbar_lordosis_angle")],
       col = 1:3,
       pch = 8,
       cex = 3)
y_kmeans_case1 <- kmeans_case1$cluster
clusplot(dataset_case1[, c("pelvic_incidence", "lumbar_lordosis_angle")],
         y_kmeans_case1,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster Plot ",
         xlab = "Pelvic Incidence",
         ylab = "Lumbar Lordosis Angle") 
# Load Required Libraries
library(ggplot2)
library(rpart)
library(rpart.plot)
library(neuralnet) 

# Load the Dataset
dataset <- read.csv(file.choose())  # Replace with the correct path
dataset$class_output <- as.factor(dataset$class_output)  # Ensure the target is a factor

# Split the Data into Training and Testing Sets
set.seed(42)
indexes <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train <- dataset[indexes, ]
test <- dataset[-indexes, ]

# Initialize Accuracy Data
model_accuracies <- data.frame(
  Model = c("Decision Tree", "ANN", "SVM", "K-Means Clustering"),
  Accuracy = c(NA, NA, NA, NA) # Placeholder for accuracies
)

# ------------------------------
# 1. Decision Tree Model
# ------------------------------
target <- class_output ~ pelvic_incidence + lumbar_lordosis_angle + pelvic_radius + degree_spondylolisthesis
tree <- rpart(target, data = train, method = "class")
pred_tree <- predict(tree, test, type = "class")

# Accuracy
conf_matrix_tree <- table(test$class_output, pred_tree)
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
model_accuracies$Accuracy[1] <- accuracy_tree


# ------------------------------
# 2. ANN Model
# ------------------------------
# Normalize Features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize Data (Exclude the Target Variable)
data_norm <- as.data.frame(lapply(data[, 1:6], normalize))  # Select feature columns
data_norm$class_output <- data$class_output

# Split Data
train_index <- sample(1:nrow(data_norm), 0.75 * nrow(data_norm))
train_data <- data_norm[train_index, ]
test_data <- data_norm[-train_index, ]

# Build the ANN Formula Dynamically
formula <- as.formula(paste("class_output ~", paste(names(data_norm)[1:6], collapse = " + ")))

# Train the ANN Model
ann_model <- neuralnet(
  formula,
  data = train_data,
  hidden = 5,  # Single hidden layer with 5 neurons
  linear.output = FALSE,
  stepmax = 1e6  # Handle convergence issues for larger datasets
)

# Predict
ann_pred <- predict(ann_model, test_data[, 1:6])  # Use feature columns for prediction
predicted_class <- ifelse(ann_pred > 0.5, 1, 0)  # Convert probabilities to binary classification

# Accuracy
conf_matrix_ann <- table(Predicted = predicted_class, Actual = test_data$class_output)
accuracy_ann <- sum(diag(conf_matrix_ann)) / sum(conf_matrix_ann)
model_accuracies$Accuracy[2] <- accuracy_ann




# ------------------------------
# 3. SVM Model
# ------------------------------
library(e1071)

# Encoding Target Feature
train$class_output <- factor(train$class_output)
test$class_output <- factor(test$class_output)

# Train SVM Model
svm_classifier <- svm(
  formula = class_output ~ pelvic_incidence + lumbar_lordosis_angle + pelvic_radius + degree_spondylolisthesis,
  data = train,
  type = "C-classification",
  kernel = "linear"
)
svm_pred <- predict(svm_classifier, newdata = test)

# Accuracy
conf_matrix_svm <- table(test$class_output, svm_pred)
accuracy_svm <- sum(diag(conf_matrix_svm)) / sum(conf_matrix_svm)
model_accuracies$Accuracy[3] <- accuracy_svm

# ------------------------------
# 4. K-Means Clustering
# ------------------------------
library(cluster)

# Clustering
features_case1 <- c("pelvic_incidence", "lumbar_lordosis_angle", "pelvic_radius", "degree_spondylolisthesis")
dataset_case1 <- dataset[, features_case1]

set.seed(240)
kmeans_model <- kmeans(dataset_case1, centers = 3, nstart = 20)

# Confusion Matrix
conf_matrix_kmeans <- table(dataset$class_output, kmeans_model$cluster)

# Adjust Clusters (Matching Predicted Labels to Actual Labels)
map_clusters <- apply(conf_matrix_kmeans, 2, which.max)
pred_kmeans <- map_clusters[kmeans_model$cluster]

# Accuracy
accuracy_kmeans <- mean(pred_kmeans == as.numeric(dataset$class_output))
model_accuracies$Accuracy[4] <- accuracy_kmeans

# ------------------------------
# Plot the Model Accuracies
# ------------------------------
ggplot(data = model_accuracies, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Model Accuracy Comparison (Vertebral Column Dataset)",
       x = "Model",
       y = "Accuracy") +
  geom_text(aes(label = round(Accuracy, 4)), vjust = -0.5, size = 4)


# Create a heatmap for model accuracies
accuracy_matrix <- as.matrix(model_accuracies$Accuracy)  # Convert the accuracy data to matrix format

# Plotting the heatmap
ggplot(data = model_accuracies, aes(x = Model, y = 1, fill = Accuracy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Model Accuracy Heatmap", x = "Model", y = "") +
  geom_text(aes(label = round(Accuracy, 4)), color = "black", size = 6, vjust = 0.5) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())


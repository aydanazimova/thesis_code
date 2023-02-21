library(readxl)
library(fuzzywuzzyR)
library(dplyr)
library(stringr)
library(stringdist)
library(dplyr)
library(caret)
library(ggplot2)

data <- read.csv("C:/Users/Aydan/Desktop/data.csv")

# Remove -- and NA values
data <- data[-which(data$description == " --"),]
sum(is.na(data)) 
data=data[complete.cases(data),]



summary(data)
data$day=as.Date.character(data$day)
data$keyword_id=as.character(data$keyword_id)
data$keyword_max_cpc=as.numeric(data$keyword_max_cpc)
data$call_to_action=as.factor(data$call_to_action)
data$clicks=as.numeric(data$clicks)
data$quality_score=as.numeric(data$quality_score)
data$impr=as.numeric(data$impr)


data$ratio1=stringdist(data$term, data$headline_1, method ="cosine" ) # Cosine distance
data$ratio2=stringdist(data$term, data$headline_2, method ="cosine" ) # Cosine distance


data <- data %>% mutate(exp_ctr = str_replace_all(exp_ctr, "Below Average", "0"))
data <- data %>% mutate(exp_ctr = str_replace_all(exp_ctr, "Above Average", "2"))
data <- data %>% mutate(exp_ctr = str_replace_all(exp_ctr, "Average", "1"))


data <- data %>% mutate(landing_page = str_replace_all(landing_page, "Below Average", "0"))
data <- data %>% mutate(landing_page = str_replace_all(landing_page, "Above Average", "2"))
data <- data %>% mutate(landing_page = str_replace_all(landing_page, "Average", "1"))


data <- data %>% mutate(ad_relevance = str_replace_all(ad_relevance, "Below Average", "0"))
data <- data %>% mutate(ad_relevance = str_replace_all(ad_relevance, "Above Average", "2"))
data <- data %>% mutate(ad_relevance = str_replace_all(ad_relevance, "Average", "1"))

data$term_match_type[grep("broad", data$term_match_type, ignore.case = TRUE)] <- 0
data$term_match_type[grep("phrase", data$term_match_type, ignore.case = TRUE)] <- 1
data$term_match_type[grep("exact", data$term_match_type, ignore.case = TRUE)] <- 2


data <- data %>% mutate(device = str_replace_all(device, "Computers", "0"))
data <- data %>% mutate(device = str_replace_all(device, "Mobile phones", "1"))
data <- data %>% mutate(device = str_replace_all(device, "Tablets", "2"))



# Define your brand names
brand_names <- c("dreamland", "graffiti", "imetec", "kidly", "bellissima", "spectrum" )
# Define the function to find approximate matches

# Use the agrep function to check for approximate matches
data$brand_match <- sapply(data$search_term, function(x) {
  brand_match <- agrep(brand_names, x, value = TRUE, max.distance = 0.2)
  if (length(brand_match) > 0) {
    return(brand_match[1])
  } else {
    return(NA)
  }
})

# Add a column indicating whether a brand was found or not
data$brand_found <- ifelse(is.na(data$brand_match), 0, 1)

# Replace the brand match column with the brand found column
data$brand_match <- data$brand_found

# Remove the brand found column
data$brand_found <- NULL

# The resulting dataframe
table(data$brand_match)

# 
# # Check the dimensions of the data
 dim(data)
# 
# # Get the summary statistics of the data
 summary(data)
# 
# # Check for missing values
 hist(data$Roas_group)

# create the histogram
ggplot(data, aes(Roas_group)) + 
  geom_histogram(binwidth = 0.5, fill = "#B6594C") + 
  labs(x = "roas_group", y = "Frequency") + 
  ggtitle("Histogram of ROAS groups")


data$Roas_group=as.factor(data$Roas_group)

ggplot(data, aes(x = weekday, fill = Roas_group)) + 
  geom_bar(position = "fill") +
  labs(x = "Weekday", y = "Proportion of ROAS Groups") +
  ggtitle("Distribution of ROAS groups by Weekday") +
  scale_fill_manual(values = c("#B6594C", "#37514D", "#90AEB2","#EEE6DE" ))


data$device=as.factor(data$device)

ggplot(data, aes(x = "", y = device, fill = device)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  scale_fill_manual(values = c("#DDBE75", "#EEE6DE", "#90AEB2")) +
  ggtitle("Proportion of Device Types")



# Create a density plot
ggplot(data, aes(x = ratio2)) +
  geom_density() +
  labs(x = "similarity 2", y = "Density") +
  ggtitle("Density Plot of similarity")

data$brand_match=as.factor(data$brand_match)

ggplot(data, aes(x = "", y = brand_match, fill = brand_match)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  scale_fill_manual(values = c("#EEE6DE", "#90AEB2")) +
  ggtitle("Appearance of brand names")



ggplot(data, aes(x = Roas_group, fill = call_to_action)) + 
  geom_bar(position = "fill") +
  labs(x = "Roas_group", y = "Proportion of ROAS Groups") +
  ggtitle("Distribution of ROAS groups by Weekday") +
  scale_fill_manual(values = c("#EEE6DE", "#B6594C" ))





data2=cbind.data.frame(data$Roas_group, data$ad_relevance, data$exp_ctr, data$ratio1, data$ratio2, data$landing_page, data$call_to_action, data$brand_match)
colnames(data2)=c("roas_group","ad_relevance","exp_ctr","ratio1", "ratio2","landing_page", "call_to_action", "brand_match")
data=data2



set.seed(123)
train_indx <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indx,]
test_data <- data[-train_indx,]


#LOGISTIC REGRESSION
library(nnet)
model <- multinom(roas_group ~ exp_ctr+ratio1+ratio2+landing_page+ad_relevance+brand_match+call_to_action, data = train_data)


predictions <- predict(model, newdata = test_data)


test_data$roas_group <- factor(test_data$roas_group, levels = c("0", "1", "2", "3"))
predictions <- factor(predictions, levels = c("0", "1", "2", "3"))

# summary(model)


confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)


# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)


varImp(model)


####k neighbors
library(class)
library(caret)

#Create the model
train_data <- train_data[complete.cases(train_data),]
test_data <- test_data[complete.cases(test_data),]
# Include only the predictor variables in the training data
train_predictors <- train_data[, -1]

# Include only the predictor variables in the test data
test_predictors <- test_data[, -1]

# Fit the KNN model
knn_model <- knn(train_data[, -1], test_data[, -1], train_data[, 1], k = 3)

#Predict using the model
predictions <- knn_model
test_data$roas_group=as.factor(test_data$roas_group)
confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)

# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)




#bagging
library(randomForest)
set.seed(123)


model <- randomForest(roas_group ~., data = train_data, ntree = 1, proximity = TRUE)
predictions <- predict(model, test_data)


test_data$roas_group <- factor(test_data$roas_group, levels = c("0", "1", "2", "3"))
predictions <- factor(predictions, levels = c("0", "1", "2", "3"))

confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)

# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

reprtree:::plot.getTree(model)


#stacking

# load the caretEnsemble package
library(caretEnsemble)

# create a list of base models
base_models <- list(
  caret::train(roas_group ~ ., data = train_data, method = "rpart"),
  caret::train(roas_group ~ ., data = train_data, method = "svmRadial")
)

# create a meta-model
meta_model <- caret::train(roas_group ~., data = train_data, method = "glm")

# create the ensemble model
stacking_model <- caretStack(base_models, meta_model, trControl = trainControl(method = "cv"))

# make predictions on the test data
predictions <- predict(stacking_model, newdata = test_data)




#bagging

library(caret)
library(ranger)
library(xgboost)

# Train the base models
model1 <- ranger(roas_group ~ ., data = train_data, num.trees = 100)
model2 <- randomForest(roas_group ~ ., data = train_data, ntree = 100)

# Make predictions using the base models
pred1 <- predict(model1, newdata = test_data, data=train_data)
pred2 <- predict(model2, newdata = test_data[,-1])

# Create a new dataset with the predictions from the base models
stacked_data <- cbind(test_data, pred1, pred2)

# Train the meta-model using the stacked dataset
meta_model <- ranger(Species ~ pred1 + pred2, data = stacked_data, num.trees = 100)

# Make predictions using the meta-model
predictions <- predict(meta_model, newdata = stacked_data)

# Evaluate the accuracy of the model
accuracy <- mean(predictions == test_data$Species)
cat("Accuracy:", accuracy)






#decision tree classifier


(importance(model))

    # Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)






# Gaussian Naive Bayes
library(e1071)

# Train the model
nb_model <- naiveBayes(roas_group ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, test_data)



test_data$roas_group <- factor(test_data$roas_group, levels = c("0", "1", "2", "3"))
predictions <- factor(predictions, levels = c("0", "1", "2", "3"))

confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)


# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

summary(svm_model)

confusion_matrix




#Support vector machine

library(e1071)


# change response variable to factor
data$roas_group <- as.factor(data$roas_group)

# split data into training and testing sets
set.seed(123)
indices <- sample(1:nrow(data), 0.8*nrow(data))
train_data <- data[indices,]
test_data <- data[-indices,]

# fit svm model with 'linear' kernel and cost of 1
svm_model <- svm(roas_group ~ ., data = train_data, kernel = "linear", cost = 1)


# make predictions on test data
predictions <- predict(svm_model, test_data[,-1])


#evaluate

test_data$roas_group <- factor(test_data$roas_group, levels = c("0", "1", "2", "3"))
predictions <- factor(predictions, levels = c("0", "1", "2", "3"))

confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)

# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

summary(svm_model)




##Multinomial logistic regression


# Load the required library
library(dplyr)
library(nnet)


# Fit the multiclass logistic regression model using the "multinom" function from the "nnet" library
model <- multinom(roas_group ~ ., data = train_data)

# Summarize the model
summary(model)

# Make predictions on the test data
predictions <- predict(model, test_data)

confusion_matrix <- confusionMatrix(test_data$roas_group, predictions)



# Print the accuracy
accuracy <- confusion_matrix$overall[1]
print(accuracy)


# Print the precision
precision <- confusion_matrix$byClass[1]
print(precision)


# Print the recall
recall <- confusion_matrix$byClass[2]
print(recall)

# Print the F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)



## Set Working Directory
setwd("C:/Users/vital/Desktop/UNIVERISTY/STAT385/PROJECT")

library(GGally)
library(caret)  
library(ggplot2)
library(randomForest) 
library(rpart)
library(rpart.plot)
library(e1071)
library(aqp)
library(corrplot)
library(class)
library(ltm)
library(glmnet)
set.seed(20241119)  # Set a random seed for reproducibility

#1. Load the Data
#Import the dataset
data <-read.table(file="wdbc.data",sep=",", header=FALSE)

#########################################################################
TI_Fivenum <- function(dataset){
  x <- sort(dataset)
  n <- length(x)
  if(n %% 2 == 1){Q <- fivenum(x[-(n+1)/2])[c(2,4)]}
  if(n %% 2 == 0){Q <- fivenum(x)[c(2,4)]}
  minimum <- min(x)
  med <- median(x)
  maximum <- max(x)
  result <- as.data.frame(list(min = minimum, Q1 = Q[1], median = med, Q3 = Q[2], max = maximum))
  return(result)
}

#########################################################################

#2. Preview the data:
head(data)
tail(data)

#Change column names 
# New names in a vector
new_names <- c("ID number", "Diagnosis", "radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean", "concavity_mean","concave.points_mean",
               "symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave.points_se","symmetry_se",
               "fractal_dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave.points_worst","symmetry_worst","fractal_dimension_worst")


# Loop through column names and assign new names
for (i in 1:length(new_names)) {
  colnames(data)[i] <- new_names[i]
}

# Drop the column named "ID number"
data <- data[, !names(data) %in% "ID number"]
head(data)

#check dimmension of data
dim(data)
#[1] 569  31
nrow(data)
#[1] 569
ncol(data)      # Number of columns
#[1] 31

#3. Handle Missing Data
# Total missing values
sum(is.na(data))      
#[1] 0

#Missing values per column
colSums(is.na(data))    
#ID number               Diagnosis             radius_mean            texture_mean          perimeter_mean               area_mean         smoothness_mean 
#0                       0                       0                       0                       0                       0                       0 
#compactness_mean          concavity_mean     concave.points_mean           symmetry_mean  fractal_dimension_mean               radius_se              texture_se 
#0                       0                       0                       0                       0                       0                       0 
#perimeter_se                 area_se           smoothness_se          compactness_se            concavity_se       concave.points_se             symmetry_se 
#0                       0                       0                       0                       0                       0                       0 
#fractal_dimension_se            radius_worst           texture_worst         perimeter_worst              area_worst        smoothness_worst       compactness_worst 
#0                       0                       0                       0                       0                       0                       0 
#concavity_worst    concave.points_worst          symmetry_worst fractal_dimension_worst 
#0                       0                       0                       0 


#Check for null values
any(is.na(data))
#[1] FALSE

str(data)

#3. Identify Data Types and Treat Variables

#Factor Diagnosis column
# Convert Diagnosis to factor (binary)
#level M=1, B=0
data$Diagnosis <- as.factor(ifelse(data$Diagnosis == "M", 1, 0))


##Univariate Analysis
#4. Visualize the Data and Detecting outliers

#Diagnosis
summary(data$Diagnosis)
table_category <- table(data$Diagnosis)
#  B   M 
#357 212 

barplot(table_category, xlab = "Diagnosis Status M=1, B=0", ylab = "Count", col = c("skyblue",  "steelblue2"), main = "Frequency of Diagnosis")

#radius_mean
TI_Fivenum(data$radius_mean)
#    min     Q1 median     Q3   max
#1 6.981 11.695  13.37 15.815 28.11

par(mfrow = c(1,3))
hist(data$radius_mean, right = FALSE, main = "Frequency of radius mean", col = "coral" )
boxplot(data$radius_mean, main= "Boxplot of radius mean", col = "gold", horizontal = TRUE)
plot(density(data$radius_mean, na.rm = TRUE), 
     main = "Density of radius mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the quantitative variable radius_mean, the data can be described as right-skewed with multiple outliers. Most values of radius_mean are concentrated between 10 and 20. 

########################################################################

#texture_mean
TI_Fivenum(data$texture_mean)
#   min    Q1 median     Q3   max
#1 9.71 16.17  18.84 21.805 39.28

par(mfrow = c(1,3))
hist(data$texture_mean, right = FALSE, main = "Frequency of texture mean", col = "orchid" )
boxplot(data$texture_mean, main= "Boxplot of texture mean", col = "orange", horizontal = TRUE)
plot(density(data$texture_mean, na.rm = TRUE), 
     main = "Density of texture mean", 
     col = "blue", 
     lwd = 2) 

# Reset graphical parameters
par(mfrow = c(1,1))

#Analyzing the histogram and boxplot of the quantitative variable texture_mean, the data can be described as right-skewed with multiple outliers. Most values of radius_mean are concentrated between 15 and 25. 

#perimeter_mean
TI_Fivenum(data$perimeter_mean)
#    min   Q1 median    Q3   max
#1 43.79 75.1  86.24 104.2 188.5

par(mfrow = c(1,3))
hist(data$perimeter_mean, right = FALSE, main = "Frequency of perimeter mean", col = "peachpuff" )
boxplot(data$perimeter_mean, main= "Boxplot of perimeter mean", col = "lightgreen", horizontal = TRUE)
plot(density(data$perimeter_mean, na.rm = TRUE), 
     main = "Density of perimeter mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the quantitative variable perimeter mean, the data can be described as right-skewed with multiple outliers. Most values of radius_mean are concentrated between 75 and 100.

#area_mean
TI_Fivenum(data$area_mean)
#    min     Q1 median    Q3  max
#1 143.5 420.05  551.1 785.6 2501

par(mfrow = c(1,3))
hist(data$area_mean, right = FALSE, main = "Frequency of area mean", col = "red" )
boxplot(data$area_mean, main= "Boxplot of area mean", col = "lightgreen", horizontal = TRUE)
plot(density(data$area_mean, na.rm = TRUE), 
     main = "Density of area mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the quantitative variable area mean, the data can be described as right-skewed with multiple outliers. 

#smoothness_mean
TI_Fivenum(data$smoothness_mean)
#      min      Q1  median      Q3    max
#1 0.05263 0.08621 0.09587 0.10535 0.1634

par(mfrow = c(1,3))
hist(data$smoothness_mean, right = FALSE, main = "Frequency of smoothness mean", col = "red" )
boxplot(data$smoothness_mean, main= "Boxplot of smoothness mean", col = "yellow", horizontal = TRUE)
plot(density(data$smoothness_mean, na.rm = TRUE), 
     main = "Density of smoothness mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the variable smoothness mean, the data can be described as slightly skewed to the right with multiple outliers. 

#compactness_mean
TI_Fivenum(data$compactness_mean)
#      min      Q1  median      Q3    max
#1 0.01938 0.06471 0.09263 0.13045 0.3454

par(mfrow = c(1,3))
hist(data$compactness_mean, right = FALSE, main = "Frequency of compactness mean", col = "purple" )
boxplot(data$compactness_mean, main= "Boxplot of compactness mean", col = "pink", horizontal = TRUE)
plot(density(data$compactness_mean, na.rm = TRUE), 
     main = "Density of compactness mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the quantitative variable area mean, the data can be described as right-skewed with multiple outliers. 

#concavity_mean
TI_Fivenum(data$concavity_mean)
#  min      Q1  median     Q3    max
#1   0 0.02952 0.06154 0.1313 0.4268

par(mfrow = c(1,3))
hist(data$concavity_mean, right = FALSE, main = "Frequency of concavity mean", col = "magenta" )
boxplot(data$concavity_mean, main= "Boxplot of concavity mean", col = "green", horizontal = TRUE)
plot(density(data$concavity_mean, na.rm = TRUE), 
     main = "Density of concavity mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

#Analyzing the histogram and boxplot of the quantitative variable area mean, the data can be described as right-skewed with multiple outliers. 

#concave.points_mean
TI_Fivenum(data$concave.points_mean)
#  min      Q1 median      Q3    max
#1   0 0.02031 0.0335 0.07402 0.2012

par(mfrow = c(1,3))
hist(data$concave.points_mean, right = FALSE, main = "Frequency of concave.points mean", col = "lightblue" )
boxplot(data$concave.points_mean, main= "Boxplot of concave.points mean", col = "violet", horizontal = TRUE)
plot(density(data$concave.points_mean, na.rm = TRUE), 
     main = "Density of concave.points mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))

##Analyzing the histogram and boxplot of the quantitative variable area mean, the data can be described as right-skewed with multiple outliers. 

#symmetry_mean
TI_Fivenum(data$symmetry_mean)
#    min     Q1 median     Q3   max
#1 0.106 0.1619 0.1792 0.1957 0.304

par(mfrow = c(1,3))
hist(data$symmetry_mean, right = FALSE, main = "Frequency of symmetry mean", col = "lightblue" )
boxplot(data$symmetry_mean, main= "Boxplot of symmetry mean", col = "pink", horizontal = TRUE)
plot(density(data$symmetry_mean, na.rm = TRUE), 
     main = "Density of symmetry mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))


#fractal_dimension_mean
TI_Fivenum(data$fractal_dimension_mean)
#    min     Q1 median     Q3   max
#1 0.106 0.1619 0.1792 0.1957 0.304

par(mfrow = c(1,3))
hist(data$fractal_dimension_mean, right = FALSE, main = "Frequency of fractal dimension mean", col = "peachpuff" )
boxplot(data$fractal_dimension_mean, main= "Boxplot of fractal dimension mean", col = "skyblue", horizontal = TRUE)
plot(density(data$fractal_dimension_mean, na.rm = TRUE), 
     main = "Density of fractal dimension mean", 
     col = "blue", 
     lwd = 2) 
par(mfrow = c(1,1))


#radius_se
TI_Fivenum(data$radius_se)
#     min      Q1 median     Q3   max
#1 0.1115 0.23235 0.3242 0.4807 2.873

par(mfrow = c(1,3))
hist(data$radius_se, right = FALSE, main = "Frequency of radius_se", col = "lightgreen" )
boxplot(data$radius_se, main= "Boxplot of radius_se", col = "skyblue", horizontal = TRUE)
plot(density(data$radius_se, na.rm = TRUE), 
     main = "Density of radius_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))

#texture_se
TI_Fivenum(data$texture_se)
#     min     Q1 median     Q3   max
#1 0.3602 0.8324  1.108 1.4745 4.885

par(mfrow = c(1,3))
hist(data$texture_se, right = FALSE, main = "Frequency of texture_se", col = "gold" )
boxplot(data$texture_se, main= "Boxplot of texture_se", col = "coral", horizontal = TRUE)
plot(density(data$texture_se, na.rm = TRUE), 
     main = "Density of texture_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#perimeter_se
TI_Fivenum(data$perimeter_se)
#    min    Q1 median    Q3   max
#1 0.757 1.604  2.287 3.363 21.98

par(mfrow = c(1,3))
hist(data$perimeter_se, right = FALSE, main = "Frequency of perimeter_se", col = "green" )
boxplot(data$perimeter_se, main= "Boxplot of perimeter_se", col = "yellow", horizontal = TRUE)
plot(density(data$perimeter_se, na.rm = TRUE), 
     main = "Density of perimeter_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))

#area_se
TI_Fivenum(data$area_se)
#    min    Q1 median    Q3   max
#1 0.757 1.604  2.287 3.363 21.98

par(mfrow = c(1,3))
hist(data$area_se, right = FALSE, main = "Frequency of area_se", col = "darkgreen" )
boxplot(data$area_se, main= "Boxplot of area_se", col = "orange", horizontal = TRUE)
plot(density(data$area_se, na.rm = TRUE), 
     main = "Density of area_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#smoothness_se          
TI_Fivenum(data$smoothness_se)
#       min        Q1  median       Q3     max
#1 0.001713 0.0051635 0.00638 0.008156 0.03113

par(mfrow = c(1,3))
hist(data$smoothness_se, right = FALSE, main = "Frequency of smoothness_se ", col = "steelblue2" )
boxplot(data$smoothness_se, main= "Boxplot of smoothness_se ", col = "deeppink", horizontal = TRUE)
plot(density(data$smoothness_se, na.rm = TRUE), 
     main = "Density of smoothness_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#compactness_se  
TI_Fivenum(data$compactness_se)
#       min       Q1  median      Q3    max
#1 0.002252 0.013015 0.02045 0.03246 0.1354

par(mfrow = c(1,3))
hist(data$compactness_se, right = FALSE, main = "Frequency of compactness_se  ", col = "purple" )
boxplot(data$compactness_se, main= "Boxplot of compactness_se  ", col = "tomato1", horizontal = TRUE)
plot(density(data$compactness_se, na.rm = TRUE), 
     main = "Density of compactness_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#concavity_se 
TI_Fivenum(data$concavity_se)
#  min       Q1  median       Q3   max
#1   0 0.015035 0.02589 0.042185 0.396

par(mfrow = c(1,3))
hist(data$concavity_se, right = FALSE, main = "Frequency of concavity_se", col = "darkseagreen" )
boxplot(data$concavity_se, main= "Boxplot of concavity_se", col = "royalblue", horizontal = TRUE)
plot(density(data$concavity_se, na.rm = TRUE), 
     main = "Density of concavity_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#concave.points_se          
TI_Fivenum(data$concave.points_se)
#  min       Q1  median      Q3     max
#1   0 0.007631 0.01093 0.01475 0.05279

par(mfrow = c(1,3))
hist(data$concave.points_se, right = FALSE, main = "Frequency of concave.points_se ", col = "palegreen" )
boxplot(data$concave.points_se, main= "Boxplot of concave.points_se ", col = "moccasin", horizontal = TRUE)
plot(density(data$concave.points_se, na.rm = TRUE), 
     main = "Density of concave.points_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#symmetry_se
TI_Fivenum(data$symmetry_se)
#       min       Q1  median       Q3     max
#1 0.007882 0.015095 0.01873 0.023485 0.07895

par(mfrow = c(1,3))
hist(data$symmetry_se, right = FALSE, main = "Frequency of symmetry_se", col = "hotpink" )
boxplot(data$symmetry_se, main= "Boxplot of symmetry_se", col = "darkorange", horizontal = TRUE)
plot(density(data$symmetry_se, na.rm = TRUE), 
     main = "Density of symmetry_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#fractal_dimension_se            
TI_Fivenum(data$fractal_dimension_se)
#        min       Q1   median       Q3     max
#1 0.0008948 0.002241 0.003187 0.004559 0.02984

par(mfrow = c(1,3))
hist(data$symmetry_se, right = FALSE, main = "Frequency of fractal_dimension_se", col = "lightslateblue" )
boxplot(data$symmetry_se, main= "Boxplot of fractal_dimension_se", col = "olivedrab", horizontal = TRUE)
plot(density(data$fractal_dimension_se, na.rm = TRUE), 
     main = "Density of fractal dimension_se", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#radius_worst           
TI_Fivenum(data$radius_worst)
#   min    Q1 median   Q3   max
#1 7.93 13.01  14.97 18.8 36.04

par(mfrow = c(1,3))
hist(data$radius_worst, right = FALSE, main = "Frequency of radius_worst", col = "lightsalmon" )
boxplot(data$radius_worst, main= "Boxplot of radius_worst ", col = "olivedrab1", horizontal = TRUE)
plot(density(data$radius_worst, na.rm = TRUE), 
     main = "Density of fractal radius worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#texture_worst         
TI_Fivenum(data$texture_worst)
#    min    Q1 median     Q3   max
#1 12.02 21.07  25.41 29.795 49.54

par(mfrow = c(1,3))
hist(data$texture_worst, right = FALSE, main = "Frequency of texture_worst", col = "lightskyblue2" )
boxplot(data$texture_worst, main= "Boxplot of texture_worst", col = "plum4", horizontal = TRUE)
plot(density(data$texture_worst, na.rm = TRUE), 
     main = "Density of texture worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#perimeter_worst              
TI_Fivenum(data$perimeter_worst)
#    min     Q1 median     Q3   max
#1 50.41 84.095  97.66 125.65 251.2

par(mfrow = c(1,3))
hist(data$perimeter_worst, right = FALSE, main = "Frequency of perimeter_worst", col = "indianred" )
boxplot(data$perimeter_worst, main= "Boxplot of perimeter_worst", col = "magenta2", horizontal = TRUE)
plot(density(data$perimeter_worst, na.rm = TRUE), 
     main = "Density of perimeter worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#area_worst        
TI_Fivenum(data$area_worst)
#    min     Q1 median   Q3  max
#1 185.2 514.65  686.5 1086 4254

par(mfrow = c(1,3))
hist(data$area_worst, right = FALSE, main = "Frequency of area_worst", col = "cyan4" )
boxplot(data$area_worst, main= "Boxplot of area_worst", col = "khaki4", horizontal = TRUE)
plot(density(data$area_worst, na.rm = TRUE), 
     main = "Density of area worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))



#smoothness_worst       
TI_Fivenum(data$smoothness_worst)
#      min     Q1 median      Q3    max
#1 0.07117 0.1166 0.1313 0.14605 0.2226

par(mfrow = c(1,3))
hist(data$smoothness_worst, right = FALSE, main = "Frequency of smoothness_worst", col = "sienna1" )
boxplot(data$smoothness_worst, main= "Boxplot of smoothness_worst", col = "navyblue", horizontal = TRUE)
plot(density(data$smoothness_worst, na.rm = TRUE), 
     main = "Density of smoothness worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#compactness_worst
TI_Fivenum(data$compactness_worst)
#      min     Q1 median     Q3   max
#1 0.02729 0.1466 0.2119 0.3395 1.058

par(mfrow = c(1,3))
hist(data$compactness_worst, right = FALSE, main = "Frequency of compactness_worst", col = "lightpink1" )
boxplot(data$compactness_worst, main= "Boxplot of compactness_worst", col = "slategray1", horizontal = TRUE)
plot(density(data$compactness_worst, na.rm = TRUE), 
     main = "Density of compactness worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))



#concavity_worst    
TI_Fivenum(data$concavity_worst)
#  min      Q1 median     Q3   max
#1   0 0.11445 0.2267 0.3841 1.252

par(mfrow = c(1,3))
hist(data$concavity_worst, right = FALSE, main = "Frequency of concavity_worst", col = "thistle1" )
boxplot(data$concavity_worst, main= "Boxplot of concavity_worst", col = "tan1", horizontal = TRUE)
plot(density(data$concavity_worst, na.rm = TRUE), 
     main = "Density of concavity worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#concave.points_worst          
TI_Fivenum(data$concave.points_worst)
#  min      Q1  median      Q3   max
#1   0 0.06453 0.09993 0.16195 0.291

par(mfrow = c(1,3))
hist(data$concave.points_worst, right = FALSE, main = "Frequency of concave.points_worst ", col = "gold2" )
boxplot(data$concave.points_worst, main= "Boxplot of concave.points_worst ", col = "plum", horizontal = TRUE)
plot(density(data$concave.points_worst, na.rm = TRUE), 
     main = "Density of concave.points worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))


#symmetry_worst 
TI_Fivenum(data$symmetry_worst)
#     min     Q1 median      Q3    max
#1 0.1565 0.2503 0.2822 0.31815 0.6638

par(mfrow = c(1,3))
hist(data$symmetry_worst, right = FALSE, main = "Frequency of symmetry_worst", col = "slateblue1" )
boxplot(data$symmetry_worst, main= "Boxplot of symmetry_worst", col = "yellow3", horizontal = TRUE)
plot(density(data$symmetry_worst, na.rm = TRUE), 
     main = "Density of symmetry worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))



#fractal_dimension_worst
TI_Fivenum(data$fractal_dimension_worst)
#      min       Q1  median       Q3    max
#1 0.05504 0.071365 0.08004 0.092085 0.2075

par(mfrow = c(1,3))
hist(data$fractal_dimension_worst, right = FALSE, main = "Frequency of fractal_dimension_worst", col = "greenyellow" )
boxplot(data$fractal_dimension_worst, main= "Boxplot of fractal_dimension_worst", col = "yellow3", horizontal = TRUE)
plot(density(data$fractal_dimension_worst, na.rm = TRUE), 
     main = "Density of fractal dimension worst", 
     col = "blue", 
     lwd = 2)
par(mfrow = c(1,1))

#########################################################################
#Transforming Variables for SVM, KNN , Logistic Regression
# Scaling features for all numeric features
data_scaled <- data %>% mutate(across(where(is.numeric), scale))
str(data_scaled)


#########################################################################
###Model Building 
#create Test Set and Training Set
# Split the data into 70% training and 30% testing
train_index <- createDataPartition(data_scaled$Diagnosis, p = 0.7, list = FALSE)
train_data <- data_scaled[train_index, ]
test_data <- data_scaled[-train_index, ]

# Separate predictors and target variable
x_train <- train_data[, -1]  # Exclude the target column "Diagnosis"
y_train <- train_data$Diagnosis
x_test <- test_data[, -1]
y_test <- test_data$Diagnosis


#########################################################################
#correlation for classification method Lasso
#########################################################################
# Preprocess the Data (ensure it's scaled and in matrix form)
x_train_matrix <- as.matrix(x_train)  # Convert training data to matrix
x_test_matrix <- as.matrix(x_test)    # Convert test data to matrix

# Apply LASSO using cross-validation
cvfit_lasso <- cv.glmnet(x_train_matrix, y_train, alpha = 1, family = "binomial")  # alpha=1 for LASSO


# Find the optimal lambda from cross-validation
best_lambda <- cvfit_lasso$lambda.min
cat("Optimal Lambda for LASSO: ", best_lambda, "\n")
#Optimal Lambda for LASSO:  0.007656641

# Fit the LASSO model using the best lambda
lasso_model <- glmnet(x_train_matrix, y_train, alpha = 1, lambda = best_lambda, family = "binomial")

# Predict on the test set
pred_lasso <- predict(lasso_model, s = best_lambda, newx = x_test_matrix, type = "response")

# Convert probabilities to binary predictions (1 for malignant, 0 for benign)
pred_lasso_binary <- ifelse(pred_lasso > 0.5, 1, 0)

# Evaluate the model (Confusion Matrix and Accuracy)
confusion <- confusionMatrix(factor(pred_lasso_binary), factor(y_test))
print(confusion)
#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0          106   4
#1            1  59

#Accuracy : 0.9706          
#95% CI : (0.9327, 0.9904)
#No Information Rate : 0.6294          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.9363          

#Mcnemar's Test P-Value : 0.3711          

#            Sensitivity : 0.9907          
#            Specificity : 0.9365          
#         Pos Pred Value : 0.9636          
#         Neg Pred Value : 0.9833          
#             Prevalence : 0.6294          
#         Detection Rate : 0.6235          
#   Detection Prevalence : 0.6471          
#      Balanced Accuracy : 0.9636          

#       'Positive' Class : 0   


# Load necessary libraries
library(caret)
library(ggplot2)

# Convert the confusion matrix to a data frame for ggplot2
conf_matrix_df <- as.data.frame(confusion$table)

# Convert Prediction and Reference to factors with proper levels
conf_matrix_df$Prediction <- factor(conf_matrix_df$Prediction, levels = c("0", "1"))
conf_matrix_df$Reference <- factor(conf_matrix_df$Reference, levels = c("0", "1"))

# Create a heatmap for the confusion matrix
ggplot(data = conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "lightblue") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(title = "Confusion Matrix Heatmap", x = "Predicted Class", y = "True Class") +
  theme_minimal()



# Print the coefficients for the LASSO model: The lasso_model$beta gives the coefficients of the features selected by LASSO. Non-zero coefficients indicate the features that are most correlated with the target variable and selected by the LASSO model.
cat("LASSO Model Coefficients (Non-Zero): \n")
print(lasso_model$beta)
#30 x 1 sparse Matrix of class "dgCMatrix"
#s0
#radius_mean              .        
#texture_mean             .        
#perimeter_mean           .        
#area_mean                .        
#smoothness_mean          .        
#compactness_mean         .        
#concavity_mean           .        
#concave.points_mean      0.3735363
#symmetry_mean            .        
#fractal_dimension_mean   .        
#radius_se                1.3414318
#texture_se               .        
#perimeter_se             .        
#area_se                  .        
#smoothness_se            .        
#compactness_se           .        
#concavity_se             .        
#concave.points_se        .        
#symmetry_se              .        
#fractal_dimension_se    -0.2167529
#radius_worst             2.9177351
#texture_worst            1.0278779
#perimeter_worst          .        
#area_worst               .        
#smoothness_worst         0.3697383
#compactness_worst        .        
#concavity_worst          0.4062928
#concave.points_worst     1.0929471
#symmetry_worst           0.2112367
#fractal_dimension_worst  . 

# Extract the coefficients from the LASSO model
lasso_coefficients <- as.matrix(lasso_model$beta)

# Convert to a data frame for ggplot
lasso_df <- data.frame(
  Feature = rownames(lasso_coefficients),
  Coefficient = as.numeric(lasso_coefficients[, 1])
)

# Filter for non-zero coefficients
lasso_df <- lasso_df[lasso_df$Coefficient != 0, ]

# Create the bar plot
library(ggplot2)
ggplot(lasso_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(
    title = "Top Features Selected by LASSO Model",
    x = "Feature",
    y = "Coefficient"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )



# Optional: Plot the cross-validation results
plot(cvfit_lasso)

#From the LASSO model, the coefficients that are non-zero indicate the features that are most correlated with the target and have been selected by the model. These are the features you want to use for your KNN model.
#the non-zero coefficients are:

#concave.points_mean
#radius_se
#fractal_dimension_se
#radius_worst
#texture_worst
#smoothness_worst
#concavity_worst
#concave.points_worst
#symmetry_worst


ggplot(data, aes(x = concave.points_mean, y = concave.points_worst, color = Diagnosis)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Diagnosis) +
  theme_minimal() +
  labs(title = "Feature Relationships by Diagnosis")

#########################################################################

#########################################################################
# Model KNN using Lasso selected feature
#########################################################################
# Select only the features that were selected by LASSO
# Select features with non-zero coefficients from LASSO
selected_features_lasso <- c("concave.points_mean", "radius_se", "fractal_dimension_se", 
                             "radius_worst", "texture_worst", "smoothness_worst", 
                             "concavity_worst", "concave.points_worst", "symmetry_worst")

# Subset the data
data_filtered_lasso <- data_scaled[, c("Diagnosis", selected_features_lasso)]

# Split into training and testing sets
train_data_lasso <- data_filtered_lasso[train_index, ]
test_data_lasso <- data_filtered_lasso[-train_index, ]

# Separate predictors and target variable
x_train2 <- train_data_lasso[, -1]  # Exclude the target column "Diagnosis"
y_train2 <- train_data_lasso$Diagnosis
x_test2 <- test_data_lasso[, -1]
y_test2 <- test_data_lasso$Diagnosis

k <- 10
knn_predictions_lasso_feature <- knn(train = x_train2, test = x_test2, cl = y_train2, k = k)

## Evaluate performance
# Confusion matrix
confusion_matrix_lasso_feature <- table(Predicted = knn_predictions_lasso_feature, Actual = y_test2)
print(confusion_matrix_lasso_feature)
#          Actual
#Predicted   0   1
#0         107   7
#1           0  56 

confusion_matrix_df <- as.data.frame(as.table(confusion_matrix_lasso_feature))

# Create the heatmap
ggplot(data = confusion_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "skyblue4") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap before tuning M = 1 B = 0",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Calculate accuracy, sensitivity, and specificity
accuracy_lasso_feature <- sum(diag(confusion_matrix_lasso_feature)) / sum(confusion_matrix_lasso_feature)
sensitivity_lasso_feature <- confusion_matrix_lasso_feature[2, 2] / sum(confusion_matrix_lasso_feature[, 2])  # True Positives / (True Positives + False Negatives)
specificity_lasso_feature <- confusion_matrix_lasso_feature[1, 1] / sum(confusion_matrix_lasso_feature[, 1])  # True Negatives / (True Negatives + False Positives)
# Precision (Positive Predictive Value)
precision_lasso_feature <- confusion_matrix_lasso_feature[2, 2] / sum(confusion_matrix_lasso_feature[2, ])  # True Positives / (True Positives + False Positives)
# F1 Score
f1_score_lasso_feature <- 2 * (precision_lasso_feature * sensitivity_lasso_feature) / (precision_lasso_feature + sensitivity_lasso_feature)

# Print results
cat("Accuracy: ", accuracy_lasso_feature, "\n")
# Accuracy:  0.9588235 
cat("Sensitivity: ", sensitivity_lasso_feature, "\n")
#Sensitivity:  0.8888889  
cat("Specificity: ", specificity_lasso_feature, "\n")
#Specificity:  1
cat("Precision: ", precision_lasso_feature, "\n")
#Precision:  1 
cat("F1 Score: ", f1_score_lasso_feature, "\n")
#F1 Score:  0.9411765


# Create a data frame with the metrics
metrics_lasso <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value = c(accuracy_lasso_feature, sensitivity_lasso_feature, specificity_lasso_feature, 
            precision_lasso_feature, f1_score_lasso_feature)
)

# Plot the metrics as a bar chart with blue bars
ggplot(metrics_lasso, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "darkblue") +  # Set the fill color to blue
  geom_text(aes(label = round(Value, 5)), vjust = -0.3) +  # Add value labels above bars
  labs(title = "Performance Metrics for KNN Model Before Tuning",
       x = "Metric",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

#########################################################################
#tuning parameter selection for KNN
#########################################################################

# Function to calculate accuracy for different k values
accuracy_k_lasso_feature <- function(k) {
  knn_pred_lasso_feature <- knn(train = x_train2, test = x_test2, cl = y_train2, k = k)
  cm_lasso_feature <- table(Predicted = knn_pred_lasso_feature, Actual = y_test2)
  return(sum(diag(cm_lasso_feature)) / sum(cm_lasso_feature))
}



# Test k values from 1 to 20
k_values_lasso_feature <- 1:20
accuracies_lasso_feature <- sapply(k_values_lasso_feature, accuracy_k_lasso_feature)



plot(k_values_lasso_feature, accuracies_lasso_feature, type = "b", col = "blue", pch = 19,
     xlab = "Number of Neighbors (k)", 
     ylab = "Accuracy",
     main = "Optimal k Selection for KNN Model")

# Find the best k
best_k_lasso_feature <- k_values_lasso_feature[which.max(accuracies_lasso_feature)]
cat("Best k: ", best_k_lasso_feature, "\n")
#Best k:  12

#performing it with tuning to see if hyperparameter optimization improves performance

knn_predictions_best_lasso <- knn(train = x_train2, test = x_test2, cl = y_train2, k = best_k_lasso_feature)

# Confusion matrix
confusion_matrix_best_lasso <- table(Predicted = knn_predictions_best_lasso, Actual = y_test2)
print(confusion_matrix_best_lasso)
#         Actual
#Predicted   0   1
#0          107  6
#1           0  56        

confusion_matrix_best_df <- as.data.frame(as.table(confusion_matrix_best_lasso))

# Create the heatmap
ggplot(data = confusion_matrix_best_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "skyblue4") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Best KNN) after tuning M = 1 B = 0 ",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Calculate accuracy, sensitivity, and specificity
accuracy_best_lasso <- sum(diag(confusion_matrix_best_lasso)) / sum(confusion_matrix_best_lasso)
sensitivity_best_lasso <- confusion_matrix_best_lasso[2, 2] / sum(confusion_matrix_best_lasso[, 2])  # True Positives / (True Positives + False Negatives)
specificity_best_lasso <- confusion_matrix_best_lasso[1, 1] / sum(confusion_matrix_best_lasso[, 1])  # True Negatives / (True Negatives + False Positives)
precision_best_lasso <- confusion_matrix_best_lasso[2, 2] / sum(confusion_matrix_best_lasso[2, ]) 
f1_score_best_lasso <- 2 * (precision_best_lasso * sensitivity_best_lasso) / (precision_best_lasso + sensitivity_best_lasso)

# Print results
cat("Accuracy: ", accuracy_best_lasso, "\n")
#Accuracy:  0.9647059 
cat("Sensitivity: ", sensitivity_best_lasso, "\n")
#Sensitivity:  0.9047619 
cat("Specificity: ", specificity_best_lasso, "\n")
#Specificity:  1
cat("Precision: ", precision_best_lasso, "\n")
#Precision:  1 
cat("F1-score: ", f1_score_best_lasso, "\n")
#F1-score:  0.95 


# Create a data frame with the metrics
metrics_knn_best <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value = c(accuracy_best_lasso, sensitivity_best_lasso, specificity_best_lasso, 
            precision_best_lasso, f1_score_best_lasso)
)

# Plot the metrics as a bar chart with blue bars
ggplot(metrics_knn_best, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Set the fill color to blue
  geom_text(aes(label = round(Value, 5)), vjust = -0.3) +  # Add value labels above bars
  labs(title = "Performance Metrics for KNN Model After Tuning",
       x = "Metric",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")






#########################################################################
#Model  Naive Bayer
#########################################################################

# Train a Naive Bayes model
nbmodel <- naiveBayes(Diagnosis ~ ., data = train_data_lasso)

# Predict on the test dataset
predictions_nbmodel <- predict(nbmodel, x_test2)

# Evaluate Model Performance
# Confusion matrix
confusion_matrix_nbmodel <- table(Predicted = predictions_nbmodel, Actual = y_test2)
print(confusion_matrix_nbmodel)
#         Actual
#Predicted   0   1
#0 106   7
#1   1  56

# Convert the confusion matrix into a data frame for ggplot
confusion_matrix_nbmodel_df <- as.data.frame(as.table(confusion_matrix_nbmodel))

# Create the heatmap
ggplot(data = confusion_matrix_nbmodel_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#A9CCE3", high = "#1F618D") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (before tuningNaive Bayes Model) M = 1 B = 0",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )



# Calculate accuracy
accuracy_nbmodel <- sum(diag(confusion_matrix_nbmodel)) / sum(confusion_matrix_nbmodel)
cat("Accuracy:", accuracy_nbmodel, "\n")
#Accuracy: 0.9529412

# Sensitivity (Recall)
TP <- confusion_matrix_nbmodel[2, 2]  # True Positives
FN <- confusion_matrix_nbmodel[2, 1]  # False Negatives
sensitivity_nbmodel <- TP / (TP + FN)
cat("Sensitivity:", sensitivity_nbmodel, "\n")
#Sensitivity: 0.9824561 

# Specificity
TN <- confusion_matrix_nbmodel[1, 1]  # True Negatives
FP <- confusion_matrix_nbmodel[1, 2]  # False Positives
specificity_nbmodel <- TN / (TN + FP)
cat("Specificity:", specificity_nbmodel, "\n")
#Specificity: 0.9380531 

# Precision
precision_nbmodel <- TP / (TP + FP)
cat("Precision:", precision_nbmodel, "\n")
#Precision: 0.8888889 

# F1-Score
f1_score_nbmodel <- 2 * (precision_nbmodel * sensitivity_nbmodel) / (precision_nbmodel + sensitivity_nbmodel)
cat("F1-Score:", f1_score_nbmodel, "\n")
#F1-Score: 0.9333333




#########################################################################
# Tuning Model  Naive Bayer
#########################################################################

# Set up 10-fold cross-validation
train_control1 <- trainControl(method = "cv", number = 10)

# Train the Naive Bayes model with cross-validation
nb_model_cv <- train(Diagnosis ~ ., 
                     data = train_data_lasso, 
                     method = "naive_bayes", 
                     trControl = train_control1)

# View cross-validation results
print(nb_model_cv)
#Naive Bayes 

#399 samples
#9 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 360, 359, 359, 359, 359, 359, ... 
#Resampling results across tuning parameters:
  
#  usekernel  Accuracy   Kappa    
#FALSE      0.9474359  0.8885048
#TRUE       0.9473718  0.8886145

#Tuning parameter 'laplace' was held constant at a value of 0
#Tuning parameter 'adjust' was held constant at a value of 1
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were laplace = 0, usekernel = FALSE and adjust = 1.


# Extract cross-validated accuracy
cat("Accuracy (Cross-Validation):", nb_model_cv$results$Accuracy, "\n")
#Accuracy (Cross-Validation): 0.9474359 0.9473718 

# Re-train Naive Bayes with optimal hyperparameters from tuning
optimal_params <- nb_model_cv$bestTune
nbmodel_tuned <- naiveBayes(Diagnosis ~ ., 
                            data = train_data_lasso, 
                            laplace = optimal_params$laplace, 
                            usekernel = optimal_params$usekernel, 
                            adjust = optimal_params$adjust)

print(nbmodel_tuned)


# Predict on the test dataset with the tuned model
predictions_nbmodel_tuned <- predict(nbmodel_tuned, x_test2)

# Evaluate Model Performance for Tuned Model
confusion_matrix_nbmodel_tuned <- table(Predicted = predictions_nbmodel_tuned, Actual = y_test2)
print(confusion_matrix_nbmodel_tuned)
#         Actual
#Predicted   0   1
#0 106   7
#1   1  56

# Convert the confusion matrix into a data frame for ggplot
confusion_matrix_nbmodel_tuned_df <- as.data.frame(as.table(confusion_matrix_nbmodel_tuned))


# Create the heatmap
ggplot(data = confusion_matrix_nbmodel_tuned_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#A9CCE3", high = "#1F618D") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Naive Bayes Model) M = 1 B = 0",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )




# Calculate accuracy for the tuned model
accuracy_nbmodel_tuned <- sum(diag(confusion_matrix_nbmodel_tuned)) / sum(confusion_matrix_nbmodel_tuned)
cat("Accuracy (Tuned Model):", accuracy_nbmodel_tuned, "\n")
#Accuracy (Tuned Model): 0.9529412

# Sensitivity (Recall) for tuned model
TP_tuned <- confusion_matrix_nbmodel_tuned[2, 2]
FN_tuned <- confusion_matrix_nbmodel_tuned[2, 1]
sensitivity_tuned <- TP_tuned / (TP_tuned + FN_tuned)
cat("Sensitivity (Tuned Model):", sensitivity_tuned, "\n")
#Sensitivity (Tuned Model): 0.9824561 

# Specificity for tuned model
TN_tuned <- confusion_matrix_nbmodel_tuned[1, 1]
FP_tuned <- confusion_matrix_nbmodel_tuned[1, 2]
specificity_tuned <- TN_tuned / (TN_tuned + FP_tuned)
cat("Specificity (Tuned Model):", specificity_tuned, "\n")
#Specificity (Tuned Model): 0.9380531 

# Precision for tuned model
precision_tuned <- TP_tuned / (TP_tuned + FP_tuned)
cat("Precision (Tuned Model):", precision_tuned, "\n")
#Precision (Tuned Model): 0.8888889 

# F1-Score for tuned model
f1_score_tuned <- 2 * (precision_tuned * sensitivity_tuned) / (precision_tuned + sensitivity_tuned)
cat("F1-Score (Tuned Model):", f1_score_tuned, "\n")
#F1-Score (Tuned Model): 0.9333333 

# Create a data frame with the metrics
metrics_nbmodel_best <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value = c(accuracy_nbmodel_tuned, sensitivity_tuned, specificity_tuned, 
            precision_tuned, f1_score_tuned)
)

# Plot the metrics as a bar chart with blue bars
ggplot(metrics_nbmodel_best, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "#1F618D") +  # Set the fill color to blue
  geom_text(aes(label = round(Value, 5)), vjust = -0.3) +  # Add value labels above bars
  labs(title = "Performance Metrics for Naive Bayes Model After Tuning",
       x = "Metric",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

#########################################################################
#Support Vector Machine (SVM) 
#########################################################################

svm_linear <- svm(Diagnosis ~., data = train_data_lasso, kernel = "linear", scale = F)
print(svm_linear)
#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "linear", scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 

#Number of Support Vectors:  38

# Make predictions on the test set
svmlinear_predvals <- predict(svm_linear, x_test2, decision.values = TRUE)


# Create confusion matrix
svm_table <- table(svmlinear_predvals, y_test2)

# Print the confusion matrix
print(svm_table)
#y_test2
#svmlinear_predvals   0   1
#0                  105   2
#1                    2  61

# Convert the confusion matrix into a data frame
svm_table_df <- as.data.frame(as.table(svm_table))

# Rename the columns for ggplot compatibility
colnames(svm_table_df) <- c("Predicted", "Actual", "Freq")

# Map the values of Actual and Predicted to "M" and "B"
svm_table_df$Predicted <- ifelse(svm_table_df$Predicted == 1, "M = 1", "B = 0")
svm_table_df$Actual <- ifelse(svm_table_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = svm_table_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "slategray2", high = "skyblue3") +  # Light to dark red color gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Extract decision values (signed distance to the separation plane)
decision_values_svm <- attr(svmlinear_predvals, "decision.values")

accuracy_svm <- sum(diag(svm_table)) / sum(svm_table)
sensitivity_svm <- svm_table[2, 2] / sum(svm_table[, 2])  # True Positive Rate (Recall)
specificity_svm <- svm_table[1, 1] / sum(svm_table[, 1])  # True Negative Rate
precision_svm <- svm_table[2, 2] / sum(svm_table[2, ])   # Positive Predictive Value
f1_score_svm <- 2 * ((precision_svm * sensitivity_svm) / (precision_svm + sensitivity_svm))

# Output the results
cat("Accuracy:", accuracy_svm, "\n")
#Accuracy: 0.9764706 
cat("Sensitivity:", sensitivity_svm, "\n")
#Sensitivity: 0.968254 
cat("Specificity:", specificity_svm, "\n")
#Specificity: 0.9813084
cat("Precision:", precision_svm, "\n")
#Precision: 0.968254 
cat("F1 Score:", f1_score_svm, "\n")
#F1 Score: 0.968254 

# Tuning Cost Parameter based on Cross-Validation
tobj<-tune.svm(Diagnosis ~ ., data = train_data_lasso, cost=c(1:25)/3) ## Cross-validation to tune cost parameter 
summary(tobj) 

#Parameter tuning of ‘svm’:

#- sampling method: 10-fold cross validation 

#- best parameters:
#  cost
#6

#- best performance: 0.0275641

#- Detailed performance results:
#  cost      error dispersion
#1  0.3333333 0.04762821 0.04480872
#2  0.6666667 0.04012821 0.03947871
#3  1.0000000 0.04262821 0.03739164
#4  1.3333333 0.04512821 0.04050281
#5  1.6666667 0.04262821 0.03739164
#6  2.0000000 0.03506410 0.03372693
#7  2.3333333 0.03756410 0.03770771
#8  2.6666667 0.03756410 0.03770771
#9  3.0000000 0.04256410 0.03732803
#10 3.3333333 0.04256410 0.03732803
#11 3.6666667 0.04256410 0.03732803
#12 4.0000000 0.04256410 0.03732803
#13 4.3333333 0.04256410 0.03732803
#14 4.6666667 0.04256410 0.03732803
#15 5.0000000 0.04256410 0.03732803
#16 5.3333333 0.04506410 0.04213039
#17 5.6666667 0.04756410 0.03983860
#18 6.0000000 0.04756410 0.03983860
#19 6.3333333 0.04506410 0.03869357
#20 6.6666667 0.04506410 0.03869357
#21 7.0000000 0.04256410 0.03914423
#22 7.3333333 0.04006410 0.04113817
#23 7.6666667 0.04256410 0.04087982
#24 8.0000000 0.04256410 0.04087982
#25 8.3333333 0.04006410 0.04113817

# Extract the best parameters
best_cost <- tobj$best.parameters$cost
cat("Best cost parameter from tuning:", best_cost, "\n")
#Best cost parameter from tuning: 6

svm_linear1 <- svm(Diagnosis ~., data = train_data_lasso, kernel = "linear", scale = F, method = "C-classification", cost = best_cost)
print(svm_linear1)
#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "linear", method = "C-classification", cost = 2, scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  6 

#Number of Support Vectors:  29

svmlinear1_predvals <- predict(svm_linear1, x_test2, decision.values = T)
svm_table1 <- table(svmlinear1_predvals, y_test2)
print(svm_table1)
#                   y_test2
#svmlinear1_predvals   0   1
#0                   105   2
#1                     2  61

# Convert the confusion matrix into a data frame
svm_table1_df <- as.data.frame(as.table(svm_table1))

# Rename the columns for ggplot compatibility
colnames(svm_table1_df) <- c("Predicted", "Actual", "Freq")

# Map the values of Actual and Predicted to "M" and "B"
svm_table1_df$Predicted <- ifelse(svm_table1_df$Predicted == 1, "M = 1", "B = 0")
svm_table1_df$Actual <- ifelse(svm_table1_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = svm_table1_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#A9CCE3", high = "#1F618D") +  # Light to dark blue color gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

attr(svmlinear1_predvals, "decision.values") # signed distance to seperation plane

accuracy_svm1 <- sum(diag(svm_table1)) / sum(svm_table1)
sensitivity_svm1 <- svm_table1[2, 2] / sum(svm_table1[, 2])  # True Positive Rate (Recall)
specificity_svm1 <- svm_table1[1, 1] / sum(svm_table1[, 1])  # True Negative Rate
precision_svm1 <- svm_table1[2, 2] / sum(svm_table1[2, ])   # Positive Predictive Value
f1_score_svm1 <- 2 * ((precision_svm1 * sensitivity_svm1) / (precision_svm1 + sensitivity_svm1))

# Print results
cat("Accuracy:", accuracy_svm1, "\n") # Accuracy: 0.9764706 
cat("Sensitivity (Recall):", sensitivity_svm1, "\n") # Sensitivity (Recall): 0.968254  
cat("Specificity:", specificity_svm1, "\n") #Specificity: 0.9813084 
cat("Precision:", precision_svm1, "\n") # Precision: 0.968254 
cat("F1-Score:", f1_score_svm1, "\n") # F1-Score: 0.968254 

# Polynomial Kernel
#########################################################################
svm_polynomial <- svm(Diagnosis ~ ., data = train_data_lasso, kernel = "polynomial", scale = F)
print(svm_polynomial)

#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "polynomial", scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  1 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  118

svmpoly_predvals <- predict(svm_polynomial, test_data_lasso, decision.values = T)
poly_table <- table(svmpoly_predvals, y_test2)
print(poly_table)
#                y_test2
#svmpoly_predvals   0   1
#0                 107  17
#1                   0  46

# Convert the confusion matrix into a data frame
poly_table_df <- as.data.frame(as.table(poly_table))

# Rename the columns for ggplot compatibility
colnames(poly_table_df) <- c("Predicted", "Actual", "Freq")

# Map the values of Actual and Predicted to "M" and "B"
poly_table_df$Predicted <- ifelse(poly_table_df$Predicted == 1, "M = 1", "B = 0")
poly_table_df$Actual <- ifelse(poly_table_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = poly_table_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFEB99", high = "#F39C12") +  # Light yellow to dark orange gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Polynomial SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

attr(svmpoly_predvals, "decision.values") # signed distance to seperation plane

accuracy_poly <- sum(diag(poly_table)) / sum(poly_table)
sensitivity_poly <- poly_table[2, 2] / sum(poly_table[, 2])  # True Positive Rate (Recall)
specificity_poly <- poly_table[1, 1] / sum(poly_table[, 1])  # True Negative Rate
precision_poly <- poly_table[2, 2] / sum(poly_table[2, ])   # Positive Predictive Value
f1_score_poly <- 2 * ((precision_poly * sensitivity_poly) / (precision_poly + sensitivity_poly))

# Print results
cat("Accuracy:", accuracy_poly, "\n") # Accuracy: 0.9
cat("Sensitivity (Recall):", sensitivity_poly, "\n") #Sensitivity (Recall): 0.7301587 
cat("Specificity:", specificity_poly, "\n") # Specificity: 1 
cat("Precision:", precision_poly, "\n") # Precision: 1 
cat("F1-Score:", f1_score_poly, "\n") # F1-Score: 0.8440367

# Tuning Cost Parameter based on Cross-Validation
tobj1<-tune.svm(Diagnosis ~ ., data = train_data_lasso, cost=c(1:8), degree = c(2:8), coef0 = c(-10,10), gamma = 1/c(1:9)) ## Cross-validation to tune cost parameter 
summary(tobj1) 
#Parameter tuning of ‘svm’:

#- sampling method: 10-fold cross validation 

#- best parameters:
#  degree     gamma    coef0 cost
#   2       0.1666667    -1    2

#- best performance: 0.0375 

# Extract the best parameters
best_cost1 <- tobj1$best.parameters$cost
best_degree1 <-tobj1$best.parameters$degree
best_gamma1 <-tobj1$best.parameters$gamma
best_coef01 <- tobj1$best.parameters$coef0


svm_polynomial_tun <- svm(Diagnosis ~ ., data = train_data_lasso, kernel = "polynomial", degree = best_degree1, cost = best_cost1, gamma = best_gamma1, coef0 = best_coef01, scale = F)
print(svm_polynomial_tun)
#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "polynomial", degree = 2, cost = 2, gamma = 0.1666667, coef0 = -1, scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  2 
#degree:  2 
#coef.0:  -1 

#Number of Support Vectors:  294

svmpoly_predvals_tun <- predict(svm_polynomial_tun, test_data_lasso, decision.values = T)
poly_table1 <- table(svmpoly_predvals_tun, y_test2)
print(poly_table1)
#                    y_test2
#svmpoly_predvals_tun  0  1
#0 43 63
#1 64  0

# Convert the confusion matrix into a data frame
poly_table1_df <- as.data.frame(as.table(poly_table1))

# Rename the columns for ggplot compatibility
colnames(poly_table1_df) <- c("Predicted", "Actual", "Freq")

# Map the values of Actual and Predicted to "M" and "B"
poly_table1_df$Predicted <- ifelse(poly_table1_df$Predicted == 1, "M = 1", "B = 0")
poly_table1_df$Actual <- ifelse(poly_table1_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = poly_table1_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#D5A6BD", high = "#9B59B6") +  # Light pink to purple gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Polynomial SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

attr(svmpoly_predvals_tun, "decision.values") # signed distance to seperation plane

accuracy_poly1 <- sum(diag(poly_table1)) / sum(poly_table1)
sensitivity_poly1 <- poly_table1[2, 2] / sum(poly_table1[, 2])  # True Positive Rate (Recall)
specificity_poly1 <- poly_table1[1, 1] / sum(poly_table1[, 1])  # True Negative Rate
precision_poly1 <- poly_table1[2, 2] / sum(poly_table1[2, ])   # Positive Predictive Value
f1_score_poly1 <- 2 * ((precision_poly1 * sensitivity_poly1) / (precision_poly1 + sensitivity_poly1))

# Print results
cat("Accuracy:", accuracy_poly1, "\n") #Accuracy: 0.2529412  
cat("Sensitivity (Recall):", sensitivity_poly1, "\n") #Sensitivity (Recall): 0
cat("Specificity:", specificity_poly1, "\n") # Specificity: 0.4018692
cat("Precision:", precision_poly1, "\n") # Precision: 0
cat("F1-Score:", f1_score_poly1, "\n") # F1-Score: NaN  


# Radial Kernel
#########################################################################
svm_radial <- svm(Diagnosis ~ ., data = train_data_lasso, kernel = "radial", scale = F)
print(svm_radial)
#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "radial", scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  1 

#Number of Support Vectors:  86
svmradial_predvals <- predict(svm_radial, test_data_lasso, decision.values = T)
radial_table <- table(svmradial_predvals, y_test2)
print(radial_table)
#                  y_test2
#svmradial_predvals   0   1
#0 107   3
#1   0  60

# Convert the confusion matrix into a data frame
radial_table_df <- as.data.frame(as.table(radial_table))

# Rename the columns for ggplot compatibility
colnames(radial_table_df) <- c("Predicted", "Actual", "Freq")
# Map the values of Actual and Predicted to "M" and "B"
radial_table_df$Predicted <- ifelse(radial_table_df$Predicted == 1, "M = 1", "B = 0")
radial_table_df$Actual <- ifelse(radial_table_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = radial_table_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#A2D9CE", high = "#1ABC9C") +  # Light to dark teal gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Radial SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

attr(svmradial_predvals, "decision.values") # signed distance to seperation plane

accuracy_radial <- sum(diag(radial_table)) / sum(radial_table)
sensitivity_radial <- radial_table[2, 2] / sum(radial_table[, 2])  # True Positive Rate (Recall)
specificity_radial <- radial_table[1, 1] / sum(radial_table[, 1])  # True Negative Rate
precision_radial <- radial_table[2, 2] / sum(radial_table[2, ])   # Positive Predictive Value
f1_score_radial <- 2 * ((precision_radial * sensitivity_radial) / (precision_radial + sensitivity_radial))

# Print results
cat("Accuracy:", accuracy_radial, "\n") # Accuracy: 0.9823529
cat("Sensitivity (Recall):", sensitivity_radial, "\n") # Sensitivity (Recall): 0.952381
cat("Specificity:", specificity_radial, "\n") # Specificity: 1
cat("Precision:", precision_radial, "\n") # Precision: 1
cat("F1-Score:", f1_score_radial, "\n") #F1-Score: 0.9756098 


# Tuning Cost Parameter based on Cross-Validation
tobj2<-tune.svm(Diagnosis ~ ., data = train_data_lasso, cost=c(1:8), coef0 = c(-1,8), gamma = 1/c(2:8)) ## Cross-validation to tune cost parameter 
summary(tobj2) 
#Parameter tuning of ‘svm’:

#- sampling method: 10-fold cross validation 

#- best parameters:
#  gamma coef0 cost
#  0.125    -1    3

#- best performance: 0.0300641 

# Extract the best parameters
best_cost2 <- tobj2$best.parameters$cost
best_gamma2 <-tobj2$best.parameters$gamma
best_coef02 <- tobj2$best.parameters$coef0

svm_radial1 <- svm(Diagnosis ~ ., data = train_data_lasso, kernel = "radial", scale = F, cost = best_cost2, coef0 = best_coef02, gamma = best_gamma2)
print(svm_radial1)
#Call:
#svm(formula = Diagnosis ~ ., data = train_data_lasso, kernel = "radial", cost = 3, coef0 = -1, gamma = 0.125, scale = F)


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  3 

#Number of Support Vectors:  74

svmradial_predvals1 <- predict(svm_radial1, test_data_lasso, decision.values = T)
radial_table1 <- table(svmradial_predvals1, y_test2)
print(radial_table1)
#                   y_test2
#svmradial_predvals1   0   1
#0 105   3
#1   2  60

# Convert the confusion matrix into a data frame
radial_table1_df <- as.data.frame(as.table(radial_table1))

# Rename the columns for ggplot compatibility
colnames(radial_table1_df) <- c("Predicted", "Actual", "Freq")

# Map the values of Actual and Predicted to "M" and "B"
radial_table1_df$Predicted <- ifelse(radial_table1_df$Predicted == 1, "M = 1", "B = 0")
radial_table1_df$Actual <- ifelse(radial_table1_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = radial_table1_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F9E79F", high = "#F39C12") +  # Light yellow to dark orange gradient
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Radial SVM Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

accuracy_radial1 <- sum(diag(radial_table1)) / sum(radial_table1)
sensitivity_radial1 <- radial_table1[2, 2] / sum(radial_table1[, 2])  # True Positive Rate (Recall)
specificity_radial1 <- radial_table1[1, 1] / sum(radial_table1[, 1])  # True Negative Rate
precision_radial1 <- radial_table1[2, 2] / sum(radial_table1[2, ])   # Positive Predictive Value
f1_score_radial1 <- 2 * ((precision_radial1 * sensitivity_radial1) / (precision_radial1 + sensitivity_radial1))

# Print results
cat("Accuracy:", accuracy_radial1, "\n") # Accuracy: 0.9705882 
cat("Sensitivity (Recall):", sensitivity_radial1, "\n") # Sensitivity (Recall): 0.952381 
cat("Specificity:", specificity_radial1, "\n") # Specificity: 0.9813084 
cat("Precision:", precision_radial1, "\n") # Precision: 0.9677419 
cat("F1-Score:", f1_score_radial1, "\n") # F1-Score: 0.96

compute_metrics <- function(conf_matrix_svm_total) {
  tn <- conf_matrix_svm[1, 1]
  fp <- conf_matrix_svm[1, 2]
  fn <- conf_matrix_svm[2, 1]
  tp <- conf_matrix_svm[2, 2]
  
  accuracy_svm_total <- (tp + tn) / sum(conf_matrix_svm_total)
  sensitivity_svm_total <- tp / (tp + fn)
  specificity_svm_total <- tn / (tn + fp)
  precision_svm_total <- tp / (tp + fp)
  f1_score_svm_total <- 2 * (precision_svm_total * sensitivity_svm_total) / (precision_svm_total + sensitivity_svm_total)
  
  return(c(accuracy = accuracy_svm_total, sensitivity = sensitivity_svm_total, specificity = specificity_svm_total, 
           precision = precision_svm_total, f1_score = f1_score_svm_total))
}



#########################################################################
#Decision Tree
#########################################################################

# Train a decision tree using the training data
decision_tree_ <- rpart(
  Diagnosis ~ ., 
  data = train_data_lasso, 
  method = "class"
)

# Visualize the decision tree
rpart.plot(decision_tree_, main = "Decision Tree")

# Predict on the test set
predictions_lasso <- predict(decision_tree_, x_test2, type = "class")

# Evaluate model performance
conf_matrix_lasso <- confusionMatrix(predictions_lasso, y_test2)
print(conf_matrix_lasso$table)

# Calculate performance metrics
accuracy_lasso <- sum(diag(conf_matrix_lasso$table)) / sum(conf_matrix_lasso$table)
sensitivity_lasso <- conf_matrix_lasso$table[2, 2] / sum(conf_matrix_lasso$table[, 2])  # True Positive Rate
specificity_lasso <- conf_matrix_lasso$table[1, 1] / sum(conf_matrix_lasso$table[, 1])  # True Negative Rate
precision_lasso <- conf_matrix_lasso$table[2, 2] / sum(conf_matrix_lasso$table[2, ])   # Positive Predictive Value
f1_score_lasso <- 2 * ((precision_lasso * sensitivity_lasso) / (precision_lasso + sensitivity_lasso))

cat("Accuracy:", accuracy_lasso, "\n")
cat("Sensitivity (Recall):", sensitivity_lasso, "\n")
cat("Specificity:", specificity_lasso, "\n")
cat("Precision:", precision_lasso, "\n")
cat("F1-Score:", f1_score_lasso, "\n")

# Tune hyperparameters using tune.rpart
tuned_rpart_lasso <- tune.rpart(
  Diagnosis ~ ., 
  data = train_data_lasso,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  cp = seq(0.0001, 0.05, by = 0.001),  # Finer cp tuning
  minsplit = seq(10, 50, by = 10),     # Wider range for minsplit
  maxdepth = seq(2, 10, by = 1)        # Explore deeper trees
)


#print(tuned_rpart_lasso$best.parameters)

#    minsplit    cp maxdepth
#752       20 1e-04        5


decision_tree1_lasso <- rpart(
  Diagnosis ~ ., 
  data = train_data_lasso, 
  method = "class", 
  control = rpart.control(cp = 1e-04, minsplit = 20, maxdepth = 5)
)

# Visualize the tuned decision tree
rpart.plot(decision_tree1_lasso, main = "Tuned Decision Tree with Best Parameters")

# Predict on the test set using the tuned model
predictions1_lasso <- predict(decision_tree1_lasso, x_test2, type = "class")

# Evaluate the tuned model's performance
conf_matrix1_lasso <- confusionMatrix(predictions1_lasso, y_test2)
print(conf_matrix1_lasso$table)

# Visualize the tuned decision tree
rpart.plot(decision_tree1_lasso, main = "Tuned Decision Tree with Best Parameters")

# Predict on the test set using the tuned model
predictions1_lasso <- predict(decision_tree1_lasso, x_test2, type = "class")

# Evaluate the tuned model's performance
conf_matrix1_lasso <- confusionMatrix(predictions1_lasso, y_test2)
print(conf_matrix1_lasso$table)

# Create a data frame from the tuned confusion matrix
conf_matrix1_df <- as.data.frame(conf_matrix1_lasso$table)

# Plot confusion matrix as a heatmap
ggplot(data = conf_matrix1_df, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  labs(title = "Confusion Matrix Heatmap (Tuned Decision Tree)",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()

# Calculate performance metrics for the tuned model
accuracy1_lasso <- sum(diag(conf_matrix1_lasso$table)) / sum(conf_matrix1_lasso$table)
sensitivity1_lasso <- conf_matrix1_lasso$table[2, 2] / sum(conf_matrix1_lasso$table[, 2])  # True Positive Rate
specificity1_lasso <- conf_matrix1_lasso$table[1, 1] / sum(conf_matrix1_lasso$table[, 1])  # True Negative Rate
precision1_lasso <- conf_matrix1_lasso$table[2, 2] / sum(conf_matrix1_lasso$table[2, ])   # Positive Predictive Value
f1_score1_lasso <- 2 * ((precision1_lasso * sensitivity1_lasso) / (precision1_lasso + sensitivity1_lasso))

cat("Accuracy (Tuned):", accuracy1_lasso, "\n")
cat("Sensitivity (Recall) (Tuned):", sensitivity1_lasso, "\n")
cat("Specificity (Tuned):", specificity1_lasso, "\n")
cat("Precision (Tuned):", precision1_lasso, "\n")
cat("F1-Score (Tuned):", f1_score1_lasso, "\n")


#########################################################################
#Random forest(Bagging) method with the 9 best features
#########################################################################
# Train the Bagging Random Forest model
bagging_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = ncol(x_train2),
  importance = TRUE,
  proximity = TRUE
)

# Predictions and confusion matrix
bagging_pred <- predict(bagging_model, x_test2, type = "class")
bagging_conf_matrix <- table(bagging_pred, y_test2)
print(bagging_conf_matrix)

# Convert confusion matrix to dataframe for heatmap
bagging_conf_matrix_df <- as.data.frame(as.table(bagging_conf_matrix))
colnames(bagging_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
bagging_conf_matrix_df$Predicted <- ifelse(bagging_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
bagging_conf_matrix_df$Actual <- ifelse(bagging_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap
ggplot(data = bagging_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Bagging(m = p) Random Forest Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Calculate evaluation metrics
bagging_accuracy <- sum(diag(bagging_conf_matrix)) / sum(bagging_conf_matrix)
bagging_sensitivity <- bagging_conf_matrix[2, 2] / sum(bagging_conf_matrix[, 2])
bagging_specificity <- bagging_conf_matrix[1, 1] / sum(bagging_conf_matrix[, 1])
bagging_precision <- bagging_conf_matrix[2, 2] / sum(bagging_conf_matrix[2, ])
bagging_f1_score <- 2 * ((bagging_precision * bagging_sensitivity) / 
                           (bagging_precision + bagging_sensitivity))

cat("Accuracy:", bagging_accuracy, "\n")
cat("Sensitivity (Recall):", bagging_sensitivity, "\n")
cat("Specificity:", bagging_specificity, "\n")
cat("Precision:", bagging_precision, "\n")
cat("F1-Score:", bagging_f1_score, "\n")

# Hyperparameter tuning
tuned_bagging <- tune.randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = ncol(x_train2), 
  ntree = c(100, 200, 300),
  tunecontrol = tune.control(sampling = "cross", cross = 10)
)

print(tuned_bagging)

# Train the model with tuned parameters
tuned_bagging_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = ncol(x_train2),
  ntree = tuned_bagging$best.parameters$ntree,
  importance = TRUE,
  proximity = TRUE
)

# Predictions and confusion matrix for tuned model
tuned_bagging_pred <- predict(tuned_bagging_model, x_test2, type = "class")
tuned_bagging_conf_matrix <- table(tuned_bagging_pred, y_test2)
print(tuned_bagging_conf_matrix)

# Convert tuned confusion matrix to dataframe for heatmap
tuned_bagging_conf_matrix_df <- as.data.frame(as.table(tuned_bagging_conf_matrix))
colnames(tuned_bagging_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
tuned_bagging_conf_matrix_df$Predicted <- ifelse(tuned_bagging_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
tuned_bagging_conf_matrix_df$Actual <- ifelse(tuned_bagging_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Create the heatmap for tuned model
ggplot(data = tuned_bagging_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Random Forest Model m=p)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Calculate evaluation metrics for tuned model
tuned_bagging_accuracy <- sum(diag(tuned_bagging_conf_matrix)) / sum(tuned_bagging_conf_matrix)
tuned_bagging_sensitivity <- tuned_bagging_conf_matrix[2, 2] / sum(tuned_bagging_conf_matrix[, 2])
tuned_bagging_specificity <- tuned_bagging_conf_matrix[1, 1] / sum(tuned_bagging_conf_matrix[, 1])
tuned_bagging_precision <- tuned_bagging_conf_matrix[2, 2] / sum(tuned_bagging_conf_matrix[2, ])
tuned_bagging_f1_score <- 2 * ((tuned_bagging_precision * tuned_bagging_sensitivity) / 
                                 (tuned_bagging_precision + tuned_bagging_sensitivity))

cat("Accuracy (Tuned):", tuned_bagging_accuracy, "\n")
cat("Sensitivity (Recall) (Tuned):", tuned_bagging_sensitivity, "\n")
cat("Specificity (Tuned):", tuned_bagging_specificity, "\n")
cat("Precision (Tuned):", tuned_bagging_precision, "\n")
cat("F1-Score (Tuned):", tuned_bagging_f1_score, "\n")

#########################################################################
#Random forest(m = sqrt(p)) method with the 9 best features
#########################################################################
# Train a Random Forest model with m = sqrt(p)
rf_sqrt_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = floor(sqrt(ncol(x_train2))),
  importance = TRUE,
  proximity = TRUE
)

# Predict on the test set
rf_sqrt_pred <- predict(rf_sqrt_model, x_test2, type = "class")

# Confusion matrix
rf_sqrt_conf_matrix <- table(rf_sqrt_pred, y_test2)
print(rf_sqrt_conf_matrix)

# Convert confusion matrix to a data frame for heatmap visualization
rf_sqrt_conf_matrix_df <- as.data.frame(as.table(rf_sqrt_conf_matrix))
colnames(rf_sqrt_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
rf_sqrt_conf_matrix_df$Predicted <- ifelse(rf_sqrt_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
rf_sqrt_conf_matrix_df$Actual <- ifelse(rf_sqrt_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Create a heatmap
ggplot(data = rf_sqrt_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Random Forest (m = sqrt(p)) Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Model evaluation metrics
rf_sqrt_accuracy <- sum(diag(rf_sqrt_conf_matrix)) / sum(rf_sqrt_conf_matrix)
rf_sqrt_sensitivity <- rf_sqrt_conf_matrix[2, 2] / sum(rf_sqrt_conf_matrix[, 2])  # Recall
rf_sqrt_specificity <- rf_sqrt_conf_matrix[1, 1] / sum(rf_sqrt_conf_matrix[, 1])  # Specificity
rf_sqrt_precision <- rf_sqrt_conf_matrix[2, 2] / sum(rf_sqrt_conf_matrix[2, ])   # Precision
rf_sqrt_f1_score <- 2 * ((rf_sqrt_precision * rf_sqrt_sensitivity) / (rf_sqrt_precision + rf_sqrt_sensitivity))

# Print metrics
cat("Accuracy:", rf_sqrt_accuracy, "\n")
cat("Sensitivity (Recall):", rf_sqrt_sensitivity, "\n")
cat("Specificity:", rf_sqrt_specificity, "\n")
cat("Precision:", rf_sqrt_precision, "\n")
cat("F1-Score:", rf_sqrt_f1_score, "\n")

# Perform Random Forest hyperparameter tuning
tuned_rf_sqrt <- tune.randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = floor(sqrt(ncol(x_train2))),
  ntree = c(100, 200, 300),
  tunecontrol = tune.control(sampling = "cross", cross = 10)
)

# Print tuned model
print(tuned_rf_sqrt)

# Train the tuned Random Forest model
tuned_rf_sqrt_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = 3,
  ntree = 200,
  importance = TRUE,
  proximity = TRUE
)

# Predict using the tuned model
tuned_rf_sqrt_pred <- predict(tuned_rf_sqrt_model, x_test2, type = "class")

# Confusion matrix for tuned model
tuned_rf_sqrt_conf_matrix <- table(tuned_rf_sqrt_pred, y_test2)
print(tuned_rf_sqrt_conf_matrix)

# Convert to data frame for heatmap
tuned_rf_sqrt_conf_matrix_df <- as.data.frame(as.table(tuned_rf_sqrt_conf_matrix))
colnames(tuned_rf_sqrt_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
tuned_rf_sqrt_conf_matrix_df$Predicted <- ifelse(tuned_rf_sqrt_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
tuned_rf_sqrt_conf_matrix_df$Actual <- ifelse(tuned_rf_sqrt_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Heatmap for tuned model
ggplot(data = tuned_rf_sqrt_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F9E79F", high = "#F39C12") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Random Forest (m = sqrt(p)))",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Metrics for tuned model
tuned_rf_sqrt_accuracy <- sum(diag(tuned_rf_sqrt_conf_matrix)) / sum(tuned_rf_sqrt_conf_matrix)
tuned_rf_sqrt_sensitivity <- tuned_rf_sqrt_conf_matrix[2, 2] / sum(tuned_rf_sqrt_conf_matrix[, 2])  # Recall
tuned_rf_sqrt_specificity <- tuned_rf_sqrt_conf_matrix[1, 1] / sum(tuned_rf_sqrt_conf_matrix[, 1])  # Specificity
tuned_rf_sqrt_precision <- tuned_rf_sqrt_conf_matrix[2, 2] / sum(tuned_rf_sqrt_conf_matrix[2, ])   # Precision
tuned_rf_sqrt_f1_score <- 2 * ((tuned_rf_sqrt_precision * tuned_rf_sqrt_sensitivity) / (tuned_rf_sqrt_precision + tuned_rf_sqrt_sensitivity))

# Print tuned metrics
cat("Accuracy (Tuned):", tuned_rf_sqrt_accuracy, "\n")
cat("Sensitivity (Recall) (Tuned):", tuned_rf_sqrt_sensitivity, "\n")
cat("Specificity (Tuned):", tuned_rf_sqrt_specificity, "\n")
cat("Precision (Tuned):", tuned_rf_sqrt_precision, "\n")
cat("F1-Score (Tuned):", tuned_rf_sqrt_f1_score, "\n")




#########################################################################
#Random forest(m = p/2) method with the 9 best features
#########################################################################

# Train the Random Forest model with mtry = p/2
rf_half_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = floor(ncol(x_train2) / 2), 
  importance = TRUE,
  proximity = TRUE
)

# Predict on test data
rf_half_pred <- predict(rf_half_model, x_test2, type = "class")

# Confusion Matrix
rf_half_conf_matrix <- table(rf_half_pred, y_test2)
print(rf_half_conf_matrix)

# Convert confusion matrix to data frame for heatmap
rf_half_conf_matrix_df <- as.data.frame(as.table(rf_half_conf_matrix))
colnames(rf_half_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
rf_half_conf_matrix_df$Predicted <- ifelse(rf_half_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
rf_half_conf_matrix_df$Actual <- ifelse(rf_half_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Create heatmap
ggplot(data = rf_half_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Random Forest(m = p/2) Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Performance Metrics
rf_half_accuracy <- sum(diag(rf_half_conf_matrix)) / sum(rf_half_conf_matrix)
rf_half_sensitivity <- rf_half_conf_matrix[2, 2] / sum(rf_half_conf_matrix[, 2])  # True Positive Rate (Recall)
rf_half_specificity <- rf_half_conf_matrix[1, 1] / sum(rf_half_conf_matrix[, 1])  # True Negative Rate
rf_half_precision <- rf_half_conf_matrix[2, 2] / sum(rf_half_conf_matrix[2, ])   # Positive Predictive Value
rf_half_f1_score <- 2 * ((rf_half_precision * rf_half_sensitivity) / (rf_half_precision + rf_half_sensitivity))

cat("Accuracy:", rf_half_accuracy, "\n")
cat("Sensitivity (Recall):", rf_half_sensitivity, "\n")
cat("Specificity:", rf_half_specificity, "\n")
cat("Precision:", rf_half_precision, "\n")
cat("F1-Score:", rf_half_f1_score, "\n")

# Hyperparameter Tuning for Random Forest
tuned_rf_half <- tune.randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = floor(ncol(x_train2) / 2), 
  ntree = c(100, 200, 300),
  tunecontrol = tune.control(sampling = "cross", cross = 10)
)

print(tuned_rf_half)

# Train the model with optimal parameters
tuned_rf_half_model <- randomForest(
  Diagnosis ~ ., 
  data = train_data_lasso,
  mtry = tuned_rf_half$best.parameters$mtry,
  ntree = tuned_rf_half$best.parameters$ntree,
  importance = TRUE,
  proximity = TRUE
)

# Predict using tuned model
tuned_rf_half_pred <- predict(tuned_rf_half_model, x_test2, type = "class")
tuned_rf_half_conf_matrix <- table(tuned_rf_half_pred, y_test2)
print(tuned_rf_half_conf_matrix)

# Convert tuned confusion matrix to data frame for heatmap
tuned_rf_half_conf_matrix_df <- as.data.frame(as.table(tuned_rf_half_conf_matrix))
colnames(tuned_rf_half_conf_matrix_df) <- c("Predicted", "Actual", "Freq")
tuned_rf_half_conf_matrix_df$Predicted <- ifelse(tuned_rf_half_conf_matrix_df$Predicted == 1, "M = 1", "B = 0")
tuned_rf_half_conf_matrix_df$Actual <- ifelse(tuned_rf_half_conf_matrix_df$Actual == 1, "M = 1", "B = 0")

# Heatmap for tuned model
ggplot(data = tuned_rf_half_conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue", high = "navy") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Tuned Random Forest Model)",
    x = "Predicted",
    y = "Actual",
    fill = "Frequency"
  )

# Performance Metrics for Tuned Model
tuned_rf_half_accuracy <- sum(diag(tuned_rf_half_conf_matrix)) / sum(tuned_rf_half_conf_matrix)
tuned_rf_half_sensitivity <- tuned_rf_half_conf_matrix[2, 2] / sum(tuned_rf_half_conf_matrix[, 2])
tuned_rf_half_specificity <- tuned_rf_half_conf_matrix[1, 1] / sum(tuned_rf_half_conf_matrix[, 1])
tuned_rf_half_precision <- tuned_rf_half_conf_matrix[2, 2] / sum(tuned_rf_half_conf_matrix[2, ])
tuned_rf_half_f1_score <- 2 * ((tuned_rf_half_precision * tuned_rf_half_sensitivity) / (tuned_rf_half_precision + tuned_rf_half_sensitivity))

cat("Accuracy (Tuned):", tuned_rf_half_accuracy, "\n")
cat("Sensitivity (Recall) (Tuned):", tuned_rf_half_sensitivity, "\n")
cat("Specificity (Tuned):", tuned_rf_half_specificity, "\n")
cat("Precision (Tuned):", tuned_rf_half_precision, "\n")
cat("F1-Score (Tuned):", tuned_rf_half_f1_score, "\n")


#oob rates before tuning plot
plot(rf_sqrt_model$err.rate[,1], type = "l", lwd = 3, lty=1, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")
lines(rf_half_model$err.rate[,1],lwd=3, lty=2, col="red")
lines(bagging_model$err.rate[,1],lwd=3, lty=3, col="green")
legend("topright",c("m=sqrt(p)","m=p/2","m=p(bagging)"),col=c("blue","red","green"), lty=c(1,2,3))


###########################################################################


# Store the metrics dynamically after computing them in your code
model_comparison <- data.frame(
  Model = c(
    "Decision Tree",
    "KNN",
    "Naive Bayes (Tuned)",
    "SVM (Linear)",
    "SVM (Radial)",
    "Random Forest"
  ),
  Accuracy = c(
    accuracy1_lasso, #Decision Tree
    accuracy_best_lasso ,  # KNN 
    accuracy_nbmodel_tuned,  # Naive Bayes (Tuned)
    accuracy_svm1,           # SVM (Linear)
    accuracy_radial1,         # SVM (Radial)
    tuned_bagging_accuracy #random forest
  ),
  Sensitivity = c(
    sensitivity1_lasso, #decision tree
    sensitivity_best_lasso ,  # KNN 
    sensitivity_tuned,          # Naive Bayes (Tuned)
    sensitivity_svm1,           # SVM (Linear)
    sensitivity_radial1,         # SVM (Radial)
    tuned_bagging_sensitivity #random forest
  ),
  Specificity = c(
    specificity1_lasso, #decision tree
    specificity_best_lasso ,  # KNN 
    specificity_tuned,          # Naive Bayes (Tuned)
    specificity_svm1,           # SVM (Linear)
    specificity_radial1,        # SVM (Radial)
    tuned_bagging_specificity
  ),
  Precision = c(
    precision1_lasso, #decision tree
    precision_best_lasso ,  # KNN 
    precision_tuned,          # Naive Bayes (Tuned)
    precision_svm1,           # SVM (Linear)
    precision_radial1,         # SVM (Radial)
    tuned_bagging_precision # precision
  ),
  F1_Score = c(
    f1_score1_lasso, #decision tree
    f1_score_best_lasso ,  # KNN 
    f1_score_tuned,          # Naive Bayes (Tuned)
    f1_score_svm1,           # SVM (Linear)
    f1_score_radial1,         # SVM (Radial)
    tuned_bagging_f1_score # random forest
  )

)

# Print the comparison table
print(model_comparison)
#                   Model  Accuracy Sensitivity Specificity Precision  F1_Score Cross_Val_Accuracy






# Arrange the data in decreasing order of Accuracy
model_comparison <- model_comparison %>%
  arrange(desc(Accuracy))

# Convert Model column to a factor with levels in decreasing order of Accuracy
model_comparison$Model <- factor(model_comparison$Model, levels = model_comparison$Model)

# Plot Accuracy with labels
ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 5)),  # Add accuracy as labels, rounded to 3 decimal places
            position = position_dodge(width = 0.9), 
            vjust = -0.3) +  # Adjust the vertical position of the labels
  coord_flip() +
  labs(title = "Model Comparison: Accuracy",
       x = "Model",
       y = "Accuracy") +
  theme_minimal()





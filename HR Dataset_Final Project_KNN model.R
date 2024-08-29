# ---------------- Importing Packages --------------------
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(fastDummies)
library(caTools)
library(class)
library(rpart)

# ---------------- Data Loading --------------------

data = read.csv('HR_Dataset.csv') 
head(data)

dim(data)

summary(data)

str(data)

# ---------------- Data Cleaning ----------------

# Remove the unwanted columns
data[,c('enrollee_id')]<-list(NULL)

# Rename the column with spelling error
names(data)[names(data) == 'relevent_experience'] <- "relevant_experience"

# Explore the unique value of each categorical variable
col_name<-names(data)
categorical_col_name<-names(Filter(is.character,data))
num_col_name<-setdiff(col_name,categorical_col_name)
num_col_name<-num_col_name[!num_col_name %in% 'target']
categorical_data<-data[,c(categorical_col_name)]
unique_value<-function(x){
  print("Unique values of categorical variables in the dataset:")
  lapply(x,unique)
}
unique_value(categorical_data)

# Function to count the unique values in each categorical variable
unique_value_count <- function(x) {
  print("Count of unique values in each categorical variable:")
  sapply(x, function(col) length(unique(col)))
}

# Apply the function to the categorical data
unique_counts <- unique_value_count(categorical_data)

# Print the results
unique_counts

# Rename the value of some categorical variables
#replace 'has relevant experience' and 'no relevant experience' with TRUE and FALSE
data$relevant_experience[data$relevant_experience=='Has relevent experience']<-'yes'
data$relevant_experience[data$relevant_experience=='No relevent experience']<-'no'
#replace the inconsistent value on company size variable
data$company_size<-replace(data$company_size,data$company_size == '10/49', '10-49')
data$company_size<-replace(data$company_size,data$company_size == '100-500', '100-499')
data$company_size<-replace(data$company_size,data$company_size == '10000+', '>9999')
data$last_new_job<-replace(data$last_new_job,data$last_new_job=='never',0)

# --------------- Data Pre-processing --------------

# detect missing values
missing_value<-function(x){
  print("Missing values in the dataset:")
  for(i in x) {
    print(paste(i,sum(data[i]==""|is.na(data[i]))))
  }
}
missing_value(col_name)

# Visualize each variable with missing values
cols_with_nan_data = data[,c('gender', 'enrolled_university', 'major_discipline', 'experience', 'company_size', 'last_new_job', 'company_type', 'education_level')] 
print(head(cols_with_nan_data))

# Create a list of bar plots with data labels
list_plots <- lapply(names(cols_with_nan_data), function(col) {
  ggplot(cols_with_nan_data, aes(.data[[col]], ..count..)) + 
    geom_bar(aes(fill = .data[[col]]), position = "dodge", stat = "count") +
    geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add data labels
    labs(title = paste("Distribution of", col), x = col, y = "Count") +
    theme_minimal()
})

# Print the list of plots
list_plots

# % of NULL values
for(i in col_name){
  data[!is.na(data[i])&data[i]=="",i]<-NA
}
# Find the size of missing value to each variable
missing_value_size<-function(x){
  print("The proportion of missing value")
  for(i in x){
    if(sum(is.na(data[i]))>0){
      print(paste(i,percent((sum(is.na(data[i]))/nrow(data)))))
    }
  }
}
missing_value_size(col_name)

# Define a function to calculate the mode
get_mode <- function(x) {
  uniq_values <- unique(na.omit(x))  # Remove NAs and find unique values
  uniq_values[which.max(tabulate(match(x, uniq_values)))]  # Return the mode
}

# Calculate the mode of the enrolled_university column
mode_enrolled_university <- get_mode(data$enrolled_university)

# Fill missing values in the enrolled_university column with the mode
data$enrolled_university[is.na(data$enrolled_university)] <- mode_enrolled_university

# Calculate the mode of the experience column
mode_experience <- get_mode(data$experience)

# Fill missing values in the experience column with the mode
data$experience[is.na(data$experience)] <- mode_experience

# Calculate the mode of the last_new_job column
mode_last_new_job <- get_mode(data$last_new_job)

# Fill missing values in the last_new_job column with the mode
data$last_new_job[is.na(data$last_new_job)] <- mode_last_new_job

# Calculate the mode of the education_level column
mode_education_level <- get_mode(data$education_level)

# Fill missing values in the education_level column with the mode
data$education_level[is.na(data$education_level)] <- mode_education_level

# Filling NULL value with 'unknown'
col_miss<-c('gender','major_discipline','company_size','company_type')
for(i in col_miss){
  data[is.na(data[i]),i]<-'unknown'
}
head(data)

missing_value(col_name)

# ------------- Feature Engineering ---------------

# handling the value of the variable 'city' as a numeric type
data <- separate(data, city, c('Name','city'),sep = '_')
data[,'Name']<-list(NULL)
head(data)

# Check outliers
for(i in num_col_name){
  boxplot(data[i],main="Boxplot")
  print(paste(i,length(data[,i][data[,i] %in% boxplot.stats(data[,i])$out])))
}

# Printing and plotting outliers
print(paste('minimum outlier value for training hour is:',min(data[,'training_hours'][data[,'training_hours'] %in% boxplot.stats(data[,'training_hours'])$out])))

print(paste('maximum outlier value for training hour is:',max(data[,'training_hours'][data[,'training_hours'] %in% boxplot.stats(data[,'training_hours'])$out])))

print(unique(data[,'city_development_index'][data[,'city_development_index'] %in% boxplot.stats(data[,'city_development_index'])$out]))

unique(filter(data,city_development_index==0.448)[,'city'])

unique(filter(data,city==33)[,'city_development_index'])

# Convert the categorical variables to dummy value for data analysis facilitating purpose
data_preprocess_categorical<-dummy_cols(data,select_columns=categorical_col_name)
data_preprocess_categorical[,c(categorical_col_name)]<-list(NULL)
dim(data_preprocess_categorical)

# ------------- Data Scaling ------------------

# Scaling the Numerical Features
scaled_data <- data_preprocess_categorical
scaled_data[num_col_name] <- scale(data_preprocess_categorical[num_col_name])

# ------------- Data Train-Test Splitting ------------------

# Split train and test dataset
preprocessed_data<-data_preprocess_categorical
preprocessed_data$target<-as.factor(preprocessed_data$target)
set.seed(1)
sample<-sample.split(preprocessed_data$target,SplitRatio=0.7)
train<-subset(preprocessed_data,sample==TRUE)
test<-subset(preprocessed_data,sample==FALSE)

# Performance Table function 
performance = function(xtab, desc=""){
  cat(desc,"\n")
  ACR = sum(diag(xtab))/sum(xtab)
  TPR = xtab[1,1]/sum(xtab[,1]); TNR = xtab[2,2]/sum(xtab[,2])
  PPV = xtab[1,1]/sum(xtab[1,]); NPV = xtab[2,2]/sum(xtab[2,])
  FPR = 1 - TNR                ; FNR = 1 - TPR
  RandomAccuracy = (sum(xtab[,2])*sum(xtab[2,]) + 
                      sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
  Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
  print(xtab)
  cat("\nAccuracy (ACR)                  :", ACR, "\n")
  cat("Sensitivity(TPR)                :", TPR, "\n")
  cat("Specificity (TNR)               :", TNR, "\n")
  cat("Positive Predictive Value (PPV) :", PPV, "\n")
  cat("Negative Predictive Value (NPV) :", NPV, "\n")
  cat("False Positive Rate (FPR)       :", FPR, "\n")
  cat("False Negative Rate(FNR)        :", FNR, "\n")
}

# Function for Predicted probability of candidate
prob_gen<-function(x){
  print(paste("Predicted probability of candidate will stay at the company",round(sum(x[2,])/sum(x),4)))
  print(paste("Predicted probability of candidate will leave the company",round(sum(x[1,])/sum(x),4)))
}

dim(train)

# ---------- xxxxxxxxxxxxxxxxxxxxxxxxxxxx-------xxxxxxxxxxxxxxxxxxxxxxx------xxxxxxxxxxxxxxxxxxx

# Sachin Prakash Kashyap (KNN Model)

# Square Root of N rule

# KNN Model with k-value of sqrt(nrow(train))
set.seed(123)
knn115_model <- knn(train, test, cl=train$target, k=floor(sqrt(nrow(train))))
cfmat_115 <- table(knn115_model, test$target)
cfmat_115 <- cfmat_115[2:1,2:1]
performance(cfmat_115)

# Finding the optimum k to score a better accuracy
set.seed(123)
k.optm=1
k_optm=c()
for (i in 1:15){
 knn.mod <- knn(train, test,cl=train$target, k=i)
 k.optm [i]<- 100 * sum(test$target == knn.mod)/NROW(test$target)
 k_optm[i]<-k.optm[i]
}
k_optm

# Selecting the optimal k value
k_optm_pred<-which(k_optm==max(k_optm))
print(k_optm_pred)

# Plot the graph
plot(k.optm, type="b", xlab="k-value", ylab="Accuracy level", pch=19)

# Add data labels 
text(1:15, k.optm, labels=1:15, pos=1, cex=0.8, col="blue")

# KNN Model with Scaled Data
knn_optm_model<-knn(train, test, cl=train$target, k=k_optm_pred)
cfmat_optm=table(knn_optm_model,test$target)
cfmat_optm = cfmat_optm[2:1,2:1]
performance(cfmat_optm)

prob_gen(cfmat_optm)

# ---------- xxxxxxxxxxxxxxxxxxxxxxxxxxxx-------xxxxxxxxxxxxxxxxxxxxxxx------xxxxxxxxxxxxxxxxxxx


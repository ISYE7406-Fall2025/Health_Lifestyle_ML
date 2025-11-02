
## Take a peek at the data
hl_class <- read.table(file= "health_lifestyle_classification.csv", header=TRUE, sep = ",")
head(hl_class)
dim(hl_class)


class(hl_class)
typeof(hl_class)
str(hl_class)
hist(hl_class$age)

## Histogram of age showing generally uniform distribution
hl_class$age <- as.integer(hl_class$age)



## Check if packages are installed
packages <- c("tidyverse", "dplyr", "VIM")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

library(tidyverse)
library(dplyr)
library(VIM)

## Isolate and observe numeric data
numeric_hl <- hl_class %>%
  select_if(is.numeric)
head(numeric_hl)
dim(numeric_hl)
str(numeric_hl)

## Remove survey code from numeric predictor subset
numeric_hl$survey_code <- NULL

## Create histograms for numeric predictors
numeric_hl_long <- numeric_hl %>%
  pivot_longer(
    cols = colnames(numeric_hl),
    names_to = "predictor_name",
    values_to = "value"
  )
head(numeric_hl_long)

## View histograms (need to scale...)
ggplot(numeric_hl_long, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue") +
  facet_wrap(~ predictor_name, scales = "free_y") +
  labs(
    title = "Histograms of Multiple Predictors",
    x = "Value",
    y = "Count"
  )

### IMPUTATION WORK_____________________________________________________________

## Code target, 1 for diseased, 0 for healthy
hl_class$target <- ifelse(hl_class$target == "diseased", 1, 0)

## Convert character types to factor types
hl_class <- hl_class %>%
  mutate(across(where(is.character), as.factor))

## Remove features with no variance or inherent meaning
hl_class <- hl_class %>% select(-electrolyte_level)
hl_class <- hl_class %>% select(-gene_marker_flag)
hl_class <- hl_class %>% select(-environmental_risk_score)
hl_class <- hl_class %>% select(-survey_code)

## Count total number of rows with na values
rows_with_na <- apply(is.na(hl_class), 1, any)
number_of_na_rows <- sum(rows_with_na)
print(number_of_na_rows)


## Assemble na count by column
for(col_name in colnames(hl_class)){
  total_na <- sum(is.na(hl_class[[col_name]]))
  formatted_str <- sprintf("%s:", col_name)
  cat(formatted_str, total_na, "\n")
}

## Impute na values for three factors after domain exploration
impute_vars_r1 <- c("blood_pressure", "heart_rate", "daily_steps")
## Hot deck sort order for similar entry imputing
sort_order_r1 <- c("age","gender","bmi","physical_activity") 

hl_class_imputed_r1 <- hl_class
for (var in impute_vars_r1) {
  hl_class_imputed_r1 <- hotdeck(
    data = hl_class_imputed_r1,
    variable = var,
    ord_var = sort_order_r1
  )
}

## Impute na values for insulin after domain exploration
hl_class_imputed_r2 <- hl_class_imputed_r1
hl_class_imputed_r2 <- hotdeck(data = hl_class_imputed_r2,
                               variable = "insulin",
                               ord_var = "glucose")

## Impute na values for income after domain exploration
sort_order_r3 <- c("age","job_type","occupation","education_level")
hl_class_imputed_r3 <- hl_class_imputed_r2
hl_class_imputed_r3 <- hotdeck(data = hl_class_imputed_r2,
                               variable = "income",
                               ord_var = sort_order_r3)

## Create cleaned dataframe for analysis
hl_class_clean <- hl_class_imputed_r3 %>%
  select(-ends_with("_imp"))

## Count total number of rows with na values for hl_class_clean
rows_with_na <- apply(is.na(hl_class_clean), 1, any)
number_of_na_rows <- sum(rows_with_na)
print(number_of_na_rows)

### MODEL CONSTRUCTION__________________________________________________________

# Create train and test sets
set.seed(123)
mark <- sort(sample(100000,20000, replace = FALSE))
marktrain <- hl_class_clean[-mark,]
marktest <- hl_class_clean[mark,]

## Extract the true response value for training and testing data
y1    <- marktrain$target;
y2    <- marktest$target;

### ATTEMPT TO IMPUTE BY KNN - TOO COMPUTATIONALLY EXPENSIVE!!!_________________
# hl_class_imputed <- preProcess(hl_class, method = "knnImpute")
# 
# hl_class_prepared <-predict(hl_class_imputed, newdata = hl_class)
###_____________________________________________________________________________


##AND ATTEMPT TO BUILD GBM MODEL (PREDICTED ONE CLASS...

## GBM construction and hyperparameter tuning (taking too long and abandoned)
# ## Define the parameter grid
# tune_grid <- expand.grid(
#   n.trees = c(100, 500),
#   interaction.depth = c(1, 2),
#   shrinkage = c(0.1, 0.01),
#   n.minobsinnode = 20
# )
# 
# ## Define the training control
# train_control <- trainControl(
#   method = "cv",
#   number = 5,
#   verboseIter = TRUE # See training progress
# )
# 
# ## Perform the grid search
# hl_gbm_tuned <- train(
#   target ~ .,
#   data = marktrain,
#   method = "gbm",
#   trControl = train_control,
#   tuneGrid = tune_grid,
#   verbose = FALSE # Suppress GBM training output
# )
# 
# ## Examine the results
# print(hl_gbm_tuned)
# 
# ## View the best performing parameters
# print(hl_gbm_tuned$bestTune)
# 
# 
# ## Make predictions and evaluate performance on the test set
# pred_hl_gbm <- predict(hl_gbm_tuned, newdata = marktrain, n.trees=100, type= "raw")
# pred_hl_gbm[1:10]

## Choose parameters for a single model without tuning
gbm.spam1 <- gbm(target ~ .,data=marktrain,
                 distribution = 'bernoulli',
                 n.trees = 500, 
                 shrinkage = 0.001, 
                 interaction.depth = 7,
                 cv.folds = 5)

## Model Inspection 
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.spam1, method="cv") 
perf_gbm1

## summary model
## Which variances are important
summary(gbm.spam1)

## Model effectively predicted only one class
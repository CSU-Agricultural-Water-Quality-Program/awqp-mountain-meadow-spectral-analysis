# Using regression to predict plan and soil N status in mountain meadow hay
# systems using spectral data derived from drone imagery.

# Created by A.J. Brown
# Date created: 5 July 2023
# Date last modified: 5 July 2023

# Script work flow:
 # Step 1: Import libraries
 # Step 2: Import data
 # Step 3: Data cleaning
 # Step 4: Data exploration via ggplot2 and other methods
 # Step 5: Regression model creation
 # Step 6: Model goodness of fit analysis
 # Step 7: Model selection, final prediction, and data export
 # Step 8: use final model to interpolate N status across the study area
 # Step 9: export final interpolated data

# Step 1: Import libraries
package.list <- c("dplyr",
                  "ggplot2",
                  "GGally",
                  "caret"
                  )
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)

# Step 2: Import data
 # Import data from .csv file
 file.path <- "../Example Data/Final Data.csv"
 data <- read.csv(file.path,
                  header = TRUE,
                  sep = ",",
                  na.strings = c("N/A", " ", "")
                  )

# Step 3: Data cleaning
 clean.df <- data

# Step 4: Data exploration via ggplot2 and other methods
 # Scatterplot and Pearson's R correlation matrix
   ggpairs(clean.df)

# Step 5: Regression model creation
  # Step 5a: Linear regression
  # Step 5b: Multiple regession
  # Step 5c: Non-linear regression

# Step 6: Model goodness of fit (GOF) analysis
  # Step 6a: create GOF functions
    # 1:1 plot
    one.to.one <- function(pred, obs) {
      qplot(obs, pred, data = df, geom = 'point', color = Field) +
      geom_abline(slope = 1) +
      ggtitle("1:1 Plot of Observed and Model Predicted Values") +
      xlab(expression(Observed~N~mg~kg^{-1})) +
      ylab(expression(Predicted~N~mg~kg^{-1})) +
      theme(plot.title = element_text(hjust = 0.5))
    }
    # RMSE - not needed, as caret includes this fxn
    # repeated k-folds cross validation RMSE fxn
    kfold <- function(df, 
                      best.model,
                      model.method="lm",
                      folds= 10,
                      repeats=1000) {
     # Set seed for reproducibility
       set.seed(123)
     # Set up repeated k-fold cross validation paremeters
       train.control = trainControl(method = "repeatedcv",
                                    number = folds,
                                    repeats = repeats)
     # Pass CV parameters to train function in caret
       model = train(best.model$formula,
                     data = df,
                     method = model.method,
                     trControl = train.control)
     # Summarize and print the results
       print(model)
    }
  # Step 6b: GOF analysis for linear regression
  # Step 6c: GOF analysis for multiple regression
  # Step 6d: GOF analysis for non-linear regression
# Step 7: Model selection
  # Step 7a: Faceted 1:1 plots
  # Step 7b: RMSE and CV-RMSE table

# Step 8: use final model to interpolate N status across the study area

# Step 9: export final interpolated data
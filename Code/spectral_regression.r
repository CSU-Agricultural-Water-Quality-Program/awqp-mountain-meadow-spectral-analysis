# Using regression to predict plan and soil N status in mountain meadow hay
# systems using spectral data derived from drone imagery.

# Created by A.J. Brown
# Date created: 5 July 2023
# Date last modified: 6 July 2023

#RStudio test commit

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
 # Step 2a: Import calibration data from .csv file
   cal.file.path <- "./Example Data/Calibration Data.csv"
   cal.data <- read.csv(cal.file.path,
                  header = TRUE,
                  sep = ",",
                  na.strings = c("N/A", " ", "")
                  )
 # Step 2b: Import interpolation data from .csv file
   int.file.path <- "./Example Data/Interpolation Data.csv"
   int.data <- read.csv(int.file.path,
                       header = TRUE,
                       sep = ",",
                       na.strings = c("N/A", " ", "")
                       )
# Step 3: Data cleaning (if needed)
  colnames(cal.data)
  # Step 3a: Remove columns not needed for analysis
    clean.df <- cal.data %>%
      select(c(ID, NDVI, NDRE, NO3_D1_ppm))
    colnames(clean.df)
# Step 4: Data exploration via ggplot2 and other methods
 # Scatterplot and Pearson's R correlation matrix
   ggpairs(clean.df)

   
   
# Step 5: Regression model creation
  # Set dependent variable for prediction (uncomment for each model)
    clean.df$N <- clean.df$NO3_D1_ppm # soil NO3, ppm
    #clean.df$N <- clean.df$N_kg.ha.1 # plant N, kg/ha
    #clean.df$N <- clean.df$kg.ha.1 # plant biomass, g/ft^2?
  # Step 5a: Linear regression
    lm.mdl <- lm(N~NDRE, data=clean.df)
    summary(lm.mdl)
    # Confidence Intervals
    confint(lm.mdl, level = 0.95)
    # Diagnostic plots
    par(mfrow=c(2,2))
    plot(lm.mdl)
    # Visualize model fit
    ggplot(data=clean.df, aes(NDRE, N)) +
      geom_point() +
      geom_smooth(method='lm') +
      ggtitle("Linear Regression Model") +
      xlab(expression(NDRE)) +
      ylab(expression(N~mg~kg^{-1})) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()

  # Step 5b: Multiple regession
    mult.mdl <- lm(N~NDRE*NDVI, data=clean.df)
    summary(mult.mdl)
    # Confidence Intervals
    confint(mult.mdl, level = 0.95)
    # Diagnostic plots
    par(mfrow=c(2,2))
    plot(mult.mdl)
  # Step 5c: Non-linear regression

# Step 6: Model goodness of fit (GOF) analysis
  # Step 6a: create GOF functions
    # 1:1 plot
    oneToOne <- function(pred, obs, data) {
      ggplot(data = data, aes(pred, obs)) +
      geom_point() +
      geom_abline(slope = 1) +
      ggtitle("1:1 Plot of Observed and Model Predicted Values") +
      xlab(expression(Observed~N~mg~kg^{-1})) +
      ylab(expression(Predicted~N~mg~kg^{-1})) +
      theme(plot.title = element_text(hjust = 0.5))
    }
    # RMSE - not needed, as caret includes this fxn
    # repeated k-folds cross validation RMSE fxn
    kFold <- function(df,
                      best.model,
                      model.method="lm",
                      folds = 10,
                      repeats = 1000) {
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
                      trControl = train.control
                      )
        # Summarize and print the results
          print(model)
    }
  # Step 6b: GOF analysis for linear regression
    clean.df$lm.pred <- predict(lm.mdl)
    oneToOne(clean.df$lm.pred, clean.df$N, data = clean.df)
    lm.rmse <- RMSE(clean.df$lm.pred, clean.df$N)
    print(paste("RMSE (mg/kg):", lm.rmse))
    kFold(clean.df, lm.mdl)
  # Step 6c: GOF analysis for multiple regression
    clean.df$mult.pred <- predict(mult.mdl)
    oneToOne(clean.df$mult.pred, clean.df$N, data = clean.df)
    mult.rmse <- RMSE(clean.df$mult.pred, clean.df$N)
    print(paste("RMSE (mg/kg):", mult.rmse))
  # Step 6d: GOF analysis for non-linear regression
# Step 7: Model selection
  # Step 7a: Faceted 1:1 plots
  # Step 7b: RMSE and CV-RMSE table

# Step 8: use final model to interpolate N status across the study area

# Step 9: export final interpolated data
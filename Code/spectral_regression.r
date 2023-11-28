# Using regression to predict plan and soil N status in mountain meadow hay
# systems using spectral data derived from drone imagery.

# Created by A.J. Brown
# Date created: 5 July 2023
# Date last modified: 6 July 2023

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
                  "caret",
                  "patchwork"
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
   cal.file.path <- "./Yampa 2023 Data/Final Sample points with NDVI and NDRE.csv"
   cal.data <- read.csv(cal.file.path,
                  header = TRUE,
                  sep = ",",
                  na.strings = c("N/A", " ", "")
                  )
 # Step 2b: Import interpolation data from .csv file
   int.file.path <- "./Yampa 2023 Data/ndvi_ndre_utm_5m.csv"
   int.data <- read.csv(int.file.path,
                       header = TRUE,
                       sep = ",",
                       na.strings = c("N/A", " ", "")
                       )
# Step 3: Data cleaning (if needed)
  clean.df <- cal.data %>%
    mutate(N_lbs = N.Content..mg./453592.37,
           N_lbs_ac = N_lbs*43560,
           N_perc = N.Content....*100
           ) %>%
    select(Date, NDVI, NDRE, Red, Green, Red.Edge, NIR, NO3_D1_ppm, N_lbs, N_lbs_ac, N_perc
           ) %>%
    filter(Date == '7/10/2023')

# Step 4: Data exploration via ggplot2 and other methods\
  # Step 4a: Remove columns not needed for analysis
  sub.df <- clean.df %>%
    filter(Date == '7/10/2023') %>%
    #select(c(Red, Green, Red.Edge, NIR, N.Content....))
    select(c(NDVI, NDRE, N_perc))
  colnames(sub.df)
 # Scatterplot and Pearson's R correlation matrix
   ggpairs(sub.df)

   
   
# Step 5: Regression model creation
  # Set dependent variable for prediction (uncomment for each model)
    clean.df$N <- clean.df$N_perc # Plant N (mg/mg)
    #clean.df$N <- clean.df$N_kg.ha.1 # plant N, kg/ha
    #clean.df$N <- clean.df$kg.ha.1 # plant biomass, g/ft^2?
  # Step 5a: Linear regression
    lm.mdl <- lm(N~NDVI, data=clean.df)
    summary(lm.mdl)
    # Confidence Intervals
    confint(lm.mdl, level = 0.95)
    # Diagnostic plots
    par(mfrow=c(2,2))
    plot(lm.mdl)
    # Visualize model fit
    ggplot(data=clean.df, aes(NDVI, N)) +
      geom_point() +
      geom_smooth(method='lm') +
      ggtitle("Linear Regression Model") +
      xlab(expression(NDVI)) +
      ylab("N (mg/mg)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()
    
    # Step 5a: Linear regression
    lm.mdl.noint <- lm(N~NDVI-1, data=clean.df)
    summary(lm.mdl.noint)
    # Confidence Intervals
    confint(lm.mdl.noint, level = 0.95)
    # Diagnostic plots
    par(mfrow=c(2,2))
    plot(lm.mdl.noint)
    # Visualize model fit
    # ask chatgpt to make this a no-intercept visualization
    ggplot(data=clean.df, aes(NDVI, N)) +
      geom_point() +
      geom_smooth(method='lm', formula = y ~ x - 1) +
      ggtitle("Linear Regression Model (No Intercept)") +
      xlab(expression(NDVI)) +
      ylab("N (mg/mg)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()

  # Step 5b: Multiple regession
    # We are gonna do the stepwise mult regression here
    clean.df$CIRE <- (clean.df$NIR/clean.df$Red.Edge) - 1
    mult.mdl <- lm(N~CIRE-1, data=clean.df)
    summary(mult.mdl)
    # Confidence Intervals
    confint(mult.mdl, level = 0.95)
    # Diagnostic plots
    par(mfrow=c(2,2))
    plot(mult.mdl)
  # Step 5c: Non-linear regression
    
    
# notes on getting eq. coefficients
    coef(lm.mdl)
    coef(lm.mdl.noint)
    coef(mult.mdl)
    
    # these give you the Beta coefficients for each input variable
    # N% = B0 + B1*NDVI
    # N% = B1*NDVI
    # N% = B0 + B1*NDVI + B2*Green + B3*NDVI*Green

# Step 6: Model goodness of fit (GOF) analysis
  # Step 6a: create GOF functions
    # 1:1 plot
    # make this look better, Jake
    # I tried
    oneToOne <- function(pred, obs, data) {
      ggplot(data = data, aes(pred, obs)) +
      geom_point() +
      geom_abline(slope = 1, linetype = 'dashed') +
      ggtitle("1:1 Plot of Observed and Model Predicted Values") +
      xlab(expression(Observed~N~mg~kg^{-1})) +
      ylab(expression(Predicted~N~mg~kg^{-1})) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed(ratio = 0.75)
    }
    
  # Step 6b: GOF analysis for linear regression
    clean.df$lm.pred <- predict(lm.mdl)
    a <- oneToOne(clean.df$lm.pred, clean.df$N, data = clean.df)
    lm.rmse <- RMSE(clean.df$lm.pred, clean.df$N)
    print(paste("RMSE (%):", lm.rmse))

  # Step 6c: GOF analysis for multiple regression
    clean.df$mult.pred <- predict(mult.mdl)
    b <- oneToOne(clean.df$mult.pred, clean.df$N, data = clean.df)
    mult.rmse <- RMSE(clean.df$mult.pred, clean.df$N)
    print(paste("RMSE (%):", mult.rmse))
    
    a + b 

  # Step 7: Model selection
  # Step 7a: Faceted 1:1 plots
  # Step 7b: RMSE and CV-RMSE table

# Step 8: use final model to interpolate N status across the study area

# Step 9: export final interpolated data
    
    
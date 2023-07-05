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

# Step 1: Import libraries
package.list <- c("dplyr",
                  "ggplot2"
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
                  na.strings = c("NA", " ", "")
                  )

# Step 3: Data cleaning
# Step 4: Data exploration via ggplot2 and other methods
# Step 5: Regression model creation
# Step 6: Model goodness of fit analysis
# Step 7: Model selection, final prediction, and data export
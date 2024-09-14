# ------------------------------------------------------------------------
# Script name: general/functions/regression_model_utils.R
# Purpose of script: Utility functions for building and summarising 
#                    linear models for gender gap analysis
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------
#
# Notes:
#   This script contains functions for building linear models with 
#   optional control variables and running regression analyses.
# ------------------------------------------------------------------------

# Function to build a linear model with optional control variables
build_lm <- function(data, predictor, dv, controls = NULL, weights = NULL) {
  # If control variables are provided, construct the formula accordingly
  if (!is.null(controls)) {
    controls <- controls[!is.na(controls)]  # Remove any NA controls
    controls <- paste(controls, collapse = ' + ')  # Combine control vars
    predictors <- paste(predictor, controls, sep = ' + ')  # Combine predictors
  } else {
    predictors <- predictor  # If no controls, just use the predictor
  }
  
  # Create the formula for the linear model
  formula <- paste(dv, predictors, sep = ' ~ ')
  
  # Run the linear model with or without weights
  if (!is.null(weights)) {
    model <- lm(as.formula(formula), data = data, weights = weights)
  } else {
    model <- lm(as.formula(formula), data = data)
  }
  
  return(model)
}

# Function to run a regression analysis for a given outcome variable
run_regression <- function(data, outcome_var, controls = NULL, weights = NULL) {
  tryCatch({
    # Build the linear model using the build_lm function
    model <- build_lm(
      data = data,
      predictor = 'WP1219',  # Gender predictor
      dv = outcome_var,  # Dependent variable
      controls = controls,  # Control variables (optional)
      weights = weights  # Weights (optional)
    )
    
    # Generate tidy model results using broom
    result <- tidy(model)
    
    return(result)
  }, error = function(e) {
    # Handle errors gracefully
    cat('\n', paste('Skipping', unique(data$WP5), 'due to error:', e$message))
    
    # Return an empty tibble instead of NULL to ensure consistent output
    return(tibble(
      term = NA,
      estimate = NA,
      std.error = NA,
      statistic = NA,
      p.value = NA
    ))
  })
}

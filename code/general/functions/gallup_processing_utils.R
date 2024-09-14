# ------------------------------------------------------------------------
# Script name: general/functions/data_processing_utils.R
# Purpose: Functions for data preprocessing and cleaning for analysis
# Author: Naomi Muggleton
# Date created: 2024-07-30
# Date last modified: 2024-09-13
# ------------------------------------------------------------------------
#
# Notes: 
#   This script contains functions to load and preprocess the Gallup World 
#   Poll data, converting specific columns to appropriate formats, and 
#   adding new calculated columns (e.g. reverse scoring for negative variables).
# ------------------------------------------------------------------------

# Load necessary libraries
library(data.table)
library(tidyverse)
library(wbstats)

# Function to load and preprocess Gallup World Poll data
load_and_preprocess_data <- function() {
  
  # Import total populations from World Bank for weighting regressions
  pop_data <- wb_data('SP.POP.TOTL', start_date = 2006, end_date = 2022) %>% 
    select(COUNTRY_ISO2 = iso2c, population = SP.POP.TOTL, YEAR_WAVE = date) %>% 
    mutate(YEAR_WAVE = as.factor(YEAR_WAVE))
  
  # Define columns to import from Gallup dataset
  cols_i_want <- c(
    'WP5', 'wpid', 'wgt', 'EMP_2010', 'INCOME_2', 'REG2_GLOBAL', 'WP16', 
    'WP23', 'WP60', 'WP63', 'WP67', 'WP68', 'WP69', 'WP70', 'WP71', 'WP74', 
    'WP1219', 'WP1220', 'WP1230', 'WP3117', 'WP1223', 'YEAR_WAVE',
    'COUNTRY_ISO2'
  )
  
  # Specify data types for each column
  factor_cols <- c(
    'WP5', 'EMP_2010', 'REG2_GLOBAL', 'WP16', 'WP23', 'WP60', 'WP63', 'WP67',
    'WP68', 'WP69', 'WP70', 'WP71', 'WP74', 'WP1219', 'WP3117', 'WP1223', 
    'YEAR_WAVE'
    )
  
  character_cols <- c('wpid', 'WP1220', 'WP1230')
  numeric_cols <- c('wgt', 'INCOME_2')
  
  # Import Gallup data and apply data types
  gwp <- fread(
    '../data/gallup/data/gwp_2023.csv',
    select = cols_i_want,
    na.strings = c('', '(DK)', '(Refused)', '(RF)'), 
    colClasses = list(
      factor = factor_cols,
      character = character_cols,
      numeric = numeric_cols
    )
  )
  
  # Convert 'Yes'/'No' responses to dummy variables
  yes_no_cols <- c(
    'WP23', 'WP60', 'WP63', 'WP67', 'WP68', 'WP69', 'WP70', 'WP71', 'WP74'
    )
  
  gwp[
    ,
    (yes_no_cols) := lapply(.SD, function(x) as.numeric(x == 'Yes')),
    .SDcols = yes_no_cols
  ]
  
  # Convert Cantril ladder scale to numeric
  cantril <- c('WP16')
  gwp[
    ,
    (cantril) := lapply(.SD, function(x) {
      x <- gsub('Best possible', '10', x)
      x <- gsub('Worst possible', '0', x)
      as.numeric(x)
    }),
    .SDcols = cantril
  ]
  
  # Ensure gender is treated as a factor with levels 'Male' and 'Female'
  gwp[, WP1219 := factor(WP1219, levels = c('Male', 'Female'))]
  
  # Ensure age is treated as numeric and handle the '99+' value
  gwp[WP1220 == '99+', WP1220 := '99']
  gwp[, WP1220 := as.numeric(WP1220)]
  
  # Convert 'Children age <15 present' to a dummy variable
  gwp[, WP1230 := str_replace_all(WP1230, '[0-9+]+', '1+') %>% 
        factor(levels = c('None', '1+'))]
  
  # Add additional variables: Cantril score, positive affect, negative affect,
  # and pain
  gwp[, cantril := WP16]
  gwp[, positive_affect := (WP60 + WP63 + WP67)]
  gwp[, negative_affect := (WP69 + WP70 + WP71 + WP74) * -1]  # Reverse score
  gwp[, pain := (WP23 + WP68) * -1]  # Reverse score
  gwp[, age_sq := WP1220 ^ 2]
  
  # Merge with population data for weighting
  gwp <- merge(gwp, pop_data, by = c('COUNTRY_ISO2', 'YEAR_WAVE'))
  gwp[, our_sample := .N, by = .(WP5, YEAR_WAVE)]
  gwp[, weight := population / our_sample]
  
  # Drop unnecessary columns after processing
  drop <- c(
    'WP16', 'WP23', 'WP60', 'WP63', 'WP67', 'WP68', 'WP69', 'WP70',  'WP71',
    'WP74', 'population', 'our_sample', 'COUNTRY_ISO2'
    )
  
  gwp[, (drop) := NULL]
  
  # Return processed data
  gwp <<- as_tibble(gwp)
}

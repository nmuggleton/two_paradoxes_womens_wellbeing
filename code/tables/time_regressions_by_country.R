# ------------------------------------------------------------------------
# Script name: tables/time_regressions_by_country.R
# Purpose of script: Conduct gender gap analysis across time using linear 
# models.
# Author: Naomi Muggleton
# Date created: 2024-09-08
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------
# 
# Outcome Variables:
#   1. Cantril Scale
#     - Please imagine a ladder, with steps numbered from 0 at the bottom to 10
#       at the top. The top of the ladder represents the best possible life for
#       you and the bottom of the ladder represents the worst possible life for
#       you. On which step of the ladder would you say you personally feel you
#       stand at this time? (WP16)
#   2. Positive Affect
#     - Did you feel well-rested yesterday? (WP60)
#     - Did you smile or laugh a lot yesterday? (WP63)
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about enjoyment? (WP67)
#   3. Negative Affect
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about worry? (WP69)
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about sadness? (WP70)
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about stress? (WP71)
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about anger? (WP74)
#   4. Physical Pain
#     - Did you experience physical pain yesterday? (WP68)
# 
# ------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source utility functions ------------------------------------------------

source('general/functions/gallup_processing_utils.R')
source('tables/functions/country_regression_utils.R')

# Define model variables --------------------------------------------------

# Outcome variables
outcome_vars <- c('cantril', 'positive_affect', 'negative_affect', 'pain')

# Run analysis ------------------------------------------------------------

# Load data
load_and_preprocess_data()

# Generate regression tables for all outcome variables
run_regression_by_country()

# ------------------------------------------------------------------------
# Script name: plots/countries_across_time.R
# Purpose of script: Analyse gender gap trends across time using linear 
# models for life evaluations, positive/negative affect, and pain.
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-09-08
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
#     - Did you experience the following feelings during a lot of the day
#       yesterday? How about physical pain? (WP68)
#
# Models:
#   Model 1: Gender (WP1219) and year as predictor.
#   Model 2: Gender (WP1219) and year as predictor, with controls for age
#     (WP1220).
#   Model 3: Gender (WP1219) as predictor, with controls for age (WP1220),
#     age squared (derived), Child age <15 present (WP1230), employment status
#     (EMP_2010), education (WP3117), income (INCOME_2), and marital status
#     (WP1223).
# 
# ------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source utility functions ------------------------------------------------

source('general/functions/gallup_processing_utils.R')
source('plots/functions/time_plot_utils.R')
source('plots/functions/model_parameters.R')

# Define model variables --------------------------------------------------

# Control variables for different model types
m1_vars <- c('YEAR_WAVE')
m2_vars <- c(m1_vars, 'WP1220')
m3_vars <- c(m2_vars, 'WP1230', 'EMP_2010', 'WP3117', 'INCOME_2', 'WP1223')

# Outcome variables
outcome_vars <- c('cantril', 'positive_affect', 'negative_affect', 'pain')

# Run analysis ------------------------------------------------------------

# Load data
load_and_preprocess_data()

# Generate time trend plots
create_time_plots()

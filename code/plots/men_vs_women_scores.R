# ------------------------------------------------------------------------
# Script name: plots/men_vs_women_scores.R
# Purpose of script: Plot and analyse gender differences in well-being 
# scores (life evaluations, positive/negative affect, pain) across 
# countries and world regions.
# Author: Naomi Muggleton
# Date created: 2024-09-07
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------
# 
# Outcome Variables:
#   1. Life Satisfaction (Cantril Ladder):
#     - WP16: Ladder of life from 0 (worst) to 10 (best).
#   2. Positive Affect:
#     - WP60: Well-rested yesterday.
#     - WP63: Smiled/laughed a lot yesterday.
#     - WP67: Experienced enjoyment yesterday.
#   3. Negative Affect:
#     - WP69: Experienced worry yesterday.
#     - WP70: Experienced sadness yesterday.
#     - WP71: Experienced stress yesterday.
#     - WP74: Experienced anger yesterday.
#   4. Physical Pain:
#     - WP68: Experienced physical pain yesterday.
# 
# ------------------------------------------------------------------------

# Clean the workspace
rm(list = ls())

# Source utility functions ------------------------------------------------

source('general/functions/gallup_processing_utils.R')
source('plots/functions/men_women_plot_utils.R')
source('plots/functions/men_women_plot_utils.R')

# Define model variables --------------------------------------------------

# Outcome variables
outcome_vars <- c('cantril', 'positive_affect', 'negative_affect', 'pain')

# Run analysis ------------------------------------------------------------

# Load data
load_and_preprocess_data()

# Generate men and women plots
plot_men_vs_women_scores()

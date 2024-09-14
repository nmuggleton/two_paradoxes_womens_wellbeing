# ------------------------------------------------------------------------
# Script name: tables/correlation_matrix.R
# Purpose of script: Generate correlation matrices for key variables 
#                    (life satisfaction, positive/negative affect, pain) 
#                    across genders.
# Author: Naomi Muggleton
# Date created: 2024-09-10
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load necessary libraries
library(knitr)
library(dplyr)
library(kableExtra)
library(stringr)

# Source utility functions ------------------------------------------------

source('general/functions/gallup_processing_utils.R')

# Load and preprocess data ------------------------------------------------

load_and_preprocess_data()

# Function to calculate the lower triangular correlation matrix ------------

calculate_lower_correlation <- function(df) {
  numeric_df <- df %>% select(where(is.numeric))
  
  if (ncol(numeric_df) > 1) {
    corr_matrix <- cor(numeric_df, use = 'pairwise.complete.obs')
    corr_matrix[upper.tri(corr_matrix)] <- NA
    corr_matrix <- round(corr_matrix, 3)
    corr_matrix <- apply(
      corr_matrix, 2, function(x) ifelse(is.na(x), "", x)
    )
    return(corr_matrix)
  } else {
    return(matrix("", nrow = ncol(numeric_df), ncol = ncol(numeric_df)))
  }
}

# Group data by WP1219 (gender) and calculate correlation matrix ----------

correlation_matrices <- gwp %>%
  select(
    'WP1219',
    `Life satisfaction` = 'cantril',
    `Positive affect` = 'positive_affect',
    `Negative affect` = 'negative_affect',
    `Pain` = 'pain'
  ) %>%
  group_by(WP1219) %>%
  summarise(
    correlation_matrix = list(
      calculate_lower_correlation(pick(where(is.numeric)))
    )
  )

# Get sample sizes for men and women
sample_sizes <- gwp %>%
  group_by(WP1219) %>%
  summarise(N = n())

n_female <- sample_sizes %>%
  filter(WP1219 == 'Female') %>%
  pull(N)

n_male <- sample_sizes %>%
  filter(WP1219 == 'Male') %>%
  pull(N)

# Extract correlation matrices for men and women -------------------------

female_correlation <- correlation_matrices %>%
  filter(WP1219 == 'Female') %>%
  pull(correlation_matrix) %>%
  .[[1]]

male_correlation <- correlation_matrices %>%
  filter(WP1219 == 'Male') %>%
  pull(correlation_matrix) %>%
  .[[1]]

# Ensure both correlation matrices have the same dimensions --------------

max_rows <- max(nrow(male_correlation), nrow(female_correlation))
max_cols <- max(ncol(male_correlation), ncol(female_correlation))

# Pad matrices to make them the same size
male_correlation <- rbind(
  male_correlation, 
  matrix("", nrow = max_rows - nrow(male_correlation), ncol = ncol(male_correlation))
)

female_correlation <- rbind(
  female_correlation, 
  matrix("", nrow = max_rows - nrow(female_correlation), ncol = ncol(female_correlation))
)

# Combine both correlation tables into one for the LaTeX output -----------
combined_table <- rbind(
  male_correlation,  # Men
  matrix('', nrow = 1, ncol = max_cols),  # Spacer row
  female_correlation  # Women
)

# Create the LaTeX table -------------------------------------------------
kable(
  combined_table,
  format = 'latex',
  booktabs = T,
  align = 'c',
  col.names = c('Life satisfaction', 'Positive affect', 
                'Negative affect', 'Pain', rep('', 4)),
  caption = paste(
    "Panel A: \\textit{Men (N =", n_male, ")} Panel B: \\textit{Women (N =", n_female, ")}"
  )
) %>%
  add_header_above(c(" " = 4, " " = 4)) %>%
  pack_rows("Panel A: \\textit{Men}", 1, max_rows) %>%
  pack_rows(
    "Panel B: \\textit{Women}", (max_rows + 2), max_rows * 2 + 1
  ) %>%
  column_spec(5:8, width = '1cm')

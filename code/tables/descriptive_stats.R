# ------------------------------------------------------------------------
# Script name: tables/descriptive_stats.R
# Purpose of script: Generate descriptive statistics for key variables 
#                    related to life evaluations, positive/negative affect, 
#                    and pain across genders and world regions.
# Author: Naomi Muggleton
# Date created: 2024-08-28
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load necessary libraries
library(stargazer)
library(dplyr)
library(stringr)
library(tools)

# Source utility functions ------------------------------------------------

source('general/functions/gallup_processing_utils.R')

# Load and preprocess data ------------------------------------------------

load_and_preprocess_data()

# Generate summary statistics ---------------------------------------------

setDT(gwp)

# Define variables for reverse coding
neg_vars <- c('pain', 'negative_affect')

# Reverse score negative variables (pain and negative affect)
gwp[, (neg_vars) := lapply(.SD, abs), .SDcols = neg_vars]

# Reshape data for generating the descriptive table
gwp[, .(WP1219, cantril, positive_affect, negative_affect, pain)] %>%
  melt(id.var = 'WP1219') %>%
  group_by(variable, WP1219) %>%
  filter(complete.cases(value)) %>%
  summarise(
    Mean = mean(value),
    SD = sd(value),
    Median = quantile(value, probs = .5),
    Min = min(value),
    Max = max(value),
    N = n()
  ) %>%
  mutate(variable = str_to_sentence(variable)) %>%
  mutate(variable = str_replace(variable, '_', ' ')) %>%
  mutate(
    variable = ifelse(
      WP1219 == lag(WP1219, default = first(WP1219)), variable, ''
    )
  ) %>%
  rename_with(toTitleCase) %>%
  rename(Gender = WP1219) %>%
  as.data.frame() %>%
  stargazer(summary = F, rownames = F, align = T)

# Reshape data for generating the descriptive table by region
gwp[, .(WP1219, REG2_GLOBAL, cantril, positive_affect, negative_affect, pain)] %>%
  melt(id.vars = c('WP1219', 'REG2_GLOBAL')) %>%
  group_by(variable, REG2_GLOBAL, WP1219) %>%
  filter(complete.cases(value)) %>%
  summarise(
    Mean = mean(value),
    SD = sd(value),
    Median = quantile(value, probs = .5),
    Min = min(value),
    Max = max(value),
    N = n()
  ) %>%
  mutate(variable = str_to_sentence(variable)) %>%
  mutate(variable = str_replace(variable, '_', ' ')) %>%
  mutate(
    variable = ifelse(
      WP1219 == lag(WP1219, default = first(WP1219)), variable, ''
    )
  ) %>%
  rename_with(toTitleCase) %>%
  rename(Gender = WP1219) %>%
  rename(Region = REG2_GLOBAL) %>%
  as.data.frame() %>%
  stargazer(summary = F, rownames = F, align = T)

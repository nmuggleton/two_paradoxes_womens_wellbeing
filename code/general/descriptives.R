# ------------------------------------------------------------------------
# Script name: general/descriptives.R
# Purpose: Generate descriptive statistics used in the main text of 
#          the manuscript.
# Author: Naomi Muggleton
# Date created: 2024-09-06
# Last modified: 2024-09-14
# ------------------------------------------------------------------------

# Clean the workspace
rm(list = ls())

# Load necessary libraries
library(data.table)
library(tidyverse)
library(wbstats)
library(broom)
library(purrr)

# Source utility functions
source('general/functions/data_processing_utils.R')

# Section 2.4: Illustration of the first paradox - Data and methods -------

# 1. Number of unique countries
cols <- c('WP5', 'REG2_GLOBAL', 'YEAR_WAVE', 'COUNTRY_ISO2')

# Import GWP data
countries <- fread(
  '../data/gallup/data/gwp_2023.csv', 
  select = cols, 
  colClasses = list(factor = cols)
)

# Get unique countries
unique_countries <- unique(countries[complete.cases(countries)]$WP5)

# 2. Range of years
years_range <- levels(countries$YEAR_WAVE)

# 3. Percent of regions covered

# Import total populations from World Bank for weighting regressions
pop <- wb_data('SP.POP.TOTL', start_date = 2006, end_date = 2022) %>%
  select(COUNTRY_ISO2 = iso2c, population = SP.POP.TOTL, YEAR_WAVE = date) %>%
  mutate(YEAR_WAVE = as.factor(YEAR_WAVE))

# Combine data and calculate coverage
countries <- merge(countries, pop, by = c('COUNTRY_ISO2', 'YEAR_WAVE'), all = T)
countries <- unique(countries)

# Calculate global population and coverage
countries[, global_population := sum(population, na.rm = T), by = YEAR_WAVE]
countries[
  complete.cases(WP5),
  coverage := sum(population, na.rm = T),
  by = YEAR_WAVE
  ]

# Coverage data by year
cover <- countries[, .(YEAR_WAVE, global_population, coverage)] %>%
  unique() %>%
  drop_na()

# Range of global population coverage
population_coverage_range <- range(
  cover[, rep := coverage / global_population]$rep
  ) %>%
  round(4)

# 4. Total number of respondents
load_and_preprocess_data()
total_respondents <- gwp %>%
  nrow()

# 5. Minimum and maximum respondents per wave
min_max_wave_respondents <- gwp %>%
  group_by(WP5, YEAR_WAVE) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == min(n) | n == max(n)) %>% 
  print()

# Section 2.4: Illustration of the first paradox - Results ----------------

# Helper function for safely running models
safe_lm <- possibly(function(data, formula) {
  lm(as.formula(formula), data = data)
}, otherwise = NULL)

# Apply models by country
results <- gwp %>%
  group_by(WP5) %>%
  nest() %>%
  mutate(
    # Cantril model
    cantril_model = map(
      data,
      ~ if (n_distinct(.x$WP1219) > 1 && n_distinct(.x$YEAR_WAVE) > 1)
        safe_lm(.x, 'cantril ~ WP1219 + YEAR_WAVE')
      else NULL
      ),
    # Negative affect model
    negative_affect_model = map(
      data, 
      ~ if (n_distinct(.x$WP1219) > 1 && n_distinct(.x$YEAR_WAVE) > 1)
        safe_lm(.x, 'negative_affect ~ WP1219 + YEAR_WAVE')
      else NULL
      ),
    # Tidy summaries
    cantril_tidied = map(cantril_model, ~ if (!is.null(.)) tidy(.) else NULL),
    negative_affect_tidied = map(
      negative_affect_model,
      ~ if (!is.null(.))
        tidy(.)
      else NULL
      )
    ) %>%
  pivot_longer(
    cols = c(cantril_tidied, negative_affect_tidied),
    names_to = 'model',
    values_to = 'tidy_results'
    ) %>%
  unnest(tidy_results, keep_empty = T)

# Identify countries with significant gender paradox (life eval. + neg. affect)
paradox <- results %>%
  filter(term == 'WP1219Female', p.value < .05) %>%
  filter(
    (model == 'cantril_tidied' & estimate > 0) |
      (model == 'negative_affect_tidied' & estimate < 0)
    ) %>%
  group_by(WP5) %>%
  summarise(n = n()) %>%
  filter(n == 2)

# Calculate percentage of countries with gender wellbeing paradox
gender_paradox_pct <- (paradox %>% nrow() / length(unique(results$WP5))) * 100
cat(
  'The gender wellbeing paradox can be observed in',
  gender_paradox_pct,
  '% of countries'
  )

# Percentage of global population covered by countries exhibiting the paradox
pop_coverage_paradox <- countries[
  ,
  .N,
  by = .(WP5, COUNTRY_ISO2, YEAR_WAVE)
  ] %>%
  merge(pop, all.x = T) %>%
  drop_na() %>%
  group_by(YEAR_WAVE) %>%
  mutate(total_pop = sum(population), prop_pop = population / total_pop) %>%
  ungroup() %>%
  merge(paradox, all = TRUE) %>%
  mutate(paradox = ifelse(is.na(n), 'no', 'yes')) %>%
  group_by(YEAR_WAVE, paradox) %>%
  summarise(sum(prop_pop) * 100)

# Adding covariates and rerunning models
results_with_covariates <- gwp %>%
  group_by(WP5) %>%
  nest() %>%
  mutate(
    cantril_model = map(
      data,
      ~ if (n_distinct(.x$WP1219) > 1 && n_distinct(.x$YEAR_WAVE) > 1)
        safe_lm(
          .x,
          'cantril ~ WP1219 + YEAR_WAVE + EMP_2010 + INCOME_2 + WP1220 + WP1230 + WP3117 + WP1223 + age_sq'
          )
      else NULL
      ),
    negative_affect_model = map(
      data, ~ if (
        n_distinct(.x$WP1219) > 1 && n_distinct(.x$YEAR_WAVE) > 1)
        safe_lm(
          .x,
          'negative_affect ~ WP1219 + YEAR_WAVE + EMP_2010 + INCOME_2 + WP1220 + WP1230 + WP3117 + WP1223 + age_sq'
          )
      else NULL
      ),
    cantril_tidied = map(cantril_model, ~ if (!is.null(.)) tidy(.) else NULL),
    negative_affect_tidied = map(
      negative_affect_model,
      ~ if (
        !is.null(.))
        tidy(.)
      else NULL
      )
  ) %>%
  pivot_longer(
    cols = c(cantril_tidied, negative_affect_tidied),
    names_to = 'model',
    values_to = 'tidy_results'
    ) %>%
  unnest(tidy_results, keep_empty = TRUE)

# Identify countries with significant gender paradox (life eval. + neg. affect)
paradox_cov <- results_with_covariates %>%
  filter(term == 'WP1219Female', p.value < .05) %>%
  filter(
    (model == 'cantril_tidied' & estimate > 0) |
      (model == 'negative_affect_tidied' & estimate < 0)
  ) %>%
  group_by(WP5) %>%
  summarise(n = n()) %>%
  filter(n == 2)

# Calculate percentage of countries with gender wellbeing paradox
paradox_cov_pct <- (paradox_cov %>% nrow() / length(unique(results$WP5))) * 100

cat(
  'The gender wellbeing paradox can be observed in',
  paradox_cov_pct,
  '% of countries'
)

# Percentage of global population covered by countries exhibiting the paradox
pop_coverage_paradox_cov <- countries[
  ,
  .N,
  by = .(WP5, COUNTRY_ISO2, YEAR_WAVE)
] %>%
  merge(pop, all.x = T) %>%
  drop_na() %>%
  group_by(YEAR_WAVE) %>%
  mutate(total_pop = sum(population), prop_pop = population / total_pop) %>%
  ungroup() %>%
  merge(paradox_cov, all = TRUE) %>%
  mutate(paradox = ifelse(is.na(n), 'no', 'yes')) %>%
  group_by(YEAR_WAVE, paradox) %>%
  summarise(sum(prop_pop) * 100)

# Calculating variance across countries and regions -----------------------

# Gender differences by country
gender_diff_country <- gwp %>%
  group_by(WP5, WP1219) %>%
  summarise(
    avg_life_eval = mean(cantril, na.rm = TRUE),
    avg_neg_affect = mean(negative_affect, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  drop_na() %>%
  pivot_wider(
    names_from = WP1219,
    values_from = c(avg_life_eval, avg_neg_affect)
    ) %>%
  mutate(
    gender_diff_life_eval = avg_life_eval_Female - avg_life_eval_Male,
    gender_diff_neg_affect = avg_neg_affect_Female - avg_neg_affect_Male
  )

# Calculate variance across countries
var_life_eval_countries <- var(
  gender_diff_country$gender_diff_life_eval,
  na.rm = T
  )

var_neg_affect_countries <- var(
  gender_diff_country$gender_diff_neg_affect,
  na.rm = T
  )

# Gender differences by region
gender_diff_region <- gwp %>%
  group_by(REG2_GLOBAL, WP1219) %>%
  summarise(
    avg_life_eval = mean(cantril, na.rm = TRUE),
    avg_neg_affect = mean(negative_affect, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = WP1219,
    values_from = c(avg_life_eval, avg_neg_affect)
    ) %>%
  mutate(
    gender_diff_life_eval = avg_life_eval_Female - avg_life_eval_Male,
    gender_diff_neg_affect = avg_neg_affect_Female - avg_neg_affect_Male
  )

# Calculate variance across regions
var_life_eval_regions <- var(
  gender_diff_region$gender_diff_life_eval,
  na.rm = T
  )

var_neg_affect_regions <- var(
  gender_diff_region$gender_diff_neg_affect,
  na.rm = T
  )

# Output variance results
cat(
  'Variance in gender differentials for life evaluations across countries:',
  var_life_eval_countries, '\n'
)

cat(
  'Variance in gender differentials for negative affect across countries:',
  var_neg_affect_countries, '\n'
)

cat(
  'Variance in gender differentials for life evaluations across regions:',
  var_life_eval_regions, '\n'
)

cat(
  'Variance in gender differentials for negative affect across regions:',
  var_neg_affect_regions, '\n'
)

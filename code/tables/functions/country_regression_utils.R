# ------------------------------------------------------------------------
# Script name: tables/functions/country_regression_utils.R
# Purpose of script: Utility functions to run regression models for gender 
# gaps over time and generate LaTeX tables.
# Author: Naomi Muggleton
# Date created: 2024-09-08
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------
# 
# Notes:
#   This script contains the function `run_regression_by_country()` that runs 
#   linear models for the specified outcome variables by country and generates
#   LaTeX tables.
# ------------------------------------------------------------------------

library(xtable)

# Generate tables for each outcome variable --------------------------------
run_regression_by_country <- function() {
  
  # Define outcome variables
  outcome_vars <- c('cantril', 'positive_affect', 'negative_affect', 'pain')
  
  # Function to run the model and generate tables for each outcome variable
  lapply(outcome_vars, function(outcome_var) {
    
    # Create necessary columns
    gwp$year <- gwp$YEAR_WAVE %>% as.character() %>% as.numeric()
    gwp$income <- gwp$INCOME_2 / 1000
    gwp$year <- (gwp$year - 2006)
    
    # Adjust predictors based on the outcome variable
    model_formula <- as.formula(paste(outcome_var, '~ WP1219 * year'))
    
    # Filter out groups where WP1219 (or any other variable) has fewer than two 
    # unique levels
    results <- gwp %>%
      group_by(WP5) %>%
      filter(n_distinct(WP1219) > 1 & n_distinct(year) > 1) %>%
      nest() %>%
      mutate(
        model = map(data, ~ lm(model_formula, data = .x)),
        summary = map(model, broom::tidy, conf.int = T)
      ) %>%
      unnest(summary)
    
    # Filter the results to focus on the interaction term of interest
    results_filtered <- results %>%
      filter(term == 'WP1219Female:year') %>%
      select(
        Country = WP5,
        Estimate = estimate,
        SE = std.error,
        t = statistic,
        `p-value` = p.value,
        `Lower CI` = conf.low,
        `Upper CI` = conf.high
      ) %>%
      drop_na()
    
    # Prepare LaTeX table
    table <- xtable(
      results_filtered,
      digits = 10,
      caption = paste('Time trends -- gender x year interaction for', outcome_var),
      label = paste('table:time_trends', outcome_var, sep = '_'),
      align = c(
        'l', 'l',
        'S[round-mode=figures,round-precision=2]',
        'S[round-mode=figures,round-precision=2]',
        'S[round-mode=places,round-precision=2]',
        'S[table-format=<1.3,round-mode=places,round-precision=3,table-space-text-post=<,round-minimum=0.001]',
        'S[round-mode=figures,round-precision=2]',
        'S[round-mode=figures,round-precision=2]'
      )
    )
    
    # Output the table
    print(
      table,
      include.rownames = F,
      tabular.environment = 'longtable',
      floating = F,
      size = 'footnotesize',
      caption.placement = 'top',
      sanitize.colnames.function = identity  # Prevent escaping of {}
    )
    
  })
}

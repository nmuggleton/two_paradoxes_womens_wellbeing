# ------------------------------------------------------------------------
# Script name: plots/functions/time_plot_utils.R
# Purpose of script: Functions for running regression analyses and
# plotting gender gap trends across time for life evaluations, 
# positive/negative affect, and pain.
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-09-08
# ------------------------------------------------------------------------
# Notes:
#   This script contains functions to run time-based regression analyses,
#   generate tidy outputs, and plot gender gap trends across time for 
#   different outcome variables.
# ------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(broom)
library(ggsci)
library(ggpubr)
library(scales)
library(gridExtra)
library(grid)
library(ggtext)
library(patchwork)

# Source external parameters and utility functions
source('general/functions/gallup_processing_utils.R')
source('plots/functions/plot_parameters.R')
source('plots/functions/model_parameters.R')

# Functions ---------------------------------------------------------------

# Function to prepare data for plotting by country and year
prepare_plot_data_per_country_year <- function(outcome_var, model_type) {
  
  controls <- switch(
    model_type,
    'm1' = NULL,
    'm2' = setdiff(m2_vars, 'YEAR_WAVE'),
    'm3' = setdiff(m3_vars, 'YEAR_WAVE')
  )
  
  # Country-level results
  country_year_results <- gwp %>%
    group_by(WP5, YEAR_WAVE, REG2_GLOBAL) %>%  # Group by country and year
    do({
      country_year_data <- .
      result <- run_regression(country_year_data, outcome_var, controls)
      if (!is.null(result) && nrow(result) > 0) {
        result$YEAR_WAVE <- unique(country_year_data$YEAR_WAVE)
        result$WP5 <- unique(country_year_data$WP5)
        result$REG2_GLOBAL <- unique(country_year_data$REG2_GLOBAL)
        result
      } else {
        tibble()  # Return an empty tibble if no valid result
      }
    }) %>%
    ungroup() %>%
    filter(term == 'WP1219Female') %>%
    mutate(
      lowerCI = estimate - 1.96 * std.error,
      upperCI = estimate + 1.96 * std.error
    )
  
  # Regional-level results
  regional_results <- gwp %>%
    group_by(REG2_GLOBAL, YEAR_WAVE) %>%  # Group by region and year
    do(run_regression(., outcome_var, controls)) %>%
    ungroup() %>%
    filter(term == 'WP1219Female') %>%
    mutate(
      lowerCI = estimate - 1.96 * std.error,
      upperCI = estimate + 1.96 * std.error,
      WP5 = 'Regional'
    )
  
  # Global-level results
  global_results <- gwp %>%
    group_by(YEAR_WAVE) %>%  # Group by year
    do(run_regression(., outcome_var, controls)) %>%
    ungroup() %>%
    filter(term == 'WP1219Female') %>%
    mutate(
      lowerCI = estimate - 1.96 * std.error,
      upperCI = estimate + 1.96 * std.error,
      WP5 = 'Global',
      REG2_GLOBAL = 'Global'
    )
  
  return(
    list(
      countries = country_year_results,
      regions = regional_results,
      global = global_results
    )
  )
}

# Function to generate plots for gender gaps over time
plot_across_time <- function(outcome_var, model_type) {
  
  # Load necessary parameters and options
  options(scipen = 999)
  theme_set(theme_minimal(base_size = def_fontsize))
  
  combined_results <- prepare_plot_data_per_country_year(outcome_var, model_type)
  
  # Calculate region-level regression coefficients for labelling
  coefs <- run_regression_by_region(outcome_var, model_type) %>%
    mutate(
      pval = case_when(
        p.value < .001 ~ '***',
        p.value < .01 ~ '**',
        p.value < .05 ~ '*',
        TRUE ~ ''
      ),
      label = paste0(
        'B = ', signif(estimate, 2), ' (', signif(std.error, 2), ')', pval
      )
    ) %>% 
    select(REG2_GLOBAL, label)
  
  # Determine range of estimate values for y-axis limits
  range_values <- range(combined_results$countries$estimate)
  lower_bound <- floor(min(range_values) * 100) / 100  # Floor to 2 decimal places
  upper_bound <- ceiling(max(range_values) * 100) / 100  # Ceiling to 2 decimal places
  
  # Merge region-level coefficients for text labels
  combined_results$regions <- merge(
    combined_results$regions, coefs, by = 'REG2_GLOBAL'
  )
  
  # Plotting
  ggplot(
    combined_results$countries,
    aes(x = YEAR_WAVE, y = estimate, group = WP5)
  ) +
    geom_line(colour = 'grey85') +  # Country-level lines
    geom_hline(yintercept = 0, linetype = 'solid', linewidth = .25) +
    facet_wrap(~ REG2_GLOBAL, scales = 'free') +
    geom_line(data = combined_results$regions, aes(colour = REG2_GLOBAL)) +  # Regional-level lines
    geom_ribbon(
      data = combined_results$regions,
      aes(ymin = lowerCI, ymax = upperCI, fill = REG2_GLOBAL), alpha = .25,
      colour = NA
    ) +
    scale_x_discrete(breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(
      name = 'Gender well-being gap',
      limits = c(lower_bound, upper_bound)
    ) +
    scale_colour_aaas() +
    scale_fill_aaas() +
    coord_cartesian(clip = 'off') +
    theme(
      axis.line.y = element_line(colour = myblack),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = myblack, size = min_fontsize, angle = 45),
      axis.text.y = element_text(colour = myblack, size = def_fontsize),
      strip.text.x = element_text(face = 'bold'),
      plot.margin = plot_margin,
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    geom_text(
      data = combined_results$regions,
      aes(x = 1, y = upper_bound, label = label, colour = REG2_GLOBAL),
      size = min_fontsize * points_to_mm, hjust = 'inward', vjust = 'inward'
    )
  
  # Save the plot as a PDF
  ggsave(
    filename = sprintf('../output/by_country_year/%s_%s.pdf', outcome_var, model_type),
    width = two_col, height = 3, units = 'in', dpi = 2000
  )
}

# Function to run the time trend plot generation for all combinations
create_time_plots <- function() {
  combinations <- expand.grid(
    outcome_var = outcome_vars, model_type = names(get_model_types(outcome_var, m1_vars, m2_vars, m3_vars))
  )
  
  lapply(1:nrow(combinations), function(i) {
    outcome_var <- combinations$outcome_var[i]
    model_type <- combinations$model_type[i]
    
    # Call the function with each combination
    plot_across_time(outcome_var, model_type)
  })
}

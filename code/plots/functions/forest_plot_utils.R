# ------------------------------------------------------------------------
# Script name: plots/functions/forest_plot_utils.R
# Purpose of script: Functions for running regression analyses and 
# plotting results
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-08-07
# ------------------------------------------------------------------------
# Notes:
# This script contains functions to run regression analyses, generate
# tidy outputs, and plot the results.
# ------------------------------------------------------------------------

# Libraries
library(tidyverse)
library(broom)
library(ggsci)
library(gganimate)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggtext)
library(scales)
library(patchwork)
library(gifski)

# Source external functions
source('plots/functions/plot_parameters.R')
source('general/functions/regression_model_utils.R')

# Functions ---------------------------------------------------------------

# 1. Utility Functions

# Create a function to generate the diamond data for each region
create_diamond_data <- function(data) {
  data.frame(
    x = c(data$lowerCI, data$estimate, data$upperCI, data$estimate),
    y = c(0, 1, 0, -1),
    REG2_GLOBAL = data$REG2_GLOBAL
  )
}

# Function to prepare data for plotting
prepare_plot_data <- function(outcome_var, model_type, debug = FALSE) {
  controls <- get_model_types(
    outcome_var, m1_vars, m2_vars, m3_vars
    )[[model_type]]
  
  combined_results <- lapply(
    c("Country", "Regional", "Global"),
    function(level) {
      results <- switch(
        level,
        "Country" = {
          gwp %>%
            group_by(WP5, REG2_GLOBAL) %>%
            do({
              country_data <- .
              if (debug) {
                print(
                  paste(
                    'Processing country:', unique(country_data$WP5),
                    'Model:', model_type
                  )
                )
              }
              result <- run_regression(country_data, outcome_var, controls, weights = country_data$weight)
              if (!is.null(result) && nrow(result) > 0) {
                result$model_type <- model_type
                result$WP5 <- unique(country_data$WP5)
                result$REG2_GLOBAL <- unique(country_data$REG2_GLOBAL)
                result
              } else {
                tibble()  # Return an empty tibble if result is NULL or empty
              }
            }) %>%
            ungroup() %>%
            filter(term == 'WP1219Female') %>%
            mutate(
              lowerCI = estimate - 1.96 * std.error,
              upperCI = estimate + 1.96 * std.error
            )
        },
        "Regional" = {
          gwp %>%
            group_by(REG2_GLOBAL) %>%
            do(run_regression(., outcome_var, controls, weights = .$weight)) %>%
            ungroup() %>%
            filter(term == 'WP1219Female') %>%
            mutate(
              lowerCI = estimate - 1.96 * std.error,
              upperCI = estimate + 1.96 * std.error,
              WP5 = 'Regional',
              model_type = model_type
            )
        },
        "Global" = {
          gwp %>%
            do(run_regression(., outcome_var, controls, weights = .$weight)) %>%
            ungroup() %>%
            filter(term == 'WP1219Female') %>%
            mutate(
              lowerCI = estimate - 1.96 * std.error,
              upperCI = estimate + 1.96 * std.error,
              WP5 = 'Global',
              REG2_GLOBAL = 'Global',
              model_type = model_type
            )
        }
      )
      results
    }
  ) %>%
    bind_rows()
  
  return(combined_results)
}

# 2. Plot Functions
# Main plot generation function
plot_regression <- function(data, outcome_var, model_type, xlims) {
  
  unique_regions <- levels(gwp$REG2_GLOBAL)
  
  region_colours <- setNames(
    aaas_colours[1:length(unique_regions)],
    unique_regions
  )
  
  # Filter the data based on the model type
  filtered_data <- data %>% filter(model_type == !!model_type)
  
  # Separate country, regional, and global data
  country_data <- filtered_data %>% filter(WP5 != 'Global' & WP5 != 'Regional')
  region_data <- filtered_data %>% filter(WP5 == 'Regional')
  global_data <- filtered_data %>% filter(WP5 == 'Global')
  
  # Sort regions in descending order
  region_order <- sort(unique(region_data$REG2_GLOBAL), decreasing = TRUE)
  
  # Prepare country data with appropriate levels and sorting
  country_data <- country_data %>%
    mutate(REG2_GLOBAL = factor(REG2_GLOBAL, levels = region_order)) %>%
    group_by(REG2_GLOBAL) %>%
    arrange(REG2_GLOBAL, estimate) %>%
    ungroup() %>%
    mutate(WP5 = factor(WP5, levels = unique(WP5)))
  
  # Create a row index for each country for plotting
  country_data$row <- 1:nrow(country_data)
  
  # Create lists to store plots and heights
  plot_list <- list()  # For country plots
  diam_list <- list()  # For diamond plots
  height_list <- list()  # For storing the number of countries
  
  # Get unique regions
  regions <- levels(gwp$REG2_GLOBAL)
  
  # Function to create individual plots for each region
  create_plot <- function(region) {
    # Calculate the number of countries in this region
    n_countries <- country_data %>% filter(REG2_GLOBAL == region) %>% nrow() + 1
    
    # Add row_number and staggered hjust to country_data
    a <- ggplot(
      country_data %>% filter(REG2_GLOBAL == region),
      aes(
        x = estimate,
        y = reorder(WP5, estimate),
        alpha = ifelse(lowerCI <= 0 & upperCI >= 0, .25, 1),
        colour = REG2_GLOBAL
      )
    ) +
      geom_vline(aes(xintercept = 0), linetype = 2, colour = mygrey) +
      geom_point(shape = 15, size = .8) +
      geom_errorbar(
        aes(xmin = lowerCI, xmax = upperCI),
        width = .5, linewidth = .3
      ) +
      scale_x_continuous(name = NULL, limits = xlims) +
      scale_y_discrete(
        expand = expansion(add = c(1, 3)),
        guide = guide_axis(n.dodge = 2)
        ) +
      scale_alpha_identity() +
      scale_colour_manual(name = NULL, values = region_colours) +
      guides(colour = NULL) +
      theme_classic() +
      labs(tag = region) +
      theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = min_fontsize),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none',
        plot.margin = plot_margin,
        strip.background = element_blank(),
        plot.tag.location = 'plot',
        plot.tag = element_text(
          size = def_fontsize,
          face = 'bold',
          colour = myblack
        )
      )
    
    # Regional-level plot (diamond plot)
    b <- ggplot(
      region_data %>%
        filter(REG2_GLOBAL == region) %>%
        do(create_diamond_data(.))
    ) +
      geom_vline(aes(xintercept = 0), linetype = 2, colour = mygrey) +
      geom_polygon(
        aes(x = x, y = y, group = REG2_GLOBAL, fill = REG2_GLOBAL),
        colour = myblack, linewidth = .28
      ) +
      scale_x_continuous(limits = xlims) +
      scale_y_continuous(expand = expansion(add = c(0, 0))) +
      scale_alpha_identity() +
      scale_fill_manual(name = NULL, values = region_colours) +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_classic() +
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = plot_margin,
        legend.position = 'none'
      )
    
    # Store the plots and number of countries in the respective lists
    plot_list[[region]] <<- a
    diam_list[[region]] <<- b
    height_list[[region]] <<- n_countries
  }
  
  # Generate plots for each region
  map(regions, create_plot)
  
  # Global-level plot (diamond plot for global data)
  globals <- ggplot(
    create_diamond_data(global_data)
  ) +
    geom_vline(aes(xintercept = 0), linetype = 2, colour = mygrey) +
    geom_polygon(
      aes(x = x, y = y, group = REG2_GLOBAL, fill = REG2_GLOBAL, colour = REG2_GLOBAL)
    ) +
    scale_x_continuous(limits = xlims, breaks = pretty_breaks(n = 5)) +
    scale_y_continuous(
      expand = expansion(add = c(1, .5)),
      breaks = 0, labels = 'Global'
    ) +
    scale_alpha_identity() +
    scale_fill_manual(values = myblack) +
    scale_colour_manual(values = myblack) +
    labs(x = 'Gender well-being gap') +
    theme_classic() +
    theme(
      axis.text.x = element_text(face = 'bold', size = min_fontsize),
      axis.text.y = element_text(face = 'bold', size = min_fontsize),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      plot.margin = plot_margin,
      legend.position = 'none'
    )
  
  # Combine all plots into one final plot
  combined_plot <- plot_list$Americas / diam_list$Americas /
    plot_list$Asia / diam_list$Asia /
    plot_list$Europe / diam_list$Europe /
    plot_list$`Former Soviet Union` / diam_list$`Former Soviet Union` /
    plot_list$`Middle East and North Africa` /
    diam_list$`Middle East and North Africa` /
    plot_list$`Sub-Saharan Africa` / diam_list$`Sub-Saharan Africa` /
    globals + 
    plot_layout(
      heights = c(
        height_list$Americas, 2,
        height_list$Asia, 2,
        height_list$Europe, 2,
        height_list$`Former Soviet Union`, 2,
        height_list$`Middle East and North Africa`, 2,
        height_list$`Sub-Saharan Africa`, 2, 5
      )
    )
  
  # Return the final combined plot
  return(combined_plot)
}

# 3. Analysis Functions
run_forest_plots <- function(outcome_var, gif = FALSE, pdf = TRUE) {
  model_types <- get_model_types(outcome_var, m1_vars, m2_vars, m3_vars)
  
  # Get limits
  lims <- lapply(
    names(model_types), function(x) {
      suppressMessages(prepare_plot_data(outcome_var, x))
    }
  ) %>%
    rbindlist()
  
  xlims <- c(min(lims$lowerCI), max(lims$upperCI))
  
  for (model_type in names(model_types)) {
    combined_results <- prepare_plot_data(outcome_var, model_type)
    
    if (pdf) {
      p <- plot_regression(combined_results, outcome_var, model_type, xlims)
      pdf_filename <- paste0(
        '../output/by_country/', outcome_var, '_', model_type, '.pdf'
      )
      ggsave(pdf_filename, p, width = one_col, height = max_height, unit = 'in')
    }
  }
}

create_forest_plots <- function() {
  model_types <- get_model_types(
    outcome_var, m1_vars, m2_vars, m3_vars
  )
  combinations <- expand.grid(
    outcome_var = outcome_vars, model_type = names(model_types)
  )
  
  lapply(1:nrow(combinations), function(i) {
    outcome_var <- combinations$outcome_var[i]
    model_type <- combinations$model_type[i]
    
    # Call the function with each combination
    run_forest_plots(outcome_var, model_type)
  })
}

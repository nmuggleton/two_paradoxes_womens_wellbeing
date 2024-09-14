# ------------------------------------------------------------------------
# Script name: plots/functions/eurobarometer_across_time.R
# Purpose of script: Create over-time plots for the Eurobarometer data
# Author: Caspar Kaiser
# Date created: 2024-09-08
# Date last modified: 2024-09-08
# ------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source utility functions ------------------------------------------------

source('plots/functions/plot_parameters.R')

# Load packages
library(tidyverse)
library(broom)
library(ggsci)
library(gganimate)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggtext)
library(scales)
library(haven)
library(patchwork)


# Load data ---------------------------------------------------------------

eb_predictions <- file.path(
  '..', 'data', 'eurobarometer', 'data', 'eb_predictions.dta'
) %>%
  read_dta() %>%
  filter(sex == 0 | sex == 1) %>%
  mutate(sex = factor(sex))

# Set plot theme ----------------------------------------------------------

theme_set(theme_minimal(base_size = 10))

#========================================================================
# 2. Functions to make and save the plots
#========================================================================

create_plots <- function(data, cntry_prefix) {
  
  # Set country-specific titles and trends
  if (cntry_prefix == 'France') {
    title_stub <- 'France'
    trend <- 'B = -0.0021425 (0.0003682)***'
  } else if (cntry_prefix == 'Belgium') {
    title_stub <- 'Belgium'
    trend <- 'B = -0.0006985 (0.000353)**'
  } else if (cntry_prefix == 'Netherlands') {
    title_stub <- 'Netherlands'
    trend <- 'B = -0.0020881 (0.0003413)***'
  } else if (cntry_prefix == 'Germany') {
    title_stub <- 'West Germany'
    trend <- 'B = 0.0001487 (0.0003609)'
  } else if (cntry_prefix == 'Italy') {
    title_stub <- 'Italy'
    trend <- 'B = 0.0002647 (0.0003746)'
  } else if (cntry_prefix == 'Denmark') {
    title_stub <- 'Denmark'
    trend <- 'B = -0.0007931 (0.0003262)**'
  } else if (cntry_prefix == 'Ireland') {
    title_stub <- 'Ireland'
    trend <- 'B = -0.0025265 (0.000391)***'
  } else if (cntry_prefix == 'UK') {
    title_stub <- 'United Kingdom'
    trend <- 'B = -0.0005427 (0.0003721)'
  }
  
  # Create absolute plot
  absolute <- ggplot(
    eb_predictions, aes(
      x = year, y = .data[[paste0(cntry_prefix, '_hat')]],
      group = sex, color = sex
    )
  ) +
    geom_line() +
    scale_color_manual(values = aaas_colours[7:8], labels = c('Men', 'Women')) +
    geom_ribbon(
      data = filter(eb_predictions, sex == 0),
      aes(
        ymin = .data[[paste0(cntry_prefix, '_hat_l')]],
        ymax = .data[[paste0(cntry_prefix, '_hat_u')]]
      ),
      alpha = .25, fill = aaas_colours[7], colour = NA
    ) +
    geom_ribbon(
      data = filter(eb_predictions, sex == 1),
      aes(
        ymin = .data[[paste0(cntry_prefix, '_hat_l')]],
        ymax = .data[[paste0(cntry_prefix, '_hat_u')]]
      ),
      alpha = .25, fill = aaas_colours[8], colour = NA
    ) +
    scale_x_continuous(
      name = 'Year', limits = c(1970, 2023),
      breaks = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
    ) +
    scale_y_continuous(name = 'Prediction', limits = c(2.5, 3.7)) +
    theme(
      plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
      axis.line.y = element_line(colour = myblack),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = myblack, size = 7, angle = 45),
      axis.text.y = element_text(colour = myblack, size = 7),
      strip.text.x = element_text(face = 'bold'),
      plot.margin = margin(10, 5, 10, 10),
      legend.position = 'right',
      legend.margin = margin(0, 0, 0, -10),
      legend.key.width = unit(.25, 'cm'),
      legend.text = element_text(size = 6),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Create difference plot
  difference <- ggplot(
    eb_predictions, aes(x = year, y = .data[[paste0(cntry_prefix, '_diff')]])
  ) +
    geom_line(aes(color = 'Difference (men - women)')) +
    scale_color_manual(values = aaas_colours[9]) +
    geom_ribbon(
      aes(
        ymin = .data[[paste0(cntry_prefix, '_diff_l')]],
        ymax = .data[[paste0(cntry_prefix, '_diff_u')]]
      ),
      alpha = .25, fill = aaas_colours[9], colour = NA
    ) +
    geom_hline(yintercept = 0, linetype = 'solid', linewidth = .25) +
    geom_text(
      x = 1972, y = 0.3, label = trend,
      color = aaas_colours[9], size = min_fontsize * points_to_mm,
      hjust = 'inward', vjust = 'inward'
    ) +
    scale_x_continuous(
      name = 'Year', limits = c(1970, 2023),
      breaks = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
    ) +
    scale_y_continuous(name = 'Gender gap', limits = c(-0.3, 0.3), n.breaks = 6) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5),
      axis.line.y = element_line(colour = myblack),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = myblack, size = min_fontsize, angle = 45),
      axis.text.y = element_text(colour = myblack, size = def_fontsize),
      strip.text.x = element_text(face = 'bold'),
      plot.margin = plot_margin,
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Combine absolute and difference plots ---------------------------------
  combined <- absolute + difference +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = title_stub,
      theme = theme(
        plot.title = element_text(size = def_fontsize, face = 'bold', hjust = 0.5)
      )
    )
  
  # Save plot -------------------------------------------------------------
  ggsave(
    plot = combined,
    filename = paste0('../output/by_country_year/', cntry_prefix, '_combined_nm.pdf'),
    width = two_col,  # Use plot width from parameters
    height = 1.5,  # Adjusted to match a reasonable ratio
    units = 'in'
  )
  
  return(combined)
}

#========================================================================
# 3. Generate plots for each country
#========================================================================

cntry_prefixes <- c('France', 'Belgium', 'Netherlands', 'Germany', 'Italy', 'Denmark', 'Ireland', 'UK')
plot_list <- list()

for (cntry_prefix in cntry_prefixes) {
  combined <- create_plots(eb_predictions, cntry_prefix)
  plot_list <- c(plot_list, list(combined))
}

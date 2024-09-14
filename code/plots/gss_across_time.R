# ------------------------------------------------------------------------
# Script name: plots/functions/gss_across_time.R
# Purpose of script: Create over-time plots for the GSS data
# Author: Caspar Kaiser
# Date created: 2024-09-08
# Date last modified: 2024-09-08
# ------------------------------------------------------------------------

# Clean the workspace
rm(list = ls())

# Source utility functions ------------------------------------------------

source('plots/functions/plot_parameters.R')

# Load libraries ----------------------------------------------------------

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

gss_predictions <- file.path(
  '..', 'data', 'gss', 'data', 'gss_predictions.dta'
) %>%
  read_dta() %>%
  filter(sex == 0 | sex == 1) %>%
  mutate(sex = factor(sex))

# Set plot theme ----------------------------------------------------------

theme_set(theme_minimal(base_size = 10))

#========================================================================
# 2. Functions to make and save the plots
#========================================================================

create_plots <- function(data, var_prefix) {

  # Set variable-specific titles and trends
  if (var_prefix == "happy") {
    title_stub <- "happiness"
    trend <- "B = -0.000896 (0.000408)*"
  } else if (var_prefix == "life") {
    title_stub <- "excitement"
    trend <- "B = -0.000181 (0.000456)"
  } else if (var_prefix == "satjob") {
    title_stub <- "job satisfaction"
    trend <- "B = 0.000711 (0.000586)"
  } else if (var_prefix == "satfin") {
    title_stub <- "financial satisfaction"
    trend <- "B = -0.001672 (0.000465)***"
  } else if (var_prefix == "class") {
    title_stub <- "subjective class"
    trend <- "B = -0.001291 (0.000421)***"
  }

  # Create absolute plot
  absolute <- ggplot(
    gss_predictions, aes(
      x = year, y = .data[[paste0(var_prefix, "_hat")]], group = sex, color = sex
    )
  ) +
    geom_line() +
    scale_color_manual(values = aaas_colours[1:2], labels = c("Men", "Women")) +
    geom_ribbon(
      data = filter(gss_predictions, sex == 0),
      aes(
        ymin = .data[[paste0(var_prefix, "_hat_l")]],
        ymax = .data[[paste0(var_prefix, "_hat_u")]]
      ),
      alpha = .25, fill = aaas_colours[1], colour = NA
    ) +
    geom_ribbon(
      data = filter(gss_predictions, sex == 1),
      aes(
        ymin = .data[[paste0(var_prefix, "_hat_l")]],
        ymax = .data[[paste0(var_prefix, "_hat_u")]]
      ),
      alpha = .25, fill = aaas_colours[2], colour = NA
    ) +
    scale_x_continuous(name = "Year", n.breaks = 10) +
    scale_y_continuous(name = paste0("Predicted ", title_stub)) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.line.y = element_line(colour = myblack),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = myblack, size = 9, angle = 45),
      axis.text.y = element_text(colour = myblack, size = 9),
      strip.text.x = element_text(face = 'bold'),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  # Save absolute plot
  ggsave(
    plot = absolute,
    filename = paste0("../output/by_country_year/", var_prefix, "_levels.pdf"),
    width = two_col,  # Use plot width from parameters
    height = 3,  # Adjusted to match a reasonable ratio
    units = 'in'
  )

  # Create difference plot
  difference <- ggplot(
    gss_predictions, aes(x = year, y = .data[[paste0(var_prefix, "_diff")]])
  ) +
    geom_line(aes(color = "Difference (men - women)")) +
    scale_color_manual(values = aaas_colours[1], name = NULL) +
    geom_ribbon(
      aes(
        ymin = .data[[paste0(var_prefix, "_diff_l")]],
        ymax = .data[[paste0(var_prefix, "_diff_u")]]
      ),
      alpha = .25, fill = aaas_colours[1], colour = NA
    ) +
    geom_hline(yintercept = 0, linetype = 'solid', linewidth = .25) +
    annotate(
      "text", x = 1984, y = 0.35, label = trend, color = aaas_colours[1]
    ) +
    scale_x_continuous(name = "Year", n.breaks = 10) +
    scale_y_continuous(
      name = paste0("Gender ", title_stub, " gap"), limits = c(-0.4, 0.4)
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.line.y = element_line(colour = myblack),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = myblack, size = 9, angle = 45),
      axis.text.y = element_text(colour = myblack, size = 9),
      strip.text.x = element_text(face = 'bold'),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  # Save difference plot
  ggsave(
    plot = difference,
    filename = paste0("../output/by_country_year/", var_prefix, "_difference.pdf"),
    width = two_col,  # Use plot width from parameters
    height = 3,  # Adjusted to match a reasonable ratio
    units = 'in'
  )

  # Combine absolute and difference plots
  combined <- absolute + difference +
    plot_layout(ncol = 2) +
    plot_annotation(title = NULL)

  # Save combined plot
  ggsave(
    plot = combined,
    filename = paste0("../output/by_country_year/", var_prefix, "_combined.pdf"),
    width = 2 * two_col,  # 2 times the column width for Science family journals
    height = 3,  # Adjusted to match a reasonable ratio
    units = 'in'
  )
}

#========================================================================
# 3. Generate plots for each variable
#========================================================================

var_prefixes <- c("happy", "life", "satjob", "satfin", "class")

for (var_prefix in var_prefixes) {
  create_plots(gss_predictions, var_prefix)
}

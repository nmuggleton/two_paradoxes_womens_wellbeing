# -------------------------------------------------------------------------
# Script name: plots/functions/men_women_plot_utils.R
# Purpose of script: Functions for plotting gender differences in wellbeing
#  scores across countries and world regions.
# Author: Naomi Muggleton
# Date created: 2024-09-07
# Date last modified: 2024-09-10
# -------------------------------------------------------------------------

# Load necessary libraries ------------------------------------------------

library(ggrepel)
library(ggpubr)

theme_set(theme_classic())

source('plots/functions/plot_parameters.R')

# Helper function to calculate gender well-being differences ---------------

calculate_gender_diff <- function(outcome_var) {
  df <- gwp %>%
    group_by(WP5, REG2_GLOBAL, WP1219) %>%
    select(WP5, REG2_GLOBAL, WP1219, var = !!sym(outcome_var)) %>%
    drop_na() %>%
    summarise(mean = mean(var)) %>%
    ungroup() %>%
    pivot_wider(names_from = WP1219, values_from = mean) %>%
    mutate(diff = Female - Male)
  
  return(df)
}

# Helper function to calculate residuals and identify outliers -------------

calculate_residuals_and_outliers <- function(df) {
  # Run linear models
  model1 <- lm(Male ~ Female, data = df)
  model2 <- lm(diff ~ Male, data = df)
  model3 <- lm(diff ~ Female, data = df)
  
  # Calculate residuals and outliers
  df <- df %>%
    mutate(
      residuals1 = resid(model1),
      residuals2 = resid(model2),
      residuals3 = resid(model3),
      outlier1 = ifelse(abs(residuals1) > 2 * sd(residuals1), T, F),
      outlier2 = ifelse(abs(residuals2) > 2 * sd(residuals2), T, F),
      outlier3 = ifelse(abs(residuals3) > 2 * sd(residuals3), T, F)
    )
  
  return(df)
}

# Helper function to generate individual plots ----------------------------

generate_plot <- function(
    df, x_var, y_var, outlier_var, x_label, y_label, intercept_slope
    ) {
  
  ggplot(df, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(aes(colour = REG2_GLOBAL), alpha = .5, size = 2) +
    geom_smooth(method = lm, se = FALSE, colour = myblack) +
    geom_label_repel(
      data = filter(df, !!sym(outlier_var)),
      aes(label = WP5),
      size = min_fontsize * points_to_mm,
      min.segment.length = 0, 
      label.padding = 0.1
    ) +
    geom_abline(
      intercept = intercept_slope[1],
      slope = intercept_slope[2],
      linetype = 2
      ) +
    scale_colour_aaas() +
    scale_fill_aaas() +
    coord_cartesian(clip = 'off') +
    guides(colour = guide_legend(nrow = 1)) +
    labs(x = x_label, y = y_label) +
    theme(
      axis.line.y = element_line(colour = myblack),
      axis.text.x = element_text(
        colour = myblack,
        size = def_fontsize,
        angle = 45
        ),
      axis.text.y = element_text(colour = myblack, size = def_fontsize),
      plot.margin = unit(c(5, 5, 0, 0), 'mm'),
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# Function to plot and save gender differences in well-being scores --------

plot_men_vs_women_scores <- function() {
  
  # Define plot width for 2-column layout
  plot_width <- two_col
  
  for (outcome_var in outcome_vars) {
    
    # Calculate gender well-being differences and residuals
    df <- calculate_gender_diff(outcome_var)
    df <- calculate_residuals_and_outliers(df)
    
    # Generate plots
    a <- generate_plot(
      df = df,
      x_var           = 'Male',
      y_var           = 'Female',
      outlier_var     = 'outlier1',
      x_label         = 'Male',
      y_label         = 'Female',
      intercept_slope = c(0, 1)
      )
    
    b <- generate_plot(
      df              = df,
      x_var           = 'Male',
      y_var           = 'diff',
      outlier_var     = 'outlier2',
      x_label         = 'Male',
      y_label         = 'Gender well-being gap',
      intercept_slope = c(0, 0)
      )
    c <- generate_plot(
      df              = df,
      x_var           = 'Female',
      y_var           = 'diff',
      outlier_var     = 'outlier3',
      x_label         = 'Female',
      y_label         = 'Gender well-being gap',
      intercept_slope = c(0, 0)
      )
    
    # Arrange the three plots side by side
    ggarrange(
      a, b, c,
      align = 'hv',
      common.legend = TRUE,
      legend = 'bottom',
      nrow = 1,
      labels = 'auto',
      vjust = 1
    ) %>%
      ggsave(
        filename = sprintf('../output/men_women/%s.pdf', outcome_var),
        width = plot_width,
        height = 3
        )
  }
}

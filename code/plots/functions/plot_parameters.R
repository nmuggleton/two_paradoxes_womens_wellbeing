# ------------------------------------------------------------------------
# Script name: plots/functions/plot_parameters.R
# Purpose of script: Define key parameters for plotting, such as figure
# dimensions, colours, font sizes, and margins.
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-09-10
# ------------------------------------------------------------------------
#
# Notes:
#   This script defines the core visual parameters used across various
#   plots, including figure dimensions, colour palettes, and text size 
#   conversion rates for consistency in visual presentation.
# ------------------------------------------------------------------------

# Parameters --------------------------------------------------------------

## 1. Define figure dimensions
#### 
## "Figures should default to widths of 1 column (3.55 in, 9 cm, or 21 picas, 3
## points) or 2 columns (7.25 in, 18.4 cm, or 43 picas, 6 points). Widths
## between those sizes can also be used, if needed. However, if the figure is
## not a full 2 columns wide, the width should not exceed 5.67 in, 14.4 cm, or
## 34 picas. Figures should be no deeper than the page height (9 in, 22.75 cm,
## or 53 picas, 9 points). However, for 2-column figures, please keep in mind
## that the legend will need to fit into the 9 inch depth. For your reference,
## legends are set at 7.5 pt (10 pt leading) Myriad Regular. We recommend a 
## maximum depth of 7.8 in, 19.9 cm, or 47 picas in order to fit the legend."
####
one_col <- 3.55
two_col <- 7.25
max_height <- 9

## 2. Define colours based on Science colour palette
aaas_colours <- pal_aaas()(10)
mygrey <- pal_aaas()(10)[9]
myblack <- pal_aaas()(10)[10]

## 3. Define conversion rate from points to mm (ggplot is strange wrt text size)
points_to_mm <- 0.35278

points_to_mm <- 0.35278

## 4. Define plot margin
plot_margin <- unit(c(0, 0, 0, 0), 'cm')

## 5. Define font size
def_fontsize <- 8
min_fontsize <- 6

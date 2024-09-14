# ------------------------------------------------------------------------
# Script name: general/functions/model_type_utils.R
# Purpose of script: Define model types (set of control variables) 
# for gender gap analysis
# Author: Naomi Muggleton
# Date created: 2024-07-29
# Date last modified: 2024-09-14
# ------------------------------------------------------------------------
#
# Notes:
#   This script contains a simple function to return the variables used 
#   in each model type for the gender gap analysis.
# ------------------------------------------------------------------------

# Function to get model types
get_model_types <- function(outcome_var, m1_vars, m2_vars, m3_vars) {
  # Return the list of control variables for each model
  return(list(
    'm1' = m1_vars,  # Basic model with wave as control
    'm2' = m2_vars,  # Adds employment status
    'm3' = m3_vars   # Full model with demographic controls
  ))
}

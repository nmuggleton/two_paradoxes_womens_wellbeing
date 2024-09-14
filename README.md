# Two Paradoxes in Women's Wellbeing

[![r](https://img.shields.io/badge/R-brightgreen.svg)](https://shields.io/) ![StataMin](https://img.shields.io/badge/stata-red) [![license](https://img.shields.io/badge/License-MIT-blue.svg)](https://shields.io/)


## Introduction

This is a repository to accompany the review of the two gender wellbeing paradoxes by C. Kaiser, N. Muggleton, E. Quispe-Torreblanca, and J.-E. De Neve.
Please see the code (and associated paper -- available from authors upon request) for a description of what the code does.
But, in summary, we run a comprehensive analysis of the gender differences in live evaluation, positive and negative affect, and pain using data from the Gallup World Poll (GWP), Eurobarometer, and General Social Survey (GSS).

## Prerequisites

### Replication and data requirements

*Note.* Replication also requires access to the Gallup World Poll dataset. Various parts of the data cannot be made public, although we have included `DataSources.txt` files to clarify how we accessed the data. More information is available [here](https://www.gallup.com/analytics/318923/world-poll-public-datasets.aspx).

### Running the R Scripts

To run the scripts in this repository, you will need a working installation of R with all the necessary package dependencies. We recommend using the `renv` package for managing dependencies and ensuring a reproducible environment.

#### Steps to Set Up the Environment:

1. Install the `renv` package in R if you don’t have it:
```R
   install.packages("renv")
```  

2. Once renv is installed, navigate to the project directory in R and run:

```R
renv::restore()
```  

This command will install all the required packages listed in the `renv.lock` file to your environment.

If you encounter any issues with package installations, ensure that your system is configured with the appropriate R version and that any system dependencies for certain packages (e.g., `data.table`, `ggplot2`) are met.

By using renv, you’ll have the same R environment as the one used to develop this project, ensuring consistent results.

### Repository Structure

Repo structure made using the ```tree```


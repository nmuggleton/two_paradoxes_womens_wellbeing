# Two Paradoxes in Women's Wellbeing

[![r](https://img.shields.io/badge/R-4.1.1-blue)](https://shields.io/) ![StataMin](https://img.shields.io/badge/stata-18-red) [![license](https://img.shields.io/badge/License-MIT-brightgreen)](https://shields.io/)


## Introduction

This repository contains the R code and supplementary material for the paper titled "Two Paradoxes in Women's Wellbeing" by C. Kaiser, N. Muggleton, E. Quispe-Torreblanca, and J.-E. De Neve.
The repository provides analyses of gender differences in life evaluation, positive and negative affect, and pain using three datasets: Gallup World Poll (GWP), Eurobarometer, and General Social Survey (GSS).
The paper explores gender paradoxes in subjective wellbeing, presenting both cross-sectional and longitudinal analyses.



## Prerequisites

To run the scripts in this repository, you will have to:

1. Download the necessary datasets (see Replication and data Requirements); and
2. Run the code (see Running the R scripts)

### Replication and data requirements

Various parts of the data cannot be made public, although we have included `DataSources.txt` files to clarify how we accessed the data.

### Running the R Scripts

To run the scripts in this repository, you will need a working installation of R with all the necessary package dependencies.
Set working directory to the `code` repo.
We recommend using the `renv` package for managing dependencies and ensuring a reproducible environment.

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

By using renv, you’ll have the same R environment as the one used to develop this project, which will give consistent results.

### Repository Structure

Repo structure made using the ```tree```:

```md
├── code                     # R scripts for analysis and plotting
│   ├── renv.lock            # Set up environment
│   ├── general              # General utility scripts and functions
│   ├── plots                # Contains plotting scripts
│   └── tables               # Scripts for generating tables
├── data                     # Raw and processed datasets
└── output                   # Plots and tables generated by scripts
```


## License
This repository is licensed under the MIT License. See the `LICENSE` file for details.

## Version Information
This repository uses R version 4.1.1.


# Supplementary materials for: `throne`; Using aerial thermography to map terrestrial thermal environments in unprecedented detail 

This repository contains supplementary materials for the manuscript submitted to Methods in Ecology & Evolution. Please also consider checking the accompanying `R` package `throne` at: https://github.com/ggcostoya/throne. 

The structure of the repository is as follows:

- `data/`: Contains all data used for the manuscript including:
  - `otm_data/`: Which contains 37 `.csv` files containing raw operative temperature model data collected in the field and named according to each OTM's id. 
  - `flights_metadata.csv`: The metadata for all files conducted in this study. 
  - `otm_metadata.csv`: The metadata for all OTMs used in this study.
  - `val_data.RData`: The validation data used in the manuscript. 
  
- `scripts/`: Contains all scripts used for the manuscript including:
  - `throne_method_validation.R`: The script used to validate the `throne` methodology. 
  - `prep_data_for_package.R`: The script used to prepare the data for the `throne` package.
  - `plot_manuscript_figures.R`: The script used to generate all figures for the manuscript.

The raw data for all flights conducted can be found at: 

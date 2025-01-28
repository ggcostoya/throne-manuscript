# Supplementary materials for: `throne`; Using aerial thermography to map terrestrial thermal environments in unprecedented detail

This repository contains supplementary materials for the manuscript submitted to Methods in Ecology & Evolution. Please also consider checking the accompanying `R` package `throne` at: <https://github.com/ggcostoya/throne>.

The structure of the repository is as follows:

-   `data/`: Contains all data used for the manuscript including:
    -   `otm_data/`: Contains all raw OTM data (`.csv` files) for validation 1, validation 2 and the case study.

    -   For the following data, there is a version for validation 1 (`val_1`) and for validation 2 (`val_2`). All `val_2` files are also used as example data sets for the `throne` package:

        -   `flights_data_no_gcp.RData`: Processed flights data sets where the original `.tif` files where not corrected using ground control points (`GCP`). In call cases flights were processed setting `resolution` to 1 $m^2$.

        -   `flights_data_gcp.RData`: Processed flights data sets where the original `.tif` files where corrected using ground control points (`GCP`). In call cases flights were processed setting `resolution` to 1 $m^2$.

        -   `flights_data_gcp_corr.RData`: Processed flights data sets where the original `.tif` files where corrected using ground control points (`GCP`) and further corrected into operative temperatures using the `correct_flights_data` function from the `throne` package. In call cases flights were processed setting `resolution` to 1 $m^2$.

        -   `flights_metadata.csv`: Metadata associated with each validation's flights.

        -   `otm_metadata.csv`: Metadata associated with operative temperature model (OTM) deployment for each validation.

        -   `otm_data.RData`: Processed OTM data for each validation.

        -   `otms_splines.RData`: OTM and day of the year (`doy`) specific cubic spline models for each validation. For validation 1 the `knot_p` parameter was set to 1 and for validation 2 set to 0.13.

        -   `matches.RData`: An OTM to tile match data set for each validation. In both cases, `coverage` was set to 0.9 when establishing the match.

        -   `val_data.RData`. Data sets for validation of the method obtained through the `method_validation.R` script.
-   `scripts/`: Contains all scripts used for the manuscript including:
    -   `method_validation.R`: Script used to validate the `throne` methodology.
    -   `plots.R`: Script used to generate all figures for the manuscript.
    -   `prep_data.R`: Script used to prepare data for the `throne` `R` package, the method validation and to plot figures.

The raw data for all flights conducted can be found at:

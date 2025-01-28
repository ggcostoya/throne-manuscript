
## Prepare data for validations ##

## Packages & functions ----

# set packages
library(throne)
library(tidyverse)

# set working directory
wd <- "C:/Users/ggarc/OneDrive/research/throne-manuscript" # change locally

# define function for easy loading files
easypaste <- function(file_path_within){
  paste(wd,file_path_within,sep = "/")
}

## Validation 1 ----

## Read and process flights data ----

# read flights metadata
flights_metadata_val1 <- read.csv(easypaste("data/flights_metadata_val1.csv"))

# define path to non-gcp corrected flights
flights_path_no_gcp <- easypaste("flights_data/validation_1/no_gcp")

# define path to gcp corrected flights
flights_path_gcp <- easypaste("flights_data/validation_1/gcp")

# read and process no gcp corrected flights
flights_data_val1_no_gcp <- rnp_flights_data(path = flights_path_no_gcp,
                                             metadata = flights_metadata_val1,
                                             resolution = 1)

# read and process gcp corrected flights
flights_data_val1_gcp <- rnp_flights_data(path = flights_path_gcp,
                                          metadata = flights_metadata_val1,
                                          resolution = 1)

# save data
save(flights_data_val1_no_gcp, file = "data/flights_data_val1_no_gcp.RData")
save(flights_data_val1_gcp, file = "data/flights_data_val1_gcp.RData")

## Read and process OTMs data ----

# read metadata file
otms_metadata_val1 <- read.csv("data/otm_metadata_val1.csv")

# define directory where OTM files are stored
otms_val1_path <- "data/otm_data/validation_1"

# read and process all OTms
otms_data_val1 <- rnp_otms_data(path = otms_val1_path,
                                rows_skip = 14,
                                date_col = 1,
                                op_temp_col = 3,
                                metadata = otms_metadata_val1)

# change OTM id names (A and B correspond to the same OTM, different deployments)
otms_data_val1 <- otms_data_val1 |>
  mutate(otm_id_new = str_sub(otm_id, 1, -3)) |> # add column with new id without the A or B
  group_by(otm_id_new) |> # group by new id
  mutate(both_dep = length(unique(otm_id))) |> # add column with number of deployments
  ungroup() |>
  filter(both_dep == 2) |> # filter OTMs deployed in both deployments
  select(-c(otm_id, both_dep)) |>
  rename(otm_id = otm_id_new)

# remove days when number of observations is small (deployment or recovery dates)
# and when OTMs were in the lab
otms_data_val1 <- otms_data_val1 |>
  filter(!doy %in% c(104,161,162,163,164,231)) |> # remove deployment and re-deployment dates
  group_by(otm_id, year, doy) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  filter(n_obs > 25) %>% # remnove days with less than 25 observations
  select(-n_obs)

# save OTMs data
save(otms_data_val1, file = "data/otms_data_val1.RData")

## Generate OTM splines ----

# generate OTM splines
otms_splines_val1 <- gen_otm_splines(otm_data = otms_data_val1, knot_p = 1)

# save OTM splines file
save(otms_splines_val1, file = "data/otms_splines_val1.RData")

## Correct flights data ----

# correct flights data
flights_data_val1_gcp_corr <- correct_flights_data(
  flights_data = flights_data_val1_gcp,
  otm_splines = otms_splines_val1,
  time_correction = TRUE,
  time_correction_metric = "mean",
  flight_specific_correction = FALSE)

# save corrected flights data
save(flights_data_val1_gcp_corr, file = "data/flights_data_val1_gcp_corr.RData")

## Match data for visualization purposes ----

# estimate matches with error_max = 5
matches_val1 <- match_data(flights_data = flights_data_val1_gcp_corr,
                           otm_splines = otms_splines_val1,
                           coverage_per = 0.9)

# save matches data
save(matches_val1, file = "data/matches_val1.RData")

## Validation 2 ----

## Read and process flights data ----

# read flights metadata
flights_metadata_val2 <- read.csv(easypaste("data/flights_metadata_val2.csv"))

# define path to no gcp corrected flights
flights_path_no_gcp <- easypaste("flights_data/validation_2/no_gcp")

# define path to gcp corrected flights
flights_path_gcp <- easypaste("flights_data/validation_2/gcp")

# read and process no gcp corrected flights
flights_data_val2_no_gcp <- rnp_flights_data(path = flights_path_no_gcp,
                                             metadata = flights_metadata_val2,
                                             resolution = 1)

# read and process gcp corrected flights
flights_data_val2_gcp <- rnp_flights_data(path = flights_path_gcp,
                                          metadata = flights_metadata_val2,
                                          resolution = 1)

# save data
save(flights_data_val2_no_gcp, file = "data/flights_data_val2_no_gcp.RData")
save(flights_data_val2_gcp, file = "data/flights_data_val2_gcp.RData")

## Read and process OTMs data ----

# read metadata file
otms_metadata_val2 <- read.csv("data/otm_metadata_val2.csv")

# define directory where OTM files are stored
otms_val2_path <- "data/otm_data/validation_2"

# read and process all OTms
otms_data_val2 <- rnp_otms_data(path = otms_val2_path,
                                rows_skip = 14,
                                date_col = 1,
                                op_temp_col = 3,
                                metadata = otms_metadata_val2)

# save OTMs data
save(otms_data_val2, file = "data/otms_data_val2.RData")

## Generate OTM splines ----

# generate OTM splines
otms_splines_val2 <- gen_otm_splines(otm_data = otms_data_val2, knot_p = 1/7.5)

# save OTM splines file
save(otms_splines_val2, file = "data/otms_splines_val2.RData")

## Correct flights data ----

# correct flights data
flights_data_val2_gcp_corr <- correct_flights_data(
  flights_data = flights_data_val2_gcp,
  otm_splines = otms_splines_val2,
  time_correction = TRUE,
  time_correction_metric = "mean",
  flight_specific_correction = FALSE)

# save corrected flights data
save(flights_data_val2_gcp_corr, file = "data/flights_data_val2_gcp_corr.RData")


## Match data for visualization purposes ----

# estimate matches with error_max = 5
matches_val2 <- match_data(flights_data = flights_data_val2_gcp_corr,
                           otm_splines = otms_splines_val2,
                           coverage_per = 0.9)

# save matches data
save(matches_val2, file = "data/matches_val2.RData")





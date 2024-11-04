
## Prepare data for validation ##

## load throne ----

library(throne)

## Read and process flights data ----

# set working directory
wd <- "C:/Users/ggarc/OneDrive/research/throne-manuscript"

# read metadata file
flights_metadata_validation <- read.csv(paste(wd,"data/flights_metadata_validation.csv",sep ="/"))

# define directory where files are stored
flights_path <- paste(wd,"flights_data/validation_flights", sep = "/")

# read and process all flights
flights_data_validation <- rnp_flights_data(path = flights_path,
                                            metadata = flights_metadata_validation,
                                            digits = 5)


## Read and process OTM data ----

# read metadata file
otms_metadata_validation <- read.csv("data/otm_metadata_validation.csv")

# define directory where OTM files are stored
otms_path <- "data/otm_data/validation"

# read and process all OTms
otms_data_validation <- rnp_otms_data(path = otms_path,
                                      rows_skip = 14,
                                      date_col = 1,
                                      op_temp_col = 3,
                                      metadata = otms_metadata_validation)


# change OTM id names (A and B correspond to the same OTM, different deployments)
otms_data_validation <- otms_data_validation %>%
  mutate(otm_id_new = str_sub(otm_id, 1, -3)) %>%
  group_by(otm_id_new) %>%
  mutate(both_dep = length(unique(otm_id))) %>%
  ungroup() %>%
  filter(both_dep == 2) %>%
  select(-c(otm_id, both_dep)) %>%
  rename(otm_id = otm_id_new)

# remove days when number of observations is small
otms_data_validation <- otms_data_validation %>%
  group_by(otm_id, year, doy) %>%
  mutate(n_obs = n()) %>%
  ungroup() %>%
  filter(n_obs > 25) %>%
  select(-n_obs)

# save OTMs data
save(otms_data_validation, file = "data/otms_data_validation.RData")

## Generate OTM splines ----

# generate OTM splines
otms_splines_validation <- gen_otm_splines(otm_data = otms_data_validation,
                                           knot_p = 0.5)

# save OTM splines file
save(otms_splines_validation, file = "data/otms_splines.RData")

## Correct flights data ----

flights_data_validation_corr <- correct_flights_data(flights_data = flights_data_validation,
                                                     otm_splines = otms_splines_validation,
                                                     time_correction = TRUE,
                                                     time_correction_metric = "mean",
                                                     flight_specific_correction = FALSE)

# save corrected flights data
save(flights_data_validation_corr,
     file = "data/flights_data_validation_corr.RData")









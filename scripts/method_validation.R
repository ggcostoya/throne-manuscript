
## Method validation ##

## Packages ----

library(throne)
library(tidyverse)

## Validation 1 ##

## Load and prepare data ----

# load flights and OTMs data
load("data/flights_data_val1_gcp_corr.RData")
load("data/otms_data_val1.Rdata")

# get flights list
flights_list <- flights_data_val1_gcp_corr |>
  dplyr::select(year, doy, mod_start, mod_end) |> unique()

# filter unique x and y info for OTMs and flights
xy_flights <- flights_data_val1_gcp_corr |> select(x, y) |> unique()
xy_otms <- otms_data_val1 |> select(otm_id, x, y) |> unique() |>
  mutate(new_x = NA, new_y = NA)

# get list of OTMs within range of flight and add column for in range
xy_otms_range <- xy_otms |> filter(x > 275300) |>   filter(y > 4415950) |>
  filter(x < 275565)
xy_otms$in_range <- ifelse(xy_otms$otm_id %in% xy_otms_range$otm_id, 1, 0)

# loop to find closest coordinates for OTMs in range only
for(i in 1:nrow(xy_otms)){
  if(xy_otms$in_range[i] == 1){
    nw <-  xy_flights[which.min(sqrt((xy_otms$x[i] - xy_flights$x)^2 + (xy_otms$y[i] - xy_flights$y)^2)),]
    xy_otms$new_x[i] <- nw$x
    xy_otms$new_y[i] <- nw$y
  }else{
    xy_otms$new_x[i] <- xy_otms$x[i]
    xy_otms$new_y[i] <- xy_otms$y[i]

  }
}

# merge OTM data with xy data and simplify name
otms <- otms_data_val1 |>
  left_join(xy_otms, by = c("otm_id","x", "y")) |>
  select(-c(x, y, in_range)) |>
  rename(x = new_x, y = new_y)

# generate splines with different knot_p parameters
a <- gen_otm_splines(otm_data = otms, knot_p = 1) |>
  mutate(knot_p = 1)
b <- gen_otm_splines(otm_data = otms, knot_p = 0.5) |>
  mutate(knot_p = 0.5)
c <- gen_otm_splines(otm_data = otms, knot_p = 0.25) |>
  mutate(knot_p = 0.25)
m_otm_splines <- bind_rows(a, b, c)

# filter flights data to only contain tiles where OTMs are deployed and simplify name
xy_otms_range <- xy_otms |> filter(in_range == 1)
tiles_otms <- xy_otms_range$new_x * xy_otms_range$new_y
flights <- flights_data_val1_gcp_corr |>
  mutate(tile = x*y) |>
  filter(tile %in% tiles_otms) |>
  select(-tile)

## Prepare parameters to test ----

# define number of flights
total_n_flights <- nrow(flights_list)

# number of flights to use
n_flights <- c(total_n_flights*0.3,total_n_flights*0.5,total_n_flights)

# number of OTMs to use
n_otms <- c(10,30,70)

# knots / h to be considered
knot_ps <- c(1, 0.5, 0.25)

# get combinations grid
combs <- expand.grid(n_flights, n_otms, knot_ps)
colnames(combs) <- c("n_flights", "n_otms", "knot_p")

# get replicates
combs <- do.call("rbind", replicate(10, combs, simplify = FALSE))

# add iterations column
combs$it <- seq(1, nrow(combs), by = 1)

## Matching ----

# prepare matches object
all_matches <- data.frame(x = c(), y = c(), otm_id = c(), error  = c(),
                          n_flights = c(), n_otms = c(), knot_p = c(), it = c())

# loop to perform matching
for(i in 1:nrow(combs)){

  # select subset of flights of interest
  if(combs$n_flights[i] < nrow(flights_list)){

    # define number of flights to sample
    n_flights_sample <- combs$n_flights[i]

    # sample morning, middle of the day and afternoon flights
    fmorn <- flights_list |> filter(mod_start < 660) |> sample_n(n_flights_sample/3)
    fmid <- flights_list |> filter(mod_start >= 660 & mod_start < 1000) |> sample_n(n_flights_sample/3)
    faft <- flights_list |> filter(mod_start >= 1000) |> sample_n(n_flights_sample/3)
    fit <- rbind(fmorn, fmid, faft)
    sub_flights <- flights |> filter(mod_start %in% fit$mod_start)

  }else{

    fit <- flights_list
    sub_flights <- flights

  }

  # select subset of OTMs of interest
  sub_otms <- sample(unique(m_otm_splines$otm_id), combs$n_otms[i])

  # select subset of OTM splines of interest
  sub_otm_splines <- m_otm_splines |> filter(knot_p == combs$knot_p[i]) |>
    filter(otm_id %in% sub_otms)

  # create a dataset to predict the temperature recorded by each OTM during each flight
  all_otms_times <- expand.grid(otm_id = sub_otms,
                                year = unique(fit$year),
                                doy = unique(fit$doy),
                                mod_start = unique(fit$mod_start),
                                mod_end = unique(fit$mod_end))

  # merge all otms times with metadata from subset of flights to be tested
  sub_otm_preds <- merge(all_otms_times, fit,
                         by = c("year", "doy","mod_start", "mod_end"))

  # add column for predicted operative temperatures
  sub_otm_preds$pred_op_temp <- NA

  # loop to predict operatuve temperatures for each OTM during each flight
  for(j in 1:nrow(sub_otm_preds)){

    # isolte the DOY and OTM id specific spline
    spec_spline <- sub_otm_splines |>
      filter(otm_id == sub_otm_preds$otm_id[j]) |>
      filter(year == sub_otm_preds$year[j]) |>
      filter(doy == sub_otm_preds$doy[j])

    # predict temperatures
    sub_otm_preds$pred_op_temp[j] <- mean(predict(spec_spline$spline[[1]],
                                                  sub_otm_preds$mod_start[j]:sub_otm_preds$mod_end[j])$y)



  }

  # build holder dataset for matches
  sub_matches <- sub_flights |> select(x,y) |> unique() |> mutate(otm_id = NA, error = NA)

  # loop to estimate best OTM match for each tile
  for(j in 1:nrow(sub_matches)){

    # filter data for specific tile
    tile_dat <- sub_flights |> filter(x == sub_matches$x[j]) |>
      filter(y == sub_matches$y[j])

    # merge tile data with otm_preds
    tile_dat <- merge(tile_dat, sub_otm_preds,
                      by = c("year", "doy", "mod_start", "mod_end"), all = TRUE)

    # find OTM match
    otm_match <- tile_dat |> group_by(otm_id) |>
      summarise(error = mean(abs(pred_op_temp - op_temp), na.rm = T)) |>
      filter(error == min(error, na.rm = T))

    # add info to holder matches data set
    sub_matches$otm_id[j] <- as.character(otm_match$otm_id[1])
    sub_matches$error[j] <- otm_match$error[1]

  }

  # add information columns
  sub_matches <- sub_matches |> mutate(n_flights = combs$n_flights[i],
                                       n_otms = combs$n_otms[i],
                                       knot_p = combs$knot_p[i],
                                       it = combs$it[i])

  # bind to all matches object
  all_matches <- bind_rows(all_matches, sub_matches)

  # print i if i is a multiple of 10
  if(i %% 27 == 0){
    print(i/270)
  }

}

# get list of actual OTM positions to merge
merge_otms <- otms |> select(otm_id, x, y) |> unique() |> rename(a_otm_id = otm_id)

# merge actual OTM positions with all matches data
val_data <- merge(all_matches, merge_otms, by = c("x", "y"))

## Validation ----

# repeat each match 100 times
val_data <- do.call("rbind", replicate(100,val_data, simplify = FALSE))

# set unique doys to sample from and remove problematic ones
unique_doy <- unique(otms$doy)
unique_doy <- ifelse(unique_doy %in% c(105,162,163,176:180), NA, unique_doy)
unique_doy <- unique_doy[!is.na(unique_doy)]

# add random doy and mod to test to val_data
val_data$doy <- sample(unique_doy, nrow(val_data), replace = TRUE)
val_data$mod <- sample(seq(0,1380, by = 60), nrow(val_data), replace = TRUE)

# expand grid to obtain unique combinations from which to extract data
otm_time_combs <- val_data |> select(a_otm_id, doy, mod, doy, knot_p) |>
  unique() |> mutate(op_temp = NA) |> rename(otm_id = a_otm_id)

# loop to pre-calculate operative temperatures
for(i in 1:nrow(otm_time_combs)){

  # filter spline of interest
  spline <- m_otm_splines |> filter(otm_id == otm_time_combs$otm_id[i],
                                    knot_p == otm_time_combs$knot_p[i],
                                    doy == otm_time_combs$doy[i])

  # predict operative temperature
  otm_time_combs$op_temp[i] <- predict(spline$spline[[1]],
                                       otm_time_combs$mod[i])$y

  print(i)
  if(i %% 28459 == 0){
    print(round(i/284590,2))
  }

}

# get observed and prediction data
actual <- otm_time_combs |> rename(a_otm_id = otm_id, obs_op_temp = op_temp)
prediction <- otm_time_combs |> rename(pred_op_temp = op_temp)

# merge validation data with actual observations and predictions
val_data <- merge(val_data, actual, by = c("a_otm_id", "doy", "mod", "knot_p"))
val_data <- merge(val_data, prediction, by = c("otm_id", "doy", "mod", "knot_p"))

# rename data and save
val1_data <- val_data
save(val1_data, file = "data/val1_data.RData")

## Summary table ----

# summarise predictive error
val1_sum <- val1_data |>
  #filter(mod > 7*60) |> filter(mod < 19*60) |> # remove or not for night time or daytime only
  mutate(minute = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) |>
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) |>
  group_by(n_flights, n_otms, knot_p) |>
  summarise(mean_error = mean(pred_op_temp - obs_op_temp),
            sd_error = sd(pred_op_temp - obs_op_temp),
            abs_mean_error = mean(abs(pred_op_temp - obs_op_temp)),
            abs_sd_error = sd(abs(pred_op_temp - obs_op_temp)))

# summarise accuracy
write.csv(val1_sum, "data/val1_sum.csv")



## Validation 2 ## ----

## Load and prepare data ----

# load flights and OTMs data
load("data/flights_data_val2_gcp_corr.RData")
load("data/otms_data_val2.Rdata")

# get flights list
flights_list <- flights_data_val2_gcp_corr |>
  dplyr::select(year, doy, mod_start, mod_end) |> unique()

# filter unique x and y info for OTMs and flights
xy_flights <- flights_data_val2_gcp_corr |> select(x, y) |> unique()
xy_otms <- otms_data_val2 |> select(otm_id, x, y) |> unique() |>
  mutate(new_x = NA, new_y = NA)

# loop to find closest coordinates
for(i in 1:nrow(xy_otms)){
  nw <-  xy_flights[which.min(sqrt((xy_otms$x[i] - xy_flights$x)^2 + (xy_otms$y[i] - xy_flights$y)^2)),]
  xy_otms$new_x[i] <- nw$x
  xy_otms$new_y[i] <- nw$y
}

# merge OTM data with xy data and simplify name
otms <- otms_data_val2 |>
  left_join(xy_otms, by = c("otm_id","x", "y")) |>
  select(-c(x, y)) |>
  rename(x = new_x, y = new_y)

# generate splines with different knot_p parameters
a <- gen_otm_splines(otm_data = otms_data_val2, knot_p = 1/2) |>
  mutate(knot_p = 1/2)
b <- gen_otm_splines(otm_data = otms_data_val2, knot_p = 1/7.5) |>
  mutate(knot_p = 1/7.5)
c <- gen_otm_splines(otm_data = otms_data_val2, knot_p = 1/15) |>
  mutate(knot_p = 1/15)
d <- gen_otm_splines(otm_data = otms_data_val2, knot_p = 1/60) |>
  mutate(knot_p = 1/60)
m_otm_splines <- bind_rows(a, b, c, d)

# filter flights data to only contain tiles where OTMs are deployed and simplify name
tiles_otms <- xy_otms$new_x * xy_otms$new_y
flights <- flights_data_val2_gcp_corr |>
  mutate(tile = x*y) |>
  filter(tile %in% tiles_otms) |>
  select(-tile)

## Prepare parameters to test ----

# define number of flights and number of OTMs to test
n_flights <- c(round(nrow(flights_list)*0.1),  round(nrow(flights_list)*0.25),
               round(nrow(flights_list)*0.5),  round(nrow(flights_list)))

# define number of OTMs used
n_otms <- c(10,20,33)

# define knot_p values to test
knot_p <- c(1/2, 1/7.5, 1/15, 1/60)

# define grid of combinations and replicate and add iteration number
combs <- expand.grid(n_flights, n_otms, knot_p) |>
  rename(n_flights = Var1, n_otms = Var2, knot_p = Var3)
combs <- do.call("rbind", replicate(10, combs, simplify = FALSE))
combs$it <- seq(1, nrow(combs), by = 1)

## Matching ----

# prepare matches object
all_matches <- data.frame(x = c(), y = c(), otm_id = c(), error  = c(),
                          n_flights = c(), n_otms = c(), knot_p = c(), it = c())

# loop to perform matching
for(i in 1:nrow(combs)){

  # select subset of flights of interest
  if(combs$n_flights[i] < nrow(flights_list)){

    # define number of flights to sample
    n_flights_sample <- combs$n_flights[i]

    # sample morning, middle of the day and afternoon flights
    fmorn <- flights_list |> filter(mod_start < 660) |> sample_n(n_flights_sample/3)
    fmid <- flights_list |> filter(mod_start >= 660 & mod_start < 1000) |> sample_n(n_flights_sample/3)
    faft <- flights_list |> filter(mod_start >= 1000) |> sample_n(n_flights_sample/3)
    fit <- rbind(fmorn, fmid, faft)
    sub_flights <- flights |> filter(mod_start %in% fit$mod_start)

  }else{

    fit <- flights_list
    sub_flights <- flights

  }

  # select subset of OTMs of interest
  sub_otms <- sample(unique(m_otm_splines$otm_id), combs$n_otms[i])

  # select subset of OTM splines of interest
  sub_otm_splines <- m_otm_splines |> filter(knot_p == combs$knot_p[i]) |>
    filter(otm_id %in% sub_otms)

  # create a dataset to predict the temperature recorded by each OTM during each flight
  all_otms_times <- expand.grid(otm_id = sub_otms,
                                year = unique(fit$year),
                                doy = unique(fit$doy),
                                mod_start = unique(fit$mod_start),
                                mod_end = unique(fit$mod_end))

  # merge all otms times with metadata from subset of flights to be tested
  sub_otm_preds <- merge(all_otms_times, fit,
                         by = c("year", "doy","mod_start", "mod_end"))

  # add column for predicted operative temperatures
  sub_otm_preds$pred_op_temp <- NA

  # loop to predict operatuve temperatures for each OTM during each flight
  for(j in 1:nrow(sub_otm_preds)){

    # isolte the DOY and OTM id specific spline
    spec_spline <- sub_otm_splines |>
      filter(otm_id == sub_otm_preds$otm_id[j]) |>
      filter(year == sub_otm_preds$year[j]) |>
      filter(doy == sub_otm_preds$doy[j])

    # predict temperatures
    sub_otm_preds$pred_op_temp[j] <- mean(predict(spec_spline$spline[[1]],
                                                  sub_otm_preds$mod_start[j]:sub_otm_preds$mod_end[j])$y)



  }

  # build holder dataset for matches
  sub_matches <- sub_flights |> select(x,y) |> unique() |> mutate(otm_id = NA, error = NA)

  # loop to estimate best OTM match for each tile
  for(j in 1:nrow(sub_matches)){

    # filter data for specific tile
    tile_dat <- sub_flights |> filter(x == sub_matches$x[j]) |>
      filter(y == sub_matches$y[j])

    # merge tile data with otm_preds
    tile_dat <- merge(tile_dat, sub_otm_preds,
                      by = c("year", "doy", "mod_start", "mod_end"), all = TRUE)

    # find OTM match
    otm_match <- tile_dat |> group_by(otm_id) |>
      summarise(error = mean(abs(pred_op_temp - op_temp), na.rm = T)) |>
      filter(error == min(error, na.rm = T))

    # add info to holder matches data set
    sub_matches$otm_id[j] <- as.character(otm_match$otm_id[1])
    sub_matches$error[j] <- otm_match$error[1]

  }

  # add information columns
  sub_matches <- sub_matches |> mutate(n_flights = combs$n_flights[i],
                                       n_otms = combs$n_otms[i],
                                       knot_p = combs$knot_p[i],
                                       it = combs$it[i])

  # bind to all matches object
  all_matches <- bind_rows(all_matches, sub_matches)

  # print i if i is a multiple of 10
  if(i %% 48 == 0){
    print(i/480)
  }

}

# get list of actual OTM positions to merge
merge_otms <- otms |> select(otm_id, x, y) |> unique() |> rename(a_otm_id = otm_id)

# merge actual OTM positions with all matches data
val_data <- merge(all_matches, merge_otms, by = c("x", "y"))

## Validation ----

# repeat each match 100 times
val_data <- do.call("rbind", replicate(100,val_data, simplify = FALSE))

# add random doy and mod to test to val_data
val_data$doy <- sample(c(237:238), nrow(val_data), replace = TRUE)
val_data$mod <- sample(seq(0,1380, by = 60), nrow(val_data), replace = TRUE)

# expand grid to obtain unique combinations from which to extract data
otm_time_combs <- val_data |> select(a_otm_id, doy, mod, doy, knot_p) |>
  unique() |> mutate(op_temp = NA) |> rename(otm_id = a_otm_id)

# loop to pre-calculate operative temperatures
for(i in 1:nrow(otm_time_combs)){

  # filter spline of interest
  spline <- m_otm_splines |> filter(otm_id == otm_time_combs$otm_id[i],
                                    knot_p == otm_time_combs$knot_p[i],
                                    doy == otm_time_combs$doy[i])

  # predict operative temperature
  otm_time_combs$op_temp[i] <- predict(spline$spline[[1]],
                                       otm_time_combs$mod[i])$y

  if(i %% 634 == 0){
    print(round(i/6336,2))
  }

}

# get observed and prediction data
actual <- otm_time_combs |> rename(a_otm_id = otm_id, obs_op_temp = op_temp)
prediction <- otm_time_combs |> rename(pred_op_temp = op_temp)

# merge validation data with actual observations and predictions
val_data <- merge(val_data, actual, by = c("a_otm_id", "doy", "mod", "knot_p"))
val_data <- merge(val_data, prediction, by = c("otm_id", "doy", "mod", "knot_p"))

# rename data and save
val2_data <- val_data
save(val2_data, file = "data/val2_data.RData")


## Summary Table (to produce Tables 1, S3, S4) ----

val2_data |> # change to val1_data for validation 1
  #filter(mod > 7*60) |> filter(mod < 19*60) |> # remove or not for night time or daytime only
  mutate(minute = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) |>
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) |>
  group_by(n_flights, n_otms, knot_p) |>
  summarise(mean_error = mean(pred_op_temp - obs_op_temp),
            sd_error = sd(pred_op_temp - obs_op_temp),
            abs_mean_error = mean(abs(pred_op_temp - obs_op_temp)),
            abs_sd_error = sd(abs(pred_op_temp - obs_op_temp)))





















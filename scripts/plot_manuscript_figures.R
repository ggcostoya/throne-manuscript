
## Plotting figures for `throne` manuscript ##

## Packages --------------------------------------------------------------------

library(throne)
library(ggridges)
library(gridExtra)
library(tidyverse)
library(viridis)
library(metR)

## Figure 1 --------------------------------------------------------------------

# figure 1 was generated using tools outside of R by combining fragments of
# figures

## Figure 3 --------------------------------------------------------------------

# the `elevation`, `otms_metadata`, `matches_5`, `matches_20` data sets of the
# `throne` package are necessary for this figure.

# panel A left ----

fig_3_panel_A_left <- matches_5 %>%
  filter(!is.na(otm_id)) %>%
  group_by(otm_id) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(otm_id, cum_times))) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/5609)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/5609))) +
  geom_col(aes(y = 100 * (times/5609), fill = 100 * (times/5609)),
           col = "black") +
  ylab("Thermal variability of the site described (%)") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  scale_fill_viridis() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15)) +
  ggtitle("A")

## panel A right ----

fig_3_panel_A_right <- matches_20 %>%
  group_by(otm_id) %>%
  mutate(times = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = longitude, y = latitude, fill = 100 * (times/5609))) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = NA, linewidth = 0.5, bins = 20) +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = c(0.2, 0.85),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 15, color = "white")) +
  labs(fill = "Area described (%)") +
  ggtitle("A")

## panel B left ----

# # define orientation colors
orientation_colors <- c("N" = "darkblue", "NW" = "#7F468B","W" = "darkorange",
                        "SW" = "#FCCE7B", "S" = "gold", "SE" = "#90B122",
                        "E" = "forestgreen", "NE" = "#11458B", "Flat" = "maroon")

# merge matches with otm metadata
matches_5_metadata <- merge(matches_5, otms_metadata, by = c("otm_id"))

# plot
fig_3_panel_B_left <- matches_5_metadata %>%
  filter(!is.na(otm_id)) %>%
  group_by(orientation) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(orientation, cum_times))) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/5609)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/5609))) +
  geom_col(aes(y = 100 * (times/5609), fill = orientation), col = "black") +
  ylab("Thermal variability of the site described (%)") +
  xlab("OTM orientation") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  scale_fill_manual(values = orientation_colors) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 15)) +
  ggtitle("B")

# panel B right ----

# merge matches with otm metadata
matches_20_metadata <- merge(matches_20, otms_metadata, by = c("otm_id"))

# plot
fig_3_panel_B_right <- ggplot() +
  geom_tile(data = matches_20_metadata,
            aes(x = longitude.x, y = latitude.x, fill = orientation)) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = "white", alpha = 0.5, linewidth = 0.5, bins = 20) +
  geom_text_contour(data = elevation, aes(x = longitude, y = latitude, z = round(elevation)),
                    skip = 1, stroke = 0.2, col = "black", size = 2, bins = 10) +
  geom_segment(aes(x = -119.6266, xend = -119.6266,  y = 39.868375, yend = 39.86850),
               data = NULL, col = "black", linewidth = 1,
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_text(aes(x = -119.6266 - 0.000075,  y = 39.8684375),
            data = NULL, col = "black", label = "N", size = 8) +
  scale_fill_manual(values = orientation_colors) +
  theme_void() +
  theme(legend.position = c(0.15, 0.82),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 15, color = "white")) +
  labs(fill = "Orientation") +
  ggtitle("D")

## combine panels ----

grid.arrange(fig_3_panel_A_left, fig_3_panel_A_right,
             fig_3_panel_B_left, fig_3_panel_B_right, ncol = 2, nrow = 2)




## Figure S3 -------------------------------------------------------------------

# format digits on latitude and longitude columns for otms_splines
otms_splines$latitude <- as.numeric(format(otms_splines$latitude, nsmall = 5))
otms_splines$longitude <- as.numeric(format(otms_splines$longitude, nsmall = 5))
otms_splines$tile_id <- otms_splines$latitude * otms_splines$longitude

# define no gcps flights data
no_gcps_path <- "C:/Users/ggarc/OneDrive/research/throne-manuscript/no_gcp_flights"

# generate new metadata file for NO GCP flights (same as with GCP flights)
flights_metadata_no_gcps <- flights_metadata %>%
  mutate(flight_id = str_sub(list.files(no_gcps_path), end = -5))

# read and process no gcp flights data
flights_data_no_gcp <- rnp_flights_data(
  path = no_gcps_path, metadata = flights_metadata_no_gcps, digits = 5)

# filter flights data no gcps in locations were OTMs were present
flights_data_no_gcp_plot <- flights_data_no_gcp %>%
  mutate(tile_id = latitude * longitude) %>%
  filter(tile_id %in% unique(otms_splines$tile_id)) %>%
  rename(ir_temp_no_gcp = ir_temp)

# filter flights_data with gcps in locations were OTMs were present
flights_data_plot <- flights_data %>%
  mutate(tile_id = latitude * longitude) %>%
  filter(tile_id %in% unique(otms_splines$tile_id)) %>%
  rename(ir_temp_gcp = ir_temp)

# filter post-correction data with gcps in locations were OTMs were present
flights_data_corr_plot <- correct_flights_data(flights_data, otms_splines) %>%
  mutate(tile_id = latitude * longitude) %>%
  filter(tile_id %in% unique(otms_splines$tile_id)) %>%
  rename(ir_temp_corr = op_temp)

# merge data for plot
figs3_data <- merge(flights_data_no_gcp_plot, flights_data_plot,
                    by = c("longitude", "latitude", "tile_id", "year", "doy", "mod_start", "mod_end"))
figs3_data <- merge(figs3_data, flights_data_corr_plot,
                    by = c("longitude", "latitude", "tile_id", "year", "doy", "mod_start", "mod_end"))

# add column for operative temperature
figs3_data$op_temp <- rep(NA, nrow(figs3_data))

# loop to estimate temperature from OTMs
for(i in 1:nrow(figs3_data)){

  # get the OTM on the tile
  otms_spline_specifc <- otms_splines %>%
    filter(tile_id == figs3_data$tile_id[i]) %>%
    filter(doy == figs3_data$doy[i])

  # predict operative temperature during flight period
  figs3_data$op_temp[i] <- mean(predict(otms_spline_specifc$spline[[1]], c(figs3_data$mod_start[i]:figs3_data$mod_end[i]))$y)

}

# plot
figs3_data %>%
  tidyr::pivot_longer(c(ir_temp_no_gcp, ir_temp_gcp, ir_temp_corr), names_to = "type", values_to = "ir_temp") %>%
  mutate(name = ifelse(type == "ir_temp_corr", "C) GCP & Corrected",
                       ifelse(type == "ir_temp_gcp", "B) GCP", "A) NO GCP"))) %>%
  ggplot(aes(x = op_temp, y = ir_temp)) +
  geom_point(aes(col = ((mod_start + mod_end)/2)/60), alpha = 0.5, size = 2) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  geom_smooth(method = "lm", col = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  xlab("Operative Temperature (°C)") +
  ylab("Surface Temperature (°C)") +
  facet_wrap(~name) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12)) +
  labs(colour = "Hour")

# estimate linear fit parameters:
summary(lm(ir_temp_corr ~ op_temp, data = figs3_data))

## Figure S4 ------------------------------------------------------------------

# the `figs3_data` object generated for Figure S3 is also necessary for Figure S4

figs3_data %>%
  ggplot(aes(x = mod_start/60, y = op_temp - ir_temp_gcp)) +
  geom_abline(intercept = 0, slope = 0, linetype = 2) +
  geom_jitter(aes(col = mod_start/60), width = 0.05, height = 0, alpha = 0.5, size = 2) +
  stat_summary(aes(fill = mod_start/60), shape = 21, size = 0.75, linewidth = 1) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  scale_fill_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  ylab("Bias (Operative - Surface Temmperature; °C)") +
  xlab("Hour of the day") +
  scale_x_continuous(breaks = seq(8, 20, 2)) +
  theme_minimal() +
  theme(axis.line = element_line(),
        axis.ticks = element_line()) +
  labs(colour = "Hour", fill = "Hour")

## Figure S5 ------------------------------------------------------------------

# the `elevation` dataset package is necessary for this figure.

# the `matches_5_metadata` and `matches_20_metadata` generated for Figure 3 are
# also needed for figure S5.

# left panel ----
fig_s5_left <- matches_5_metadata %>%
  filter(!is.na(otm_id)) %>%
  group_by(microhabitat) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(microhabitat, cum_times))) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/5609)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/5609))) +
  geom_col(aes(y = 100 * (times/5609), fill = microhabitat),
           col = "black") +
  ylab("Thermal variability of the site described (%)") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15))

## right panel ----
fig_s5_right <- matches_20_metadata %>%
  ggplot() +
  geom_tile(aes(x = longitude.x, y = latitude.x, fill = microhabitat)) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = "white", linewidth = 0.5, bins = 20) +
  geom_text_contour(data = elevation, aes(x = longitude, y = latitude, z = round(elevation)),
                    skip = 1, stroke = 0.2, col = "black", size = 2, bins = 10) +
  theme_void() +
  theme(legend.position = c(0.2, 0.85),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 15, color = "white")) +
  labs(fill = "Area described (%)")

## combine panels ----
grid.arrange(fig_s5_left, fig_s5_right, ncol = 2)


## Figure S5 ------------------------------------------------------------------

# predict the thermal landscape
prediction_237 <- predict_thermal_landscape(matches = matches_20,
                                            otm_splines = otms_splines,
                                            doy = 237, mod = seq(0,1440, by = 60))
# plot
prediction_237 %>%
  filter(mod %in% seq(6*60,21*60,by = 60)) %>%
  filter(!is.na(mod)) %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~mod/60) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "top",strip.text = element_text(size = 12)) +
  guides(fill = guide_colorbar(title = "Predicted Operative Temperature (C)"))

## Figure S7 & S8 --------------------------------------------------------------

# load validation data
load("data/val_data.RData")

# all-day vs only day-time color
ridge_color <- "royalblue" # for figure S5
#ridge_color <- "darkorange" # for figure S6

fig_s5 <- val_data %>%
  #filter(mod > 7*60) %>% filter(mod < 20*60) %>% # for day-time only (figure S7)
  mutate(mod = round(mod/96)) %>% # rounding every 15 minutes
  group_by(n_flights, n_otms, knot_p, mod) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ungroup() %>%
  mutate(knot_p = round(knot_p, digits = 3)) %>%
  ggplot(aes(x = pred_op_temp - obs_op_temp, y = as.factor(knot_p))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, fill = ridge_color, col = NA, scale = 0.75) +
  stat_summary(fun.data = mean_sdl, size = 0.25) +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  xlab("Prediction Error (°C)") +
  ylab("Knot_p parameter") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.border = element_rect(fill = NA),
        axis.ticks = element_line())

grid.arrange(fig_s5, top = "Flights used", right = "OTMs used")

## Figure S9 -------------------------------------------------------------------

# load validation data
load("data/val_data.RData")

val_data %>%
  mutate(mod = round(mod/96)) %>% # rounding every 15 minutes
  group_by(n_flights, n_otms, knot_p, mod) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(knot_p = round(knot_p, digits = 3)) %>%
  ggplot(aes(x = (mod*96)/60, y = pred_op_temp - obs_op_temp)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(group = as.factor(knot_p), col = as.factor(knot_p)),
            linewidth = 1.25, alpha = 0.75) +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = NULL, labels = NULL, name = "OTMs used")) +
  scale_x_continuous(expand = c(0,0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL, name = "Flights used")) +
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "darkblue")) +
  ylab("Prediction Error (°C)") +
  xlab("Hour of the day") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.ticks = element_line(),
        axis.title.y.right = element_text(margin = margin(t = 0,r = 0, b= 0, l = 10)),
        axis.title.x.top = element_text(margin = margin(t = 0,r = 0, b= 10, l = 0))) +
  labs(colour = "Knots / Hour")




## Plot figures for `throne` manuscript ##

## Packages --------------------------------------------------------------------

library(throne)
library(ggridges)
library(gridExtra)
library(tidyverse)
library(viridis)
library(ggpubr)

## Figure 1 & 2 ----------------------------------------------------------------

# these figures were generated outside of R

## Figure 3 --------------------------------------------------------------------

# load data
load("data/matches_val1.RData")
load("data/matches_val2.RData")

# panel A (val 1 % of variance explained area)
ntile_1 = nrow(matches_val1)
panela <- matches_val1 |>
  filter(!is.na(otm_id)) |>
  group_by(otm_id) |>
  mutate(times = n()) |>
  mutate(times = 100 * (times/ntile_1)) |>
  ggplot(aes(x = x, y = y, fill = times)) +
  geom_raster() +
  theme_void() +
  scale_fill_viridis(name = "Area described (%)")

# panel B (val 2 % of variance explained area)
panelb <- matches_val1 %>%
  group_by(otm_id) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  mutate(n_otm = seq(1,nrow(.),1)) |>
  ggplot(aes(x = n_otm)) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/ntile_1)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/ntile_1))) +
  geom_col(aes(y = 100 * (times/ntile_1), fill = 100 * (times/ntile_1)),
           col = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,101)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,72)) +
  scale_fill_viridis() +
  ylab("Thermal variability described (%)") +
  xlab("Number of OTMs") +
  theme_classic() +
  theme(legend.position = "none")

# panel C (val 2 % of variance explained area)
ntile_2 = nrow(matches_val2)
panelc <- matches_val2 |>
  filter(!is.na(otm_id)) |>
  group_by(otm_id) |>
  mutate(times = n()) |>
  mutate(times = 100 * (times/ntile_2)) |>
  ggplot(aes(x = x, y = y, fill = times)) +
  geom_tile() +
  theme_void() +
  scale_fill_viridis(name = "Area described (%)")

# panel D (val 2 % of variance explained area)
paneld <- matches_val2 %>%
  group_by(otm_id) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  mutate(n_otm = seq(1,nrow(.),1)) |>
  ggplot(aes(x = n_otm)) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/ntile_2)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/ntile_2))) +
  geom_col(aes(y = 100 * (times/ntile_2), fill = 100 * (times/ntile_2)),
           col = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,101)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,28)) +
  scale_fill_viridis() +
  ylab("Thermal variability described (%)") +
  xlab("Number of OTMs") +
  theme_classic() +
  theme(legend.position = "none")

ggarrange(panela, panelb, panelc, paneld, ncol = 2, nrow = 2,
          labels = c("A", "B", "C", "D"))
## Figure S3 -------------------------------------------------------------------

# load necessary data
load("data/otms_data_val1.RData")

# plot
otms_data_val1 %>%
  group_by(doy, otm_id) %>%
  summarise(mean_op_temp = mean(op_temp, na.rm = T),
            max_op_temp = max(op_temp, na.rm = T),
            min_op_temp = min(op_temp, na.rm = T)) %>%
  ungroup() %>%
  group_by(doy) %>%
  summarise(mean_op_temp = mean(mean_op_temp, na.rm = T),
            max_op_temp = mean(max_op_temp, na.rm = T),
            min_op_temp = mean(min_op_temp, na.rm = T)) %>%
  ggplot(aes(x = doy, y = mean_op_temp)) +
  geom_segment(aes(y = min_op_temp, yend = max_op_temp,
                   col = mean_op_temp), alpha = 0.25,
               linewidth = 1) +
  geom_line(aes(col = mean_op_temp)) +
  scale_color_gradient(low = "darkblue", high = "darkorange") +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  xlab("Day of the year") +
  ylab("Operative temperature (°C)")

## Figure S4 -------------------------------------------------------------------

# load data for validation 1 figure (top)
load("data/flights_data_val1_no_gcp.RData")
load("data/flights_data_val1_gcp.RData")
load("data/flights_data_val1_gcp_corr.RData")

# load data for validation 2 figure (bottom)
load("data/flights_data_val2_no_gcp.RData")
load("data/flights_data_val2_gcp.RData")
load("data/flights_data_val2_gcp_corr.RData")

# load OTM splines
load("data/otms_splines_val1.RData") # for validation 1
load("data/otms_splines_val2.RData") # for validation 2

# assign objects
no_gcp <- flights_data_val1_no_gcp # change depending on validation to plot
gcp <- flights_data_val1_gcp # change depending on validation to plot
gcp_corr <- flights_data_val1_gcp_corr # change depending on validation to plot
splines <- otms_splines_val1 # change depending on validation to plot

# use eval_flights_correction to get matching data
no_gcp <- eval_flights_correction(no_gcp, splines) |> mutate(type = "no_gcp")
gcp <- eval_flights_correction(gcp, splines) |> mutate(type = "gcp")
gcp_corr <- eval_flights_correction(
  flights_data = gcp_corr |> rename(surf_temp = op_temp),
  otm_splines = splines) |>
  mutate(type = "gcp_corr")

# run regressions to determine intercept, slope and R^2
# (values were added to the figure outside of R)
summary(lm(surf_temp ~ op_temp, data = no_gcp))
summary(lm(surf_temp ~ op_temp, data = gcp))
summary(lm(surf_temp ~ op_temp, data = gcp_corr))

# generate dataset for figure S4
figs4_data <- bind_rows(no_gcp, gcp, gcp_corr) |>
  mutate(type = case_when(
    type == "no_gcp" ~ "No GCP",
    type == "gcp" ~ "GCP",
    type == "gcp_corr" ~ "GCP + Corrected"
  )) |>
  mutate(type = factor(type, levels = c("No GCP", "GCP", "GCP + Corrected")))

# plot figure S4
figs4_data |>
  filter(surf_temp > 5) |> # outlier point on validation 2
  ggplot(aes(x = op_temp, y = surf_temp, col = mod_start/60)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",
                        midpoint = 12, name = "Hour") +
  xlab("Operative Temperature (°C)") +
  ylab("Surface Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.01),
        strip.text = element_text(size = 12)) +
  facet_grid(cols = vars(type))

## Figure S5 -------------------------------------------------------------------

# load validation 1 and validation 2 flights data and otm splines
load(easypaste("data/flights_data_val1_gcp.RData"))
load(easypaste("data/flights_data_val2_gcp.RData"))
load(easypaste("data/otms_splines_val1.RData"))
load(easypaste("data/otms_splines_val2.RData"))

# use `eval_flights_correction` to obtain correction data from val2 flights
corr_data_val1 <- eval_flights_correction(flights_data = flights_data_val1_gcp,
                                          otm_splines = otms_splines_val1)
corr_data_val1 <- corr_data_val1 %>% mutate(validation = "A) Validation 1")

# use `eval_flights_correction` to obtain correction data from val2 flights
corr_data_val2 <- eval_flights_correction(flights_data = flights_data_val2_gcp,
                                          otm_splines = otms_splines_val2)
corr_data_val2 <- corr_data_val2 %>% mutate(validation = "B) Validation 2")

# combine datasets
corr_data <- bind_rows(corr_data_val1,corr_data_val2)

# plotting
corr_data %>%
  mutate(hour = mod_start / 60) %>%
  ggplot(aes(x = hour, y = op_temp - surf_temp)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_jitter(aes(col = hour),
              width = 0.05, height = 0, alpha = 0.5, size = 2) +
  stat_summary(aes(fill = hour), shape = 21, size = 0.75, linewidth = 1) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",
                        midpoint = 12) +
  scale_fill_gradient2(low = "black", mid = "orange", high = "darkblue",
                       midpoint = 12) +
  scale_x_continuous(breaks = seq(8, 20, 2)) +
  ylab("Bias (Operative - Surface Temperature, °C)") +
  xlab("Hour of the day") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(),
    axis.ticks = element_line(),
    strip.text = element_text(
      hjust = 0,
      size = 11,
      colour = "black"),
    panel.grid = element_line(linewidth = 0.01)
  ) +
  facet_wrap(~validation, ncol = 1, scales = "free_x")

## Figure S6 ----

# load validation 1 and validation 2 flights data and otm splines
load(easypaste("data/flights_data_val1_gcp.RData"))
load(easypaste("data/otms_splines_val1.RData"))

# use `eval_flights_correction` to obtain correction data from val2 flights
corr_data_val1 <- eval_flights_correction(flights_data = flights_data_val1_gcp,
                                          otm_splines = otms_splines_val1)

# plotting
corr_data_val1 %>%
  ggplot(aes(x = doy, y = op_temp - surf_temp)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_jitter(aes(col = doy),
              width = 1, height = 0, alpha = 0.5, size = 2) +
  stat_summary(aes(fill = doy), shape = 21, size = 0.75, linewidth = 1) +
  scale_color_gradient(low = "forestgreen", high = "orange") +
  scale_fill_gradient(low = "forestgreen", high = "orange") +
  ylab("Bias (Operative - Surface Temperature, °C)") +
  xlab("Day of the year") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(),
    axis.ticks = element_line(),
    strip.text = element_text(
      hjust = 0,
      size = 11,
      colour = "black"),
    panel.grid = element_line(linewidth = 0.01)
  )

## Figure S7 -------------------------------------------------------------------

# load validation 1 and validation 2 flights data and otm splines
load(easypaste("data/matches_val1.RData"))
load(easypaste("data/otms_splines_val1.RData"))

# predict thermal landscapes
pred <- predict_thermal_landscape(matches = matches_val1,
                                  otm_splines = otms_splines_val1,
                                  doy = c(107, 147, 187, 227),
                                  mod = c(0,8*60,12*60,17*60,19*60))

# plot thermal landscape prediction
figS7 <- pred %>%
  filter(!is.na(pred_op_temp)) |>
  ggplot(aes(x = x, y = y, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom"
  ) +
  facet_grid(cols = vars(mod/60), rows = vars(doy)) +
  guides(fill = guide_colorbar(title = "Predicted operative temperature (°C)"))

grid.arrange(figS7, top = "Hour of the day", right = "Day of the year")

## Figure S8, S9, S10 & S11 ----------------------------------------------------------

# load validation data
load("data/val1_data.RData") # for validation 1
load("data/val2_data.RData") # for validation 2
val_data <- val2_data # change depending on the validation to plot

# colors
ridge_color <- "royalblue" # for nighttime + daytime (Figure S8)
#ridge_color <- "darkorange" # for daytime only (Figure S9)

# plot figure
fig <- val_data |>
  #filter(mod >= 7*60 & mod <= 20*60) |> # for day-time only
  mutate(mod = round(mod/96)) |> # round every 15 minutes
  group_by(n_flights, n_otms, knot_p, mod) |>
  summarise(obs_op_temp = mean(obs_op_temp),
            pred_op_temp = mean(pred_op_temp)) |>
  ungroup() |>
  mutate(knot_p = round(knot_p, digits = 3)) |>
  ggplot(aes(x = pred_op_temp - obs_op_temp, y = as.factor(knot_p))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, fill = ridge_color, col = NA, scale = 0.7) +
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

grid.arrange(fig, top = "Flights used", right = "OTMs used")

## Figure S12 ------------------------------------------------------------------

# load validation data
load("data/val1_data.RData") # for validation 1
load("data/val2_data.RData") # for validation 2
val_data <- val1_data # change depending on the validation to plot

val_data |>
  mutate(mod = round(mod/96)) |> # rounding every 15 minutes
  group_by(n_flights, n_otms, knot_p, mod) |>
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) |>
  mutate(knot_p = round(knot_p, digits = 3)) |>
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

## Figure S13 ------------------------------------------------------------------

# load validation data
load("data/matches_val2.RData")

# load otms_splines data
load("data/otms_splines_val2.RData")
otms_positions <- otms_splines_val2 |> select(x, y) |> unique()

# prepare density data
error <- matches_val2$error
density <- density(error, n = 2^12)

# plot density
figs13a <- ggplot(data.frame(x = density$x, y = density$y),
                 aes(x,y)) +
  geom_segment(aes(xend = x, yend = 0, col = x)) +
  geom_line() +
  scale_color_gradient(low = "white", high = "darkred", limits = c(0,12)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  xlab("Matching error (°C)") +
  ylab("Density") +
  ggtitle("A")

# plot panel B
figs13b <- ggplot() +
  geom_raster(data = matches_val2,aes(x = x, y = y, fill = error)) +
  geom_point(data = otms_positions, aes(x = x, y = y)) +
  scale_fill_gradient(low = "white", high = "darkred", limits = c(0,12)) +
  theme_void() +
  guides(fill = guide_colorbar(title = "Matching Error (°C)  ")) +
  ggtitle("B")

ggarrange(figs13a, figs13b, ncol = 2, nrow = 1, widths = c(1, 1))

## Figure S14 ------------------------------------------------------------------

# load otms_splines data
load("data/otms_data_val2.RData")

# extract spline for one OTM
otm_x <- otms_data_val2 %>% filter(otm_id %in% c("OTM02", "OTM03")) %>% filter(doy == 237)

# define holder dataset
preds <- data.frame(x = c(), y = c(), knot_p = c())
knots <- c(0.5, 0.06667, 0.0333, 0.016667)

# loop to generate data from splines
for(i in 1:length(knots)){

  otm_spline <- gen_otm_splines(otm_data = otm_x, knot_p = knots[i])
  otm_spline <- otm_spline |> filter(otm_id == "OTM02")
  pred <- as.data.frame(predict(otm_spline$spline[[1]], seq(0,1440)))
  pred$knot_p <- rep(knots[i], nrow(pred))
  preds <- rbind(preds, pred)

}

# filter for data of only one OTM
otm_x <- otm_x |> filter(otm_id == "OTM02")
preds <- preds |> mutate(knot_p = paste("Knots/h = ", as.factor(round((knot_p*1440)/24, digits = 2))))

# plotting
ggplot() +
  geom_point(data = otm_x, aes(x = mod/60, y = op_temp), size = 1, alpha = 0.25) +
  geom_line(data = preds, aes(x = x/60, y = y, alpha = 0.5), size = 1.25, col = "orange") +
  xlab("Hour of the day") +
  ylab("Operative temperature (C)") +
  theme_classic() +
  facet_wrap(~knot_p) +
  theme(
    legend.position = "none",
    panel.border = element_rect(fill = NA)
  )

## Figure S15 ------------------------------------------------------------------

# read flights metadata
flights_metadata_val2 <- read.csv(easypaste("data/flights_metadata_val2.csv"))

# define path to gcp corrected flights
flights_path_gcp <- easypaste("flights_data/validation_2/gcp")

# process flights at different resolutions
res1 <- rnp_flights_data(path = flights_path_gcp, metadata = flights_metadata_val2, resolution = 1)
res05 <- rnp_flights_data(path = flights_path_gcp, metadata = flights_metadata_val2, resolution = 0.5)
res4 <- rnp_flights_data(path = flights_path_gcp, metadata = flights_metadata_val2, resolution = 4)

# add resolution columns
res1 <- res1 |> mutate(res = 1)
res05 <- res05 |> mutate(res = 0.5)
res4 <- res4 |> mutate(res = 4)

# combine datasets
res <- bind_rows(res1, res05, res4)
colnames(res) <- c("x","y","year","doy","mod_start","mod_end","surf_temp", "res")

res |>
  filter(mod_start == 515) |>
  filter(surf_temp > 12) |>
  ggplot(aes(x = x, y = y, fill = surf_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~paste(res,"m^2")) +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12)) +
  guides(fill = guide_colorbar(title = "Surface temperature (°C)"))



## Figures included in response to reviewers -----------------------------------

# load data
load("data/otms_data_val1.RData")

# plot
otms_data_val1 |>
  filter(doy %in% c(125, 127)) |>
  mutate(rain = ifelse(doy == 125, "Overcast + Rain", "Clear sky")) |>
  ggplot(aes(x = mod/60, y = op_temp)) +
  geom_point(aes(col = rain), alpha = 0.5) +
  facet_wrap(~paste("Day of the year ",doy, sep = "")) +
  geom_smooth(alpha = 0, col = "black", show_guide = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c("orange", "skyblue4")) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.ticks = element_line(),
    strip.text = element_text(size = 12),
    panel.border = element_rect(fill = NA),
    legend.title = element_blank(),
    panel.grid = element_line(linewidth = 0.01)
  ) +
  xlab("Hour of the day") +
  ylab("Operative temperature (°C)") +
  guides(color = guide_legend(override.aes = list(size=5)))

# load datasets
load("data/matches_val1.RData")
load("data/otms_splines_val1.RData")

# predict thermal landscape and plot
predict_thermal_landscape(
  matches = matches_val1,
  otm_splines = otms_splines_val1,
  doy = c(125,127),
  mod = 13*60) |>
  mutate(doy = ifelse(doy == 125, "Day of the year 125 (Overcast +Rain)","Day of the year 127 (Clear sky)")) |>
  ggplot(aes(x = x, y = y, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma", name = "Predicted operative temperature (C)") +
  facet_wrap(~doy) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    plot.title = element_text(margin = margin(1,1,20,1))
  ) +
  ggtitle("Predicted operative thermal landscape at 13:00")

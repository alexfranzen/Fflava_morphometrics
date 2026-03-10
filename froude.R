## set wd and load dplyer

#setwd("~/Dropbox (Univ. of Oklahoma)/Fusconaia_project_data/biomechanical_experiments/stream_flow_metrics")
setwd("/Volumes/Extreme SSD/biomechanical_experiments/stream_flow_metrics")
library(dplyr)
library(ggplot2)
library(patchwork)
library(readxl)

## read in data

Hazelgreen <- read.csv(file = 'USGS_06928000_Gasconade_River_Hazelgreen_metric.csv')
Rich_Fountain <- read.csv(file = 'USGS_06934000_Gasconade_River_Rich_Fountain_metric.csv')

## filter out any missing data and only main channel
 
main_channel <- c('Main', 'main', 'mc', 'Hazelgreen Main', 'main channel', 'Main channel', 'MAIN') # these are the possible values for the main channel
Hazelgreen_clean <- filter(Hazelgreen, !is.na(Hazelgreen$channel_width_ft)) # first remove and row that don't have channel width data
Hazelgreen_clean <- filter(Hazelgreen_clean, channel_name %in% main_channel) # only main channel rows

Rich_Fountain_clean <- filter(Rich_Fountain, !is.na(Rich_Fountain$channel_width_ft)) # remove na rows
Rich_Fountain_clean <- filter(Rich_Fountain_clean, channel_name %in% main_channel)

## Froude calculation
# For the Fr #, use Fr = u/sqrt(gd); u = mean flow velocity (m/s), g = gravity, d = flow depth (m)

FroudeNumber <- function(u, g, d) {
  Fr <- u/sqrt(g*d)
  return(Fr)
}

g <- 9.81 # gravity constant m/s^2

Hazelgreen_depth <- Hazelgreen_clean$channel_area_m2/Hazelgreen_clean$channel_width_m
Rich_Fountain_depth <- Rich_Fountain_clean$channel_area_m2/Rich_Fountain_clean$channel_width_m
Hazelgreen_vel <- Hazelgreen_clean$channel_velocity_ms
Rich_Fountain_vel <- Rich_Fountain_clean$channel_vel_ms

# Froude numbers for Hazelgreen
num_rows_hazelgreen <- length(Hazelgreen_vel)
Hazelgreen_Froude <- numeric(num_rows_hazelgreen)

for (i in 1:num_rows_hazelgreen) {
  Hazelgreen_Froude[i] <- FroudeNumber(Hazelgreen_vel[i], g, Hazelgreen_depth[i])
}

# Froude numbers for Rich Fountain
num_rows_rich_fountain <- length(Rich_Fountain_vel)
Rich_Fountain_Froude <- numeric(num_rows_rich_fountain)

for (i in 1:num_rows_rich_fountain) {
  Rich_Fountain_Froude[i] <- FroudeNumber(Rich_Fountain_vel[i], g, Rich_Fountain_depth[i])
}

Hazelgreen_Froude  # Froude numbers for Hazelgreen
Rich_Fountain_Froude  # Froude numbers for Rich Fountain

## add Froude to data frame for each site

Hazelgreen_final <- cbind(Hazelgreen_clean, Hazelgreen_Froude)
colnames(Hazelgreen_final)[ncol(Hazelgreen_final)] <- "Fr"

Rich_Fountain_final <- cbind(Rich_Fountain_clean, Rich_Fountain_Froude)
colnames(Rich_Fountain_final)[ncol(Rich_Fountain_final)] <- "Fr"

write.csv(Hazelgreen_final, file = 'USGS_06928000_Gasconade_River_Hazelgreen_froude.csv')
write.csv(Rich_Fountain_final, file = 'USGS_06934000_Gasconade_River_Rich_Fountain_froude.csv')

plot(channel_flow_m3s~Fr, data = Hazelgreen_final)

## plot distribution of Gasconade Fr

Hazelgreen_final <- Hazelgreen_final %>% mutate(Location = "Hazelgreen")
Rich_Fountain_final <- Rich_Fountain_final %>% mutate(Location = "Rich Fountain")

# combine
froudes <- bind_rows(Hazelgreen_final, Rich_Fountain_final)

# plot (experimental Fr not marked)
ggplot(froudes, aes(x = Fr, fill = Location)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~Location, scales = "free") +
  labs(x = "Froude Number (Fr)", y = "Count", title = "Distribution of Froude Numbers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_fill_manual(values = c("steelblue", "darkorange"))
ggsave(filename = "froudes.svg")

# plot (experimental Fr marked with vertical red lines)

ggplot(froudes, aes(x = Fr, fill = Location)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  geom_vline(xintercept = c(0.09), color = "black", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = c(0.27, 0.31), color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~Location, scales = "free") +
  labs(x = "Froude Number (Fr)", y = "Count", title = "Distribution of Froude Numbers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("steelblue", "darkorange"))
ggsave(filename = "froudes_marked.svg")

## Reynolds calculation
# For the Re #, use Re = u*d/v; u = mean flow velocity (m/s), d = flow depth (m), v = kinematic viscosity of water (m2/s)

ReynoldsNumber <- function(u, d, v) {
  Re <- (u*d)/v
  return(Re)
}

v <- 0.0000011088 # kinematic viscosity of water (m2/s) at 16C 

Hazelgreen_depth <- Hazelgreen_clean$channel_area_m2/Hazelgreen_clean$channel_width_m
Rich_Fountain_depth <- Rich_Fountain_clean$channel_area_m2/Rich_Fountain_clean$channel_width_m
Hazelgreen_vel <- Hazelgreen_clean$channel_velocity_ms
Rich_Fountain_vel <- Rich_Fountain_clean$channel_vel_ms

# Reynolds numbers for Hazelgreen
num_rows_hazelgreen <- length(Hazelgreen_vel)
Hazelgreen_Re <- numeric(num_rows_hazelgreen)

for (i in 1:num_rows_hazelgreen) {
  Hazelgreen_Re[i] <- ReynoldsNumber(Hazelgreen_vel[i], Hazelgreen_depth[i], v)
}

# Reynolds numbers for Rich Fountain
num_rows_rich_fountain <- length(Rich_Fountain_vel)
Rich_Fountain_Re <- numeric(num_rows_rich_fountain)

for (i in 1:num_rows_rich_fountain) {
  Rich_Fountain_Re[i] <- ReynoldsNumber(Rich_Fountain_vel[i], Rich_Fountain_depth[i], v)
}

Hazelgreen_Re  # Reynolds numbers for Hazelgreen
Rich_Fountain_Re  # Reynolds numbers for Rich Fountain

## add Reynolds to data frame for each site

Hazelgreen_all <- cbind(Hazelgreen_final, Hazelgreen_Re)
colnames(Hazelgreen_all)[ncol(Hazelgreen_all)] <- "Re"

Rich_Fountain_all <- cbind(Rich_Fountain_final, Rich_Fountain_Re)
colnames(Rich_Fountain_all)[ncol(Rich_Fountain_all)] <- "Re"

write.csv(Hazelgreen_all, file = 'USGS_06928000_Gasconade_River_Hazelgreen_all.csv')
write.csv(Rich_Fountain_all, file = 'USGS_06934000_Gasconade_River_Rich_Fountain_all.csv')

plot(channel_flow_m3s~Re, data = Hazelgreen_all)

## plot distribution of Gasconade Re

Hazelgreen_all <- Hazelgreen_all %>% mutate(Location = "Hazelgreen")
Rich_Fountain_all <- Rich_Fountain_all %>% mutate(Location = "Rich Fountain")

# combine
Reynolds <- bind_rows(Hazelgreen_all, Rich_Fountain_all)
Reynolds <- subset(Reynolds, Re <=1000000) # <= 1000000

# plot (experimental Re not marked)
ggplot(Reynolds, aes(x = Re, fill = Location)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~Location, scales = "free") +
  labs(x = "Reynolds Number (Re)", y = "Count", title = "Distribution of Reynolds Numbers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_fill_manual(values = c("steelblue", "darkorange"))
ggsave(filename = "Reynolds.svg")

# plot (experimental Re marked with vertical red lines)

ggplot(Reynolds, aes(x = Re, fill = Location)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  geom_vline(xintercept = c(94900), color = "black", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = c(284800, 332300), color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~Location, scales = "free") +
  labs(x = "Reynolds Number (Re)", y = "Count", title = "Distribution of Reynolds Numbers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("steelblue", "darkorange"))
ggsave(filename = "Reynolds_marked.svg")

### Burrowing frequency analysis for Big River and SmallRiver form Pigtoes ###
## Code for Franzen et al. 202X Pigtoe burrowing experiment

#### PRELUDE ####
# load packages

# set working directory!!
# setwd("~/unionidae/hall_sensor_analysis") # local directory
#setwd("C:/Users/Quadrula/Univ. of Oklahoma Dropbox/Alex Franzen/Fusconaia_project_data/biomechanical_experiments/hall_sensor_analysis")
setwd("~/Univ. of Oklahoma Dropbox/Alex Franzen/Fusconaia_project_data/biomechanical_experiments/hall_sensor_analysis")
library(ggplot2)
library(tidyverse)
library(gridExtra)
rm(list=ls())

#### IMPORT DATA ####
# read in data
# clean up data
# data is formatted weirdly, presumably because of the way the data logger stores it. 
## BIG RIVER IND A
BigRiverA <- read.csv(file = 'BigRiver_A_6Dec2023/BigRiver_A_20231206.txt', sep = ",", header = T)
BigRiverA <- BigRiverA[-c(2,3),] # remove 2nd and 3rd rows 
BigRiverA <- BigRiverA[,-c(3,5,6,7,8)] # remove unit columns
names(BigRiverA) <- BigRiverA[1,] # make the the first row (the actual header) the header
BigRiverA <- BigRiverA[-c(1),] # delete the first row since it's a dupe

## BIG RIVER IND B
BigRiverB <- read.csv(file = 'BigRiver_B_8Dec2023/BigRiver_B_20231208.txt', sep = ",", header = T)
BigRiverB <- BigRiverB[-c(2,3),] # remove 2nd and 3rd rows 
BigRiverB <- BigRiverB[,-c(3,5,6,7,8)] # remove unit columns
names(BigRiverB) <- BigRiverB[1,] # make the the first row (the actual header) the header
BigRiverB <- BigRiverB[-c(1),] # delete the first row since it's a dupe

## BIG RIVER IND C
BigRiverC <- read.csv(file = 'BigRiver_C_11Dec2023/BigRiver_C_20231211.txt', sep = ",", header = T)
BigRiverC <- BigRiverC[-c(2,3),] # remove 2nd and 3rd rows 
BigRiverC <- BigRiverC[,-c(4,5,6,7,8)] # remove unit columns
names(BigRiverC) <- BigRiverC[1,] # make the the first row (the actual header) the header
BigRiverC <- BigRiverC[-c(1),] # delete the first row since it's a dupe

## SMALL RIVER IND A
SmallRiverA <- read.csv(file = 'SmallRiver_A_5Dec2023/SmallRiver_A_20231205.csv', sep = ",", header = T)
SmallRiverA <- SmallRiverA[-c(2,3),] # remove 2nd and 3rd rows 
SmallRiverA <- SmallRiverA[,-c(3,5,6,7,8)] # remove unit columns
names(SmallRiverA) <- SmallRiverA[1,] # make the the first row (the actual header) the header
SmallRiverA <- SmallRiverA[-c(1),] # delete the first row since it's a dupe

## SMALL RIVER IND B
SmallRiverB <- read.csv(file = 'SmallRiver_B_7Dec2023/SmallRiver_B_7Dec2023.txt', sep = ",", header = T)
SmallRiverB <- SmallRiverB[-c(2,3),] # remove 2nd and 3rd rows 
SmallRiverB <- SmallRiverB[,-c(3,5,6,7,8)] # remove unit columns
names(SmallRiverB) <- SmallRiverB[1,] # make the the first row (the actual header) the header
SmallRiverB <- SmallRiverB[-c(1),] # delete the first row since it's a dupe

## SMALL RIVER IND C
SmallRiverC <- read.csv(file = 'SmallRiver_C_12Dec2023/SmallRiver_C_20231212.txt', sep = ",", header = T)
SmallRiverC <- SmallRiverC[-c(2,3),] # remove 2nd and 3rd rows 
SmallRiverC <- SmallRiverC[,-c(4,5,6,7,8)] # remove unit columns
names(SmallRiverC) <- SmallRiverC[1,] # make the the first row (the actual header) the header
SmallRiverC <- SmallRiverC[-c(1),] # delete the first row since it's a dupe

# needs to change the format of the timestamp in each dataset to POSIXct
datasets <- c('BigRiverA','BigRiverB','BigRiverC','SmallRiverA','SmallRiverB','SmallRiverC')
for (i in datasets) {
  data <- get(i)
  data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP)
  assign(i, data)
}

# standardize column names
colnames(BigRiverA)[3] <- "VOLTAGE"
colnames(BigRiverB)[3] <- "VOLTAGE"
colnames(BigRiverC)[3] <- "VOLTAGE"
colnames(SmallRiverA)[3] <- "VOLTAGE"
colnames(SmallRiverB)[3] <- "VOLTAGE"
colnames(SmallRiverC)[3] <- "VOLTAGE"


##
#### BEGIN ANALYSIS ####
##

# simple plots

plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverA, main = "SmallRiverA", xlab = "Time", ylab = "Voltage (mV)")
plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverB, main = "SmallRiverB", xlab = "Time", ylab = "Voltage (mV)")
plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverC, main = "SmallRiverC", xlab = "Time", ylab = "Voltage (mV)")
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverA, main = "BigRiverA", xlab = "Time", ylab = "Voltage (mV)")
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverB, main = "BigRiverB", xlab = "Time", ylab = "Voltage (mV)")
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverC, main = "BigRiverC", xlab = "Time", ylab = "Voltage (mV)")

# compare filtered

SmallRiverA_filter <- SmallRiverA[-c(1:700),]
SmallRiverB_filter <- SmallRiverB[-c(1:700),]
SmallRiverC_filter <- SmallRiverC[-c(1:700),]
BigRiverA_filter <- BigRiverA[-c(1:700),]
BigRiverB_filter <- BigRiverB[-c(1:700),]
BigRiverC_filter <- BigRiverC[-c(1:700),]

#SmallRiverA_filter <- SmallRiverA %>% filter(TIMESTAMP >= as.POSIXct("2023-12-05 09:10:00")) # this is when the flow starts according to the flume log
#SmallRiverB_filter <- SmallRiverB %>% filter(TIMESTAMP >= as.POSIXct("2023-12-07 08:38:00")) # this is when the flow starts according to the flume log
#SmallRiverC_filter <- SmallRiverC %>% filter(TIMESTAMP >= as.POSIXct("2023-12-12 08:20:00")) # this is when the flow starts according to the flume log
#BigRiverA_filter <- BigRiverA %>% filter(TIMESTAMP >= as.POSIXct("2023-12-05 09:00:00"))
#BigRiverB_filter <- BigRiverB %>% filter(TIMESTAMP >= as.POSIXct("2023-12-05 09:00:00"))
#BigRiverC_filter <- BigRiverC %>% filter(TIMESTAMP >= as.POSIXct("2023-12-05 09:00:00"))

# export filtered data as csv

#write.csv(SmallRiverA_filter, file = "SmallRiverA_FILTERED.csv")
#write.csv(SmallRiverB_filter, file = "SmallRiverB_FILTERED.csv")
#write.csv(SmallRiverC_filter, file = "SmallRiverC_FILTERED.csv")
#write.csv(BigRiverA_filter, file = "BigRiverA_FILTERED.csv")
#write.csv(BigRiverB_filter, file = "BigRiverB_FILTERED.csv")
#write.csv(BigRiverC_filter, file = "BigRiverC_FILTERED.csv")

## mean observations across trials

observations <- c(nrow(SmallRiverA_filter), 
                 nrow(SmallRiverB_filter), 
                 nrow(SmallRiverC_filter), 
                 nrow(BigRiverA_filter), 
                 nrow(BigRiverB_filter), 
                 nrow(BigRiverC_filter))
mean_observations <- mean(observations)
mean_observations

SmallRiverA_filter$VOLTAGE <- as.numeric(SmallRiverA_filter$VOLTAGE)
SmallRiverB_filter$VOLTAGE <- as.numeric(SmallRiverB_filter$VOLTAGE)
SmallRiverC_filter$VOLTAGE <- as.numeric(SmallRiverC_filter$VOLTAGE)
BigRiverA_filter$VOLTAGE <- as.numeric(BigRiverA_filter$VOLTAGE)
BigRiverB_filter$VOLTAGE <- as.numeric(BigRiverB_filter$VOLTAGE)
BigRiverC_filter$VOLTAGE <- as.numeric(BigRiverC_filter$VOLTAGE)

# plot raw data
png("raw_hall_voltage_grid.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverA, type = "l",
     main = "SmallRiverA raw data", ylab = "Voltage (mV)")
# add a red box over the first 700 records to show what was omitted
with(SmallRiverA, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverB, type = "l",
     main = "SmallRiverB raw data", ylab = "Voltage (mV)")
with(SmallRiverB, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
plot(VOLTAGE ~ TIMESTAMP, data = SmallRiverC, type = "l",
     main = "SmallRiverC raw data", ylab = "Voltage (mV)")
with(SmallRiverC, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverA, type = "l",
     main = "BigRiverA raw data", ylab = "Voltage (mV)")
with(BigRiverA, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverB, type = "l",
     main = "BigRiverB raw data", ylab = "Voltage (mV)")
with(BigRiverB, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
plot(VOLTAGE ~ TIMESTAMP, data = BigRiverC, type = "l",
     main = "BigRiverC raw data", ylab = "Voltage (mV)")
with(BigRiverC, {
  rect(xleft = TIMESTAMP[1],
       xright = TIMESTAMP[700],
       ybottom = par("usr")[3],
       ytop = par("usr")[4], 
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
})
dev.off()

# with filter
png("trimmed_hall_voltage_grid.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
plot(VOLTAGE~TIMESTAMP, data = SmallRiverA_filter, main = "SmallRiverA trimmed data", type = "l", ylab = "Voltage (mV)")
plot(VOLTAGE~TIMESTAMP, data = SmallRiverB_filter, main = "SmallRiverB trimmed data", type = "l", ylab = "Voltage (mV)")
plot(VOLTAGE~TIMESTAMP, data = SmallRiverC_filter, main = "SmallRiverC trimmed data", type = "l", ylab = "Voltage (mV)")
plot(VOLTAGE~TIMESTAMP, data = BigRiverA_filter, main = "BigRiverA trimmed data", type = "l", ylab = "Voltage (mV)")
plot(VOLTAGE~TIMESTAMP, data = BigRiverB_filter, main = "BigRiverB trimmed data", type = "l", ylab = "Voltage (mV)")
plot(VOLTAGE~TIMESTAMP, data = BigRiverC_filter, main = "BigRiverC trimmed data", type = "l", ylab = "Voltage (mV)")
dev.off()

## log transform
SmallRiverA_filter$ln_volt <- log(SmallRiverA_filter$VOLTAGE)
SmallRiverB_filter$ln_volt <- log(SmallRiverB_filter$VOLTAGE)
SmallRiverC_filter$ln_volt <- log(SmallRiverC_filter$VOLTAGE)
BigRiverA_filter$ln_volt <- log(BigRiverA_filter$VOLTAGE)
BigRiverB_filter$ln_volt <- log(BigRiverB_filter$VOLTAGE)
BigRiverC_filter$ln_volt <- log(BigRiverC_filter$VOLTAGE)

## testing differences between observations
# how many observations significantly deviate from the "norm"?
# visually see by plotting

df <- SmallRiverA_filter %>%
  mutate(VOLTAGE_DIFF = VOLTAGE - lag(VOLTAGE))

plot(VOLTAGE_DIFF~TIMESTAMP, data = df, main = "SmallRiverA")

BigRiverB_filter$VOLTAGE <- as.numeric(BigRiverB_filter$VOLTAGE)
df2 <- BigRiverB_filter %>%
  mutate(VOLTAGE_DIFF = VOLTAGE - lag(VOLTAGE))

plot(VOLTAGE_DIFF~TIMESTAMP, data = df2, main = "BigRiverB")

## z-scores ##

SR_A <- SmallRiverA_filter
SR_A$VOLTAGE_DIFF <- c(NA, diff(SR_A$ln_volt))
# Remove the NA for calculations
diffs <- SR_A$VOLTAGE_DIFF[-1]
# Calculate mean and standard deviation of differences
mu <- mean(diffs)
sigma <- sd(diffs)
# Compute z-scores for all differences (keeping NA for the first row)
SR_A$z_score <- c(NA, (diffs - mu) / sigma)
# Identify rows where the absolute z-score is above a threshold (more than 2 standard deviations above the mean)
threshold <- 2
SR_A$flag <- ifelse(abs(SR_A$z_score) > threshold, TRUE, FALSE)
# designate high flow start and end
SRA_HF_start <- as.POSIXct("2023-12-05 11:20:00")
SRA_HF_end   <- as.POSIXct("2023-12-05 13:20:00")
# assign observations to a flow interval
SR_A$flow_phase <- with(SR_A, ifelse(TIMESTAMP < SRA_HF_start, "LF1",
                                 ifelse(TIMESTAMP <= SRA_HF_end, "HF", "LF2")))
# plot
SR1 <- ggplot(SR_A[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-05 11:20:00"),  # High flow start time
                xmax = as.POSIXct("2023-12-05 13:20:00"),  # High flow end time
                ymin = -Inf, ymax = Inf),                  # Full y-range
            fill = "antiquewhite3", alpha = 0.3) +  # Box color & transparency
  geom_vline(xintercept = as.POSIXct("2023-12-05 11:34:00"), # add point when mussel completely burrowed
             color = "red", linetype = "solid", size = 1) +
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Small River A", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-05 08:00:00"), as.POSIXct("2023-12-05 16:00:00"))
SR1
head(SR_A)

# SR2
SR_B <- SmallRiverB_filter
SR_B$VOLTAGE_DIFF <- c(NA, diff(SR_B$ln_volt))
diffs <- SR_B$VOLTAGE_DIFF[-1]
mu <- mean(diffs)
sigma <- sd(diffs)
SR_B$z_score <- c(NA, (diffs - mu) / sigma)
threshold <- 2
SR_B$flag <- ifelse(abs(SR_B$z_score) > threshold, TRUE, FALSE)
SRB_HF_start <- as.POSIXct("2023-12-07 10:43:00")
SRB_HF_end   <- as.POSIXct("2023-12-07 12:43:00")
SR_B$flow_phase <- with(SR_B, ifelse(TIMESTAMP < SRB_HF_start, "LF1",
                                     ifelse(TIMESTAMP <= SRB_HF_end, "HF", "LF2")))
SR2 <- ggplot(SR_B[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-07 10:40:00"),
                xmax = as.POSIXct("2023-12-07 12:40:00"),
                ymin = -Inf, ymax = Inf),
            fill = "antiquewhite3", alpha = 0.3) +
  geom_vline(xintercept = as.POSIXct("2023-12-07 12:03:00"), 
             color = "red", linetype = "solid", size = 1) +
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Small River B", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-07 08:00:00"), as.POSIXct("2023-12-07 16:00:00"))
SR2

# SR3
SR_C <- SmallRiverC_filter
SR_C$VOLTAGE_DIFF <- c(NA, diff(SR_C$ln_volt))
diffs <- SR_C$VOLTAGE_DIFF[-1]
mu <- mean(diffs)
sigma <- sd(diffs)
SR_C$z_score <- c(NA, (diffs - mu) / sigma)
threshold <- 2
SR_C$flag <- ifelse(abs(SR_C$z_score) > threshold, TRUE, FALSE)
SRC_HF_start <- as.POSIXct("2023-12-12 10:20:00")
SRC_HF_end   <- as.POSIXct("2023-12-12 12:20:00")
SR_C$flow_phase <- with(SR_C, ifelse(TIMESTAMP < SRC_HF_start, "LF1",
                                     ifelse(TIMESTAMP <= SRC_HF_end, "HF", "LF2")))
SR3 <- ggplot(SR_C[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-12 10:20:00"),
                xmax = as.POSIXct("2023-12-12 12:20:00"),
                ymin = -Inf, ymax = Inf),
            fill = "antiquewhite3", alpha = 0.3) +
  geom_vline(xintercept = as.POSIXct("2023-12-12 11:55:00"), 
             color = "red", linetype = "solid", size = 1) +
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Small River C", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-12 08:00:00"), as.POSIXct("2023-12-12 16:00:00"))
SR3

# BR1
BR_A <- BigRiverA_filter
BR_A$VOLTAGE_DIFF <- c(NA, diff(BR_A$ln_volt))
diffs <- BR_A$VOLTAGE_DIFF[-1]
mu <- mean(diffs)
sigma <- sd(diffs)
BR_A$z_score <- c(NA, (diffs - mu) / sigma)
threshold <- 2
BR_A$flag <- ifelse(abs(BR_A$z_score) > threshold, TRUE, FALSE)
BRA_HF_start <- as.POSIXct("2023-12-06 11:08:00")
BRA_HF_end   <- as.POSIXct("2023-12-06 13:08:00")
BR_A$flow_phase <- with(BR_A, ifelse(TIMESTAMP < BRA_HF_start, "LF1",
                                     ifelse(TIMESTAMP <= BRA_HF_end, "HF", "LF2")))
BR1 <- ggplot(BR_A[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-06 11:08:00"),
                xmax = as.POSIXct("2023-12-06 13:08:00"),
                ymin = -Inf, ymax = Inf),
            fill = "antiquewhite3", alpha = 0.3) + 
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Big River A", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-06 08:00:00"), as.POSIXct("2023-12-06 16:00:00"))
BR1

# BR2
BR_B <- BigRiverB_filter
BR_B$VOLTAGE_DIFF <- c(NA, diff(BR_B$ln_volt))
diffs <- BR_B$VOLTAGE_DIFF[-1]
mu <- mean(diffs)
sigma <- sd(diffs)
BR_B$z_score <- c(NA, (diffs - mu) / sigma)
threshold <- 2
BR_B$flag <- ifelse(abs(BR_B$z_score) > threshold, TRUE, FALSE)
BRB_HF_start <- as.POSIXct("2023-12-08 10:20:00")
BRB_HF_end   <- as.POSIXct("2023-12-08 12:20:00")
BR_B$flow_phase <- with(BR_B, ifelse(TIMESTAMP < BRB_HF_start, "LF1",
                                     ifelse(TIMESTAMP <= BRB_HF_end, "HF", "LF2")))
BR2 <- ggplot(BR_B[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-08 10:20:00"),
                xmax = as.POSIXct("2023-12-08 12:20:00"),
                ymin = -Inf, ymax = Inf),
            fill = "antiquewhite3", alpha = 0.3) + 
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Big River B", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-08 08:00:00"), as.POSIXct("2023-12-08 16:00:00"))
BR2

# BR3
BR_C <- BigRiverC_filter
BR_C$VOLTAGE_DIFF <- c(NA, diff(BR_C$ln_volt))
diffs <- BR_C$VOLTAGE_DIFF[-1]
mu <- mean(diffs)
sigma <- sd(diffs)
BR_C$z_score <- c(NA, (diffs - mu) / sigma)
threshold <- 2
BR_C$flag <- ifelse(abs(BR_C$z_score) > threshold, TRUE, FALSE)
BRC_HF_start <- as.POSIXct("2023-12-11 10:30:00")
BRC_HF_end   <- as.POSIXct("2023-12-11 12:30:00")
BR_C$flow_phase <- with(BR_C, ifelse(TIMESTAMP < BRC_HF_start, "LF1",
                                     ifelse(TIMESTAMP <= BRC_HF_end, "HF", "LF2")))
BR3 <- ggplot(BR_C[-1, ], aes(x = TIMESTAMP, y = VOLTAGE_DIFF)) +
  geom_rect(aes(xmin = as.POSIXct("2023-12-11 10:30:00"),
                xmax = as.POSIXct("2023-12-11 12:30:00"),
                ymin = -Inf, ymax = Inf),
            fill = "antiquewhite3", alpha = 0.3) + 
  geom_line() +
  geom_point(aes(color = flag)) +
  labs(title = "Big River C", y = "ln Voltage (mV) Difference") +
  theme_minimal() +
  ylim(-0.1, 0.1) +
  xlim(as.POSIXct("2023-12-11 08:00:00"), as.POSIXct("2023-12-11 16:00:00"))
BR3

## plot
pdf(file="hall_diffplots.pdf", height = 7, width = 14,)
grid.arrange(SR1,SR2,SR3,BR1,BR2,BR3, ncol = 3, nrow = 2)
dev.off()
tiff(file = "hall_diffplots.tiff", height = 7, width = 14, units = "in", res = 600)
grid.arrange(SR1, SR2, SR3, BR1, BR2, BR3, ncol = 3, nrow = 2)
dev.off()

### z-score test ###

# Q: Are z-scores different between compressed and inflated?
# need to bin values based on 'ecotype'
# assign group labels
datasets <- list(
  SR_A = SR_A, SR_B = SR_B, SR_C = SR_C,
  BR_A = BR_A, BR_B = BR_B, BR_C = BR_C
)
# add 'group' column to each dataset and combine them
all.data <- do.call(rbind, lapply(names(datasets), function(name) {
  df <- datasets[[name]]
  df$group <- ifelse(grepl("^SR", name), "compressed", "inflated")
  df$dataset <- name  # Keep dataset origin
  return(df)
}))
head(all.data, n=25)
all.data <- subset(all.data, !is.na(z_score) & !is.na(group)) # remove rows with NA in z_score or group

# KS test to compare distributions. applying jitter() to introduce small random noise to break ties.
ks.test(jitter(all.data$z_score[all.data$group == "compressed"]),  
        jitter(all.data$z_score[all.data$group == "inflated"]))
# sample size
n1 <- sum(all.data$group == "compressed")
n1 
n2 <- sum(all.data$group == "inflated")
n2
# accounting for autocorrelation, run Generalized least squares to test if z_score are time dependent
library(nlme)

# basic GLS model, group as predictor
gls_fit <- gls(z_score ~ group, data = all.data)
summary(gls_fit)

# accounting for autocorrelation
gls_ar1 <- gls(
  z_score ~ group,
  data = all.data,
  correlation = corAR1(form = ~ as.numeric(RECORD) | dataset)
)
summary(gls_ar1)
plot(gls_ar1)
acf(resid(gls_ar1))  # should show reduced autocorrelation

## chi-square test & fisher's exact test
z_score_2_CT<- table(all.data$group, all.data$flag) # global contingency table
z_score_2_CT
prop.table(z_score_2_CT, margin = 1)
chisq.test(z_score_2_CT)
fisher.test(z_score_2_CT) # redundancy

# compare values near-zero. More zeros = less movement
all.data$near_zero <- abs(all.data$z_score) <= 1  # TRUE if within -1 to 1
table_near_zero <- table(all.data$group, all.data$near_zero)
table_near_zero
prop.table(table_near_zero, margin = 1)  # Proportion of near-zero values per group

# Chi-square test
# tests whether the distribution of near-zero values differs significantly between groups
chisq.test(table_near_zero)
fisher.test(table_near_zero)

# table for each dataset
table_subgroup <- table(all.data$dataset, all.data$near_zero)
prop.table(table_subgroup, margin = 1)

## boxplots
# compressed vs. inflated
head(all.data)
flag_counts_total <- all.data %>%
  filter(flag == TRUE) %>%
  group_by(dataset, group) %>%
  summarise(flag_count = n(), .groups = "drop")
flag_counts_total
flow_boxplots<-ggplot(flag_counts_total, aes(x = group, y = flag_count, fill = group)) +
  geom_boxplot() +
  labs(title = "Count of Flagged Observations by River Group",
       x = "Ecotype",
       y = "Number of z-scores > 2",
       fill = "River Group") +
  scale_fill_manual(values = c("compressed" = "#1f78b4", "inflated" = "#33a02c")) +
  theme_minimal() + theme(legend.position = "none")
flow_boxplots
ggsave("flow_boxplots.svg", plot = flow_boxplots, device = "svg")

# group summary (basic stats for the boxplots)
flag_counts_total %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(flag_count),
    median = median(flag_count),
    sd = sd(flag_count),
    min = min(flag_count),
    max = max(flag_count),
    range = max(flag_count) - min(flag_count)
  )

# count TRUE flags by flow phase and dataset
flag_counts <- all.data %>%
  filter(flag == TRUE) %>%
  group_by(dataset, group, flow_phase) %>%
  summarise(flag_count = n(), .groups = "drop")
flag_counts
# plot
flag_counts$flow_phase <- factor(flag_counts$flow_phase, levels = c("LF1", "HF", "LF2"))
flow_phase_boxplots<-ggplot(flag_counts, aes(x = group, y = flag_count, fill = group)) +
  geom_boxplot() +
  facet_wrap(~ flow_phase) +
  labs(title = "Count of Flagged Observations by River Group and Flow Phase",
       x = "Ecotype",
       y = "Number of z-scores > 2",
       fill = "River Group") +
  scale_fill_manual(values = c("compressed" = "#1f78b4", "inflated" = "#33a02c")) +
  theme_minimal() + theme(legend.position = "none")
flow_phase_boxplots
ggsave("flow_phase_boxplots.svg", plot = flow_phase_boxplots, device = "svg")

print(flag_counts)

# ANOVA to test if movement during flow phases is significantly different between groups

ANOVA_model <- aov(flag_count ~ flow_phase, data = flag_counts)
qqnorm(residuals(ANOVA_model))
qqline(residuals(ANOVA_model))
shapiro.test(residuals(ANOVA_model))
summary(ANOVA_model)
TukeyHSD(ANOVA_model, "flow_phase")

## ANOVA and KW tests for within flow phase differences
# low flow phase 1
lf1 <- subset(flag_counts, flow_phase == "LF1")
aov_lf1 <- aov(flag_count ~ group, data = lf1)
summary(aov_lf1)
kruskal.test(flag_count ~ group, data = lf1)
# group summary (basic stats for the boxplots)
lf1 %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(flag_count),
    median = median(flag_count),
    sd = sd(flag_count),
    min = min(flag_count),
    max = max(flag_count),
    range = max(flag_count) - min(flag_count)
  )

# high flow anova
hf <- subset(flag_counts, flow_phase == "HF")
aov_hf <- aov(flag_count ~ group, data = hf)
summary(aov_hf)
kruskal.test(flag_count ~ group, data = hf)

hf %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(flag_count),
    median = median(flag_count),
    sd = sd(flag_count),
    min = min(flag_count),
    max = max(flag_count),
    range = max(flag_count) - min(flag_count)
  )

# low flow phase 2
lf2 <- subset(flag_counts, flow_phase == "LF2")
aov_lf2 <- aov(flag_count ~ group, data = lf2)
summary(aov_lf2)
kruskal.test(flag_count ~ group, data = lf2)

lf2 %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(flag_count),
    median = median(flag_count),
    sd = sd(flag_count),
    min = min(flag_count),
    max = max(flag_count),
    range = max(flag_count) - min(flag_count)
  )

## KW test for within group difference across flow phases

compressed <- flag_counts %>% filter(group == "compressed")
inflated <- flag_counts %>% filter(group == "inflated")
kruskal.test(flag_count ~ flow_phase, data = compressed)
kruskal.test(flag_count ~ flow_phase, data = compressed)
pairwise.wilcox.test(compressed$flag_count, compressed$flow_phase, p.adjust.method = "BH")
pairwise.wilcox.test(compressed$flag_count, compressed$flow_phase, p.adjust.method = "BH")

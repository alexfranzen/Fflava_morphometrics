## shell thickness analysis
# "Evaluating correlations between freshwater mussel shell morphology and valve-gaping activity across riverine conditions"

library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(cowplot)

setwd("~/Univ. of Oklahoma Dropbox/Alex Franzen/Fusconaia_project_data/Alex_scanning/shell_thickness")
rm(list=ls())

## read data
thick <- read.csv(file = "measurements.csv")
habitat <- read_xlsx("Alex_scans_master_list_habitat_20250127.xlsx", sheet = "thick")

## combine datasets
thick <- thick %>%
  mutate(meshid = as.character(meshid))
DATA <- left_join(thick, habitat, by = "meshid")
head(DATA)

## calculate total width and natural log
DATA$width <- DATA$`LV_Width (mm)`*2
DATA$ln_width <- log(DATA$width)

# convert flow from cfs to m3s
DATA$qe_ma<-DATA$qe_ma*0.028316847

# natural log
DATA$ln_cSize <- log(DATA$cSize)
DATA$ln_thick <- log(DATA$thick_mm)
DATA$ln_flow <- log(DATA$qe_ma)
DATA$ln_DA <- log(DATA$totdasqkm)

## Control for the confounding effect of shell size
# does thickness increase as a function of size? if yes, need to account for size.
ggplot(DATA, aes(x = ln_cSize, y = ln_thick)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(
    x = "ln(Centroid Size)",
    y = "ln(Shell Thickness)",
    title = "Shell Thickness vs. Centroid Size"
  ) +
  theme_minimal()
summary(lm(ln_thick ~ ln_cSize, data = DATA))

# This gives a measure of thickness relative to what you'd expect based on size alone
# How thick should a shell be, given its overall size (centroid size)?
size_model_thick <- lm(ln_thick ~ ln_cSize, data = DATA)
DATA$thickness_resid <- resid(size_model_thick)
hist(DATA$thickness_resid, main = "Residual Shell Thickness", xlab = "Residual Thickness", col = "skyblue")
plot(DATA$ln_flow, DATA$thickness_resid,
     xlab = "ln_flow",
     ylab = "Residual Thickness",
     main = "Residual Thickness vs Flow",
     pch = 16)
abline(lm(thickness_resid ~ ln_flow, data = DATA), col = "blue")

# does width increase as a function of size? if yes, need to account for size.
# log(shell width) as a function of log(cSize) 

ggplot(DATA, aes(x = ln_cSize, y = ln_width)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    x = "ln(Centroid Size)",
    y = "ln(Shell Width)",
    title = "Shell Width vs. Centroid Size"
  ) +
  theme_minimal()
summary(lm(ln_width ~ ln_cSize, data = DATA))

# This gives a measure of width relative to what you'd expect based on size alone
# How wide should a shell be, given its overall size (centroid size)?
size_model_width <- lm(ln_width ~ ln_cSize, data = DATA)
DATA$width_resid <- resid(size_model_width)
hist(DATA$width_resid, main = "Residual Shell Width", xlab = "Residual Width", col = "skyblue")
plot(DATA$ln_flow, DATA$width_resid,
     xlab = "ln_flow",
     ylab = "Residual Width",
     main = "Residual Width vs Flow",
     pch = 16)
abline(lm(width_resid ~ ln_flow, data = DATA), col = "blue")

## non size-corrected thickness plots
# thick~SO
thick_streamorder <- ggplot(DATA, aes(x = streamorde, y = ln_thick)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Stream Order", y = "ln median shell thickness (mm)", title = "ln Thickness vs. Stream Order") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
thick_streamorder
thick_SO_lm <- lm(ln_thick~streamorde, data = DATA)
summary(thick_SO_lm)
# thick~flow
thick_flow <- ggplot(DATA, aes(x = ln_flow, y = ln_thick)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(x = "ln Flow rate (cfs)", y = "ln median shell thickness (mm)", title = "ln Thickness vs. ln Flow rate") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
thick_flow
thick_flow_lm <- lm(ln_thick~ln_flow, data = DATA)
summary(thick_flow_lm)
# thick~DA
thick_DA <- ggplot(DATA, aes(x = ln_DA, y = ln_thick)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(x = "ln Catchment area (sqkm)", y = "ln median shell thickness (mm)", title = "ln Thickness vs. ln Catchment area") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
thick_DA
thick_DA_lm <- lm(ln_thick~ln_DA, data = DATA)
summary(thick_DA_lm)

### non size-corrected width plots ###
# width~SO
width_streamorder <- ggplot(DATA, aes(x = streamorde, y = ln_width, label = meshid)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(x = "Stream Order", y = "ln Shell width (mm)", title = "ln Width vs. Stream Order") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
width_streamorder
width_SO_lm <- lm(ln_width~streamorde, data = DATA)
summary(width_SO_lm)
# width~flow
width_flow <- ggplot(DATA, aes(x = ln_flow, y = ln_width, label = meshid)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(x = "ln Flow rate (cfs)", y = "ln Shell width (mm)", title = "ln Width vs. ln Flow rate") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
width_flow
width_flow_lm <- lm(ln_width~ln_flow, data = DATA)
summary(width_flow_lm)
# width~DA
width_DA <- ggplot(DATA, aes(x = ln_DA, y = ln_width, label = meshid)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(x = "ln Catchment area (sqkm)", y = "ln Shell width (mm)", title = "ln Width vs. ln Catchment area") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
width_DA
width_DA_lm <- lm(ln_width~ln_DA, data = DATA)
summary(width_DA_lm)

pdf("non_size_corrected_shell_traits_plots.pdf", width = 14, height = 8)
grid.arrange(
  thick_streamorder, thick_flow, thick_DA,
  width_streamorder, width_flow, width_DA,
  ncol = 3
)
dev.off()

## size-corrected habitat trait plots

compressed_mussels <- c("623_SmallRiverA_LV_downsize_50p","624_SmallRiverC","625_SmallRiverB")
inflated_mussels <- c("627_BigRiverC","629_BigRiverB","632_BigRiverA")
DATA$group <- ifelse(DATA$meshid %in% compressed_mussels, "compressed",
                     ifelse(DATA$meshid %in% inflated_mussels, "inflated", "Other"))
# thick~SO
sc_thick_streamorder <- ggplot(DATA, aes(x = streamorde, y = thickness_resid)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(color = group)) +
  labs(x = "Stream Order", y = "thick~Size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_thick_streamorder
sc_thick_SO_lm <- lm(thickness_resid~streamorde, data = DATA)
summary(sc_thick_SO_lm)

# thick~flow
sc_thick_flow <- ggplot(DATA, aes(x = ln_flow, y = thickness_resid)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(color = group)) +
  labs(x = "ln Flow rate (m3/s)", y = "thick~Size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_thick_flow
sc_thick_flow_lm <- lm(thickness_resid~ln_flow, data = DATA)
summary(sc_thick_flow_lm)
# thick~DA
sc_thick_DA <- ggplot(DATA, aes(x = ln_DA, y = thickness_resid)) +
  geom_smooth(method = "lm", color = "black") +  
  geom_point(aes(color = group)) +
  labs(x = "ln Catchment area (sqkm)", y = "thick~Size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_thick_DA
sc_thick_DA_lm <- lm(thickness_resid~ln_DA, data = DATA)
summary(sc_thick_DA_lm)

## size-corrected width plots
# width~SO
sc_width_streamorder <- ggplot(DATA, aes(x = streamorde, y = width_resid)) +
  geom_smooth(method = "lm", color = "black") +  
  geom_point(aes(color = group)) +
  labs(x = "Stream Order", y = "width~size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_width_streamorder
sc_width_SO_lm <- lm(width_resid~streamorde, data = DATA)
summary(sc_width_SO_lm)
# width~flow
sc_width_flow <- ggplot(DATA, aes(x = ln_flow, y = width_resid)) +
  geom_smooth(method = "lm", color = "black") +  
  geom_point(aes(color = group)) +
  labs(x = "ln Flow rate (m3/s)", y = "width~size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_width_flow
sc_width_flow_lm <- lm(width_resid~ln_flow, data = DATA)
summary(sc_width_flow_lm)
# width~DA
sc_width_DA <- ggplot(DATA, aes(x = ln_DA, y = width_resid)) +
  geom_smooth(method = "lm", color = "black") +  
  geom_point(aes(color = group)) +
  labs(x = "ln Catchment area (sqkm)", y = "width~size") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_width_DA
sc_width_DA_lm <- lm(width_resid~ln_DA, data = DATA)
summary(sc_width_DA_lm)

plots <- plot_grid(
  sc_thick_streamorder, sc_width_streamorder,
  sc_thick_flow,       sc_width_flow,
  sc_thick_DA,         sc_width_DA,
  ncol = 2,
  align = "hv")
plots
ggsave("size_corrected_shell_traits_plots.svg", plots, width = 9, height = 9)

## plots were subsequently edited in Illustrator for final touches

## cSize plots
cSize_streamorder <- ggplot(DATA, aes(x = streamorde, y = ln_cSize)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(x = "Stream Order", y = "ln Shell centroid size (mm)", title = "ln size vs. Stream Order") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
cSize_streamorder
cSize_SO_lm <- lm(ln_cSize~streamorde, data = DATA)
summary(cSize_SO_lm)

cSize_flow <- ggplot(DATA, aes(x = ln_flow, y = ln_cSize)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(x = "ln Flow rate (cfs)", y = "ln Shell centroid size (mm)", title = "ln size vs. ln Flow rate") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
cSize_flow
cSize_flow_lm <- lm(ln_cSize~ln_flow, data = DATA)
summary(cSize_flow_lm)

cSize_DA <- ggplot(DATA, aes(x = ln_DA, y = ln_cSize)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  
  labs(x = "ln Catchment area (sqkm)", y = "ln Shell centroid size (mm)", title = "ln size vs. ln Catchment area") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
cSize_DA
cSize_DA_lm <- lm(ln_cSize~ln_DA, data = DATA)
summary(cSize_DA_lm)

## thickness as a function of width

thick_width <- ggplot(DATA, aes(x = ln_width, y = ln_thick)) +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "ln Shell width (mm)", y = "ln median Shell thickness (mm)", title = "ln median shell Thickness vs. ln Width") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
thick_width
thick_lm <- lm(ln_thick~ln_width, data = DATA)
summary(thick_lm)
ggsave("thickness_V_width.svg", thick_width, width = 6, height = 6, units = "in")

## corrected for size

sc_thick_width_size <- ggplot(DATA, aes(x = width_resid, y = thickness_resid)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(aes(color = group)) +
  labs(x = "lm(ln_width~ln_cSize) residuals", y = "lm(ln_thickness~ln_cSize) residuals", title = "Shell Thickness vs. Shell Width (size-corrected)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("compressed" = "blue", "inflated" = "red", "Other" = "black"))
sc_thick_width_size
sc_thick_size_lm <- lm(thickness_resid~width_resid, data = DATA)
summary(sc_thick_size_lm)
ggsave("size_corrected_thickness_V_width.svg", sc_thick_width_size)

## Multiple regression
sc_thick_size_model <- lm(ln_thick ~ ln_width + ln_cSize, data = DATA)
summary(sc_thick_size_model)

## Multivariate multiple regression of hydrology on shell width+thickness (corrected for the effects of cSize) ##
MMR <- manova(cbind(ln_width, ln_thick) ~ ln_cSize + ln_flow + ln_DA + streamorde, data = DATA)
summary(MMR)
summary.aov(MMR)
summary.lm(MMR)

# lm to test interactions separately
lm_width_hydro<- lm(ln_width ~ ln_cSize + ln_flow * ln_DA, data = DATA)
summary(lm_width_hydro)
lm_thick_hydro <- lm(ln_thick ~ ln_cSize + ln_flow * ln_DA, data = DATA)
summary(lm_thick_hydro)

library(tidyverse)
library(janitor)

## LOAD DATA
data <- read.csv("data/Measurements.csv")

## TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Number"] <- "Number"
# Clean names for R
data <- data %>%
  clean_names()
# Take the measurements, do algebra
measure <- data %>%
  mutate(oral_disk_mm = (oral_disk_diameter_mm_a + oral_disk_diameter_mm_b)/2) %>%
  mutate(wet_mass_g = wet_mass_plus_vial_g-vial_mass_g) %>%
  mutate(dry_mass_g = dry_mass_plus_vial_g_day_3-vial_mass_g) %>%
  select(id, oral_disk_mm, mouth_length_mm, wet_mass_g, dry_mass_g)

plot(measure$dry_mass_g~measure$oral_disk_mm)
plot(measure$dry_mass_g~measure$mouth_length_mm)
plot(measure$oral_disk_mm~measure$mouth_length_mm)

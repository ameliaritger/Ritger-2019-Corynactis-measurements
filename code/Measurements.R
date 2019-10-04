
library(tidyverse)

## LOAD DATA
data <- read.csv("data/Measurements.csv")

## TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Number"] <- "Number"
measure <- data %>%
  mutate(oral_disk_mm = (Oral_disk_diameter_mm_a + Oral_disk_diameter_mm_b)/2) %>%
  mutate(wet_mass_g = Wet_mass_vial_g-Vial_mass_g) %>%
  mutate(dry_mass_g = Dry_mass_vial_g_3-Vial_mass_g) %>%
  select(ID, oral_disk_mm, Mouth_length_mm, wet_mass_g, dry_mass_g)

plot(measure$dry_mass_g~measure$oral_disk_mm)
plot(measure$dry_mass_g~measure$Mouth_length_mm)
plot(measure$oral_disk_mm~measure$Mouth_length_mm)

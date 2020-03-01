
library(tidyverse)
library(janitor)
library(gtools)

## LOAD DATA
data <- read_csv("data/Measurements.csv")

## TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Number"] <- "Number"
# Remove columns not yet measured
data <- data[-c(31,34:36),]
# Clean names for R, convert NAs to 9999 for minimum value calculation next
data <- data %>%
  clean_names() %>%
  replace_na(list(dry_mass_plus_vial_g_day_2="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_3="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_4="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_5="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_7="9999"))

# Take the measurements, do algebra
measure <- data %>%
  mutate(oral_disk_mm = (oral_disk_diameter_mm_a + oral_disk_diameter_mm_b)/2) %>%
  mutate(wet_mass_g = wet_mass_plus_vial_g-vial_mass_g) %>%
  mutate(min_dry_mass_plus_vial_g = pmin(dry_mass_plus_vial_g_day_2, dry_mass_plus_vial_g_day_3,dry_mass_plus_vial_g_day_4,dry_mass_plus_vial_g_day_5, dry_mass_plus_vial_g_day_7)) %>%
  mutate(dry_mass_g = as.numeric(min_dry_mass_plus_vial_g)-vial_mass_g) %>%
  mutate(basal_disk_mm = (basal_disk_diameter_mm_a + basal_disk_diameter_mm_b)/2) %>%
  select(id, oral_disk_mm, basal_disk_mm, mouth_length_mm, vial_mass_g, min_dry_mass_plus_vial_g, wet_mass_g, dry_mass_g)

# Make plot of oral disk ~ mouth length, dry mass/wet mass ~ oral disk/mouth length
ggplot(measure, aes(x=oral_disk_mm, y=wet_mass_g)) +
  geom_point() +
  geom_point(aes(color=id)) + # plot points colored by ID
  geom_smooth(method="lm", se=FALSE, color="grey12", size=1, linetype="dashed") +
  labs(x="Oral Disk Diameter (mm)", y="Dry mass (g)") +
  theme_bw()

# Get regression line
lm(measure$wet_mass_g~measure$oral_disk_mm)

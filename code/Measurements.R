
library(tidyverse)
library(janitor)
library(gtools)
library(lubridate)

## LOAD DATA
data <- read_csv("data/Measurements.csv") %>% 
  clean_names()

## TIDY DATA
# Remove columns not yet measured
#data <- data[-c(31,34:36),]
# convert NAs to 9999 for minimum value calculation next
data_no_na <- data %>%
  replace_na(list(dry_mass_plus_vial_g_day_1="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_2="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_3="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_4="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_5="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_6="9999")) %>%
  replace_na(list(dry_mass_plus_vial_g_day_7="9999"))

# Take the measurements, do algebra
measure <- data_no_na %>%
  mutate(oral_disk_mm = (oral_disk_diameter_mm_a + oral_disk_diameter_mm_b)/2) %>%
  mutate(basal_disk_mm = (basal_disk_diameter_mm_a + basal_disk_diameter_mm_b)/2) %>%
  mutate(wet_mass_g = wet_mass_plus_vial_g-vial_mass_g) %>%
  mutate(min_dry_mass_plus_vial_g = pmin(dry_mass_plus_vial_g_day_2, dry_mass_plus_vial_g_day_3,dry_mass_plus_vial_g_day_4,dry_mass_plus_vial_g_day_5, dry_mass_plus_vial_g_day_6, dry_mass_plus_vial_g_day_7)) %>%
  mutate(dry_mass_g = as.numeric(min_dry_mass_plus_vial_g)-vial_mass_g) %>%
  select(number, id, oral_disk_mm, basal_disk_mm, mouth_length_mm, vial_mass_g, min_dry_mass_plus_vial_g, wet_mass_g, dry_mass_g) %>% 
  filter(is.na(dry_mass_g)==FALSE) %>% 
  mutate(dry_mass_g=ifelse(dry_mass_g < 0, 0, dry_mass_g)) #make negative values (samples 85, 160) zero

measure_dry <- measure %>% 
  filter(dry_mass_g>0) #remove zero values

mass <- data %>% 
  mutate("3" = dry_mass_plus_vial_g_day_3,
         "4" = dry_mass_plus_vial_g_day_4,
         "5" = dry_mass_plus_vial_g_day_5,
         "6" = dry_mass_plus_vial_g_day_6,
         "7" = dry_mass_plus_vial_g_day_7) %>% 
  pivot_longer("3":"7",
               names_to = "day",
               values_to = "dmass") %>% 
  mutate(year=lubridate::year(as.Date(date, format = "%m/%d/%Y"))) %>% 
  filter(year=="2020")

#make plot of dry mass measurements over time
ggplot(mass, aes(x=day, y=dmass, group=number, color=number)) +
  geom_point() +
  geom_line()

# Make plot of oral disk ~ mouth length, dry mass/wet mass ~ oral disk/mouth length
ggplot(measure, aes(x=basal_disk_mm, y=dry_mass_g)) +
  geom_point() +
  #geom_point(aes(color=id)) + # plot points colored by ID
  geom_smooth(method="nls", se=FALSE, color="grey12", size=1, linetype="dashed") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ exp(x))
  labs(x="Oral Disk Diameter (mm)", y="Wet mass (g)") +
  theme_bw()

fit1 <- lm(measure$wet_mass_g~measure$basal_disk_mm)
fit2 <- lm(measure$wet_mass_g~poly(measure$basal_disk_mm,2,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
x_samp <- seq(1,15, length=50)

plot(measure$wet_mass_g~measure$basal_disk_mm)
lines(x_samp, predict(fit1, data.frame(x=x_samp)), col="red")
lines(x_samp, predict(fit2, data.frame(x=x_samp)), col="green")

formula = 'y ~ expFct(x,beta1, beta2, beta3)', 
start=list(beta1=1,beta2=0.01,beta3=40)

fit2 <- lm(y~poly(x,2,raw=TRUE))


# Get regression line
lm(measure$wet_mass_g~measure$oral_disk_mm)

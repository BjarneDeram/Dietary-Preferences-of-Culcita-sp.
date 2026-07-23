if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("vegan", quietly = TRUE)) install.packages("vegan")
if (!requireNamespace("pairwiseAdonis", quietly = TRUE)) install.packages("pairwiseAdonis")
if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("electivity", quietly = TRUE)) install.packages("electivity")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(vegan)
# library(pairwiseAdonis) NOT AVAILABLE IN THIS VERSION OF R, CHECK FOR REPLACEMENT 
library(rstatix)
library(FSA)
library(car)
library(electivity)
library(openxlsx)

cots_data <- read_excel("~/Desktop/COTs_Abund_MASTER.xlsx")
cots_data <- cots_data %>%
  mutate(Site = case_when(
    Site == "Red Rock" ~ "Red Rock Wall",
    TRUE ~ Site
  ), Site = str_replace_all(trimws(Site), "\\s+", "_")
  )

site_density <- cots_data %>%
  group_by(Site, Survey_ID) %>% 
  summarise(
    cots_count = sum(Specimen != 0, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    density_2000m2 = cots_count,
    density_ha = density_2000m2 * 5
  ) %>%
  group_by(Site) %>%
  summarise(
    mean_density_ha = mean(density_ha, na.rm = TRUE),
    sd_density_ha = sd(density_ha, na.rm = TRUE),
    n_surveys = n(),
    se_density_ha = sd_density_ha / sqrt(n_surveys)
  ) %>%
  rename(location = Site) 

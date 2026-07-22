#### SPUE + No Scars percentage ####
#### 19/05/2026, Bjarne ####

#### 0. Load required packages #### 
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("vegan", quietly = TRUE)) install.packages("vegan")
if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("dplyr", quitely = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggrepel", quitely = TRUE)) install.packages("ggrepel")
if (!requireNamespace("rlang", quitely = TRUE)) install.packages("rlang")
if (!requireNamespace("DHARMa", quitely = TRUE)) install.packages("DHARMa")
if (!requireNamespace("multcompView", quitely = TRUE)) install.packages("multcompView")

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(vegan)
library(rstatix)
library(FSA)
library(car)
library(dplyr)
library(ggrepel)
library(lme4)

#### 1. Data import and cleaning ####
## A. SPUE data
SPUE <- read_excel("~/Desktop/Culcita Data/CPUE.xlsx") %>% 
  rename(location = Site)

## B. No Scars 
No_scars <- read_excel("~/Desktop/Culcita Data/Culcita habitat preferences.xlsx") %>% 
  rename(location = Location)

## C. Cots density
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
    n_surveys = n()
  ) %>%
  rename(location = Site) %>% 
  mutate(location = case_when(
    location == "Green_Rock_Wall" ~ "Green_Rock", location == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ location
  ))

## D. Family Availability
cpce <- read_csv("~/Desktop/Culcita Data/17042026Reefcloud.csv")
cpce <- cpce %>%
  mutate(
    coral_family = coalesce(point_human_group_code, point_machine_group_code) ## Coalesce uses point_human_group_code first, then point_machine_group_code if first is not available
  ) %>%
  filter(!is.na(coral_family), coral_family != "Other")

family_availability_site <- cpce %>%
  group_by(site_name, coral_family) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

family_availability_site <- family_availability_site %>% 
  mutate(site_name = gsub(" ", "_", site_name)) %>% 
  rename(location = site_name) %>% 
  mutate(location = case_when(
    location == "Green_Rock_Wall" ~ "Green_Rock", location == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ location
  ))

## E. Pits
pit <- read_excel("~/Desktop/Culcita Data/Culcita PITs Data.xlsx") %>%
  clean_names() %>%
  dplyr::mutate(date = as.Date(date)) %>% 
  mutate(
    location = factor(location),
    transect_id = factor(transect_id),
    growth_form = factor(growth_form),
    colony_size = factor(colony_size,
                         levels = c("<5", "5-10", "10-20", ">20")),
    count = as.integer(count)
  ) %>% 
  mutate(morph_size = paste(growth_form, colony_size, sep = "_"))

str(pit)
head(pit)

#### 2. SPUE #### 
SPUE_data <- SPUE %>% 
  mutate(
    effort = Time * Surveyors, 
    spue_hour = Count / effort * 60 # per hour per surveyor
  )

SPUE_site <- SPUE_data %>% 
  group_by(location) %>% 
  summarise(
    mean_spue_hour = mean(spue_hour, na.rm = TRUE),
    sd_spue = sd(spue_hour, na.rm = TRUE),
    n_surveys = n(),
    .groups = "drop")

# significance between sites? 
# normality by site
by(SPUE_data$spue_hour, SPUE_data$location, shapiro.test)

# homogeneity of variance
car::leveneTest(spue_hour ~ location, data = SPUE_data)

# One-way AN0VA
spue_aov <- aov(spue_hour ~ location, data = SPUE_data)
summary(spue_aov)

# Post-hoc 
tukey_spue <- TukeyHSD(spue_aov)
print(tukey_spue)

# significance letters
library(multcompView)

letters <- multcompLetters4(spue_aov, tukey_spue)

letters_df <- data.frame(
  location = names(letters$location$Letters),
  letter = letters$location$Letters
)

letters_df

# Y positions for letters 
library(dplyr)

letter_pos <- SPUE_data %>%
  group_by(location) %>%
  summarise(y = max(spue_hour, na.rm = TRUE) + 2)

letters_df <- left_join(letters_df, letter_pos, by = "location")

# Put letters at same height in plot
letters_df <- letters_df %>%
  mutate(y = 40)

# Plotting 
library(ggplot2)

ggplot(SPUE_data, aes(location, spue_hour)) +
  geom_boxplot(
    fill = "grey80",
    colour = "grey20",
    outlier.shape = NA,
    width = 0.65
  ) +
  geom_text(
    data = letters_df,
    aes(x = location, y = y, label = letter),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    x = "Site",
    y = "SPUE (individuals/hr/surveyor)"
  ) +
  coord_cartesian(
    ylim = c(0, max(letters_df$y) + 2),
    clip = "off"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1)
)

### Look into family composition effects on SPUE 
family_matrix <- family_availability_site %>% 
  dplyr::select(location, coral_family, availability) %>% 
  pivot_wider(
    names_from = coral_family,
    values_from = availability,
    values_fill = 0
  ) 

family_spue <- family_matrix %>% 
  left_join(SPUE_site, by = "location")

# Run PERMANOVA
Y <- family_spue %>% 
  dplyr::select(-location, -mean_spue_hour, -sd_spue, -n_surveys)

adonis2(
  Y ~ mean_spue_hour, 
  data = family_spue,
  method = "bray"
)

# Plotting (NMDS + SPUE)
nmds_Family <- metaMDS(Y, distance = "bray", k = 2, trymax = 100)

# scores
nmds_scores_family <- as.data.frame(scores(nmds_Family, display = "sites"))

# add metadata
nmds_scores_family <- nmds_scores_family %>%
  mutate(
    location = family_spue$location,
    mean_spue_hour = family_spue$mean_spue_hour
  )

# plot
ggplot(nmds_scores_family, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = mean_spue_hour), colour = "steelblue") +
  geom_text_repel(
    aes(label = location), 
    size = 4, 
    box.padidng = 0.4, 
    point.padding = 0.3, 
    force = 1, 
    max.overlaps = Inf
  ) +
  theme_minimal() +
  labs(
    title = "NMDS of coral family composition",
    subtitle = "Point size = mean SPUE",
    x = "NMDS1",
    y = "NMDS2",
    size = "SPUE"
  )

### Look into growth form composition 
# summarise GF per site
GF_site <- pit %>%
  group_by(location, growth_form) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(location) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  ungroup()

GF_matrix <- GF_site %>%
  dplyr::select(location, growth_form, proportion) %>%
  pivot_wider(
    names_from = growth_form,
    values_from = proportion,
    values_fill = 0
  )

GF_spue <- GF_matrix %>%
  left_join(SPUE_site, by = "location")

# PERMANOVA
X <- GF_spue %>%
  dplyr::select(-location, -mean_spue_hour, -sd_spue, -n_surveys)

adonis2(
  X ~ mean_spue_hour,
  data = GF_spue,
  method = "bray"
)

# Plotting 
nmds_GF <- metaMDS(X, distance = "bray", k = 2, trymax = 100)

scores_GF <- as.data.frame(scores(nmds_GF, display = "sites"))

scores_GF <- scores_GF %>%
  mutate(
    location = GF_spue$location,
    mean_spue_hour = GF_spue$mean_spue_hour
  )

ggplot(scores_GF, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = mean_spue_hour), colour = "steelblue") +
  geom_text_repel(
    aes(label = location), 
    size = 4, 
    box.padding = 0.4, 
    point.padding = 0.3, 
    force = 1, 
    max.overlaps = Inf
  ) +
  theme_minimal() +
  labs(
    title = "NMDS of coral growth form composition",
    subtitle = "Point size = mean SPUE",
    size = "SPUE"
  )

#### 3. No Scars % ####
No_scars_data <- No_scars %>% 
  group_by(location) %>% 
  summarise(
    Scar_Y = sum(Scar == "Y", na.rm = TRUE),
    Scar_N = sum(Scar == "N", na.rm = TRUE),
    Total = n(), 
    percent_scarred = (Scar_Y / Total) * 100,
    percent_no_scar = (Scar_N / Total) * 100, 
    .groups = "drop"
  )

mean(No_scars_data$percent_no_scar)

## COTs effect ## 
# add cots to scarring data 
scar_data <- No_scars %>%  
  left_join(site_density, by = c("location")) %>% 
  mutate(
    Scar_bin = ifelse(Scar == "Y", 1, 0)
  )

# use GLMM (for GLM remove random variable for location and 'er' from test)
scar_glmm <- glmer(Scar_bin ~ mean_density_ha + (1|location), 
                   data = scar_data, 
                   family = binomial)
summary(scar_glmm)

# Plot residuals of glmm
# library(DHARMa)

# sim_res <- simulateResiduals(scar_glmm)
# plot(sim_res)

## Plotting ## 
scar_site_plot <- No_scars_data %>%
  left_join(site_density, by = "location")

ggplot(scar_site_plot, aes(x = mean_density_ha, y = percent_scarred)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text_repel(
    aes(label = location), 
    size = 4, 
    box.padidng = 0.4, 
    point.padding = 0.3, 
    force = 1, 
    max.overlaps = Inf
  ) +
  theme_minimal() +
  labs(
    x = "Mean COTS density (ind/ha)",
    y = "% scarred cushion stars",
    title = "Scarring prevalence in relation to COTS density"
  )

## Residuals ## 
#diag_data <- data.frame(
#  fitted = fitted(scar_glmm),
#  resid = residuals(scar_glmm, type = "deviance")
#)

#ggplot(diag_data, aes(fitted, resid)) +
#  geom_point(size = 2, colour = "firebrick") +
#  geom_hline(yintercept = 0, linetype = "dashed") +
# theme_minimal() +
#  labs(
#    x = "Fitted probability",
#    y = "Deviance residuals",
#    title = "Residuals vs fitted values"
#  )

library(DHARMa)

simres <- simulateResiduals(scar_glmm)

plot(simres)
testDispersion(simres)
testOutliers(simres)
testUniformity(simres)

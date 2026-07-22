#### Dietary Preferences ####
#### 29/03/2026, Bjarne #### 

#### 0. Load required packages ####
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
if (!requireNamespace("dplyr", quitely = TRUE)) install.packages("dplyr")
if (!requireNamespace("patchwork", quitely = TRUE)) install.packages("patchwork")
if (!requireNamespace("gt", quitely = TRUE)) install.packages("gt")

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
library(dplyr)

#### 1. Data import and cleaning ####
## A. Scars data 
scars <- read_excel("~/Desktop/Culcita Data/Culcita Scars Data.xlsx") %>%
  clean_names() %>%
  dplyr::mutate(date = as.Date(date)) %>% 
  mutate(
    location = factor(location),
    growth_form = factor(growth_form),
    colony_size = factor(colony_size,
                         levels = c("<5", "5-10", "10-20", ">20")),
    coral_family = factor(coral_family),
    count = as.integer(count)
  ) %>% 
  filter(!growth_form %in% c("Digitate", "Intracolonial_Morph._Var.")) %>%
    mutate(combo = paste(growth_form, coral_family, sep = "_"))
  

str(scars)
head(scars)

## B. PITs data 
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
  filter(!growth_form %in% c("Digitate", "Intracolonial_Morph._Var."))


str(pit)
head(pit)

# Get extra data
length(unique(pit$transect_id)) # sum all unique transect IDs 
sum(pit$count)                  # sum all count values within the count collumn

## C. CPCE data
cpce <- read_csv("~/Desktop/Culcita Data/NewData.csv") %>%
  mutate(
    classification = coalesce(
      na_if(point_human_classification, ""),
      na_if(point_machine_classification, "")
    ),
    
    # Everything before the underscore
    family_code = str_extract(classification, "^[^_]+"),
    
    # Everything after the underscore
    growth_form_code = str_extract(classification, "(?<=_)[^_]+$")
  ) %>%
  filter(
    !is.na(classification),
    !classification %in% c("OT", "Other")
  ) %>%
  mutate(
    coral_family = case_when(
      family_code == "ACR" ~ "Acroporidae",
      family_code == "AGA" ~ "Agaricidae",
      family_code == "DIP" ~ "Diploastraeidae",
      family_code == "EUP" ~ "Euphylliidae",
      family_code == "FUN" ~ "Fungiidae",
      family_code == "LOB" ~ "Lobophylliidae",
      family_code == "MER" ~ "Merulinidae",
      family_code == "POC" ~ "Pociloporidae",
      family_code == "POR" ~ "Poritidae",
      TRUE ~ NA_character_
    ),
    
    growth_form = case_when(
      growth_form_code == "AR" ~ "Arborescent",
      growth_form_code == "CA" ~ "Caespitose",
      growth_form_code == "CO" ~ "Corymbose",
      growth_form_code == "EN" ~ "Encrusting",
      growth_form_code == "FO" ~ "Foliose",
      growth_form_code == "LA" ~ "Laminar",
      growth_form_code == "MA" ~ "Massive",
      growth_form_code == "SM" ~ "Sub_Massive",
      growth_form_code == "SO" ~ "Solitary",
      growth_form_code == "TB" ~ "Tabular",
      TRUE ~ NA_character_
    )
  )

# Family availability
family_availability_site <- cpce %>%
  filter(!is.na(coral_family)) %>%
  group_by(site_name, coral_family) %>%
  summarise(
    n_points = n(),
    .groups = "drop"
  ) %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

# Growth form availability 
growth_form_availability_site <- cpce %>%
  filter(!is.na(growth_form)) %>%
  group_by(site_name, growth_form) %>%
  summarise(
    n_points = n(),
    .groups = "drop"
  ) %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

# COMBO
combo_avail <- cpce %>% 
  mutate(combo = paste(growth_form, coral_family, sep = "_"))

combo_avail <- combo_avail %>%
  group_by(site_name, combo) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

combo_avail <- combo_avail %>% 
  mutate(site_name = gsub(" ", "_", site_name)) %>% 
  mutate(site_name = case_when(
    site_name == "Green_Rock_Wall" ~ "Green_Rock", site_name == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ site_name
  ))

## Percentage scars per morphology
scar_growthform <- scars %>%   # replace with your scar dataset
  group_by(growth_form) %>%
  summarise(
    scars = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent = scars / sum(scars) * 100
  ) %>%
  arrange(desc(percent)) %>% 
  select(-scars)

scar_growthform

library(gt)
gt_tbl <- gt(scar_growthform, rowname_col = "growth_form") |>
  tab_header(title = md("**Percentage of total scars per growth form**")) |> ### title of table
  tab_stubhead(label = "Growth form") |> ### label for the most left column 
  cols_align(align = "left", columns = "growth_form") ### alignment of the words in the column

gt_tbl 

#### 2. Electivity function ####
electivity <- function(avail, use, group_vars, avail_col = "count") {
  # availability
  avail_data <- avail %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(avail = sum(.data[[avail_col]]), .groups = "drop") %>%
    mutate(p = avail / sum(avail))
  
  # use
  use_data <- use %>%
    group_by(across(all_of(group_vars))) %>%
    filter(!is.na(count)) %>%
    summarise(use = sum(count), .groups = "drop") %>%
    mutate(r = use / sum(use))
  
  # Merge & test
  result <- use_data %>%
    full_join(avail_data, by = group_vars) %>%
    replace_na(list(use = 0, r = 0, avail = 0, p = 0)) %>% 
    filter(p > 0) %>% 
    mutate(forage_ratio = r / p, Ei = vs_electivity(r, p))
  
  return(print(result)) 
}

# Run function x4 
family_data <- electivity(
  avail = family_availability_site,
  use = scars,
  group_vars = "coral_family",
  avail_col = "n_points"
)

GF_data <- electivity(
  avail = growth_form_availability_site,
  use = scars,
  group_vars = "growth_form",
  avail_col = "n_points"
)

size_data <- electivity(
  avail = pit,
  use = scars,
  group_vars = "colony_size"
)

combo_data <- electivity(
  avail = combo_avail,
  use = scars,
  group_vars = "combo",
  avail_col = "n_points"
)

#### 3. Plotting function ####
## Patchwork and add plotting for different growth forms into combo data
plot_electivity <- function(data, x_var, title, x_label, n_col) {
    
    if (x_var == "combo") {
      data <- data %>%
        filter(!(Ei == -1 & combo != "Tabular_Acroporidae"))
    }
  data %>%
    mutate(selectivity = ifelse(Ei > 0, "Preference", "Avoidance")) %>%
    ggplot(aes(x = reorder(.data[[x_var]], -Ei), y = Ei, fill = selectivity)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -0.3, linetype = "dashed") +
    geom_hline(yintercept = 0.3, linetype = "dashed") +
    geom_rect(aes(xmin = 0, xmax = n_col, ymin = -0.3, ymax = 0.3), fill = "gray20", alpha = 0.01) +
    scale_fill_manual(values = c("Preference" = "chartreuse4", "Avoidance" = "red3")) +
    ylim(-1, 1) +
    # annotate(
    #    "segment",
    #   x = n_col -2, xend = n_col -2.5,
    #  y = 0.375, yend = 0.15,
    # arrow = arrow(length = grid::unit(0.18, "cm")),
    #linewidth = 0.5 
    #  ) +
    # annotate(
    #  "text",
    #  x = n_col -2.75,
    #  y = 0.4,
    # label = "Neutral selection",
    # hjust = 0,
    #  vjust = -0.4,
    #  size = 3
    #  ) +
    labs(
      title = title,
      x = x_label,
      y = "Vanderploeg & Scavia electivity (Ei)",
      fill = "Selectivity"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

# Run function x3
plot_electivity(
  family_data,
  x_var = "coral_family",
  title = "Selectivity by coral family",
  x_label = "Coral family",
  n_col = 10
)

plot_electivity(
  GF_data,
  x_var = "growth_form",
  title = "Selectivity by coral colony growth form",
  x_label = "Colony growth form",
  n_col = 11
)

plot_electivity(
  size_data,
  x_var = "colony_size",
  title = "Selectivity by coral colony size",
  x_label = "Colony size class",
  n_col = 5
)

plot_electivity(
  combo_data,
  x_var = "combo",
  title = "Selectivity by coral growth form and family",
  x_label = "Colony growth form and family",
  n_col = 18
)
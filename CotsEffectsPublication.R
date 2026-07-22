#### CoTs Dietary Preferences ####
#### 16/03/2026, Bjarne #### 

#### 0. Load required packages ####
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("vegan", quietly = TRUE)) install.packages("vegan")
if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("electivity", quietly = TRUE)) install.packages("electivity")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")


library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(vegan)
library(rstatix)
library(FSA)
library(car)
library(electivity)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)


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
  mutate(combo = paste(growth_form, coral_family, sep = "_"))

scars <- scars %>% 
  mutate(location = gsub(" ", "_", location))

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
  mutate(morph_size = paste(growth_form, colony_size, sep = "_"))

pit <- pit %>% 
  mutate(location = gsub(" ", "_", location))

str(pit)
head(pit)

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
      family_code == "AGA" ~ "Agariciidae",
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
cots_family_availability_site <- cpce %>%
  group_by(site_name, coral_family) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

cots_family_availability_site <- cots_family_availability_site %>% 
  mutate(site_name = gsub(" ", "_", site_name)) %>% 
  mutate(site_name = case_when(
    site_name == "Green_Rock_Wall" ~ "Green_Rock", site_name == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ site_name
  ))

cots_family_availability_site <- cots_family_availability_site %>% 
  rename(location = site_name)

# Growth form availability 
cots_growth_form_availability_site <- cpce %>%
  group_by(site_name, growth_form) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

cots_growth_form_availability_site <- cots_growth_form_availability_site %>% 
  mutate(site_name = gsub(" ", "_", site_name)) %>% 
  mutate(site_name = case_when(
    site_name == "Green_Rock_Wall" ~ "Green_Rock", site_name == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ site_name
  ))

cots_growth_form_availability_site <- cots_growth_form_availability_site %>% 
  rename(location = site_name)

# COMBO
cots_combo_avail <- cpce %>% 
  mutate(combo = paste(growth_form, coral_family, sep = "_"))

cots_combo_avail <- cots_combo_avail %>%
  group_by(site_name, combo) %>%
  summarise(n_points = n(), .groups = "drop") %>%
  group_by(site_name) %>%
  mutate(
    total_points = sum(n_points),
    availability = n_points / total_points
  ) %>%
  ungroup()

cots_combo_avail <- cots_combo_avail %>% 
  mutate(site_name = gsub(" ", "_", site_name)) %>% 
  mutate(site_name = case_when(
    site_name == "Green_Rock_Wall" ~ "Green_Rock", site_name == "Red_Rock_Wall" ~ "Red_Rock",
    TRUE ~ site_name
  ))

cots_combo_avail <- cots_combo_avail %>% 
  rename(location = site_name)

## D. Cots density
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

#### 2. Function implementation #### 
cots_electivity <- function(avail, use, group_vars, site_density, avail_col = "count") {
  # availability
  avail_data <- avail %>%
    group_by(location, across(all_of(group_vars))) %>%
    summarise(avail = sum(.data[[avail_col]]), .groups = "drop") %>%
    group_by(location) %>%
    mutate(p = avail / sum(avail))
  
  # use
  use_data <- use %>%
    group_by(location, across(all_of(group_vars))) %>%
    filter(!is.na(count)) %>%
    summarise(use = sum(count), .groups = "drop") %>%
    group_by(location) %>% 
    mutate(r = use / sum(use))
  
  # Merge & test
  result <- use_data %>%
    full_join(avail_data, by = c("location", group_vars)) %>%
    replace_na(list(use = 0, r = 0, avail = 0, p = 0)) %>% 
    filter(p > 0) %>% 
    left_join(site_density, by = "location") %>%
    mutate(
      Ei = vs_electivity(r, p))
  
  return(print(result)) 
}

## Run function x4
cots_family_data <- cots_electivity(
  avail = cots_family_availability_site,
  use = scars,
  group_vars = "coral_family",
  site_density = site_density,
  avail_col = "n_points"
)

cots_GF_data <- cots_electivity(
  avail = cots_growth_form_availability_site,
  use = scars,
  group_vars = "growth_form",
  site_density = site_density,
  avail_col = "n_points"
)

cots_size_data <- cots_electivity(
  avail = pit,
  use = scars, 
  group_vars = "colony_size", 
  site_density = site_density
)

cots_combo_data <- cots_electivity(
  avail = cots_combo_avail,
  use = scars,
  group_vars = "combo", 
  site_density = site_density,
  avail_col = "n_points"
)

#### 3. PERMANOVA + scatterplot Function #### 
# Function 
run_permanova <- function(data, category_var, cat_title, n_col, method = "euclidean") {
  
  # Make PERMANOVA matrix
  data_matrix <- data %>%
    dplyr::select(location, mean_density_ha, dplyr::all_of(category_var), Ei) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(category_var), values_from = Ei) %>%
    dplyr::ungroup()
  
  # Metadata
  meta <- data_matrix %>%
    dplyr::select(location, mean_density_ha)
  
  # Numeric response matrix
  Ei_matrix <- data_matrix %>%
    dplyr::select(-location, -mean_density_ha) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    as.data.frame()
  
  # Remove NAs
  Ei_matrix[is.na(Ei_matrix)] <- 0
  
  # Grouping vector
  group <- data_matrix$mean_density_ha
  
  # Run PERMANOVA     #### No assumptions check due to continuous predictor (acanthaster density) 
  permanova_result <- vegan::adonis2(
    Ei_matrix ~ mean_density_ha,
    data = meta,
    method = method
  )
  
  line_data <- data %>%
    group_by(across(all_of(category_var))) %>%
    group_modify(~{
      
      dat <- .x %>%
        filter(!is.na(Ei), !is.na(mean_density_ha))
      
      # Not enough data or no variation in x
      if (nrow(dat) < 2 || length(unique(dat$mean_density_ha)) < 2) {
        return(data.frame(
          mean_density_ha = dat$mean_density_ha,
          Ei_pred = dat$Ei,
          selectivity = ifelse(dat$Ei > 0, "Preference", "Avoidance"),
          slope = NA_real_,
          pval = NA_real_
        ))
      }
      
      mod <- lm(Ei ~ mean_density_ha, data = dat)
      coef_table <- summary(mod)$coefficients
      
      slope <- if ("mean_density_ha" %in% rownames(coef_table)) {
        coef_table["mean_density_ha", "Estimate"]
      } else {
        NA_real_
      }
      
      pval <- if ("mean_density_ha" %in% rownames(coef_table)) {
        coef_table["mean_density_ha", "Pr(>|t|)"]
      } else {
        NA_real_
      }
      
      newdat <- data.frame(
        mean_density_ha = seq(
          min(dat$mean_density_ha, na.rm = TRUE),
          max(dat$mean_density_ha, na.rm = TRUE),
          length.out = 100
        )
      )
      
      newdat$Ei_pred <- predict(mod, newdata = newdat)
      newdat$selectivity <- ifelse(newdat$Ei_pred > 0, "Preference", "Avoidance")
      newdat$slope <- slope
      newdat$pval <- pval
      
      newdat
    }) %>%
    ungroup()
  
  # Only significant groups
  line_data_filtered <- line_data %>%
    group_by(across(all_of(category_var))) %>%
    filter(
      !is.na(first(pval)),
      first(pval) < 0.05
    ) %>%
    ungroup()
  
  # labels 
  label_data <- line_data_filtered %>%
    group_by(dplyr::across(dplyr::all_of(category_var))) %>%
    slice_max(mean_density_ha, n = 1) %>%
    ungroup()
  
  x_max <- max(line_data$mean_density_ha, na.rm = TRUE)
  x_min <- min(line_data$mean_density_ha, na.rm = TRUE)
  x_range <- x_max - x_min
  
  label_data2 <- label_data %>%
    mutate(label_x = x_max + 0.12 * x_range,
           label_y = seq(0.8, -0.8, length.out = n())
    )
  
  # Filter point data 
  valid_categories <- unique(line_data_filtered[[category_var]])
  
  data_filtered <- data %>%
    dplyr::filter(.data[[category_var]] %in% valid_categories)
  
  # Plotting
  if(nrow(line_data_filtered) > 0){
    scatterplot <- ggplot2::ggplot() +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = "grey50"
      ) +
      # geom_point(data = data_filtered, 
      #             aes(
      #               x = mean_density_ha, 
      #              y = Ei, 
      #             fill = location
      #            ),
      #            shape = 21,
      #           colour = "black",
      #          size = 2
      #) +
      #scale_fill_brewer(palette = "Set1"
      #) +
      ggplot2::geom_line(
        data = line_data_filtered,
        mapping = ggplot2::aes(
          x = mean_density_ha,
          y = Ei_pred,
          colour = selectivity,
          group = interaction(.data[[category_var]], selectivity)
        ),
        linewidth = 1
      ) +
      ggplot2::scale_color_manual(
        values = c("Preference" = "chartreuse4", "Avoidance" = "red3")
      ) +
      scale_x_continuous(
        expand = expansion(mult = c(0.02, 0.22))
      ) + 
      ggplot2::coord_cartesian(ylim = c(-1, 1), clip = "off") + 
      facet_wrap(~.data[[category_var]], ncol = n_col, axes = "all") +
      ggplot2::labs(
        title = cat_title,
        x = "Mean Acanthaster sp. density",
        y = "Vanderploeg & Scavia electivity (Ei)",
        colour = "Selectivity"
      ) +
      ggplot2::theme_minimal() + 
      theme(
        strip.text = element_text(face = "bold"), 
        legend.position = "right"
      )
    
    # Plotting with points
    if(nrow(line_data_filtered) > 0){
      scatterplot_points <- ggplot2::ggplot() +
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = "dashed",
          colour = "grey50"
        ) +
        geom_point(data = data_filtered, 
                   aes(
                     x = mean_density_ha, 
                     y = Ei, 
                     fill = location
                   ),
                   shape = 21,
                   colour = "black",
                   size = 2
        ) +
        scale_fill_brewer(palette = "Set1"
        ) +
        ggplot2::geom_line(
          data = line_data_filtered,
          mapping = ggplot2::aes(
            x = mean_density_ha,
            y = Ei_pred,
            colour = selectivity,
            group = interaction(.data[[category_var]], selectivity)
          ),
          linewidth = 1
        ) +
        ggplot2::scale_color_manual(
          values = c("Preference" = "chartreuse4", "Avoidance" = "red3")
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0.02, 0.22))
        ) + 
        ggplot2::coord_cartesian(ylim = c(-1, 1), clip = "off") + 
        facet_wrap(~.data[[category_var]], ncol = n_col, axes = "all") +
        ggplot2::labs(
          title = cat_title,
          x = "Mean Acanthaster sp. density",
          y = "Vanderploeg & Scavia electivity (Ei)",
          colour = "Selectivity"
        ) +
        ggplot2::theme_minimal() + 
        theme(
          strip.text = element_text(face = "bold"), 
          legend.position = "right"
        )
    } else {
      
      message("No clear trends detected; plot not generated.")
      
    }
    
    return(list(
      wide_data = data_matrix,
      metadata = meta,
      Ei_matrix = Ei_matrix,
      group = group,
      permanova = permanova_result,
      scatterplot = scatterplot, 
      scatterplot_points = scatterplot_points
    ))
  }}

## Run function x3
family_results <- run_permanova(
  data = cots_family_data,
  category_var = "coral_family",
  cat_title = "Selectivity changes by coral family",
  n_col = 2
)
family_results$permanova
family_results$scatterplot
family_results$scatterplot_points

GF_results <- run_permanova(
  data = cots_GF_data,
  category_var = "growth_form", 
  cat_title = "Selectivity changes by growth from",
  n_col = 2
)
GF_results$permanova
GF_results$scatterplot
GF_results$scatterplot_points

size_results <- run_permanova(
  data = cots_size_data,
  category_var = "colony_size",
  cat_title = "Selectivity changes by colony size",
  n_col = 2
)
size_results$permanova
size_results$scatterplot
size_results$scatterplot_points

combo_results <- run_permanova(
  data = cots_combo_data,
  category_var = "combo",
  cat_title = "Selectivity changes by coral growth form and family",
  n_col = 2
)
combo_results$permanova
combo_results$scatterplot
combo_results$scatterplot_points

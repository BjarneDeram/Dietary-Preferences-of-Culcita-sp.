#### Habitat Preferences ####
#### 03/04/2026, Bjarne ####

#### 0. Load required packages ####
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
if (!requireNamespace("multcomp", quietly = TRUE)) install.packages("multcomp")
if (!requireNamespace("multcompView", quietly = TRUE)) install.packages("multcompView")
if (!requireNamespace("dunn.test", quietly = TRUE)) install.packages("dunn.test")
if (!requireNamespace("rcompanion", quietly = TRUE)) install.packages("rcompanion")
if (!requireNamespace("ggthemes", quietly = TRUE)) install.packages("ggthemes")
if (!requireNamespace("rphylopic", quietly = TRUE)) install.packages("rphylopic")

library(rphylopic)
library(tidyverse)
library(readxl)
library(rstatix)
library(FSA)
library(car)
library(janitor)
library(lubridate)
library(emmeans)
library(multcomp)
library(multcompView)
library(dunn.test)
library(rcompanion)

#### 1. Data import and Cleaning ####
habitat <- read_excel("~/Desktop/Culcita Data/Culcita habitat preferences.xlsx") %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    location = factor(location),
    depth = as.numeric(depth),
    coral_cover = str_trim(coral_cover)
  )

length(unique(habitat$culcita_id))

# Create depth classes (<10m, 10m-15m, >15m)
habitat_classed <- habitat %>%
  mutate(
    depth_class = case_when(
      depth < 8 ~ "<8",
      depth >= 8 & depth < 10 ~ "8-10",
      depth >= 10 & depth < 12 ~ "10-12",
      depth >= 12 & depth < 14 ~ "12-14",
      depth >= 14 ~ ">14"
    )
  )

table(habitat_classed$depth_class)

# Coral cover ordered factor 
habitat_classed <- habitat_classed %>%
  mutate(
    coral_cover = factor(coral_cover, levels = c("<10", "10-30", "30-50", ">50"), ordered = TRUE)
  )

# Filter NAs 
habitat_classed <- habitat_classed %>%
  filter(!is.na(depth), !is.na(coral_cover))

#### 2. Tests ####
## A. Depth
# count culcita per depth class per site
depth_counts <- habitat_classed %>%
  group_by(location, depth_class) %>%
  summarise(n = n(), .groups = "drop") %>% 
  tidyr::complete(location, depth_class, fill = list(n = 0))

# calculate percentages per site
depth_counts <- depth_counts %>%
  group_by(location) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  ungroup() %>% 
  mutate(
    depth_class = factor(
      depth_class,
      levels = c("<8", "8-10", "10-12", "12-14", ">14")
    )
  )

# Kruskal-Wallis
kruskal.test(percent ~ depth_class, data = depth_counts)

# Safe labels
depth_counts2 <- depth_counts %>%
  mutate(
    depth_safe = dplyr::recode(depth_class,
                               "<8"    = "d1",
                               "8-10"  = "d2",
                               "10-12" = "d3",
                               "12-14" = "d4",
                               ">14"   = "d5"
    )
  )

# Post-hoc
dunn_res_depth2 <- FSA::dunnTest(
  percent ~ depth_safe,
  data = depth_counts2,
  method = "holm"
)

# extract p-values
depth_pvals <- dunn_res_depth2$res$P.adj
names(depth_pvals) <- gsub(" - ", "-", dunn_res_depth2$res$Comparison)

# letters on safe labels
depth_letters_vec <- multcompView::multcompLetters(depth_pvals)$Letters

# convert to dataframe and map back to original labels
depth_letters <- data.frame(
  depth_safe = names(depth_letters_vec),
  letter = depth_letters_vec,
  stringsAsFactors = FALSE
) %>%
  mutate(
    depth_class = dplyr::recode(depth_safe,
                                "d1" = "<8",
                                "d2" = "8-10",
                                "d3" = "10-12",
                                "d4" = "12-14",
                                "d5" = ">14"
    )
  ) %>%
  left_join(
    depth_counts %>%
      group_by(depth_class) %>%
      summarise(y = max(percent, na.rm = TRUE) + 4, .groups = "drop"),
    by = "depth_class"
  )

depth_letters

# Put letters at same height in plot
depth_letters <- depth_letters %>%
  mutate(y = 75)

## Outlier depth
depth_outliers <- depth_counts %>%
  group_by(depth_class) %>%
  mutate(
    Q1 = quantile(percent, 0.25, na.rm = TRUE),
    Q3 = quantile(percent, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(percent < lower | percent > upper) %>%
  ungroup()

depth_outliers

## B. Coral Cover percentage 
# count culcita per coral cover class per site
CC_counts <- habitat_classed %>%
  group_by(location, coral_cover) %>%
  summarise(n = n(), .groups = "drop") %>% 
  tidyr::complete(location, coral_cover, fill = list(n = 0))

# calculate percentages per site
CC_counts <- CC_counts %>%
  group_by(location) %>%
  mutate(percent = n / sum(n) * 100)

# Kruskal-wallis 
kruskal.test(percent ~ coral_cover, data = CC_counts)

# make temporary safe labels
CC_counts2 <- CC_counts %>%
  mutate(
    CC_safe = dplyr::recode(coral_cover,
                               "<10"    = "CC1",
                               "10-30"  = "CC2",
                               "30-50" = "CC3",
                               ">50" = "CC4"
    )
  )

# rerun Dunn test on SAFE labels
dunn_res_CC2 <- dunn.test(
  x = CC_counts2$percent,
  g = CC_counts2$CC_safe,
  method = "holm"
)

# extract p-values from dunn.test output
CC_pvals <- dunn_res_CC2$P.adjusted
names(CC_pvals) <- gsub(" - ", "-", dunn_res_CC2$comparisons)

# letters on safe labels
CC_letters_vec <- multcompView::multcompLetters(CC_pvals)$Letters

# convert to dataframe and map back to original labels
CC_letters <- data.frame(
  CC_safe = names(CC_letters_vec),
  letter = CC_letters_vec,
  stringsAsFactors = FALSE
) %>%
  mutate(
    coral_cover = dplyr::recode(CC_safe,
                                "CC1" = "<10",
                                "CC2" = "10-30",
                                "CC3" = "30-50",
                                "CC4" = ">50"
    )
  ) %>%
  left_join(
    CC_counts %>%
      group_by(coral_cover) %>%
      summarise(y = max(percent, na.rm = TRUE) + 4, .groups = "drop"),
    by = "coral_cover"
  )

CC_letters

# Put letters at same height in plot
CC_letters <- CC_letters %>%
  mutate(y = 70)

## Outliers CC 
library(dplyr)

# identify outliers within each coral_cover group
CC_outliers <- CC_counts %>%
  group_by(coral_cover) %>%
  mutate(
    Q1 = quantile(percent, 0.25, na.rm = TRUE),
    Q3 = quantile(percent, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(percent < lower | percent > upper) %>%
  ungroup()

CC_outliers

#### 3. Plotting #### 
library(ggthemes)
library(ggrepel)

## A. Depth 
p1 <- ggplot(depth_counts, aes(depth_class, percent)) +
  geom_hline(yintercept = 50, colour = "grey40", linetype = "dashed") +
  geom_boxplot(fill = "grey") +
  # geom_jitter(color = "darkblue", width = 0.1, size = 1) +
  geom_text(
    data = depth_letters,
    aes(x = depth_class, y = y, label = letter),
    inherit.aes = FALSE,
    size = 5
  ) + 
  geom_label_repel(
    data = depth_outliers,
    aes(label = location),
    size = 3.5,
    min.segment.length = 0,
    seed = 1,
    nudge_x = 0,
    nudge_y = 2
  ) +
  labs(
    x = "Depth class (m)",
    y = "Culcita observations (%)", 
    title = "A. Percentage of Culcita observations vs depth classes"
  ) +
theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, colour = "grey20"),
    axis.line = element_line(colour = "grey20"),
    plot.margin = margin(5.5, 30, 5.5, 5.5)
  )

## B. Coral Cover 
p2 <- ggplot(CC_counts, aes(coral_cover, percent)) +
  geom_hline(yintercept = 50, colour = "grey40", linetype = "dashed") +
  geom_boxplot(fill = "grey") +
  # geom_jitter(color = "darkblue", width = 0.1, size = 1) +
  geom_text(
    data = CC_letters,
    aes(x = coral_cover, y = y, label = letter),
    inherit.aes = FALSE,
    size = 5
  ) + 
  geom_label_repel(
    data = CC_outliers,
    aes(label = location),
    size = 3.5,
    min.segment.length = 0,
    seed = 1,
    nudge_y = 2
  ) +
  labs(
    x = "Hard coral cover class (%)",
    y = "Culcita observations (%)",
    title = "B. Percentage of Culcita observations vs hard coral cover classes"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, colour = "grey20"),
    axis.line = element_line(colour = "grey20"),
    plot.margin = margin(5.5, 30, 5.5, 5.5)
  )

library(patchwork)
patch_habitat <- p1 + p2 + plot_layout(axis_titles = "keep")
print(patch_habitat)
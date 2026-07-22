## Cohen's kappa accuracy analysis of Reefcloud.AI ##
## Bjarne 01/04/2026 ## 

#### 0. Load required packages + data import####
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
library(ggplot2)
library(tidyr)
library(scales)

cpce <- read_csv("~/Desktop/Culcita Data/NewData.csv")

#### 1. Cohens Kappa analysis ####
## A. irr package 
install.packages("irr")
library(irr)

kappa2(cpce[, c("point_machine_classification", "point_human_classification")])

## B. psych package
install.packages("psych")
library(psych)

cohen.kappa(table(cpce$point_machine_classification,
                  cpce$point_human_classification))

## C. tidyverse-friendly pipeline 
library(dplyr)
library(irr)

cpce %>%
  select(point_machine_classification, point_human_classification) %>%
  kappa2()

## D. Mismatches? 
prop.table(table(cpce$point_machine_classification,
                 cpce$point_human_classification), 1)

conf_mat_prop <- prop.table(table(cpce$point_machine_classification,
                 cpce$point_human_classification), 2)

conf_df <- as.data.frame(conf_mat_prop)
colnames(conf_df) <- c("Prediction", "Actual", "Proportion")

# Convert to percentage
conf_df$Percent <- conf_df$Proportion * 100

##### 2. Plotting ####
ggplot(conf_df, aes(x = Actual, y = Prediction, fill = Percent)) +
  geom_tile(color = "white") +
  
  # Add percentage labels
  geom_text(aes(label = round(Percent)), size = 3) +
  
  scale_fill_viridis_c(name = "Actual (%)") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  
  labs(
    x = "Actual",
    y = "Prediction"
  )
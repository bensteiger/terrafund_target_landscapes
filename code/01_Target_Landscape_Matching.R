# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 08/09/2024
# Last Updated: 08/14/2024
# Description: Creating Target Landscapes for Cohort 1 Projects for UDB

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)
library(here)
library(readxl)
library(snakecase)
library(stringdist)
library(fuzzyjoin)
library(stringi)

# Load data ---------------------------------------------------------------

target_landscape_areas <- read_excel(
  here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Target Landscapes",
    "target_landscape_areas.xlsx"
  )
)

project_data <- read.csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Target Landscapes",
    "Projects-Target Landscape Mapping.csv"
  ),
  encoding = "UTF-8"
)

# Convert data to snakecase -----------------------------------------------

names(target_landscape_areas) <- to_snake_case(names(target_landscape_areas))

names(project_data) <- to_snake_case(names(project_data))

# create new subset -------------------------------------------------------

project_data_sub <- project_data

# separate rows -----------------------------------------------------------

# create string to bypass DRC due to "," in name
ignore_string <- "Congo, Democratic Republic of the"

placeholder <- "IGNORE_ME"

# start with GADM level 0 and "," issue

project_data_sub <- project_data_sub %>%
  mutate(project_location_gadm_level_0 = str_replace(project_location_gadm_level_0, ignore_string, placeholder)) %>%
  # Separate rows by commas
  separate_rows(project_location_gadm_level_0, sep = ",") %>%
  # Replace the placeholder back to the original string
  mutate(project_location_gadm_level_0 = str_replace(project_location_gadm_level_0, placeholder, ignore_string))

# GADM level 1, 2

project_data_sub <- project_data_sub %>%
  separate_rows(project_location_gadm_level_1, sep = ",") %>%
  separate_rows(project_location_gadm_level_2, sep = ",")

# remove "" from text -----------------------------------------------------

project_data_sub <- project_data_sub %>%
  mutate(project_location_gadm_level_0 = str_remove_all(project_location_gadm_level_0, '"'))


# drop Côte d'Ivoire from project data ------------------------------------

# not in target landscapes - one Cohort 1 project overlaps border

project_data_sub <- project_data_sub %>%
  filter(project_location_gadm_level_0 != "Côte d'Ivoire")

# str_trim all data -------------------------------------------------------

# removes whitespace before and after strings

project_data_sub <- project_data_sub %>%
  mutate_all(str_trim)

target_landscape_areas <- target_landscape_areas %>%
  mutate_all(str_trim)

# compare results using join ----------------------------------------------

# drop target landscape from project data to recreate it
project_data_sub <- project_data_sub %>%
  select(-target_landscape)

join <-
  left_join(
    project_data_sub, 
    target_landscape_areas,
    by = c(
      "project_location_gadm_level_0" = "project_country",
      "project_location_gadm_level_1" = "region",
      "project_location_gadm_level_2" = "eligible_constituencies"
      )
  ) %>%
  filter(!is.na(target_landscape))

# check anti-joins to see if anything got messed up -----------------------

anti_join <-
  anti_join(
    project_data_sub, 
    target_landscape_areas,
    by = c(
      "project_location_gadm_level_0" = "project_country",
      "project_location_gadm_level_1" = "region",
      "project_location_gadm_level_2" = "eligible_constituencies"
    )
  )

anti_join %>%
  filter(project_location_gadm_level_1 %in% target_landscape_areas$region) %>%
  print(n = Inf)

anti_join %>%
  filter(!tm_project_id %in% join_distinct$tm_project_id,
         project_location_country %in% c("Congo, Democratic Republic of the", "Ghana", "Kenya", "Burundi", "Rwanda")) %>%
  group_by(tm_project_id) %>%
  slice(1) %>%
  ungroup() %>%
  view()

#  keep distinct tm_project_id --------------------------------------------

join_distinct <- join %>%
  distinct(tm_project_id, .keep_all = TRUE) %>%
  select(tm_project_id, target_landscape)

# save data ---------------------------------------------------------------

write_csv(
  join_distinct,
  here(
    "Unified Database",
    "Data",
    "All",
    "Processed",
    "Target Landscapes",
    "cohort_1_complete_target_landscapes.csv"
  )
)



library("jsonlite")
library(dplyr)
library(tidyr)
library("ggplot2")
library("survival")
library("survminer")
library(scales)
library(lubridate)

library(readxl)

# Import Excel file 
SCM_2024 <- read_excel(
  path = "/Users/gdiaz2/Projects/Truveta Telehealth/County Health Ranking Data/CHR_Data_2019_2025.xlsx",
  sheet = "Sheet1"   
)
# drop unnecessary columns
SCM_2024 <- SCM_2024 %>% select(-c("County FIPS Code"))

# Loading state concept id table
state_age_agg_Truveta = fromJSON("total_people_in_truveta.json")
state_names = state_age_agg_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["tConcepts"]][[1]][["conceptName"]]
state_codes_df <- data.frame(state_name = state_names)
state_id <- state_age_agg_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["tConcepts"]][[1]][["conceptCode"]]
state_codes_df$state_code <- state_id
state_codes_df$state_code <- as.numeric(state_codes_df$state_code)

# Loading total people in truveta state aggregates 
total_people = state_age_agg_Truveta$data$runVetaQuery$otherAggregates$result$table$rows[[1]]

# Loading trans people in truveta state aggregates
trans_agg_Truveta = fromJSON("trans_people_in_truveta_yearly.json")
trans_people = trans_agg_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["table"]][["rows"]][[1]]

total_people_df = as.data.frame(total_people)
trans_people_df = as.data.frame(trans_people)

# Remove Nulls 
total_people_df <- total_people_df[total_people_df$V1 != "Null" & total_people_df$V2 != "Null" & total_people_df$V3 != "Null", ]
trans_people_df <- trans_people_df[trans_people_df$V1 != "Null" & trans_people_df$V2 != "Null" & trans_people_df$V3 != "Null", ]

# Format aggregates as numbers and rename V2 to state_code
total_people_df$V2 <- as.numeric(total_people_df$V2)
total_people_df$V1 <- as.numeric(total_people_df$V1)
total_people_df$V3 <- as.numeric(total_people_df$V3)

trans_people_df$V2 <- as.numeric(trans_people_df$V2)
trans_people_df$V1 <- as.numeric(trans_people_df$V1)
trans_people_df$V3 <- as.numeric(trans_people_df$V3)

# Filter out pre 2019
trans_people_df <- trans_people_df %>%
  filter(V1 >= 2019 & V1 <= 2025)

# Join total_people_df and trans_people_df to see numbers in truveta by state
truveta_aggs <- full_join(total_people_df, trans_people_df, by = c("V1", "V2"))
colnames(truveta_aggs)[colnames(truveta_aggs) == "V3.x"] <- "Total People In Truveta"
colnames(truveta_aggs)[colnames(truveta_aggs) == "V3.y"] <- "Trans People In Truveta"

# Change NA values to 0
truveta_aggs$`Trans People In Truveta` <- 
  replace_na(truveta_aggs$`Trans People In Truveta`, 0)
truveta_aggs$`Total People In Truveta` <- 
  replace_na(truveta_aggs$`Total People In Truveta`, 0)

# Join truveta_aggs with state_codes_df
truveta_aggs <- full_join(truveta_aggs, state_codes_df, by = c("V2" = "state_code"))

# Join truveta_aggs with SCM_2024
SCM_2024$'Release Year' <- as.numeric(SCM_2024$'Release Year')
SCM_2024 <- full_join(truveta_aggs, SCM_2024, by = c("state_name" = "Name", "V1" = "Release Year"))

# -------------Add in T Prescription Events (outcomes) from Tuveta Agg Tables--------------
# Loading T prescription events from  truveta state aggregates
t_prescr_Truveta = fromJSON("T_numbers_by_state.json")
total_t_prescr = t_prescr_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["table"]][["rows"]][[1]]
virtual_t_prescr = t_prescr_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["table"]][["rows"]][[2]]
total_t_prescr_df = as.data.frame(total_t_prescr)
virtual_t_prescr_df = as.data.frame(virtual_t_prescr)

#Join together two dataframes
t_numbers <- full_join(total_t_prescr_df, virtual_t_prescr_df, by = c("V1", "V2"))
colnames(t_numbers)[colnames(t_numbers) == "V3.x"] <- "Total T Prescriptions"
colnames(t_numbers)[colnames(t_numbers) == "V3.y"] <- "Virtual T Prescriptions"

# Remove Nulls 
t_numbers <- t_numbers[t_numbers$V1 != "Null" & t_numbers$V2 != "Null", ]

# Format aggregate values as numbers
t_numbers$`Total T Prescriptions` <- as.numeric(t_numbers$`Total T Prescriptions`)
t_numbers$`Virtual T Prescriptions` <- as.numeric(t_numbers$`Virtual T Prescriptions`)

# Change NA values to 0
t_numbers$`Total T Prescriptions` <- 
  replace_na(t_numbers$`Total T Prescriptions`, 0)
t_numbers$`Virtual T Prescriptions` <- 
  replace_na(t_numbers$`Virtual T Prescriptions`, 0)

# Merge with State Names df
t_numbers$V2 <- as.numeric(t_numbers$V2)
t_numbers_joined <- full_join(t_numbers, state_codes_df, by = c("V2" = "state_code"))
t_numbers_joined <- t_numbers_joined %>% select(-"V2")

# Group data by year
t_numbers_joined <- t_numbers_joined %>%
  mutate(
    date = as.Date(paste0(V1, "-01"))  # "2001-02-01"
  )
t_numbers_joined <- t_numbers_joined %>% select(-"V1")

t_numbers_joined <- t_numbers_joined %>%
  mutate(year = year(date)) %>%  
  group_by(state_name, year) %>%
  summarise(
    across(
      c(`Total T Prescriptions`, `Virtual T Prescriptions`),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
t_numbers_joined <- t_numbers_joined %>%
  filter(!is.na(year))
t_numbers_joined <- t_numbers_joined %>%
  filter(year >= 2019 & year <= 2025)

# join to SCM
SCM_2024 <- full_join(t_numbers_joined, SCM_2024, by = c("state_name" = "state_name", "year" = "V1"))
colnames(SCM_2024)[colnames(SCM_2024) == "year"] <- "Year"


# Clean up data
# Remove British Columbia and District of Columbia
SCM_2024 <- SCM_2024[!(SCM_2024$state_name %in% c("British Columbia", "District of Columbia", "Ontario", "Puerto Rico", "Null")), ]
colnames(SCM_2024)[colnames(SCM_2024) == "V1"] <- "Year"
SCM_2024 <- SCM_2024 %>%
  # drop state_code
  select(-"V2") %>%
  # reorder so state_name and State Abbreviation are first
  select(Year, state_name, `State Abbreviation`, everything())
SCM_2024 <- SCM_2024 %>%
  filter(
    !is.na(`Total People In Truveta`) &
      `Total People In Truveta` != 0
  )

# load in medicaid and medicare data 
med_df <- read_excel(
  path = "Medicaid_Medicare.xlsx",
  sheet = "Sheet1"   
)

# load in political indicator data
pol_ind <- read_excel(
  path = "Political Indicator.xlsx",
  sheet = "Sheet1"   
)

# join tables
SCM_2024 <- full_join(SCM_2024, med_df, by = c("Year", "state_name" = "State_Name", "State Abbreviation" = "State_Abbrev"))
SCM_2024 <- full_join(SCM_2024, pol_ind, by = c("State Abbreviation"))

# Write result to excel file
write_xlsx(SCM_2024, "SCM_Dataframe.xlsx")

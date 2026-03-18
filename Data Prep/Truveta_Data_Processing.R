# Script to Process all data from Truveta into neat dataframe and export to EXCEL

# Import libraries
library("jsonlite")
library(dplyr)
library(tidyr)
library("ggplot2")
library("survival")
library("survminer")
library(scales)
library(writexl)
library(readxl)


#------------- Load in State and State Code data----------------
# Loading state concept id table
states_json = fromJSON("trans_people_in_truveta_monthly_FINAL.json")
state_names = states_json[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["tConcepts"]][[1]][["conceptName"]]
state_codes_df <- data.frame(state_name = state_names)
state_id <- states_json[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["tConcepts"]][[1]][["conceptCode"]]
state_codes_df$state_code <- state_id
state_codes_df$state_code <- as.numeric(state_codes_df$state_code)


#-------------Load in Counts from Truveta------------------------
# Includes # Trans people and # total people in truveta by state

# Loading trans and total people in truveta state aggregates
trans_agg_Truveta = fromJSON("trans_people_in_truveta_monthly_FINAL.json")
trans_people = trans_agg_Truveta[["data"]][["runVetaQuery"]][["otherAggregates"]][["result"]][["table"]][["rows"]][[1]]
trans_people_df = as.data.frame(trans_people)

total_agg_Truveta = fromJSON("total_people_in_truveta_monthly_FINAL.json")
total_people = total_agg_Truveta$data$runVetaQuery$otherAggregates$result$table$rows[[1]]
total_people_df = as.data.frame(total_people)

# Remove Nulls 
trans_people_df <- trans_people_df[trans_people_df$V1 != "Null" & trans_people_df$V2 != "Null" & trans_people_df$V3 != "Null", ]
total_people_df <- total_people_df[total_people_df$V1 != "Null" & total_people_df$V2 != "Null" & total_people_df$V3 != "Null", ]

# Format aggregates as numbers 
trans_people_df$V2 <- as.numeric(trans_people_df$V2)
trans_people_df$V3 <- as.numeric(trans_people_df$V3)

total_people_df$V2 <- as.numeric(total_people_df$V2)
total_people_df$V3 <- as.numeric(total_people_df$V3)

# Join total_people_df and trans_people_df to see numbers in truveta by state
truveta_aggs <- full_join(total_people_df, trans_people_df, by = c("V1", "V2"))
colnames(truveta_aggs)[colnames(truveta_aggs) == "V3.x"] <- "Total People In Truveta"
colnames(truveta_aggs)[colnames(truveta_aggs) == "V3.y"] <- "Trans People In Truveta"

# Change NA values to 0
truveta_aggs$"Total People In Truveta" <- 
  replace_na(truveta_aggs$"Total People In Truveta", 0)
truveta_aggs$"Trans People In Truveta" <- 
  replace_na(truveta_aggs$"Trans People In Truveta", 0)

# Join trans_people_df with state_codes_df
people_df <- full_join(truveta_aggs, state_codes_df, by = c("V2" = "state_code"))



#-------------------Load in T Prescription data--------------------------
# Loading T prescription events from  truveta state aggregates
t_prescr_Truveta = fromJSON("T_numbers_by_state_FINAL.json")
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



#------------------------Merge together all data-----------------------
merged_df <- inner_join(t_numbers_joined, people_df, by = c("state_name" = "state_name", "V1"))
colnames(merged_df)[colnames(merged_df) == "V1"] <- "Date"
colnames(merged_df)[colnames(merged_df) == "V3"] <- "Trans People In Truveta"
colnames(merged_df)[colnames(merged_df) == "state_name"] <- "State"
merged_df <- subset(merged_df, select = -V2) #remove state code column
merged_df <- merged_df %>% select(Date, State, everything()) #reorder columns

# Only keep states that will be used in control or treatment groups
keep_states <- c("Michigan", "Wisconsin", "Pennsylvania", "Virginia", "Hawaii", 
                 "Alaska", "North Carolina", "California", "Iowa", "Louisiana", 
                 "Maryland", "Missouri", "Ohio", "Oregon", "Texas", "Washington",
                 "Florida", "Illinois", "Arizona") # Arizona only for telehealth analysis

merged_df <- merged_df[merged_df$State %in% keep_states, ]

# Add column for rates of total T and virtual T per 1000 people in Truveta
merged_df <- merged_df %>%
  mutate(`Virtual_T_Rate` = `Virtual T Prescriptions` / (`Total People In Truveta` / 1000))

merged_df <- merged_df %>%
  mutate(`Total_T_Rate` = `Total T Prescriptions` / (`Total People In Truveta` / 1000))


#----------------------Add date for legislation dates
# Make Dataframe for legislation dates for "treatment group" states
states <- c(
  "California", "Florida", "Illinois", "Iowa", "Louisiana",
  "Maryland", "Missouri", "Ohio", "Oregon", "Texas", "Washington"
)

leg_dates <- as.Date(c(
  "2023-01-01", "2023-05-01", "2023-01-12", "2023-03-01", "2021-01-01",
  "2024-01-01", "2023-08-01", "2024-08-01", "2024-01-01", "2023-09-01", "2023-08-01"
))

leg_df <- data.frame(
  state = states,
  legislation_date = leg_dates
)

# Merge this dataframe with merged_df
merged_df <- full_join(merged_df, leg_df, by = c("State" = "state"))

#--------------------------Output Data to Excel--------------------------
# Write result to excel file
write_xlsx(merged_df, "Truveta_Prescriptions.xlsx")



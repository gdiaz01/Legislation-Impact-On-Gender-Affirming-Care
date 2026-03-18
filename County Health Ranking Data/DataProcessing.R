library("jsonlite")
library(dplyr)
library(tidyr)
library("ggplot2")
library("survival")
library("survminer")
library(scales)
library(writexl)
library(readxl)

# ------------COUNTY HEALTH RANKING DATA-----------------
path  <- "County Health Ranking Data/"
files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# read each file into a list element
dfs <- lapply(files, read.csv)

# only keep rows for states (filter out all counties)
dfs_filtered <- lapply(
  dfs,
  function(x) x[x$County.FIPS.Code %in% c("000", "0"), ]
)

# use file names (without extension) as object names
names(dfs_filtered) <- tools::file_path_sans_ext(basename(files))
names(dfs_filtered)
names(dfs) <- tools::file_path_sans_ext(basename(files))


# list column names for each dataframe 
cols_list  <- lapply(dfs_filtered, names)

# filter columns

# -------------- 2019 data ----------------
cols_to_keep_2019 <- c(
  "County.FIPS.Code",
  "State.Abbreviation",
  "Name",
  "Release.Year",
  "Uninsured.raw.value",
  "Median.household.income.raw.value",
  "X..below.18.years.of.age.raw.value",
  "X..65.and.older.raw.value",
  "X..Non.Hispanic.African.American.raw.value",
  "X..American.Indian.and.Alaskan.Native.raw.value",
  "X..Asian.raw.value",
  "X..Native.Hawaiian.Other.Pacific.Islander.raw.value",
  "X..Hispanic.raw.value",
  "X..Non.Hispanic.white.raw.value",
  "X..Females.raw.value"
)
dfs_filtered[["analytic_data2019"]] <- dfs_filtered[["analytic_data2019"]] |>
  select(any_of(cols_to_keep_2019))

# -------------- 2020 data ----------------
cols_to_keep_2020 <- c(
  "County.FIPS.Code",
  "State.Abbreviation",
  "Name",
  "Release.Year",
  "Uninsured.raw.value",
  "Median.household.income.raw.value",
  "X..below.18.years.of.age.raw.value",
  "X..65.and.older.raw.value",
  "X..Non.Hispanic.Black.raw.value",
  "X..American.Indian...Alaska.Native.raw.value",
  "X..Asian.raw.value",
  "X..Native.Hawaiian.Other.Pacific.Islander.raw.value",
  "X..Hispanic.raw.value",
  "X..Non.Hispanic.White.raw.value",
  "X..Females.raw.value"
)
dfs_filtered[["analytic_data2020_0"]] <- dfs_filtered[["analytic_data2020_0"]] |>
  select(any_of(cols_to_keep_2020))

# -------------- 2021 data ----------------
cols_to_keep_2021 <- c(
  "County.FIPS.Code",
  "State.Abbreviation",
  "Name",
  "Release.Year",
  "Uninsured.raw.value",
  "Median.household.income.raw.value",
  "X..below.18.years.of.age.raw.value",
  "X..65.and.older.raw.value",
  "X..Non.Hispanic.Black.raw.value",
  "X..American.Indian...Alaska.Native.raw.value",
  "X..Asian.raw.value",
  "X..Native.Hawaiian.Other.Pacific.Islander.raw.value",
  "X..Hispanic.raw.value",
  "X..Non.Hispanic.White.raw.value",
  "X..Females.raw.value"
)
dfs_filtered[["analytic_data2021"]] <- dfs_filtered[["analytic_data2021"]] |>
  select(any_of(cols_to_keep_2021))

# -------------- 2022 data ----------------
cols_to_keep_2022 <- c(
  "County.FIPS.Code",
  "State.Abbreviation",
  "Name",
  "Release.Year",
  "Uninsured.raw.value",
  "Median.household.income.raw.value" ,
  "X..below.18.years.of.age.raw.value",
  "X..65.and.older.raw.value",
  "X..non.Hispanic.Black.raw.value",
  "X..American.Indian...Alaska.Native.raw.value",
  "X..Asian.raw.value",
  "X..Native.Hawaiian.Other.Pacific.Islander.raw.value",
  "X..Hispanic.raw.value",
  "X..non.Hispanic.white.raw.value",
  "X..female.raw.value"
)
dfs_filtered[["analytic_data2022"]] <- dfs_filtered[["analytic_data2022"]] |>
  select(any_of(cols_to_keep_2022))

# ------------2023-2025 years----------------
cols_to_keep <- c(
  "County.FIPS.Code",
  "State.Abbreviation",
  "Name",
  "Release.Year",
  "Uninsured.raw.value",
  "Median.Household.Income.raw.value",
  "X..Below.18.Years.of.Age.raw.value",
  "X..65.and.Older.raw.value",
  "X..Non.Hispanic.Black.raw.value",
  "X..American.Indian.or.Alaska.Native.raw.value",
  "X..Asian.raw.value",
  "X..Native.Hawaiian.or.Other.Pacific.Islander.raw.value",
  "X..Hispanic.raw.value",
  "X..Non.Hispanic.White.raw.value",
  "X..Female.raw.value"
)
dfs_filtered[["analytic_data2023_0"]] <- dfs_filtered[["analytic_data2023_0"]] |>
  select(any_of(cols_to_keep))
dfs_filtered[["analytic_data2024"]] <- dfs_filtered[["analytic_data2024"]] |>
  select(any_of(cols_to_keep))
dfs_filtered[["analytic_data2025_v3"]] <- dfs_filtered[["analytic_data2025_v3"]] |>
  select(any_of(cols_to_keep))

# standard names you want for all 15 columns
std_names <- c(
  "County FIPS Code",                                   
  "State Abbreviation",                                   
  "Name",                                  
  "Release Year",                                        
  "Uninsured raw value",                                  
  "Median Household Income raw value",                    
  "% Below 18 Years of Age raw value",                   
  "% 65 and Older raw value",                     
  "% Non-Hispanic Black raw value",                       
  "% American Indian or Alaska Native raw value",         
  "% Asian raw value",        
  "% Native Hawaiian or Other Pacific Islander raw value",
  "% Hispanic raw value",
  "% Non-Hispanic White raw value",                       
  "% Female raw value"
)

# rename all columns in every data frame in the list
dfs_filtered <- lapply(
  dfs_filtered,
  function(x) {
    names(x) <- std_names
    x
  }
)

# Bind all dataframe rows together
final_df <- bind_rows(dfs_filtered)

# Removes rows with US or DC as state abbrev.
final_df <- final_df[!(final_df$'State Abbreviation' %in% c("US", "DC")), ]

# Write result to excel file
write_xlsx(final_df, "County Health Ranking Data/CHR_Data_2019_2025.xlsx")

#------------------MEDICARE/MEDICAID ENROLLMENT DATA-----------------

# Load in data
med_df <- read.csv("Medicare_Monthly_Enrollment_Sep_2025.csv")
pop_df <- read_excel(
  path = "Population_Data.xlsx",
  sheet = "Annual"   
)

# Remove unnecessary rows
med_df <- med_df[!(med_df$BENE_STATE_ABRVTN %in% c("DC", "AS", "GU", "MP", "PR", "VI", "UK", "FO")), ]
med_df <- med_df[(med_df$YEAR %in% c("2019", "2020", "2021", "2022", "2023", "2024")), ]

# drop unnecessary columns
med_df <- med_df %>% select(c("YEAR", "BENE_STATE_ABRVTN", "BENE_STATE_DESC", "DUAL_TOT_BENES"))

# transpose rows and columns of population data
pop_df_t <- as.data.frame(t(pop_df))
# use first row as column names
colnames(pop_df_t) <- pop_df_t[1, ]
# drop the first row (now redundant)
pop_df_t <- pop_df_t[-1, ]

# convert entries to numeric
pop_df_t[] <- lapply(pop_df_t, as.numeric)

# multiply all entries by 1000 because raw data is in thousands
pop_df_t <- pop_df_t * 1000

# rename columns
pop_df_t$RowName <- rownames(pop_df_t)        # create a new column from row names
pop_df_t <- pop_df_t[, c("RowName", names(pop_df_t)[names(pop_df_t) != "RowName"])]  # move it to first position
rownames(pop_df_t) <- NULL              # optional: drop row names
colnames(pop_df_t)[1] <- "State_Abbrev"  # e.g. "State" or whatever you want
# rename columns 2 through 6 using their first 4 characters
names(pop_df_t)[2:7] <- substr(names(pop_df_t)[2:7], 1, 4)

# update state abbreviations for consistency
pop_df_t[[1]] <- substr(pop_df_t[[1]], 1, 2)

# pivot data to match med_df for joining
long_pop_df <- pop_df_t %>%
  pivot_longer(
    cols = `2019`:`2024`,        # all year columns
    names_to = "Year",           # new column for year
    values_to = "Population"     # new column for population
  )

# join medicare/medicaid and population data frames
long_pop_df$Year <- as.numeric(long_pop_df$Year)
med_pop <- full_join(med_df, long_pop_df, by = c("YEAR" = "Year", "BENE_STATE_ABRVTN" = "State_Abbrev"))

# calculate fraction of state population on medicaid or medicare
med_pop$DUAL_TOT_BENES <- as.numeric(gsub(",", "", med_pop$DUAL_TOT_BENES))
med_pop$FRAC_ON_MED <- med_pop$DUAL_TOT_BENES / med_pop$Population

# create new dataframe from med_pop with only relevant columns
final_med_pop <- med_pop %>%
  select(
    Year = YEAR,
    State_Abbrev = BENE_STATE_ABRVTN,
    State_Name = BENE_STATE_DESC,
    Fraction_Medicaid_Medicare = FRAC_ON_MED
  )

# Write result to excel file
write_xlsx(final_med_pop, "Medicaid_Medicare.xlsx")

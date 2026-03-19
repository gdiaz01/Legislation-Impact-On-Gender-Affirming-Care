# Legislation-Impact-On-Gender-Affirming-Care
This project analyzes different policies in the U.S. and their effects on gender affirming care access.

## Conclusions
### Telehealth Policy Analysis:
- 4 / 7 states analyzed had higher prescription rates after a policy that expanded telemedicine allowances
    - Results indicate 2020 Telehealth Policy led to an increase in hormone therapy access
### Gender-Affirming Care-Related Legislation Analysis:
- 4 / 6 states had lower prescription rates after restrictive gender-affirming care legislation
    - Results indicate reduced hormone therapy access for transgender (specifically trans-        masculine) people
- Only 2 / 5 states had higher prescription rates after protective gender-affirming care     legislation
    - Results indicate that formal protections do not necessarily translate to a uniform increase in access to hormone therapy

## Included Files
This package includes the following files:
1. PDF of all figures from results
2. Data Prep/Raw Data Folders
    a. County Health Ranking Data
    b. Data Prep
    c. JSON Files
3. Final Dataframes used in analysis
    a. SCM_Dataframe.xlsx
    b. Truveta_Prescriptions.xlsx
4. Analysis R Markdown Files
    a. Healthcare_Policies_SCM_Analysis.Rmd
    b. Telehealth_Policies_ITS_Analysis.Rmd


## Steps For Usage:
***Note: File pathways In R scripts and markdown files will need to be updated to your active directory.

### Step 1: Create SCM Covariates DataFrame
1. In the County Health Rankings Folder, run DataProcessing.R
    This will output two excel files:
      - CHR_Data_2019_2025.xlsx
      - Medicaid_Medicare.xlsx
    These two files contain County Health Demographic Data, Medicaid and Medicare data, and state population data that will all be used     in the final SCM covariates data frame.
2. Run SCM_covariate.R to create the final covariates dataframe
    This will output SCM_Dataframe.xlsx which will be used for the final analysis.

### Step 2: Create Truveta Prescriptions DataFrame
1. Run Truveta_Data_Processing.R
    This will take in the three JSON files that contain prescription rates from Truveta and output Truveta_Prescriptions.xlsx which         will be used for the final analysis.

### Step 3: Run Analysis
1. Run Telehealth_Policies_ITS_Analysis.Rmd
    The resulting figures from this markdown file can also be found in the Figures folder
2. Run Healthcare_Policies_SCM_Analysis.Rmd
    The resulting figures from this markdown file can also be found in the Figures folder

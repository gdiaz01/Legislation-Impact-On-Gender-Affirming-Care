# Legislation-Impact-On-Gender-Affirming-Care
This project analyzes different policies in the U.S. and their effects on gender affirming care access.

## Conclusions

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

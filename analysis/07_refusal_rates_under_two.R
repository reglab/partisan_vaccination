# Fetch refusal rates before age 2 and vaccination rates 

#Our corresponding refusal rates before age 2 are considerably lower, at 

source('~/afc-claire/code/analysis/00_partisan_difference_functions.R')

data = process_data(filename = 'child_politics_refusals_covariates_01202026.csv.zip', 
                    time_var = 'time_to_refusal', 
                    filter_yr = 2000, 
                    filter_outcome_time = 2,
                    filter_engage_time = 1,
                    remove_after_4 = FALSE)

mean(data$indic)

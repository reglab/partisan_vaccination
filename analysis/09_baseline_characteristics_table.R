library(readr)
library(dplyr)
source('~/afc-claire/code/analysis/00_partisan_difference_functions.R')

# creates summary table of overall, refusal, delay, MMR cohort characteristics
df <- read_csv("/share/pi/deho-pi/AFC/mortonc/processed/patients_ruca_svi.csv.zip")
df <- df %>%
  filter(!is.na(household_id))

data_vax_late = process_data(filename = 'child_politics_covariates_01202026.csv.zip',
                             time_var = 'time_to_vax',
                             filter_yr = 1988,
                             filter_outcome_time= 100,
                             filter_engage_time = 1,
                             remove_after_4 = FALSE)
data <- data_vax_late %>%
  filter(indic == TRUE)

data_no_outliers <- data %>%
  filter(time_to_vax < 6+11/12) %>%
  select(-c(gender, race, raceeth, hispanic, urban_rural, party, svi))

mmr_df = process_data(filename = 'child_politics_covariates_01202026.csv.zip', 
                    time_var = 'time_to_vax', 
                    filter_yr = 1988, 
                    filter_outcome_time= 3+11/12, # must have been vaccinated for an on time second dose
                    filter_engage_time = 1,
                    remove_after_4 = FALSE) %>%
  select(-c(gender, race, raceeth, hispanic, urban_rural, party, svi))

refusal_df = process_data(filename = 'child_politics_refusals_covariates_01202026.csv.zip', 
                    time_var = 'time_to_refusal', 
                    filter_yr = 2000, 
                    filter_outcome_time = 6+11/12,
                    filter_engage_time = 1,
                    remove_after_4 = FALSE) %>%
  select(-c(gender, race, raceeth, hispanic, urban_rural, party, svi))

# group SVI df
df$SVI <- cut(
  df$svi,
  breaks = c(0, 0.25, 0.50, 0.75, 1.00),
  labels = c("[0,25)", "[25,50)", "[50,75)", "[75,100]"),
  include.lowest = TRUE,
  right = FALSE
)

# add things to df, data_no_outliers, mmr_df, refusal_df
data_no_outliers <- merge(data_no_outliers, df, by = "patientuid")
mmr_df <- merge(mmr_df, df, by = "patientuid")
refusal_df <- merge(refusal_df, df, by = "patientuid")

# helper
summarize_var <- function(data, var, var_name) {
  data %>%
    mutate(
      variable = var_name,
      description = if (var_name == "n") "n" else .data[[var]]
    ) %>%
    group_by(variable, description) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(
      total_n = sum(n),
      excl_n = sum(
        n[!is.na(description) &
            !description %in% c("Other/Unknown", "Unknown")]
      ),
      denom = ifelse(
        is.na(description) | description %in% c("Other/Unknown", "Unknown"),
        total_n,
        excl_n
      ),
      value = if (var_name == "n") {
        as.character(n)
      } else {
        paste0(
          prettyNum(n, ","),
          " (",
          round(n / denom * 100, 1),
          "%)"
        )
      }
    ) %>%
    select(variable, description, value)
}

make_one_tab <- function(df){
  bind_rows(
    summarize_var(df, NULL, "n"),
    summarize_var(df, "gender", "gender"),
    summarize_var(df, "raceeth", "raceeth"),
    summarize_var(df, "party", "party"),
    summarize_var(df, "SVI", "svi"),
    summarize_var(df, "urban_rural", "urban_rural")
  )
}

final_table <- make_one_tab(df) %>%
  left_join(make_one_tab(refusal_df), by = c("variable", "description")) %>%
  left_join(make_one_tab(mmr_df), by = c("variable", "description")) %>%
  left_join(make_one_tab(data_no_outliers), by = c("variable", "description"))
  

# To create elements of the US column that aren't simply cited from a separate source
# u.s. pop breakdown of SVI
# https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html 
# download 2022 csv for whole u.s.
svi <- read_csv("SVI_2022_US_county.csv") %>% 
  transmute(
    pop = E_TOTPOP,
    svi_quartile = case_when(
      RPL_THEMES < 0.25 ~ "[0,25)",
      RPL_THEMES < 0.5 ~ "[25,50)",
      RPL_THEMES < 0.75 ~ "[50,75)",
      TRUE ~ "[75,100]"
    )
  ) %>% 
  group_by(svi_quartile) %>% 
  summarize(pop = sum(pop,na.rm=T)) %>% 
  mutate(perc = pop/sum(pop))

# u.s. pop breakdown of RUCA
# https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes
# download https://www.ers.usda.gov/media/5443/2020-rural-urban-commuting-area-codes-census-tracts.csv?v=90750 
library(censusapi)

zip_pop <- getCensus(
  name = "acs/acs5",
  vintage = 2023,
  region = "zip code tabulation area:*",
  vars = "B01001_001E"
) %>% 
  rename(ZIP = zip_code_tabulation_area, pop = B01001_001E)

ruca <- read_csv("RUCA-codes-2020-zipcode.csv") %>% 
  transmute(
    ZIP = ZIPCode,
    ruca = ifelse(PrimaryRUCA %in% c(7,8,9,10), "R", "U")
  ) %>% 
  left_join(zip_pop) %>% 
  group_by(ruca) %>% 
  summarize(pop = sum(pop,na.rm=T)) %>% 
  mutate(perc = pop/sum(pop))
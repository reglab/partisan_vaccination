library(readr)
library(dplyr)
source('~/afc-claire/code/analysis/00_partisan_difference_functions.R')

# creates dropout analysis table comparing characteristics of
# 1. General people with no household ID
# 2. Potential children with no parent L2 data
# 3. Potential children with mixed political affiliation parents
# 4. Potential children who had Dem/Rep parents

df <- read_csv("/share/pi/deho-pi/AFC/mortonc/processed/patients_ruca_svi.csv.zip") %>%
  filter(!is.na(household_id))

# group SVI df
df$SVI <- cut(
  df$svi,
  breaks = c(0, 0.25, 0.50, 0.75, 1.00),
  labels = c("[0,25)", "[25,50)", "[50,75)", "[75,100]"),
  include.lowest = TRUE,
  right = FALSE
)

df_no_l2 <- df %>%
  filter(is.na(party))
df_mixed <- df %>%
  filter(party %in% c("Both", "Other"))
df_dem_rep <- df %>%
  filter(party %in% c("Democratic", "Republican"))

df_no_household <- read_csv("/share/pi/deho-pi/AFC/mortonc/processed/patients_ruca_svi.csv.zip") %>%
  filter(is.na(household_id))
df_no_household$SVI <- cut(
  df_no_household$svi,
  breaks = c(0, 0.25, 0.50, 0.75, 1.00),
  labels = c("[0,25)", "[25,50)", "[50,75)", "[75,100]"),
  include.lowest = TRUE,
  right = FALSE
)

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
    summarize_var(df, "SVI", "SVI"),
    summarize_var(df, "urban_rural", "urban_rural")
  )
}

final_table <- make_one_tab(df_no_household) %>%
  left_join(make_one_tab(df_no_l2), by = c("variable", "description")) %>%
  left_join(make_one_tab(df_mixed), by = c("variable", "description")) %>%
  left_join(make_one_tab(df_dem_rep), by = c("variable", "description"))

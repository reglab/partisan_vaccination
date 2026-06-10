# Plotting code for map
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(st)
library(usmap) 
library(patchwork)
options(tigris_class = "sf")

df <- read_csv("/share/pi/deho-pi/AFC/mortonc/processed/state_counts_01202026.csv.zip")

# Compute percent differences
df <- df %>%
  mutate(
    pct_acs = (children_merged_population/sum(children_merged_population)) - (acs_population/sum(acs_population)),
    pct_afc = (children_merged_population/sum(children_merged_population)) - (afc_population/sum(afc_population))
  )

# Categorize pct_acs into bins including <-5%
df <- df %>%
  mutate(
    pct_acs_cat = cut(
      pct_acs * 100, # convert to %
      breaks = c(-Inf, -2, -1, 1, 2, Inf),
      labels = c("< -2%", "-2% to -1%", "-1% to 1%", "1% to 2%", "> 2%"),
      right = TRUE
    ),
    pct_afc_cat = cut(
      pct_afc * 100, # convert to %
      breaks = c(-Inf, -2, -1, 1, 2, Inf),
      labels = c("< -2%", "-2% to -1%", "-1% to 1%", "1% to 2%", "> 2%"),
      right = TRUE
    )
  )

fips_to_state <- tibble(
  fips_state = c(
    "01","02","04","05","06","08","09","10","11","12","13","15",
    "16","17","18","19","20","21","22","23","24","25","26","27",
    "28","29","30","31","32","33","34","35","36","37","38","39",
    "40","41","42","44","45","46","47","48","49","50","51","53",
    "54","55","56"
  ),
  state_name = c(
    "alabama","alaska","arizona","arkansas","california","colorado","connecticut",
    "delaware","district of columbia","florida","georgia","hawaii",
    "idaho","illinois","indiana","iowa","kansas","kentucky","louisiana",
    "maine","maryland","massachusetts","michigan","minnesota","mississippi",
    "missouri","montana","nebraska","nevada","new hampshire","new jersey",
    "new mexico","new york","north carolina","north dakota","ohio","oklahoma",
    "oregon","pennsylvania","rhode island","south carolina","south dakota",
    "tennessee","texas","utah","vermont","virginia","washington","west virginia",
    "wisconsin","wyoming"
  )
)

# Join data with state names
df_map <- df %>% left_join(fips_to_state, by = "fips_state")

# get simplified US state polygons (includes AK, HI, DC)
us_states <- tigris::states(cb = TRUE, resolution = "20m") %>%
  filter(NAME != "Puerto Rico")

# make lowercase state name
us_states <- us_states %>%
  mutate(state_name = tolower(NAME))

# join with your df_map
map_data_sf <- us_states %>%
  left_join(df_map, by = "state_name")

# define palette
cols <- rev(RColorBrewer::brewer.pal(5, "RdYlBu"))

# get US state geometry with Alaska & Hawaii repositioned
us_states_shifted <- us_map(regions = "states") %>%  # usmap places AK/HI as insets bottom-left:contentReference[oaicite:0]{index=0}
  filter(full != "Puerto Rico") %>%
  mutate(full = tolower(full))

# merge your data into usmap sf
map_data_shifted <- us_states_shifted %>%
  rename(state_name = full) %>%
  left_join(df_map, by = "state_name")

# plot pct_acs (Alaska & Hawaii as insets with no huge empty margin)
map_acs <- ggplot(map_data_shifted) +
  geom_sf(aes(fill = pct_acs_cat), color = "black", size = 0.2) +
  scale_fill_manual(
    values = setNames(cols, levels(map_data_shifted$pct_acs_cat)),
    drop = FALSE
  ) +
  labs(title = "Matched Child Population Relative to 2024 ACS Population") +
  labs(fill = "")+
  theme_void()
c(min(map_data_shifted$pct_acs), max(map_data_shifted$pct_acs))

# plot pct_afc
map_afc <- ggplot(map_data_shifted) +
  geom_sf(aes(fill = pct_afc_cat), color = "black", size = 0.2) +
  scale_fill_manual(
    values = setNames(cols, levels(map_data_shifted$pct_afc_cat)),
    drop = FALSE
  ) +
  labs(title = "Matched Child Population Relative to AFC Population") +
  labs(fill = "")+
  theme_void()
c(min(map_data_shifted$pct_afc), max(map_data_shifted$pct_afc))

map_acs / map_afc

ggsave(
  '~/afc-claire/code/figures/maps.png',
  width = 5,
  height = 7)

ggsave(
  '~/afc-claire/code/figures/maps.pdf',
  width = 5,
  height = 7)

library(nflfastR)
library(dplyr)
library(gt)

pbp <- load_pbp(2016:2025)


#Epa per occurrence w/ totals

ep_per_event_table <- pbp %>%
  filter(!is.na(season)) %>%
  group_by(season) %>%
  summarise(
    interceptions = sum(interception == 1, na.rm = TRUE),
    sacks_no_fumble = sum(sack == 1 & fumble_lost != 1, na.rm = TRUE),
    sack_fumbles = sum(sack == 1 & fumble_lost == 1, na.rm = TRUE),
    
    epa_per_interception = mean(epa[interception == 1], na.rm = TRUE),
    epa_per_sack = mean(epa[sack == 1 & fumble_lost != 1], na.rm = TRUE),
    epa_per_sack_fumble = mean(epa[sack == 1 & fumble_lost == 1], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    epa_per_interception = round(epa_per_interception, 3),
    epa_per_sack = round(epa_per_sack, 3),
    epa_per_sack_fumble = round(epa_per_sack_fumble, 3)
  ) %>%
  arrange(season)

#Building table

ep_per_event_table %>%
  gt() %>%
  tab_header(
    title = "EPA per Event: Interceptions, Sacks, and Sack Fumbles",
    subtitle = "Average EPA Lost per Play and Total Occurrences (NFL 2019–2025)"
  ) %>%
  cols_label(
    season = "Season",
    
    epa_per_interception = "EPA/INTs",
    interceptions = "Total INTs",
    
    epa_per_sack = "EPA/Sack",
    sacks_no_fumble = "Sacks(No Fumble)",
    
    epa_per_sack_fumble = "EPA/Sack Fumble",
    sack_fumbles = "Sack Fumbles"
  ) %>%
  cols_move(interceptions, after = epa_per_interception) %>%
  cols_move(sacks_no_fumble, after = epa_per_sack) %>%
  cols_move(sack_fumbles, after = epa_per_sack_fumble)
library(nflfastR)
library(dplyr)
library(gt)

pbp <- load_pbp(2016:2025)

# Epa per event and their totals

ep_per_event_table <- pbp %>%
  filter(!is.na(season)) %>%
  group_by(season) %>%
  summarise(
    interceptions = sum(interception == 1, na.rm = TRUE),
    sacks_no_fumble = sum(sack == 1 & fumble_lost != 1, na.rm = TRUE),
    conversion_rate = (sum(sack == 1 & fumble_lost !=1, na.rm = TRUE))/(sum(interception == 1, na.rm = TRUE)),
    epa_per_interception = mean(epa[interception == 1], na.rm = TRUE),
    epa_per_sack = mean(epa[sack == 1 & fumble_lost != 1], na.rm = TRUE),
    epa_per_sack_interception_rate = epa_per_sack*conversion_rate,
    .groups = "drop"
  ) %>%
  mutate(
    epa_per_interception = round(epa_per_interception, 3),
    epa_per_sack = round(epa_per_sack, 3),
  ) %>%
  arrange(season)

ep_per_event_table %>%
  gt() %>%
  tab_header(
    title = "EPA per Event: Interceptions, Sacks, and Sack Fumbles",
    subtitle = "Average EPA Lost per Play and Total Occurrences (2016–2025)"
  ) %>%
  cols_label(
    
    season = "Season",
    epa_per_interception = "EPA/Int",
    interceptions = "Total INTs",
    epa_per_sack = "EPA/Sack",
    sacks_no_fumble = "Sacks(No TO)",
    epa_per_sack_interception_rate = "EPA/Sack = INT",
    conversion_rate = "Conversion"
    
  ) %>%
  cols_move(interceptions, after = epa_per_interception) %>%
  cols_move(sacks_no_fumble, after = epa_per_sack) %>%
  cols_move(conversion_rate, after = sacks_no_fumble)%>%
  cols_move(epa_per_sack_interception_rate, after = conversion_rate)

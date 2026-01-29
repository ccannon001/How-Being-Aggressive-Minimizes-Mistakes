library(nflfastR)
library(dplyr)
library(gt)

pbp <- load_pbp(2016:2025)

ep_per_event_table <- pbp %>%
  filter(!is.na(season)) %>%
  group_by(season) %>%
  summarise(
    
    interceptions = sum(interception == 1, na.rm = TRUE),
    sacks = sum(sack == 1, na.rm = TRUE),
    conversion_rate = sacks/interceptions,
    epa_per_interception = mean(epa[interception == 1], na.rm = TRUE),
    epa_per_sack = mean(epa[sack == 1], na.rm = TRUE),
    EPA_sack_equal = epa_per_sack*conversion_rate,
    .groups = "drop"
  ) %>%
  mutate(
    epa_per_interception = round(epa_per_interception, 3),
    epa_per_sack = round(epa_per_sack, 3),
    conversion_rate = round(conversion_rate, 3),
    EPA_sack_equal = round(EPA_sack_equal, 3),
  ) %>%
  arrange(season)
ep_per_event_table %>%
  gt() %>%
  tab_header(
    title = "EPA per Event: Interceptions vs All Sacks",
    subtitle = "Average EPA Lost per Play and Total Occurrences (NFL 2016–2025)"
  ) %>%
  cols_label(
    
    season = "Season",
    epa_per_interception = "EPA/INT",
    interceptions = "Total INTs",
    epa_per_sack = "EPA/Sack",
    conversion_rate = "Conversion",
    EPA_sack_equal = "Conv EPA/Sack"
    
  ) %>%
  cols_move(interceptions, after = epa_per_interception)%>%
  cols_move(conversion_rate, after = epa_per_sack)
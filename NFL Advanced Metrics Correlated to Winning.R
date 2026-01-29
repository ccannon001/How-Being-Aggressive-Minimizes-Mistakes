library(nflfastR)
library(tidyverse)

seasons <- 2016:2025
pbp <- load_pbp(seasons)


# Pulling results

games <- pbp %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  group_by(season, game_id, home_team, away_team) %>%
  summarise(
    home_score = max(home_score),
    away_score = max(away_score),
    .groups = "drop"
  ) %>%
  mutate(
    home_win = as.integer(home_score > away_score),
    away_win = as.integer(away_score > home_score)
  )


# Pulling stats

team_game <- pbp %>%
  filter(play_type %in% c("pass", "run")) %>%
  mutate(
    success = as.integer(success == 1),
    explosive = as.integer(
      (play_type == "pass" & air_yards >= 20) |
        (play_type == "run" & yards_gained >= 10)
    ),
    early_down = as.integer(down %in% c(1, 2))
  ) %>%
  group_by(season, game_id, posteam) %>%
  summarise(
    epa_play = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    early_down_epa = mean(epa[early_down == 1], na.rm = TRUE),
    explosive_rate = mean(explosive, na.rm = TRUE),
    yards_per_play = mean(yards_gained, na.rm = TRUE),
    plays = n(),
    turnovers = sum(interception == 1 | fumble_lost == 1, na.rm = TRUE),
    .groups = "drop"
  )

team_game <- team_game %>%
  left_join(
    games %>%
      pivot_longer(
        cols = c(home_team, away_team),
        names_to = "side",
        values_to = "posteam"
      ) %>%
      mutate(win = ifelse(side == "home_team", home_win, away_win)) %>%
      select(season, game_id, posteam, win),
    by = c("season", "game_id", "posteam")
  )


# How each correlate to winning

win_cor_table <- team_game %>%
  select(-season, -game_id, -posteam) %>%
  drop_na() %>%
  summarise(
    across(
      -win,
      ~ cor(.x, win),
      .names = "{.col}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "correlation"
  ) %>%
  arrange(desc(abs(correlation)))

win_cor_table

library(gt)

win_cor_table <- team_game %>%
  select(-season, -game_id, -posteam, -explosive_rate, -plays) %>%
  drop_na() %>%
  summarise(
    across(
      -win,
      ~ cor(.x, win),
      .names = "{.col}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Metric",
    values_to = "Correlation_with_Win"
  ) %>%
  arrange(desc(abs(Correlation_with_Win)))
win_cor_table %>%
  gt() %>%
  fmt_number(
    columns = Correlation_with_Win,
    decimals = 3
  ) %>%
  tab_header(
    title = "What Correlates With Winning?",
    subtitle = "2016-2025"
  ) %>%
  cols_label(
    Metric = "Stat",
    Correlation_with_Win = "Correlation"
  )
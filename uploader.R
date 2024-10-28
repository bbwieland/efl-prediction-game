suppressMessages(library(worldfootballR))
suppressMessages(library(tidyverse))
suppressMessages(library(googlesheets4))
suppressWarnings(suppressMessages(library(logger)))

team_mapping <- suppressMessages(read_csv("/Users/ben/Desktop/Code/efl-prediction-game/team-mapping.csv"))

match_results <- suppressMessages(worldfootballR::load_match_results(country = "ENG",
                                   gender = "M",
                                   season_end_year = 2025, 
                                   tier = "2nd"))

logger::log_info("Successfully accessed match data from worldfootballR API.")

team_games <- match_results %>%
  pivot_longer(c(Home,Away), names_to = "location", values_to = "team") %>%
  mutate(goals_for = ifelse(location == "Home", HomeGoals, AwayGoals),
         goals_against = ifelse(location == "Away", HomeGoals, AwayGoals),
         xg_for = ifelse(location == "Home", Home_xG, Away_xG), 
         xg_against = ifelse(location == "Away", Home_xG, Away_xG)) %>%
  mutate(points_won = case_when(
    goals_for > goals_against ~ 3,
    goals_for == goals_against ~ 1,
    goals_for < goals_against ~ 0,
    TRUE ~ NA
  )) %>%
  filter(Date <= Sys.Date() & !is.na(goals_for)) %>%
  select(team, location, goals_for, goals_against, xg_for, xg_against, points_won)

last_match <- match_results %>%
  filter(!is.na(HomeGoals)) %>%
  arrange(desc(Date), desc(Time)) %>%
  head(1)

last_match_str <- glue::glue("Most recent match: {last_match$Home} {last_match$HomeGoals} vs. {last_match$Away} {last_match$AwayGoals} on {last_match$Day} {last_match$Date}")

logger::log_info(last_match_str)

team_results <- team_games %>%
  group_by(team) %>%
  summarise(games = n(), pts = sum(points_won), gf = sum(goals_for), ga = sum(goals_against), xgf = sum(xg_for), xga = sum(xg_against)) %>%
  mutate(gd = gf - ga, xgd = xgf - xga, gd_90 = gd / games, xgd_90 = xgd / games, gf_xg = gf - xgf, ga_xg = ga - xga) %>%
  mutate(pts = ifelse(team == "Sheffield Utd", pts - 2, pts)) %>% # handler for Sheffield United points deduction
  select(team, games, pts, gd, xgd_90, gd_90, gf, xgf, gf_xg, ga, xga, ga_xg) %>%
  arrange(-pts, -gd, -gf) %>%
  left_join(team_mapping, by = c("team" = "team_fbref")) %>%
  select(-team) %>%
  rename(team = team_sheet) %>%
  mutate(rank = row_number()) %>%
  select(rank, team, everything())

sheet_url = "https://docs.google.com/spreadsheets/d/1otcoW54G-rZu1hQoX7KAUGgkR0WcrPJgMgqqzKgv0mM/edit?gid=62777184#gid=62777184"
tab = "rawtable"

suppressMessages(write_sheet(data = team_results, ss = sheet_url, sheet = tab))

logger::log_info("Successfully uploaded table to Google Sheets.")

## This code schedules a cronjob for the uploader.R R script when run on Mac.

# library(cronR)
# 
# script_name = "/Users/ben/Desktop/Code/efl-prediction-game/uploader.R"
# 
# CRONR_FREQUENCY = "daily"
# CRONR_TIME_OF_DAY = "7AM"
# CRONR_ID = "efl_uploader"
# 
# script_job = cron_rscript(script_name)
# cron_add(script_job,
#          frequency = CRONR_FREQUENCY,
#          at = CRONR_TIME_OF_DAY,
#          id = CRONR_ID
# )
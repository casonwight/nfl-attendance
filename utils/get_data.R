library(tidyverse)

get_revenue_data <- function(){
  rev_data <- data.frame(
    Year = factor(2010:2020), 
    Total_Revenue = c(8.35, 8.82, 9.17, 9.58, 11.09, 12.16, 13.16, 13.68, 14.48, 15.26, NA),
    Label = c(rep("",9), "$15.26 B", "")
  )
  
  return(rev_data)
}

get_attendance_data <- function(){
  tidy_tuesday_URL <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/'
  attendance <- read_csv(paste0(tidy_tuesday_URL, 'attendance.csv'), show_col_types=FALSE)
  games <- read_csv(paste0(tidy_tuesday_URL, 'games.csv'), show_col_types=FALSE)
  
  all_data <- games %>%  # All fame data (year, week, home team, away_team, winner, etc.)
    filter(!grepl("\\D", week)) %>% # Remove atypical weeks
    mutate(week = as.numeric(week)) %>% # Convert week to numbers
    left_join(attendance, by = c("home_team_name" = "team_name", "year" = "year", "week" = "week")) %>% # Add attendance data as well
    select(year, week, weekly_attendance, home_team_name, away_team_name, winner) %>% 
    rename(home_team=home_team_name, away_team=away_team_name) %>% 
    mutate(home_team_won = str_detect(winner, home_team)) 
  
  wins <- all_data %>% group_by(year, winner) %>% 
    summarize(num_games_won = length(week), .groups='drop_last') %>% 
    ungroup() %>% 
    mutate(home_team = word(winner,-1)) %>% 
    select(year, home_team, num_games_won)
  
  home_games <- all_data %>% 
    group_by(year, home_team) %>% 
    summarize(num_home_games = length(week), .groups='drop_last') %>%
    mutate(team = home_team) %>% 
    select(-home_team) %>% 
    ungroup()
  
  num_games <- all_data %>% 
    group_by(year, away_team) %>% 
    summarize(num_away_games = length(week), .groups='drop_last') %>% 
    ungroup() %>% 
    mutate(team = away_team) %>% 
    select(-away_team) %>% 
    right_join(home_games, by=join_by(year, team)) %>% 
    mutate(num_games = num_away_games + num_home_games, home_team = team) %>% 
    select(year, home_team, num_games)
  
  data_reshape <- all_data %>% 
    group_by(year, home_team) %>% 
    summarize(total_attendance = mean(weekly_attendance), .groups='drop_last') %>% 
    ungroup() %>% 
    left_join(num_games, by = join_by(year, home_team)) %>% 
    left_join(wins, by = join_by(year, home_team)) %>% 
    mutate(num_games_won = ifelse(is.na(num_games_won), 0, num_games_won)) %>% 
    group_by(home_team) %>% 
    mutate(prev_attendance = lag(total_attendance), prev_games_won = lag(num_games_won)) %>% 
    ungroup() %>% 
    filter(!is.na(prev_attendance)) %>% 
    mutate(home_team = as.factor(home_team), prev_games_won = as.numeric(prev_games_won)) %>% 
    filter(home_team != "Texans")
  
  return(data_reshape)
}

if(!interactive()){
  attendance <- get_attendance_data()
  attendance
}

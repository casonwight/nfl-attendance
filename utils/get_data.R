library(tidyverse)
library(rvest)

get_revenue_data <- function() {
  rev_data <- data.frame(
    Year = factor(2010:2020), 
    Total_Revenue = c(8.35, 8.82, 9.17, 9.58, 11.09, 12.16, 13.16, 13.68, 14.48, 15.26, NA),
    Label = c(rep("",9), "$15.26 B", "")
  )
  
  return(rev_data)
}

get_team_revenue_data <- function(team_names, team_cities) {
  
  revenues = vector("list", length = length(team_names))
  
  pb = txtProgressBar(min = 1, max = length(team_names), initial = 1) 
  
  for (i in 1:length(team_names)){
    setTxtProgressBar(pb, i)
    
    var_team_name <- team_names[i] %>% str_replace_all(" ", "-") %>% tolower()
    city <- team_cities[i] %>% str_replace_all(" ", "-") %>% tolower()
    
    revenues[[i]] <- "https://www.forbes.com/teams/" %>% 
      paste0(city, "-", var_team_name) %>% 
      read_html() %>% 
      html_node(xpath="/html/body/div[1]/main/div/div[1]/div[10]/div/div[2]/div[2]") %>% 
      html_nodes("[class='bar__item']") %>% 
      html_text() %>% 
      as.data.frame() %>% 
      setNames("year") %>% 
      mutate(year=str_replace_all(year, "\\$", " ")) %>% 
      separate(year, c("year", "rev"), " ", remove=TRUE) %>% 
      mutate(rev=gsub("M", "e6", rev)) %>% 
      mutate(rev=gsub("B", "e9", rev)) %>%
      mutate(rev=as.numeric(rev)) %>% 
      mutate(year=as.numeric(year)) %>% 
      filter(year < 2020) %>% 
      mutate(team_name=team_names[i]) %>% 
      mutate(rev=rev/1000000) %>% 
      mutate(label=ifelse(year==2019, paste0("$", round(rev, 1), " M"),""))
    
  }
  close(pb)
  
  return(do.call(rbind, revenues))
}

get_average_ticket_price_data <- function() {
  price_data <- "https://www.tickpick.com/blog/how-much-are-nfl-tickets/" %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    filter(X2 != "" & X1 != "NFL Team") %>% 
    setNames(c("team_name", "avg_cost_2018", "avg_cost_2019", "avg_cost_2021")) %>% 
    mutate(avg_cost_2019=as.numeric(gsub("\\$", "", avg_cost_2019))) %>% 
    separate(team_name, c("city", "team_name"),sep=" (?=[^ ]*$)") %>% 
    filter(team_name != "Texans") %>% 
    select(team_name, avg_cost_2019) %>% 
    arrange(team_name)
  
  return(price_data)
  
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

get_2020_attendance_data <- function(attendance_data) {
  attendance_2020 <- "https://www.pro-football-reference.com/years/2020/attendance.htm" %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    select(Tm, Home) %>% 
    mutate(Home=replace_na(as.numeric(str_replace(Home, ",", "")), 0) / 16) %>% 
    filter(!is.na(Tm)) %>% 
    filter(Tm != "") %>% 
    separate(Tm, c("city", "team_name"),sep=" (?=[^ ]*$)") %>% 
    filter(team_name != "Texans")
  
  attendance_2020[which("Team" == attendance_2020 %>% pull(team_name)), 1] <- "Washington"
  attendance_2020[which("Team" == attendance_2020 %>% pull(team_name)), 2] <- "Redskins"
  
  attendance_2020 <- attendance_2020 %>% 
    arrange(team_name)
  return(attendance_2020)
}

if(!interactive()){
  attendance <- get_attendance_data()
  attendance
}

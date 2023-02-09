library(tidyverse)

# Average attendance across all years
plot_avg_attendance_all_years <- function(attendance_data){
  plt <- attendance_data %>% 
    group_by(home_team) %>% 
    summarize(avg_attendance = mean(total_attendance)) %>% 
    ungroup() %>%  
    mutate(home_team = fct_reorder(home_team, avg_attendance)) %>% 
    ggplot(aes(x=home_team, y=avg_attendance, fill = avg_attendance)) +
    geom_bar(stat="identity") + 
    coord_flip() + 
    scale_y_continuous(labels = scales::comma) + 
    scale_fill_continuous(labels = scales::comma) +
    labs(title = sprintf("Average Home-Game Attendance from %s to %s", 2000, 2019), x = "Team", y = "Average Home-Game Attendance", fill = "") + 
    theme(legend.position = "none") + 
    theme_light()
  print(plt)
}


# Average win rate across all years
plot_avg_win_rate_all_years <- function(attendance_data){
  plt <- attendance_data %>% 
    group_by(home_team) %>% 
    summarize(total_games_won = sum(num_games_won), total_games = sum(num_games)) %>% 
    ungroup() %>% 
    mutate(win_pct = total_games_won / total_games) %>% 
    mutate(home_team = fct_reorder(home_team, total_games_won)) %>% 
    ggplot(aes(x=home_team, y=total_games_won, fill = total_games_won)) +
    geom_bar(stat="identity") + 
    coord_flip() + 
    geom_hline(yintercept = .5*304, lty = "dashed") + 
    scale_y_continuous(name = "Total Games Won", sec.axis = sec_axis(trans=~./304, name = "Win Percentage", labels = scales::percent)) + 
    labs(title = sprintf("Regular Game Wins (%s to %s)", 2000, 2019), x = "Team", y = "Total Games Won", fill = "") + 
    theme_light() + 
    theme(legend.position = "none")
  print(plt)
}

# Average attendance each year
plot_avg_attendance_each_year <- function(attendance_data){
  plt <- attendance_data %>%
    ggplot(aes(y = total_attendance, x = year, color = home_team)) +
    geom_line() + 
    geom_line(data=data_reshape %>% 
                group_by(year) %>% 
                summarize(total_attendance = mean(total_attendance)) %>% 
                mutate(home_team_name = "Average"), 
              aes(y=total_attendance, x=year, color = home_team), 
              color = "black", lwd = 2) + 
    labs(title = "Average Home-Game Attendance", color = "Team", x = "Year", y = "Attendance") +
    scale_y_continuous(labels = scales::comma) + 
    theme_light()
  print(plt)
}


# Team by team average attendance
plot_avg_attendance_each_team <- function(attendance_data){
  for(team_name in attendance_data %>% pull(home_team) %>% unique()){
    time_plot <- attendance_data %>% 
      filter(home_team == team_name) %>% 
      ggplot(aes(y = total_attendance, x = year)) +
      geom_line() +
      geom_point(aes(size = prev_games_won)) +
      labs(title = team_name, x = "Year", y = "Average Home Game Attendance") + 
      scale_y_continuous(labels = scales::comma)
    print(time_plot)
  }
}


# Yearly revenue for NFL
plot_yearly_revenue <- function(rev_data){
  plt <- rev_data %>% 
    ggplot(aes(x = Year, y = Total_Revenue)) + 
    geom_bar(stat = "identity", width = .4, fill = "royalblue") + 
    theme_light() + 
    labs(title = "Total NFL Revenue", x = "Year", y = "Revenue (in $Bil.)") + 
    geom_text(aes(label = Label), nudge_y = 1) +
    ylim(c(0,max(rev_data$Total_Revenue)*1.3))
  print(plt)
}


if(!interactive()){
  source("utils/get_data.R")
  attendance_data <- get_attendance_data()
  plot_avg_attendance_all_years(attendance_data)
  plot_avg_win_rate_all_years(attendance_data)
  plot_avg_attendance_each_year(attendance_data)
  plot_avg_attendance_each_team(attendance_data)
  
  rev_data <- get_revenue_data()
  plot_yearly_revenue(rev_data)
}

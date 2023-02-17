

get_post_predicted_attendance <- function(attendance, samples, team_names, team_name=NA) {
  
  if (is.na(team_name) | team_name == "All teams"){
    xs <- attendance %>% 
      filter(year == 2019) %>% 
      select(home_team, num_games_won, total_attendance)
    
    post_attendance <- sapply(1:length(team_names), function(i) {
      (
        samples$alphas[, i] +
          xs %>% filter(home_team==team_names[i]) %>% pull(total_attendance) * samples$betas[, i] +
          xs %>% filter(home_team==team_names[i]) %>% pull(num_games_won) * samples$theta
      ) %>% 
        rnorm(n=length(samples$overall_alpha), mean=., sd=samples$sigma)
      
    }) %>% as.data.frame() %>% setNames(team_names)
    
  } else {
    xs <- attendance %>% 
      filter(year == 2019 & home_team==team_name) %>% 
      select(home_team, num_games_won, total_attendance)
    
    mean_attendance <- samples$alphas[, which(team_name==team_names)] + # Team Intercept
      xs$total_attendance * samples$betas[, which(team_name==team_names)] + # Team effect of prior year attendance
      xs$num_games_won * samples$theta # Overall effect of prior year wins
    
    post_attendance <- rnorm(n=length(samples$sigma), mean=mean_attendance,sd=samples$sigma)
  }
  
  return(post_attendance)
}


get_post_predicted_lost_attendance <- function(post_pred_attendance, attendance_2020, team_names, team_name=NA) {
  if (is.na(team_name) | team_name == "All teams"){
    mean_pred_attendance <- post_pred_attendance %>% 
      colMeans() %>% 
      as.data.frame() %>% 
      mutate(team_name=team_names) %>% 
      setNames(c("pred_attendance", "team_name")) %>% 
      left_join(attendance_2020, by = join_by(team_name)) %>% 
      mutate(diff=Home-pred_attendance)
  } else {
    var_team_name <- team_name
    mean_pred_attendance <- post_pred_attendance %>% 
      mean() %>% 
      as.data.frame() %>%  
      mutate(team_name=var_team_name) %>% 
      setNames(c("pred_attendance", "team_name")) %>% 
      left_join(attendance_2020 %>% filter(team_name==var_team_name), by = join_by(team_name)) %>% 
      mutate(diff=Home-pred_attendance)
  }
  return(mean_pred_attendance)
}

dollarfy <- function(dollars) { 
  is_neg <- dollars < 0
  dollars <- abs(dollars)
  div <- findInterval(as.numeric(gsub("\\,", "", dollars)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  
  
  out <- paste0(
    ifelse(is_neg, "\U2012", ""),
    "$",
    round(as.numeric(gsub("\\,","", dollars))/10^(3*(div-1)), 2), 
    c("","K","M","B","T")[div]
  )
  return(out)
}

get_est_lost_rev <- function(attendance_lost, prices_2019, team_name=NA) {
  if (is.na(team_name) | team_name == "All teams") {
    est_lost_rev <- attendance_lost %>% 
      left_join(prices_2019) %>% 
      mutate(total_lost=diff * 16 * avg_cost_2019) %>% 
      pull(total_lost) %>% 
      sum()
  } else {
    var_team_name <- team_name
    avg_price_2019 <- prices_2019 %>% filter(team_name==var_team_name) %>% pull(avg_cost_2019)
    est_lost_rev <- attendance_lost$diff * 16 * avg_price_2019
  }
  out <- dollarfy(est_lost_rev)
  return(out)
}

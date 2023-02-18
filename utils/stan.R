library(tidyverse)

prepare_data_for_stan <- function(data){
  t <- data %>% 
    pull(year) %>% 
    unique()
  
  y <- data %>% 
    select(home_team, total_attendance, year) %>% 
    pivot_wider(names_from = home_team, values_from = total_attendance) %>% 
    select(-year)
  
  ylag1 <- data %>% 
    select(home_team, prev_attendance, year) %>% 
    pivot_wider(names_from = home_team, values_from = prev_attendance) %>% 
    select(-year)
  
  x <- data %>% 
    select(home_team, prev_games_won, year) %>% 
    pivot_wider(names_from = home_team, values_from = prev_games_won) %>% 
    select(-year)
  
  num_teams <- ncol(y)
  teams <- colnames(y)
  n <- nrow(y)
  
  known_data <- list(
    t = t, 
    y = y, 
    ylag1 = ylag1, 
    x = x, 
    num_teams = num_teams, 
    teams = teams, 
    n = n, 
    C = num_teams
  )
  
  return(known_data)
}


get_priors <- function(){
  # Tight priors based on my little knowledge
  priors1 <- list(mu_a = 30000, sigma_a = 20000,
                  mu_b = .7, sigma_b = .5,
                  mu_theta = 1000, sigma_theta = 1000,
                  a_sigma = 10000^2 / 15000^2, b_sigma = 10000 / 15000^2,
                  lambda = 8000, eta = .1)
  
  # Noninformative priors
  priors2 <- list(mu_a = 66000, sigma_a = 2000000,
                  mu_b = 0, sigma_b = 10,
                  mu_theta = 0, sigma_theta = 2000000,
                  a_sigma = 5000^2 / 100000^2, b_sigma = 5000 / 100000^2,
                  lambda = 100000, eta = 20)
  
  # Blend of the first two
  priors3 <- list(mu_a = 66000, sigma_a = 6000,
                  mu_b = .6, sigma_b = .2,
                  mu_theta = 1000, sigma_theta = 500,
                  a_sigma = 5000^2 / 1000^2, b_sigma = 5000 / 1000^2,
                  lambda = 15000, eta = .4)
  
  all_priors <- list(
    priors1=priors1,
    priors2=priors2,
    priors3=priors3
  )
}


get_stan_model <- function(data, priors){
  
  fit <- stan(model_code = readLines("models/bayesian.stan"),
              data = c(data, priors), iter = 20000, warmup = 1000, 
              thin = 2, chains = 4, control = list(max_treedepth = 20))
  
  return(fit)
}
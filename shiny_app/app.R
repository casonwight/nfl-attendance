# Load libraries
library(shiny)
library(shinyBS)


# Load necessary functions
source("utils/eda.R")
source("utils/get_data.R")
source("utils/mcmc_diagnostics.R")
source("utils/stan.R")
source("models/bayesian.R")
source("results/calculations.R")
source("results/plots.R")


# Load data
attendance <<- get_attendance_data() %>% mutate(home_team = str_replace(home_team, "Redskins", "Commanders"))
attendance_2020 <<- get_2020_attendance_data() %>% mutate(team_name = str_replace(team_name, "Redskins", "Commanders"))
prices_2019 <<- get_average_ticket_price_data()
team_names <<- attendance_2020 %>% pull(team_name) %>% unique() %>% as.character()
team_cities <<- sapply(team_names, function(name) attendance_2020 %>% filter(team_name == name) %>% pull(city) %>% .[1])
rev_data <<- get_revenue_data()
rev_team_data <<- get_team_revenue_data(team_names, team_cities)


# Load models and modeling information
prior_names <<- c("Tight priors", "Non-informative priors", "Balanced")
prior <<- get_priors()[[1]]
fit <<- load_bayesian_fit()
fit_samps <<- load_bayesian_samps()
var_names <<- fit_samps %>% names() %>% .[1:6]


source('shiny_app/myUI.R', local = TRUE)
source('shiny_app/myServer.R')


shinyApp(
  ui = myUI,
  server = myServer
)

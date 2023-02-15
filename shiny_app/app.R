library(shiny)
library(shinyBS)
library(scales)

source("utils/get_data.R")
source("utils/eda.R")
source("utils/mcmc_diagnostics.R")
source("utils/stan.R")
source("models/bayesian.R")
source("results/plots.R")
source("results/calculations.R")


attendance <- get_attendance_data()
attendance_2020 <- get_2020_attendance_data()
prices_2019 <- get_average_ticket_price_data()
team_names <- attendance %>% pull(home_team) %>% unique() %>% as.character()
prior_names <- c("Tight priors", "Non-informative priors", "Balanced")
priors <- get_priors()

rev_data <- get_revenue_data()
fits <- load_bayesian_mods()

samples <- vector(mode="list", length=length(fits))

for (i in 1:length(fits)){
  samples[[i]] <- rstan::extract(fits[[i]])
}

var_names <- samples[[1]] %>% names() %>% .[1:6]


runApp('shiny_app')

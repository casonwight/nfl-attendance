options(repos = c(CRAN = "https://cran.rstudio.com/", STAN = "https://mc-stan.org/r-packages/")) 
rsconnect::writeManifest(
  appPrimaryDoc = 'shiny_app/app.R',
  appFiles = c(
    "utils/eda.R",
    "utils/get_data.R",
    "utils/mcmc_diagnostics.R",
    "utils/stan.R",
    "models/bayesian.R",
    "results/calculations.R",
    "results/plots.R",
    "shiny_app/myServer.R",
    "shiny_app/app.R",
    "shiny_app/myUI.R",
    paste("shiny_app/rsconnect", list.files("shiny_app/rsconnect", recursive=TRUE), sep="/"),
    paste("shiny_app/www", list.files("shiny_app/www", recursive=TRUE), sep="/")
  )
)


rsconnect::deployApp(
  appPrimaryDoc = 'shiny_app/app.R',
  appFiles=c(
    "utils/eda.R",
    "utils/get_data.R",
    "utils/mcmc_diagnostics.R",
    "utils/stan.R",
    "models/bayesian.R",
    "results/calculations.R",
    "results/plots.R",
    "results/bayesian_fit.rda",
    "results/bayesian_samps.rda",
    "shiny_app/myServer.R",
    "shiny_app/app.R",
    "shiny_app/myUI.R",
    paste("data", list.files("data", recursive=TRUE), sep="/"),
    paste("shiny_app/rsconnect", list.files("shiny_app/rsconnect", recursive=TRUE), sep="/"),
    paste("shiny_app/www", list.files("shiny_app/www", recursive=TRUE), sep="/")
  ),
  appName="nflattendance", 
  appTitle = "NFL Covid Ticket Sales"
)




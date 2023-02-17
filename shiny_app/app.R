options(repos = c(CRAN = "https://cran.rstudio.com/", STAN = "https://mc-stan.org/r-packages/")) 
rsconnect::deployApp('shiny_app', appName = "nflattendance", appTitle = "NFL Covid Ticket Sales")

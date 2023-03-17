# NFL Attendance
> Compare results to [statista](https://www.statista.com/statistics/1130256/ticket-revenue-loss-nfl-coronavirus-team/)

This project uses a Bayesian hierarchical time series model to estimate the number of attendees at NFL games in 2020. The point is to predict attendance and compare it to actual attendance to estimate the impact of covid on attendance. Then it calculates how much revenue was lost on ticket sales in 2020 based on average ticket costs in 2019.

The model incorporates team-specific hierarchical intercepts and effects of previous year's avg weekly attendance.
It also includes an effect of previous year's number of wins:  

$$
\begin{align}
\text{Est 2020 Attendance}_\text{team}&\sim\mathcal{N}\left(\mu_\text{team}, \sigma^2\right) \\
\mu_\text{team}&=\alpha_\text{team} \\
      &~~~~~~+\beta_\text{team} \times \text{2019 Year Attendance} \\
      &~~~~~~+\theta \times \text{2019 Num Games Won} \\
\sigma&\sim\text{Gamma}(\alpha_\sigma, \beta_\sigma) \\
\alpha_\text{team}&\sim\mathcal{N}(\alpha, \lambda^2) \\
\alpha&\sim\mathcal{N}(\mu_{\alpha}, \sigma^2_\alpha) \\
\beta_\text{team}&\sim\mathcal{N}(\beta, \eta^2) \\
\beta&\sim\mathcal{N}(\mu_{\beta}, \sigma^2_\beta) \\
\theta&\sim\mathcal{N}(\mu_{\theta}, \sigma^2_\theta) \\
\end{align}
$$

## Installation
To run this project, you need to have `R` and `rstan` installed on your machine. You also need the following `R` packages: `tidyverse`, `shiny`, `shinyBS`, `rvest`, `curl`, `coda`.

You can clone this repo using git:
`git clone https://github.com/casonwight/nfl-attendance.git`
Or you can download the zip file from GitHub.

## App
The app contained in this repo is currently run on [a free shiny server](https://cason-wight.shinyapps.io/nflattendance/).
The app has three tabs, Estimated Revenue Loss, Parameter Modeling, and Exploratory Analysis.
For all tabs, you can select a specific team (or all teams at once, at the bottom of the drop down) to see results/analysis.

### Estimated Revenue Loss

This tab contains the following: 
- The estimated dollar revenue loss as a result of covid, calculated as the following:
$\text{avg price of 2019 tickets}\times(\text{estimated 2020 weekly avg attendees}-\text{actual 2020 weekly avg attendees})\text{num yearly games}$
- Plot of the posterior predicted 2020 weekly average attendance vs the actual
- The structure of the Bayesian model (shown above)

### Parameter Modeling
After selecting a parameter, the prior and posterior predicted distributions are shown.
There is also a dropdown for seeing relevant MCMC diagnostics like the Gelman statistic, effective sample size, trace plots, and autocorrelation plots.
If "all teams" and "alphas" or "betas" are selected, then the alpha/beta for all teams are shown.

### Exploratory Analysis
This tab contains the following plots:
- Average weekly attendance by year for the selected team (or overall average since 2000 if "all teams" is selected)
- Win rate by year for the selected team (or overall average win rate since 2000 if "all teams" is selected)
- Total revenue per year for the selected team (or total NFL revenue per year if "all teams" is selected)

## Usage
To train the model, you can use the `fit_bayesian_mods` function in the `models/bayesian.R` script. You can also fit a simpler frequentist model in `models/linear.R`. 
This will save a `rstan::fit` object as an `fit.rda` file in the `results` directory.
The app uses results from only the tight priors.
To save these specific results, use `save_fit_tight_priors` and `save_samps_tight_priors` in `models/bayesian.R`
The samples can then be loaded with `load_bayesian_samps() %>% downsample_samps()`.

Once `bayesian_samps.rda` is saved, you can run the app yourself using `shiny_app/app.R` or deploy with `run_app.R`.

## Structure
Static data for this project is found in the `data` directory.  
`main.Rmd`/`main.html` is a static analysis, comparable to the shiny app on "All teams".  
The code to generate models are stored in the `models` directory.  
The model results and code to generate postprocessing calculations and plots are in the `results` directory.  
All shiny app functions and tools are in the `shiny_app` directory.  
Other tools for MCMC diagnostics, EDA, and data scraping are in the `utils` directory.  
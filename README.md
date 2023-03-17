# NFL Attendance
> Compare results to [statista](https://www.statista.com/statistics/1130256/ticket-revenue-loss-nfl-coronavirus-team/)

This project uses a Bayesian hierarchical time series model to estimate the number of attendees at NFL games in 2020. The point is to predict attendance and compare it to actual attendance to estimate the impact of covid on attendance. Then it calculates how much revenue was lost on ticket sales in 2020 based on average ticket costs in 2019.

## Installation
To run this project, you need to have `R` and `rstan` installed on your machine. You also need the following `R` packages: `tidyverse`, `shiny`, `shinyBS`, `rvest`, `curl`, `coda`.

You can clone this repo using git:
`git clone https://github.com/casonwight/nfl-attendance.git`
Or you can download the zip file from GitHub.

## App
The app contained in this repo is currently run on [a free shiny server](https://cason-wight.shinyapps.io/nflattendance/).
The app has three tabs, Estimated Revenue Loss, Parameter Modeling, and Exploratory Analysis.

### Estimated Revenue Loss

This tab contains the following: 
- The estimated dollar revenue loss as a result of covid, calculated as the following:
$\text{avg price of 2019 tickets}\times(\text{estimated 2020 weekly avg attendees}-\text{actual 2020 weekly avg attendees})\text{num yearly games}$
- Plot of the posterior predicted 2020 weekly average attendance vs the actual
- The structure of the Bayesian model: 
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
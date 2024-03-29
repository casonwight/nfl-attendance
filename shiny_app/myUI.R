myUI <- fluidPage(
  titlePanel("NFL Ticket Sales in 2020"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team_name", "Select a Team", c(team_names, "All teams")),
      # selectInput("prior_i", "Select a Set of Priors", prior_names, 2) # Not enough RAM to include model info for for all 3
    ),
    mainPanel(
      # 3 different tabs (est revenue loss, parameter modeling, EDA)
      tabsetPanel(
        ########################################
        ###      Estimated Revenue Loss      ###
        ########################################
        tabPanel(title="Estimated Revenue Loss",
           # Revenue loss
           h3("Estimated impact of Covid on ticket sales:"),
           h2(uiOutput("est_loss_revenue")),
           
           # Plot of predicted vs actual attendance
           plotOutput("pred_attendance_plot"),
           
           # Equations for calculations/Bayesian model
           bsCollapse(
             bsCollapsePanel("Modeling Explanation",
              br(),
              uiOutput("att_lost")
             )
           )
        ), 
        ########################################
        ###        Parameter Modeling        ###
        ########################################
        tabPanel(title="Parameter Modeling",
           selectInput("var_name", "Select a parameter", var_names, 1),
           
           # Parameter distribution
           plotOutput("dist_plot"),
           
           # MCMC diagnostics (collapsible)
           bsCollapse(
             bsCollapsePanel("MCMC Diagnostics (please be patient)",
                # n_eff
                h3("Effective Sample Size:"),
                h2(textOutput("n_eff")),
                br(),
                br(),
                
                # Rhat
                h3("Gelman and Rubin Convergence Statistic:"),
                h2(textOutput("rhat")),
                br(),
                br(),
                
                # Trace plot(s)
                plotOutput("trace_plot"),
                
                # acf plot(s)
                plotOutput("acf_plot")
             ) 
           ), 
        ),
        ########################################
        ###               EDA                ###
        ########################################
        tabPanel(title="Exploratory Analysis",
           # Weekly Average Attendance
           plotOutput("avg_attendance_by_year_plot"),
           
           # Wins/losses
           plotOutput("games_by_year_plot"),
           
           # Revenue
           plotOutput("rev_by_year_plot")
        )
      ) 
    ) 
  ) 
) 
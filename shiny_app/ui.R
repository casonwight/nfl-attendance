fluidPage(
  titlePanel("NFL Ticket Sales in 2020"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team_name", "Select a Team", team_names),
      selectInput("prior_i", "Select a Set of Priors", prior_names, 2)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title="Predicted Revenue Lost",
           h3("Estimated impact of Covid on ticket sales:"),
           h2(uiOutput("est_loss_revenue")),
           plotOutput("pred_attendance_plot"),
           bsCollapse(
             bsCollapsePanel("Modeling Explanation",
              br(),
              uiOutput("att_lost")
             )
           )
        ), # End tabPanel
        tabPanel(title="Parameter Modeling",
           selectInput("var_name", "Select a parameter", var_names, 1),
           plotOutput("dist_plot"),
                 
           bsCollapse(
             bsCollapsePanel("MCMC Diagnostics (please be patient)",
                
                plotOutput("trace_plot"),
                plotOutput("acf_plot"),
                h3("Effective Sample Size:"),
                h2(textOutput("n_eff")),
                br(),
                br(),
                h3("Gelman and Rubin Convergence Statistic:"),
                h2(textOutput("rhat"))
             ) # End bsCollapsePanel
           ), # End bsCollapse
        ), # End tabPanel
        tabPanel(title="Exploratory Analysis",
                 plotOutput("avg_attendance_by_year_plot"),
                 plotOutput("games_by_year_plot"),
                 plotOutput("rev_by_year_plot"),
        )
      ) # End tabsetPanel
    ) # End mainPanel
  ) # End sidebarLayout
) # End fluidPage
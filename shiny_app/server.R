server <- function(input, output) {
  ########################################
  ###      Predicted Revenue Lost      ###
  ########################################
  
  # Posterior predicted attendance for team(s)
  post_predicted_attendance <- reactive({
    get_post_predicted_attendance(attendance, samples[[which(input$prior_i==prior_names)]], team_names, input$team_name)
  })
  
  # Difference between posterior predicted attendance and actual 2020 attendance for team(s)
  att_lost <- reactive({
    get_post_predicted_lost_attendance(post_predicted_attendance(), attendance_2020, team_names, input$team_name)
  })
  
  # Estimated Ticket Sale Revenue lost from covid
  output$est_loss_revenue <- renderUI({
    HTML(as.character(div(style="color:red", get_est_lost_rev(att_lost(), prices_2019, input$team_name))))
  })
  
  # Plot of the predicted vs actual 2020 weekly avg attendance
  output$pred_attendance_plot <- renderPlot({
    plot_attendance(post_predicted_attendance(), attendance_2020, team_names, input$team_name)
  })
  
  # Bayesian and mathematical equations for calculating lost revenue
  output$att_lost <- renderUI({
    if (is.na(input$team_name) | input$team_name == "All teams") {
      stmt <- withMathJax(
        helpText('
          $$
          \\begin{aligned}
          \\text{Est 2020 Attendance}_\\text{team}&\\sim\\mathcal{N}\\left(\\mu_\\text{team}, \\sigma^2\\right) \\\\
          \\mu_\\text{team}&=\\alpha_\\text{team} \\\\
                &~~~~~~+\\beta_\\text{team} \\times \\text{2019 Year Attendance} \\\\
                &~~~~~~+\\theta \\times \\text{2019 Num Games Won} \\\\
          \\sigma&\\sim\\text{Gamma}(\\alpha_\\sigma, \\beta_\\sigma) \\\\
          \\alpha_\\text{team}&\\sim\\mathcal{N}(\\alpha, \\lambda^2) \\\\
          \\alpha&\\sim\\mathcal{N}(\\mu_{\\alpha}, \\sigma^2_\\alpha) \\\\
          \\beta_\\text{team}&\\sim\\mathcal{N}(\\beta, \\eta^2) \\\\
          \\beta&\\sim\\mathcal{N}(\\mu_{\\beta}, \\sigma^2_\\beta) \\\\
          \\theta&\\sim\\mathcal{N}(\\mu_{\\theta}, \\sigma^2_\\theta) \\\\
          \\end{aligned}
          $$
        '),
        br(),
        helpText('
          $$
          \\text{Impact of Covid on Weekly Attendance}_\\text{team}\\approx\\text{Actual 2020 Attendance}_\\text{team}-\\text{Est 2020 Attendance}_\\text{team} \\\\
          $$
        '),
        br(),
        helpText('
          $$
          \\text{Est Revenue Impact}\\approx\\sum_{\\text{team}\\in\\text{All teams}}{\\text{Impact of Covid on Weekly Attendance}_\\text{team} \\times \\text{Avg. 2019 Ticket Cost}_\\text{team} \\times \\text{Num Weeks}} \\\\
          $$
        ')
      )
     
    } else {
      var_team_name <- input$team_name
      stmt <- withMathJax(
        helpText(str_replace_all(string='
         $$
         \\begin{aligned}
         \\text{Est 2020 Attendance}&\\sim\\mathcal{N}\\left(\\mu_\\text{team_name}, \\sigma^2\\right) \\\\
         \\mu_\\text{team_name}&=\\alpha_\\text{team_name} \\\\
               &~~~~~~+\\beta_\\text{team_name} \\times \\text{2019 Year Attendance} \\\\
               &~~~~~~+\\theta \\times \\text{2019 Num Games Won} \\\\
         \\sigma&\\sim\\text{Gamma}(\\alpha_\\sigma, \\beta_\\sigma) \\\\
         \\alpha_\\text{team_name}&\\sim\\mathcal{N}(\\alpha, \\lambda^2) \\\\
         \\alpha&\\sim\\mathcal{N}(\\mu_{\\alpha}, \\sigma^2_\\alpha) \\\\
         \\beta_\\text{team_name}&\\sim\\mathcal{N}(\\beta, \\eta^2) \\\\
         \\beta&\\sim\\mathcal{N}(\\mu_{\\beta}, \\sigma^2_\\beta) \\\\
         \\theta&\\sim\\mathcal{N}(\\mu_{\\theta}, \\sigma^2_\\theta) \\\\
         \\end{aligned}
         $$', pattern="team_name", replacement=input$team_name)
        ),
        br(),
        helpText(sprintf('
         $$
         \\begin{alignat}{3}
         \\text{Impact of Covid on Weekly Attendance}&\\approx\\text{Actual 2020 Attendance}&&-\\text{Est 2020 Attendance}_\\text{Sqr Err Loss} \\\\
         %s&\\approx%s&&-%s
         \\end{alignat}
         $$', 
         format(att_lost()$diff, big.mark="{,}", scientific=FALSE),
         format(att_lost()$Home, big.mark="{,}", scientific=FALSE),
         format(att_lost()$pred_attendance, big.mark="{,}", scientific=FALSE)
        )
        ),
        br(),
        helpText(sprintf('
         $$
         \\begin{alignat}{3}
         \\text{Est Revenue Impact}&\\approx\\text{Impact of Covid on Weekly Attendance} &&\\times \\text{Avg. 2019 Ticket Cost} &&\\times \\text{Num Weeks} \\\\
         %s&\\approx%s&&\\times%s&&\\times%s
         \\end{alignat}
         $$', 
         format(att_lost()$diff * 16 * prices_2019 %>% filter(team_name==var_team_name) %>% pull(avg_cost_2019), big.mark="{,}", scientific=FALSE),
         format(att_lost()$diff, big.mark="{,}", scientific=FALSE),
         format(prices_2019 %>% filter(team_name==var_team_name) %>% pull(avg_cost_2019), big.mark="{,}", scientific=FALSE),
         format(16, big.mark="{,}", scientific=FALSE)
        )
        )
      )
    }
    return(stmt)
  })
  
  
  
  ########################################
  ###        Parameter Modeling        ###
  ########################################
  # Parameter distribution
  output$dist_plot <- renderPlot({
    plot_distribution(samples[[which(input$prior_i==prior_names)]], priors[[which(input$prior_i==prior_names)]], input$var_name, team_names, input$team_name)
  })
  
  # MCMC Diagnostics
  output$trace_plot <- renderPlot({
    get_trace_plot(samples[[which(input$prior_i==prior_names)]], input$var_name, team_names, team_name=input$team_name)
  })
  output$acf_plot <- renderPlot({
    get_acf_plot(samples[[which(input$prior_i==prior_names)]], input$var_name, team_names, team_name=input$team_name)
  })
  output$n_eff <- renderText({
    comma(get_effective_sample_size(samples[[which(input$prior_i==prior_names)]], input$var_name, team_names, team_name=input$team_name))
  })
  output$rhat <- renderText({
    sprintf(get_rhat(fits[[which(input$prior_i==prior_names)]], input$var_name, team_names, team_name=input$team_name), fmt = '%#.4f')
  })
    
    
  ########################################
  ###               EDA                ###
  ########################################
  output$avg_attendance_by_year_plot <- renderPlot({
    plot_avg_attendance_all_years(attendance, input$team_name)
  })
  
  output$games_by_year_plot <- renderPlot({
    plot_avg_win_rate_all_years(attendance, input$team_name)
  })
  
  output$rev_by_year_plot <- renderPlot({
    plot_yearly_revenue(rev_data, rev_team_data, input$team_name)
  })
  
}

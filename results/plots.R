library(tidyverse)

plot_distribution <- function(samples, prior_set, var_name, team_names, team_name=NA) {
  
    num_points <- 300
    
    # Group parameters
    if (str_sub(var_name, -1, -1) != 's') {
      short_var_name <- str_replace(var_name, 'overall_', '')
      cleaned_var_name <- parse(text=short_var_name)
      cleaned_title <- parse(text=paste0("'Distribution'~'of'~", short_var_name))
      
      if (var_name == "overall_alpha") {
        xs = seq(
          from=prior_set[['mu_a']] - 3 * prior_set[['sigma_a']],
          to=prior_set[['mu_a']] + 3 * prior_set[['sigma_a']],
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_a']], sd=prior_set[['sigma_a']]))  
      } else if (var_name=="overall_beta") {
        xs = seq(
          from=prior_set[['mu_b']] - 3 * prior_set[['sigma_b']],
          to=prior_set[['mu_b']] + 3 * prior_set[['sigma_b']],
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_b']], sd=prior_set[['sigma_b']]))  
      } else if (var_name=="theta") {
        xs = seq(
          from=prior_set[['mu_theta']] - 3 * prior_set[['sigma_theta']],
          to=prior_set[['mu_theta']] + 3 * prior_set[['sigma_theta']],
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_theta']], sd=prior_set[['sigma_theta']]))  
      } else if (var_name=="sigma") {
        mean_sigma <- prior_set[['a_sigma']] / prior_set[['b_sigma']]
        var_sigma <- prior_set[['a_sigma']] / prior_set[['b_sigma']] ^ 2
        shape_sigma <- mean_sigma^2 / var_sigma
        scale_sigma <- var_sigma / mean_sigma
        xs = seq(
          from=qgamma(.05, shape=shape_sigma, scale=scale_sigma),
          to=qgamma(.95, shape=shape_sigma, scale=scale_sigma),
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dgamma(xs, shape=shape_sigma, scale=scale_sigma))
      }
      
      posterior_samples_df <- data.frame(xs=samples[[var_name]])
      
      plt <- ggplot(posterior_samples_df, aes(x=xs, color="Posterior")) +
        geom_density() + 
        geom_line(data=prior_density_df, aes(x=xs, y=ys, color="Prior")) + 
        labs(title=cleaned_title, x=cleaned_var_name, y="Density", color="") + 
        theme_light()
    
    # Inidivdual parameters, but for every team
    } else if (is.na(team_name) | team_name == "All teams") {
      short_var_name <- str_sub(var_name, end=-2)
      cleaned_var_name <- parse(text=short_var_name)
      cleaned_title <- parse(text=paste0("'Distributions'~'of'~", short_var_name))
      
      if (var_name == 'alphas'){
        xs = seq(
          from=prior_set[['mu_a']] - 3 * sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2),
          to=prior_set[['mu_a']] + 3 * sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2),
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_a']], sd=sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2)))  
        
      } else if (var_name == "betas") {
        xs = seq(
          from=prior_set[['mu_b']] - 3 * sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2),
          to=prior_set[['mu_b']] + 3 * sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2),
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_b']], sd=sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2)))  
    }
      
      posterior_samples_df <- data.frame(xs=samples[[var_name]]) %>% 
        setNames(team_names) %>% 
        pivot_longer(cols=all_of(team_names))
      
      
      plt <- ggplot(posterior_samples_df, aes(x=value, color="Posterior")) +
        geom_density() + 
        geom_line(data=prior_density_df, aes(x=xs, y=ys, color="Prior")) + 
        facet_wrap(~name, scales="free") + 
        labs(title=cleaned_title, x=cleaned_var_name, y="Density", color="") + 
        theme_light()
      
      
    } else {
      short_var_name <- str_sub(var_name, end=-2)
      cleaned_var_name <- parse(text=paste0(short_var_name, "['", team_name, "']"))
      cleaned_title <- parse(text=paste0("'Distribution'~'of'~", short_var_name, "['", team_name, "']"))
      
      if (var_name == 'alphas'){
        xs = seq(
          from=prior_set[['mu_a']] - 3 * sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2),
          to=prior_set[['mu_a']] + 3 * sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2),
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_a']], sd=sqrt(prior_set[['sigma_a']]^2 + prior_set[['lambda']]^2)))  
        
      } else if (var_name == "betas") {
        xs = seq(
          from=prior_set[['mu_b']] - 3 * sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2),
          to=prior_set[['mu_b']] + 3 * sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2),
          length.out=num_points
        )
        
        prior_density_df <- data.frame(xs=xs, ys=dnorm(xs, mean=prior_set[['mu_b']], sd=sqrt(prior_set[['sigma_b']]^2 + prior_set[['eta']]^2)))  
      }
      
      posterior_samples_df <- data.frame(xs=samples[[var_name]]) %>%
        setNames(team_names) %>% 
        pull(c(team_name)) %>% 
        data.frame() %>% 
        setNames(c("value"))
      
      
      plt <- ggplot(posterior_samples_df, aes(x=value, color="Posterior")) +
        geom_density() + 
        geom_line(data=prior_density_df, aes(x=xs, y=ys, color="Prior")) + 
        labs(title=cleaned_title, x=cleaned_var_name, y="Density", color="") + 
        theme_light()
      
    }
    return(plt)
}


plot_attendance <- function(post_predicted_attendance, attendance_2020, team_names, team_name=NA) {
  
  if (is.na(team_name) | team_name == "All teams"){
    
    team_name_order <- order(colMeans(post_predicted_attendance))
    attendance_2020_ordered <- attendance_2020 %>% 
      mutate(team_name=factor(team_name, levels=team_names[team_name_order])) %>% 
      arrange(team_name)
      
    
    plt <- bayesplot::mcmc_areas(post_predicted_attendance[,rev(team_name_order)], prob = .95) + 
      theme_light() +
      scale_x_continuous(labels = scales::comma) +
      labs(title = "2020 Predicted Attendance",
           x = "Average Weekly Attendance",
           y = "Team") +
      theme(panel.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_blank()) +
      geom_segment(data = attendance_2020_ordered, 
                   aes(x = Home, xend = Home,
                       y = as.numeric(team_name), yend = as.numeric(team_name)+1, color="Actual"), 
                   lwd = 1, color = "red") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      coord_flip()
    
  } else{
    
    var_team_name <- team_name
    act_attendance <- attendance_2020 %>% 
      filter(team_name == var_team_name)
    
    plt <- post_predicted_attendance %>% 
      data.frame() %>% 
      setNames(c("x")) %>% 
      ggplot(aes(x=x, color="Posterior Predicted")) + 
      geom_density() + 
      geom_vline(data=data.frame(act_attendance), aes(xintercept=Home, color="Actual"), lwd = 1) +
      labs(title=paste("2020 NFL Attendance for", team_name), 
           x="Avg. Weekly Attendance", 
           y="Density",
           color="") + 
      scale_x_continuous(label=scales::comma) +
      theme_light()
    
  }
  
  return(plt)
}


if (!interactive()) {
  source("utils/stan.R")
  priors <- get_priors()
  
}
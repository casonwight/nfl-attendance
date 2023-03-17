library(tidyverse)


get_trace_plot <- function(samples, var_name, team_names, team_name=NA, num_chains=4){
  num_samps <- length(samples[[1]]) / num_chains
  
  if (str_sub(var_name, -1, -1) != 's'){
    short_var_name <- str_replace(var_name, 'overall_', '')
    cleaned_var_name <- parse(text=short_var_name)
    cleaned_title <- parse(text=paste0("'Trace'~'Plot'~'of'~", short_var_name))
    
    plt <- samples[[var_name]] %>% 
      cbind(rep(1:num_samps, num_chains)) %>% 
      as.data.frame() %>% 
      mutate(chain=as.factor(rep(1:num_chains, each=num_samps))) %>% 
      setNames(c("alpha", "Index", "chain")) %>% 
      ggplot(aes(x = Index, y = alpha, color=chain)) +
      geom_line() +
      labs(title = cleaned_title,
           y = cleaned_var_name, x = "Index") + 
      theme_light()
  } else {
    cleaned_df <- samples[[var_name]] %>% 
      as.data.frame() %>%  
      setNames(c(team_names)) %>% 
      mutate(chain=as.factor(rep(1:num_chains, each=num_samps))) %>% 
      mutate(Index=rep(1:num_samps, num_chains))
    
    short_var_name <- str_sub(var_name, end=-2)
    
    if (is.na(team_name) | team_name == "All teams") {
      cleaned_var_name <- parse(text=short_var_name)
      cleaned_title <- parse(text=paste0("'Trace'~'Plots'~'of'~", short_var_name))
      
      plt <- cleaned_df %>% 
        pivot_longer(all_of(team_names)) %>%
        ggplot(aes(y = value, x = Index, color=chain)) + 
        geom_line() + 
        facet_wrap(~name, scales = "free_y") +
        labs(title = cleaned_title,
             y = cleaned_var_name, 
             x = "Index") + 
        theme_light()
    } else {
      short_var_name <- str_replace(var_name, 's', '')
      cleaned_var_name <- parse(text=paste0(short_var_name, "['", team_name, "']"))
      cleaned_title <- parse(text=paste0("'Trace'~'Plot'~'of'~", short_var_name, "['", team_name, "']"))
      
      plt <- cleaned_df %>% 
        select(c("chain", "Index", team_name)) %>%
        setNames(c("chain", "Index", "val")) %>% 
        ggplot(aes(y = val, x = Index, color=chain)) + 
        geom_line() + 
        labs(title = cleaned_title,
             y = cleaned_var_name, 
             x = "Index") + 
        theme_light()
    }
  }
  return(plt)
}


get_acf_plot <- function(samples, var_name, team_names, team_name=NA, num_chains=4){
  num_samps <- length(samples[[1]])/num_chains
  
  if (str_sub(var_name, -1, -1) != 's'){
    short_var_name <- str_replace(var_name, 'overall_', '')
    cleaned_title <- parse(text=paste0("'Autocorrelation'~'Plot'~'of'~", short_var_name))
    
    plt <- samples[[var_name]] %>% 
      acf(plot=FALSE) %>% 
      with(data.frame(lag,acf)) %>% 
      ggplot(aes(x=lag,y=acf)) +
      geom_bar(stat="identity", position = "identity") + 
      labs(title = cleaned_title,
           x = "Lag", 
           y = "Autocorrelation") + 
      theme_light()
  } else if (is.na(team_name) | team_name == "All teams") {
    short_var_name <- str_sub(var_name, end=-2)
    cleaned_var_name <- parse(text=short_var_name)
    cleaned_title <- parse(text=paste0("'Autocorrelation'~'Plots'~'of'~", short_var_name))
    
    plt <- samples[[var_name]] %>% 
      as.data.frame() %>% 
      setNames(team_names) %>% 
      apply(2,acf,plot=FALSE) %>% 
      lapply(function(x) x[[1]]) %>% 
      do.call(what = rbind, args = .) %>% 
      t() %>% 
      as.data.frame() %>% 
      pivot_longer(all_of(team_names)) %>% 
      group_by(name) %>% 
      mutate(index = seq_along(value)) %>% 
      ggplot(aes(x = index, y = value)) +
      geom_bar(stat = "identity", position = "identity") +
      facet_wrap(~name, scales = "free_y") + 
      labs(title = cleaned_title,
           x = "Lag", 
           y = "Autocorrelation") + 
      theme_light()
  } else {
    short_var_name <- str_sub(var_name, end=-2)
    cleaned_var_name <- parse(text=paste0(short_var_name, "['", team_name, "']"))
    cleaned_title <- parse(text=paste0("'Autocorrelation'~'Plot'~'of'~", short_var_name, "['", team_name, "']"))
    
    plt <- samples[[var_name]] %>%
      as.data.frame() %>% 
      select(which(team_name==team_names)) %>% 
      acf(plot=FALSE) %>% 
      with(data.frame(lag,acf)) %>% 
      ggplot(aes(x=lag,y=acf)) +
      geom_bar(stat="identity", position = "identity") + 
      labs(title = cleaned_title,
           x = "Lag", 
           y = "Autocorrelation") + 
      theme_light()
  }
  return(plt)
}


get_effective_sample_size <- function(samples, var_name, team_names, team_name=NA) {
  
  if (str_sub(var_name, -1, -1) != 's'){
    n_eff <- coda::effectiveSize(c(samples[[var_name]])) %>% set_names(var_name)
  } else if (is.na(team_name) | team_name == "All teams") {
    short_var_name <- str_sub(var_name, end=-2)
    n_eff <- coda::effectiveSize(samples[[var_name]]) %>% setNames(paste(short_var_name, team_names, sep="_"))
  } else {
    short_var_name <- str_sub(var_name, end=-2)
    n_eff <- coda::effectiveSize(c(samples[[var_name]][, which(team_name==team_names)])) %>% setNames(paste(short_var_name, team_name, sep="_"))
  }
  return(n_eff)
}

get_rhat <- function(samples, var_name, team_names, team_name=NA) {
  if (str_sub(var_name, -1, -1) != 's'){
    rhat <- rstan::Rhat(matrix(samples[[var_name]], ncol=4)) %>% set_names(var_name)
  } else if (is.na(team_name) | team_name == "All teams") {
    all_row_names <- paste0(var_name, "[", 1:length(team_names), "]")
    short_var_name <- str_sub(var_name, end=-2)
    rhat <- sapply(team_names, function(tm) rstan::Rhat(matrix(samples[[var_name]][, which(tm==team_names)], ncol=4))) %>% setNames(paste(short_var_name, team_names, sep="_"))
  } else {
    short_var_name <- str_sub(var_name, end=-2)
    row_name <- paste0(var_name, "[", which(team_name==team_names), "]")
    rhat <- rstan::Rhat(matrix(samples[[var_name]][, which(team_name==team_names)], ncol=4)) %>% setNames(paste(short_var_name, team_name, sep="_"))
  }
  return(rhat)
}
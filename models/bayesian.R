library(rstan)
source('utils/stan.R')


fit_bayesian_mods <- function(data, path="results/bayesian_fits.rda"){
  stan_data <- prepare_data_for_stan(data)
  all_priors <- get_priors()
  
  fits <- vector(mode='list', length=length(all_priors))
  
  options(mc.cores=parallel::detectCores())
  rstan_options(auto_write=TRUE)
  
  for (i in 1:length(all_priors)){
    fit <- get_stan_model(stan_data, all_priors[[i]])
    fits[[i]] <- fit
  }
  
  if (!is.na(path)){
    save(fits, file=path)
  }
  
  return(fits)
}


load_bayesian_mods <- function(path="results/bayesian_fits.rda"){
  if (!file.exists(path)){
    stop(paste("Error: Fits file does not exist at", path))
  }
  assign('fits', get(load(path)))
  return(fits)
}

load_bayesian_fit <- function(path="results/bayesian_fit.rda"){
  if (!file.exists(path)){
    stop(paste("Error: Fits file does not exist at", path))
  }
  assign('fit', get(load(path)))
  return(fit)
}

load_bayesian_samps <- function(path="results/bayesian_samps.rda"){
  if (!file.exists(path)){
    stop(paste("Error: Fits file does not exist at", path))
  }
  assign('fit_samps', get(load(path)))
  return(fit_samps)
}





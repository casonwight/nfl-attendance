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

save_fit_tight_priors <- function(fits, path="results/bayesian_samps.rda") {
  fit <- fits[[1]]
  save(fit, file=path)
  return(fit)
}

save_samps_tight_priors <- function(fit, path="results/bayesian_samps.rda") {
    samps <- rstan::extract(fit)
    save(samps, file=path)
    return(samps)
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


downsample_samps <- function(samps, new_n=10000 / 4){
  n <- length(samps[['lp__']]) / 4
  sample_vals <- as.vector(sapply(1:4, function(i) sample(x=((1:n) + (n*(i-1))), size=new_n, replace=FALSE)))
  
  for (var in names(samps)){
    if (length(dim(samps[[var]])) == 1) {
      samps[[var]] <- samps[[var]][sample_vals]
    } else {
      samps[[var]] <- samps[[var]][sample_vals, ]
    }
  }
  return(samps)
}





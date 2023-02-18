

fit_lin_mod <- function(data, path='results/lin_mod.rda'){
  lin_mod <- lm(total_attendance ~ -1 + home_team + home_team * prev_attendance + prev_games_won - prev_attendance, data=data)
  
  if (!is.na(path)) {
    save(lin_mod, file=path)
  }
  return(lin_mod)
}

load_lin_mod <- function(path="results/lin_mod.rda"){
  if (!file.exists('path')){
    stop(paste("Error: Linear model file does not exist at", path))
  }
  assign('lin_mod', get(load(path)))
  return(lin_mod)
}
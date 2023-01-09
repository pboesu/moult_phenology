#from scratch approach to simulating biased moult sample
library(dplyr)
library(moult)
library(moultmcmc)
library(ggplot2)
set.seed(2325)

source('analysis/sampling_bias_mk2/simulate_moult_data_function.R')
get_recapture_stats <- function(df){
  n_ind = n_distinct(df$ID)
  capt_summs = df %>% group_by(ID) %>% summarize(n = n(), any_active_moult = any(pfmg != 0 & pfmg !=1))
  overall_recaptures = sum(capt_summs$n > 1)
  at_least_one_active_moult = (sum(capt_summs$n > 1 & capt_summs$any_active_moult))
  active_recap_summs = df %>% filter(pfmg != 0 & pfmg !=1) %>% group_by(ID) %>% summarize(n = n())
  active_moult_recaps = (sum(active_recap_summs$n > 1))
  return(list(n_ind = n_ind, overall = overall_recaptures, active = at_least_one_active_moult, active_recap = active_moult_recaps))
}

#simulate
library(lhs)
lhc = optimumLHS(84,2)
plot(lhc)

scenarios <- data.frame(superpopulation = c(10^qunif(lhc[,1],1.5,3),rep(c(200,500,1000,1000),2),rep(c(50,200,500,1000),2)), moult_capture_prob = c(qunif(lhc[,2],0.1,1),c(rep(1,8),rep(0.1,8))))
scenarios$scenario <- 1:nrow(scenarios)

plot(scenarios$superpopulation, scenarios$moult_capture_prob)

sim_data <- list()
for (i in 1:nrow(scenarios)){
  df = sample_one_year(n_individuals = scenarios$superpopulation[i], rel_p_moult = scenarios$moult_capture_prob[i])
  df$replicate <- scenarios$replicate[i]
  df$scenario <- scenarios$scenario[i]
  sim_data[[i]] <- df
}

scenario_recapture_stats <- lapply(sim_data, get_recapture_stats)

scenarios$n_ind <- sapply(scenario_recapture_stats, function(x)x$n_ind)
scenarios$n_recap_total <- sapply(scenario_recapture_stats, function(x)x$overall)
scenarios$n_recap_moult <- sapply(scenario_recapture_stats, function(x)x$active)
scenarios$n_recap_active_moult <- sapply(scenario_recapture_stats, function(x)x$active_recap)

plot(scenarios$superpopulation, scenarios$n_recap_active_moult)
plot(log10(scenarios$superpopulation), scenarios$n_recap_active_moult)
plot(scenarios$n_recap_active_moult, scenarios$moult_capture_prob)
plot(scenarios$n_recap_active_moult/scenarios$n_ind, scenarios$moult_capture_prob)
plot(scenarios$n_recap_active_moult/520, scenarios$moult_capture_prob)

saveRDS(scenarios, 'data/simulated_datasets/biased_sampling_mk2/scenario_lookup.rds')
saveRDS(sim_data, 'data/simulated_datasets/biased_sampling_mk2/sim_data.rds')

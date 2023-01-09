library(moult)
library(dplyr)
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


set.seed(0987)
#simulate
library(lhs)
lhc = optimumLHS(184,2)
plot(lhc)

scenarios <- data.frame(superpopulation = c(10^qunif(lhc[,1],1.48,3),rep(c(50,200,500,1000),2),rep(c(50,200,500,1000),2)),
                        tau_sd = c(qunif(lhc[,2],0.1,8),c(rep(0.1,8),rep(8,8))))
scenarios$scenario <- 1:nrow(scenarios)

plot(scenarios$superpopulation, scenarios$tau_sd)

sim_data <- list()

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

sim_data = foreach (i = 1:nrow(scenarios), .packages = c('dplyr','moultmcmcExtra')) %dopar% {#  1:nrow(scenarios)){
  df = sample_one_year(n_individuals = scenarios$superpopulation[i],
                       duration_sd = scenarios$tau_sd[i],
                       rel_p_moult = 'siskin',
                       pop_start = 197,
                       pop_duration = 78,
                       pop_sd = 9,
                       rel_p_new = 0.1,
                       sampling_dates = sample(150:300, size = 25), sample_per_occ = 30)
  #df$replicate <- scenarios$replicate[i]
  df$scenario <- scenarios$scenario[i]
  df
  #sim_data[[i]] <- df
  #print(paste0('*****',i,'*****'))
}
stopCluster(cl)

scenario_recapture_stats <- lapply(sim_data, get_recapture_stats)

scenarios$n_ind <- sapply(scenario_recapture_stats, function(x)x$n_ind)
scenarios$n_recap_total <- sapply(scenario_recapture_stats, function(x)x$overall)
scenarios$n_recap_moult <- sapply(scenario_recapture_stats, function(x)x$active)
scenarios$n_recap_active_moult <- sapply(scenario_recapture_stats, function(x)x$active_recap)

plot(scenarios$n_recap_active_moult, scenarios$tau_sd/78)
plot(log(scenarios$n_recap_active_moult/scenarios$n_ind), scenarios$tau_sd/78)

saveRDS(scenarios, 'data/simulated_datasets/siskin_sampling_mk2/scenario_lookup.rds')
saveRDS(sim_data, 'data/simulated_datasets/siskin_sampling_mk2/sim_data.rds')




# system.time({
# sisk_test = sample_one_year(rel_p_moult = 'siskin',
#                             pop_start = 197,
#                             pop_duration = 78,
#                             pop_sd = 9,
#                             rel_p_new = 0.1,
#                             n_individuals = 1000,
#                             sampling_dates = sample(150:300, size = 25), sample_per_occ = 30)
# })
# get_recapture_stats(sisk_test)
# ggplot(sisk_test, aes(x = yday, y = pfmg)) + geom_point()
# ggplot(sisk_test, aes(x = pfmg)) + geom_histogram()
# t3ml = moult(pfmg ~ yday, type=3, data = sisk_test)
# t5ml = moult(pfmg ~ yday, type=5, data = sisk_test)
# t3 = moultmcmc('pfmg','yday', type = 3, data = sisk_test, chains = 2, cores = 2)
# t5 = moultmcmc('pfmg','yday', type = 5, data = sisk_test, chains = 2, cores = 2)
# t3r = moultmcmc('pfmg','yday','ID', type = 3, data = sisk_test, chains = 2, cores = 2)
# t3ra = moultmcmc('pfmg','yday','ID', type = 3, data = sisk_test, chains = 2, cores = 2, active_moult_recaps_only = TRUE)
# t5r = moultmcmc('pfmg','yday','ID', type = 5, data = sisk_test, chains = 2, cores = 2)
# t5ra = moultmcmc('pfmg','yday','ID', type = 5, data = sisk_test, chains = 2, cores = 2, active_moult_recaps_only = TRUE)})
# compare_plot(t3,t5,t3r,t5r,t3ml,t5ml,t3ra,t5ra)

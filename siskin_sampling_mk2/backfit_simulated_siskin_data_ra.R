#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(moultmcmc)
sim_data <- readRDS('data/simulated_datasets/siskin_sampling_mk2/sim_data.rds')

#for(i in 1:length(sim_data[1:40])){
i = as.numeric(args[1])
uz5r_iter = as.numeric(args[2])
  uz3ra <- try(moultmcmc('pfmg','yday','ID',data = sim_data[[i]], type = 3, active_moult_recaps_only = TRUE, flat_prior = FALSE, chains = 2, log_lik = FALSE, cores = 2, warmup = min(floor(uz5r_iter/2), 2000), iter = uz5r_iter,thin=floor(uz5r_iter/2000)))
  saveRDS(uz3ra,paste0('simulation_outputs/siskin_sampling/uz3ra_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'bzip2')
  uz5ra <- try(moultmcmc('pfmg','yday','ID',data = sim_data[[i]], type = 5,active_moult_recaps_only = TRUE, flat_prior = FALSE, chains = 2, log_lik = FALSE, cores = 2, warmup = min(floor(uz5r_iter/2), 2000), iter = uz5r_iter,thin=floor(uz5r_iter/2000)))
  saveRDS(uz5ra,paste0('simulation_outputs/siskin_sampling/uz5ra_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'bzip2')
#}
# uz2_list <- setNames(uz2_list, paste0("scenario",1:8))
# list2env(uz2_list,globalenv())
# compare_plot(scenario1,scenario2,scenario3,scenario4,scenario8)
# do.call(compare_plot, uz2_list)
#compare_plot(uz2,uz2r)
#rstan::traceplot(uz2r$stanfit, inc_warmup=T)
#summary_table(uz2r)

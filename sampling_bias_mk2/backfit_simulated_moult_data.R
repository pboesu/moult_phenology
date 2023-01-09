#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(moultmcmc)
sim_data <- readRDS('data/simulated_datasets/biased_sampling_mk2/sim_data.rds')

#for(i in 1:length(sim_data[1:40])){
i = as.numeric(args[1])
uz2r_iter = as.numeric(args[2])
  uz2 <- try(moultmcmc('pfmg','yday',data = sim_data[[i]], chains = 2, log_lik = FALSE, cores = 2))
  saveRDS(uz2,paste0('simulation_outputs/biased_sampling/uz2_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'bzip2')
  uz1 <- try(moultmcmc('pfmg','yday',data = sim_data[[i]], type = 1, chains = 2, log_lik = FALSE, cores = 2))
  saveRDS(uz1,paste0('simulation_outputs/biased_sampling/uz1_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'bzip2')
  uz2r <- try(moultmcmc('pfmg','yday','ID',data = sim_data[[i]], flat_prior = FALSE, chains = 2, log_lik = FALSE, cores = 2, warmup = min(floor(uz2r_iter/2), 2000), iter = uz2r_iter,thin=floor(uz2r_iter/2000)))
  saveRDS(uz2r,paste0('simulation_outputs/biased_sampling/uz2r_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'gzip')
  uz1r <- try(moultmcmc('pfmg','yday','ID',data = sim_data[[i]], type = 1, flat_prior = FALSE, chains = 2, log_lik = FALSE, cores = 2, warmup = min(floor(uz2r_iter/2), 2000), iter = uz2r_iter,thin=floor(uz2r_iter/2000)))
  saveRDS(uz1r,paste0('simulation_outputs/biased_sampling/uz1r_scenario',formatC(i,flag=0,width=3),'.rds'),compress = 'gzip')
#}
# uz2_list <- setNames(uz2_list, paste0("scenario",1:8))
# list2env(uz2_list,globalenv())
# compare_plot(scenario1,scenario2,scenario3,scenario4,scenario8)
# do.call(compare_plot, uz2_list)
#compare_plot(uz2,uz2r)
#rstan::traceplot(uz2r$stanfit, inc_warmup=T)
#summary_table(uz2r)

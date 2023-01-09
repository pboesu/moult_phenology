#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#TODO: add ML T1,T2,T3 
library(dplyr)
library(parallel)
library(foreach)
library(moultmcmc)
library(ggplot2)
i = as.numeric(args[1])
#i=1
set.seed(120 + i)
#sigma_tau_ratio = 0.1
#sim_data_list <-  list.files('data/simulated_datasets/', pattern = glob2rx('*no_recaptures*no_lrg_linear*set*'), full.names = TRUE)

pop_duration = runif(1, 35, 200)

source('analysis/sampling_bias_mk2/simulate_moult_data_function.R')

#use parameters from Oschadleus 2005 PhD Thesis
#start date 14 Jan = 167 days since 01 August
#duration = 75 days
#sd start date = 29 days




#indices <- sample(length(sim_data_list), size= 30)
#i <- indices[as.numeric(args[1])]

#clus <- parallel::makeCluster(8)

for (sigma_tau_ratio in c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3,0.35, 0.4,0.45, 0.5, 0.55)+runif(11, -0.024, 0.024)){

  sim_data = sample_one_year(pop_start = 167,
                             pop_duration = pop_duration,
                             pop_sd = sigma_tau_ratio*pop_duration,
                             n_occasions = 156,
                             n_individuals = 7*156,
                             sample_per_occ = 7,
                             removal_sampling = TRUE,
                             rel_p_moult = 1
  ) %>% mutate(moult_numcat = case_when(pfmg == 0 ~ 1,
                                        pfmg == 1 ~ 3,
                                        TRUE ~ 2))


uz1 <- moultmcmc("pfmg",
                        date_column = "yday",
                        duration_formula = ~ 1,
                        start_formula = ~ 1,
                        data = sim_data,
                        log_lik = FALSE,
                 type = 1,
                 chains = 2,
                 cores = 2,
                 iter = 2000)
#summary_table(uz1)
uz1l <- try(moultmcmc("pfmg",
                   date_column = "yday",
                   lump_non_moult=TRUE,
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   data = sim_data,
                   log_lik = FALSE,
                   type = 1,
                   chains = 2,
                   cores = 2,
                   iter = 2000))


uz2 <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   data = sim_data,
                   log_lik = FALSE,
                 type = 2,
                   chains = 2,
                   cores = 2,
                   iter = 2000)

uz3 <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   data = sim_data,
                   log_lik = FALSE,
                 type = 3,
                   chains = 2,
                   cores = 2,
                   iter = 6000)

uz2l <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   lump_non_moult = T,
                  type = 2,
                   data = sim_data,
                   log_lik = FALSE,
                   chains = 2,
                   cores = 2,
                   iter = 2000)

#compare_plot(uz1,uz12_75)
if (inherits(uz1l, 'try-error')){
  uz1ls = NULL
} else {
  uz1ls = summary_table(uz1l) %>% mutate(model = 'T1L')
}
  


bind_rows(summary_table(uz1) %>% mutate(model = 'T1'),
          uz1ls,
          summary_table(uz2) %>% mutate(model = 'T2'),
          summary_table(uz3) %>% mutate(model = 'T3'),
          summary_table(uz2l) %>% mutate(model = 'T2L')
          ) %>%
  #left_join(tibble(prop_cat = as.character(1:5), prop_score = c(0,25,50,75,100))) %>%
  filter(!(parameter %in% c('lp__', 'log_sd_(Intercept)'))) %>%
  mutate(sigma_tau_ratio = sigma_tau_ratio,
         pop_duration = pop_duration,
         set = i) %>%
  readr::write_rds(file = paste('simulation_outputs/sigma_tau_ratio/sigma_tau_ratio_backfit_',formatC(sigma_tau_ratio, digits = 2, format = "g", flag = "#"), '_',formatC(i, width=2, flag="0"),'.rds', sep = ''))
  #ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') + xlab("Proportion of birds with feather scores in active moult category") + theme_classic()
#}

}

#stopCluster(cl = clus)


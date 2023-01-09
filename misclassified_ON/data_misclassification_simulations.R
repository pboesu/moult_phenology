#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


library(dplyr)
library(parallel)
library(foreach)
library(moultmcmc)
library(ggplot2)
i = as.numeric(args[1])
set.seed(120 + i)
prop_misclass <-0.01
#sim_data_list <-  list.files('data/simulated_datasets/', pattern = glob2rx('*no_recaptures*no_lrg_linear*set*'), full.names = TRUE)

source('analysis/sampling_bias_mk2/simulate_moult_data_function.R')

#use parameters from Oschadleus 2005 PhD Thesis
#start date 14 Jan = 167 days since 01 August
#duration = 75 days
#sd start date = 29 days
sim_data = sample_one_year(pop_start = 167,
                pop_duration = 75,
                pop_sd = 29,
                n_occasions = 156,
                n_individuals = 7*156,
                sample_per_occ = 7,
                removal_sampling = TRUE,
                rel_p_moult = 1
                ) %>% mutate(moult_numcat = case_when(pfmg == 0 ~ 1,
                                                      pfmg == 1 ~ 3,
                                                      TRUE ~ 2))



#indices <- sample(length(sim_data_list), size= 30)
#i <- indices[as.numeric(args[1])]

#clus <- parallel::makeCluster(8)

for (prop_misclass in c(0,0.01, 0.03, 0.05, 0.07, 0.1, 0.15, 0.2)){
scramble_on_scores <- function(data, prop = 0.1){
  #randomly subsample 1/3 of rows and set moult score to NA
  o_birds = which(data$moult_numcat == 1)
  n_birds = which(data$moult_numcat == 3)
  idx.o <- sample(o_birds,size = floor(length(o_birds)*prop))
  idx.n <- sample(n_birds,size = floor(length(n_birds)*prop))
  data$pfmg[idx.o] <- 1
  data$moult_numcat[idx.o] <- 3
  data$pfmg[idx.n] <- 0
  data$moult_numcat[idx.n] <- 1
  data$misclass <- FALSE
  data$misclass[c(idx.o,idx.n)] <- TRUE
  return(data)
  }



#foreach (i=indices, .packages=c('dplyr','moultmcmc'), .errorhandling = 'remove') %dopar% {
  
  #sim_data_all <- readRDS(sim_data_list[[i]]) %>% ungroup()  
  
#sim_data <- sample_n(sim_data_all, size = 1000)

scram_data <- scramble_on_scores(sim_data,prop_misclass)

uz1 <- moultmcmc("pfmg",
                        date_column = "yday",
                        duration_formula = ~ 1,
                        start_formula = ~ 1,
                        data = scram_data,
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
                   data = scram_data,
                   log_lik = FALSE,
                   type = 1,
                   chains = 2,
                   cores = 2,
                   iter = 2000))


uz2 <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   data = scram_data,
                   log_lik = FALSE,
                 type = 2,
                   chains = 2,
                   cores = 2,
                   iter = 2000)

uz3 <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                   data = scram_data,
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
                   data = scram_data,
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
  mutate(prop_misclass = prop_misclass, set = i) %>%
  readr::write_rds(file = paste('simulation_outputs/misclass/misclass_backfit_',formatC(prop_misclass, digits = 2, format = "g", flag = "#"), '_',formatC(i, width=2, flag="0"),'.rds', sep = ''))
  #ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') + xlab("Proportion of birds with feather scores in active moult category") + theme_classic()
#}

}

#stopCluster(cl = clus)


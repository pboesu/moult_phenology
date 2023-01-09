#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(moultmcmc)
library(ggplot2)
i = as.numeric(args[1])
#i=1
set.seed(120 + i)

#sim_data_list <-  list.files('data/simulated_datasets/', pattern = glob2rx('*no_recaptures*no_lrg_linear*set*'), full.names = TRUE)

source('analysis/sampling_bias_mk2/simulate_moult_data_function.R')
#sim_data_list <-  list.files('data/simulated_datasets/', pattern = glob2rx('*no_recaptures*no_lrg_linear*set*'), full.names = TRUE)

#indices <- sample(length(sim_data_list), size= 30)




remove_m_scores <- function(data, prop = 0.5, subsample_nonmoult = FALSE){
  #randomly subsample 1/3 of rows and set moult score to NA
  m_birds = which(data$moult_numcat == 2)
  cats <- sample(m_birds,size = floor(length(m_birds)*prop))
  data$pfmg[cats] <- NA
  if (subsample_nonmoult){
    o_birds = which(data$moult_numcat == 1)
    n_birds = which(data$moult_numcat == 3)
    data$moult_numcat[sample(o_birds,size = floor(length(o_birds)*prop))] <- NA
    data$moult_numcat[sample(n_birds,size = floor(length(n_birds)*prop))] <- NA
  }
  data$moult_scorecat <- consolidate_moult_records(data$pfmg, data$moult_numcat)
  return(data)
  }



#for (i in 1:length(indices)){
  
  #sim_data_all <- readRDS(sim_data_list[[i]]) %>% ungroup()  
  
#sim_data <- sample_n(sim_data_all, size = 1000)
  
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
  



uz1 <- moultmcmc("moult_numcat",
                        date_column = "yday",
                        duration_formula = ~ 1,
                        start_formula = ~ 1,
                 type=1,
                        data = sim_data,
                        log_lik = FALSE,
                   chains = 2,
                   cores = 2)
#summary_table(uz1)

uz12_75 <- moultmcmc("moult_scorecat",
                        date_column = "yday",
                        duration_formula = ~ 1,
                        start_formula = ~ 1,
                     type = 12,
                        data = remove_m_scores(sim_data,0.75),
                        log_lik = FALSE,
                        chains = 2,
                        cores = 2)

df_2_75 = filter(remove_m_scores(sim_data,0.75), !is.na(pfmg))
uz2_75 <- moultmcmc("pfmg",
                        date_column = "yday",
                        duration_formula = ~ 1,
                        start_formula = ~ 1,
                    type=2,
                        data = df_2_75,
                        log_lik = FALSE,
                        chains = 2,
                        cores = 2)
n_2_75 = nrow(df_2_75)

dfs_2_75 = filter(remove_m_scores(sim_data,0.75,subsample_nonmoult = TRUE), !is.na(pfmg) & !is.na(moult_numcat))
uz2s_75 <- moultmcmc("pfmg",
                      date_column = "yday",
                      duration_formula = ~ 1,
                      start_formula = ~ 1,
                     type=2,
                      data = dfs_2_75,
                      log_lik = FALSE,
                      chains = 2,
                      cores = 2)
n_2s_75 = nrow(dfs_2_75)

uz12_50 <- moultmcmc("moult_scorecat",
                     date_column = "yday",
                     duration_formula = ~ 1,
                     start_formula = ~ 1,
                     type = 12,
                        data = remove_m_scores(sim_data,0.5),
                        log_lik = FALSE,
                        chains = 2,
                        cores = 2)

df_2_50 = filter(remove_m_scores(sim_data,0.5), !is.na(pfmg))
uz2_50 <- moultmcmc("pfmg",
                      date_column = "yday",
                      duration_formula = ~ 1,
                      start_formula = ~ 1,
                    type=2,
                      data = df_2_50,
                      log_lik = FALSE,
                      chains = 2,
                      cores = 2)
n_2_50 = nrow(df_2_50)

dfs_2_50 = filter(remove_m_scores(sim_data,0.5,subsample_nonmoult = TRUE), !is.na(pfmg) & !is.na(moult_numcat))
uz2s_50 <- moultmcmc("pfmg",
                       date_column = "yday",
                       duration_formula = ~ 1,
                       start_formula = ~ 1,
                     type=2,
                       data = dfs_2_50,
                       log_lik = FALSE,
                       chains = 2,
                       cores = 2)
n_2s_50 = nrow(dfs_2_50)

uz12_25 <- moultmcmc("moult_scorecat",
                     date_column = "yday",
                     duration_formula = ~ 1,
                     start_formula = ~ 1,
                     type = 12,
                        data = remove_m_scores(sim_data,0.25),
                        log_lik = FALSE,
                        chains = 2,
                        cores = 2)
df_2_25 = filter(remove_m_scores(sim_data,0.25), !is.na(pfmg))
uz2_25 <- moultmcmc("pfmg",
                      date_column = "yday",
                      duration_formula = ~ 1,
                      start_formula = ~ 1,
                    type=2,
                      data = df_2_25,
                      log_lik = FALSE,
                      chains = 2,
                      cores = 2)
n_2_25 = nrow(df_2_25)

dfs_2_25 = filter(remove_m_scores(sim_data,0.25,subsample_nonmoult = TRUE), !is.na(pfmg) & !is.na(moult_numcat))
uz2s_25 <- moultmcmc("pfmg",
                       date_column = "yday",
                       duration_formula = ~ 1,
                       start_formula = ~ 1,
                     type = 2,
                       data = dfs_2_25,
                       log_lik = FALSE,
                       chains = 2,
                       cores = 2)
n_2s_25 = nrow(dfs_2_25)

uz2 <- moultmcmc("pfmg",
                   date_column = "yday",
                   duration_formula = ~ 1,
                   start_formula = ~ 1,
                 type = 2,
                   data = sim_data,
                   log_lik = FALSE,
                   chains = 2,
                   cores = 2)
#compare_plot(uz1,uz12_75)

bind_rows(summary_table(uz1) %>% mutate(model = 'uz1'),
          #summary_table(uz12_90),
          #summary_table(uz12_80),
          summary_table(uz12_75) %>% mutate(model = 'uz12_75'),
          summary_table(uz2_75) %>% mutate(model = 'uz2_75',  n = n_2_75),
          summary_table(uz2s_75) %>% mutate(model = 'uz2s_75', n = n_2s_75),
          summary_table(uz12_50) %>% mutate(model = 'uz12_50'),
          summary_table(uz2_50) %>% mutate(model = 'uz2_50', n = n_2_50),
          summary_table(uz2s_50) %>% mutate(model = 'uz2s_50', n = n_2s_50),
          summary_table(uz12_25) %>% mutate(model = 'uz12_25'),
          summary_table(uz2_25) %>% mutate(model = 'uz2_25',  n = n_2_25),
          summary_table(uz2s_25) %>% mutate(model = 'uz2s_25', n = n_2s_25),
          summary_table(uz2) %>% mutate(model = 'uz2')
          ) %>%
  #left_join(tibble(prop_cat = as.character(1:5), prop_score = c(0,25,50,75,100))) %>%
  filter(!(parameter %in% c('lp__', 'log_sd_(Intercept)'))) %>%
  mutate(set = i)%>%
  readr::write_rds(file = paste('simulation_outputs/euring/integrated_backfit',formatC(i, width=2, flag="0"),'.rds', sep = ''))
  #ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') + xlab("Proportion of birds with feather scores in active moult category") + theme_classic()
#}



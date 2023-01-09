library(dplyr)
library(ggplot2)
library(moultmcmc)

scenario_lookup =  readRDS('data/simulated_datasets/siskin_sampling_mk2/scenario_lookup.rds')
recap_summ <- function(x){
  summary_table(x, pars = c("mean_(Intercept)","duration_(Intercept)","sd_(Intercept)","beta_star","sigma_mu_ind"))
}


#ingest fitted models and visualize
recap_models <- list.files('simulation_outputs/siskin_sampling/', pattern = 'uz3r', full.names = TRUE)
names(recap_models) <- tools::file_path_sans_ext(basename(recap_models))
readsumm <- function(x){mod =readRDS(x); recap_summ(mod)}
recap_summaries <- lapply(recap_models, function(x) try(readsumm(x)))#slow! - corrupt file somewhere, needs to use a try error construct

#lumped recap and active moult recap
other_recap_models <- list.files('simulation_outputs/siskin_sampling/', pattern = 'uz5r', full.names = TRUE)
names(other_recap_models) <- tools::file_path_sans_ext(basename(other_recap_models))
other_recap_summaries <- lapply(other_recap_models, function(x){mod =readRDS(x); recap_summ(mod)})#slow!

#lumped std model

#holding the full models in memory is way too expensive, need to summarize and discard instead
std_models <- list.files('simulation_outputs/siskin_sampling/', pattern = 'uz3_|uz5_', full.names = TRUE)
names(std_models) <- tools::file_path_sans_ext(basename(std_models))

std_summaries <- lapply(std_models, function(x){mod = readRDS(x); summary_table(mod, pars = c("mean_(Intercept)","duration_(Intercept)","sd_(Intercept)"))})


summary_df <- bind_rows(
  bind_rows(recap_summaries[-which(sapply(recap_summaries, inherits, 'try-error'))], .id = 'scenario'),
  bind_rows(std_summaries, .id = 'scenario'),
  bind_rows(other_recap_summaries, .id = 'scenario')
) %>%
  tidyr::separate(scenario, into = c("model","scenario"), sep = '_') %>% mutate(scenario = as.numeric(gsub('[a-z]+','',scenario))) %>%
  left_join(scenario_lookup) %>% 
  mutate(model = as.factor(model),
         rate_recap_active_moult = n_recap_active_moult/n_ind)

readr::write_rds(summary_df, 'simulation_outputs/siskin_bias_mk2_summary_df.rds')

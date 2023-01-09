#assess unconverged models
uz2r <- readRDS("C:/local/projects/2022_moult_methods_paper/simulation_outputs/biased_sampling/uz2r_scenario008.rds")
summary_table(uz2r)
rstan::traceplot(uz2r$stanfit)
rstan::stan_dens(uz2r$stanfit, separate_chains = T)

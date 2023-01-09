library(dplyr)
library(ggplot2)
library(moultmcmc)

scenario_lookup =  readRDS('data/simulated_datasets/biased_sampling_mk2/scenario_lookup.rds')
recap_summ <- function(x){
  summary_table(x, pars = c("mean_(Intercept)","duration_(Intercept)","sd_(Intercept)","sigma_mu_ind"))
}

summary(scenario_lookup)

#ingest fitted models and visualize
recap_models <- list.files('simulation_outputs/biased_sampling/', pattern = 'uz2r|uz1r', full.names = TRUE)
names(recap_models) <- tools::file_path_sans_ext(basename(recap_models))
readsumm <- function(x){mod =readRDS(x); recap_summ(mod)}
recap_summaries <- lapply(recap_models, function(x) try(readsumm(x)))#slow! - corrupt file somewhere, needs to use a try error construct

#lumped recap and active moult recap
other_recap_models <- list.files('simulation_outputs/biased_sampling/', pattern = 'uz2lr', full.names = TRUE)
names(other_recap_models) <- tools::file_path_sans_ext(basename(other_recap_models))
other_recap_summaries <- lapply(other_recap_models, function(x){mod =readRDS(x); recap_summ(mod)})#slow!

#lumped std model

#holding the full models in memory is way too expensive, need to summarize and discard instead
std_models <- list.files('simulation_outputs/biased_sampling/', pattern = 'uz2_|uz2l_|uz1_', full.names = TRUE)
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

readr::write_rds(summary_df, 'simulation_outputs/sampling_bias_mk2_summary_df.rds')
summary_df <- readr::read_rds('simulation_outputs/sampling_bias_mk2_summary_df.rds')

summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.05)) %>% ungroup() %>% group_by(model) %>% summarize(n_conv = sum(converged))

#pull out unconverged model for new array job / needs to be broken down by model type...
summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.05)) %>% filter(converged==FALSE) %>% pull(scenario) %>% paste(collapse=',')


summary_df %>% filter(Rhat > 1.05 ) %>% group_by(model,parameter) %>% summarize(n_failed=n())

#
 summary_df %>%
   filter(model == "uz2r" & parameter != 'beta_star') %>%
   ggplot(aes(x=rate_recap_active_moult,y=Rhat, col = parameter)) +
   geom_point() + facet_wrap(~parameter)
 
 summary_df %>%
   filter(model == "uz2r" & parameter != 'beta_star') %>%
   ggplot(aes(x=n_recap_active_moult,y=Rhat, col = parameter)) +
   geom_point() + facet_wrap(~parameter)#

summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.05)) %>% filter(converged==FALSE)

summary_df %>% ggplot(aes(x = n_recap_active_moult, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% ggplot(aes(x = rate_recap_active_moult, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% ggplot(aes(x = moult_capture_prob, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% filter(Rhat < 1.05) %>% ggplot(aes(x = moult_capture_prob, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

library(mgcv)
library(gratia)
duration_fit <- gam((estimate-65)/65 ~ model + te(n_recap_active_moult, moult_capture_prob, by = model, k = 4), data = filter(summary_df, parameter == 'duration_(Intercept)' & Rhat < 1.05))
draw(duration_fit, constant = coef(duration_fit)[1], dist = 0.25)

start_fit <- gam((estimate-150)/65 ~ model + te(n_recap_active_moult, moult_capture_prob, by = model, k = 4), data = filter(summary_df, parameter == 'mean_(Intercept)' & Rhat < 1.05))
draw(start_fit, constant = coef(duration_fit)[1], dist = 0.25)

sd_start_fit <- gam((estimate-10)/10 ~ model + te(n_recap_active_moult, moult_capture_prob, by = model, k = 4), data = filter(summary_df, parameter == 'sd_(Intercept)' & Rhat < 1.05))
draw(sd_start_fit, constant = coef(duration_fit)[1], dist = 0.25)

grid_res = 100
pred_grid = expand.grid(moult_capture_prob = seq(0.1,1, length.out = 2*grid_res), n_recap_active_moult = seq(1,25,length.out=grid_res), model = c('uz1','uz1r','uz2','uz2l','uz2r','uz2ra','uz2lr','uz2lra'))
pred_grid$duration_bias <- predict(duration_fit, newdata = pred_grid)
pred_grid$start_bias <- predict(start_fit, newdata = pred_grid)
pred_grid$sd_start_bias <- predict(sd_start_fit, newdata = pred_grid)

pred_lines = expand.grid(moult_capture_prob = seq(0.1,1, length.out = 2*grid_res), n_recap_active_moult = c(1,3,5,10,20), model = c('uz2','uz2r'))
pred_lines$duration_bias_preds <- predict(duration_fit, newdata = pred_lines)

col_breaks = c(seq(-0.4,-0.1,by = 0.05),-0.05,0.05,0.1,0.15)

duration_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = moult_capture_prob, fill = duration_bias, z = duration_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.4,.2)) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'duration_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = moult_capture_prob), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('') + ylab('relative capture probability of moulting individuals')

start_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = moult_capture_prob, fill = start_bias, z = start_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'mean_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = moult_capture_prob), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('individuals with active moult recaptures') + ylab('')

sd_start_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = moult_capture_prob, fill = sd_start_bias, z = sd_start_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = c(seq(-0.4,-0.1,by = 0.05),-0.05,0.05)) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'sd_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = moult_capture_prob), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('') + ylab('')

cowplot::plot_grid(duration_bias_plot + theme(legend.position = 'none',strip.background.y = element_blank(),
                   strip.text.y = element_blank()),
                   start_bias_plot + theme(legend.position = 'none',strip.background.y = element_blank(),
                                           strip.text.y = element_blank()),
                   sd_start_bias_plot + theme(legend.position = 'none'),
                   nrow = 1)

#plot might be nicer when you reshape the data first
pred_grid %>% 
  filter(model %in% c('uz2','uz2r','uz1','uz1r')) %>%
  tidyr::pivot_longer(duration_bias:sd_start_bias, names_to = c('parameter','stat'), names_sep = '_',values_to = 'values') %>%
  mutate(parameter = case_when(parameter == 'sd' ~ 'sd_(Intercept)',
                                parameter == 'start' ~ 'mean_(Intercept)',
                                parameter == 'duration' ~ 'duration_(Intercept)'),
         pretty_par = case_when(parameter == 'sd_(Intercept)' ~ 'Start date std. deviation',
                               parameter == 'mean_(Intercept)' ~ 'Start date',
                               parameter == 'duration_(Intercept)' ~ 'Duration'),
         model_pretty = factor(case_when(model == 'uz2' ~ 'Standard model (T2)',
                                  model == 'uz2r' ~ 'Recaptures model (T2R)',
                                  model == 'uz1' ~ 'Standard model (T1)',
                                  model == 'uz1r' ~ 'Recaptures model (T1R)'), 
                               levels = c('Standard model (T1)','Recaptures model (T1R)','Standard model (T2)','Recaptures model (T2R)'))) %>%
    ggplot(aes(x = n_recap_active_moult, y = moult_capture_prob, fill = values, z = values)) +
    geom_tile() +
  facet_grid(model_pretty~pretty_par) +
  scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.4,.2)) +
  theme_classic() +
  guides(fill = guide_colorbar(barheight = 15)) +
  geom_point(data = filter(summary_df, !(parameter %in% c('beta_star','finite_sd','sigma_mu_ind')) & Rhat < 1.05 & model %in% c('uz2','uz2r','uz1','uz1r')),
             aes(x = n_recap_active_moult, y = moult_capture_prob), inherit.aes = FALSE, pch = 3, col = 'black', size=1) +
  xlim(1,23) +
  xlab('Individuals with active moult recaptures') + ylab('Relative capture probability of moulting individuals') + ggtitle('Constant sampling bias')
ggsave('figures/constant_sampling_bias_T1T2_vs_T1RT2R_surface_plots.png', width = 6.5, height = 7)

pred_grid %>% 
  tidyr::pivot_longer(duration_bias:sd_start_bias, names_to = c('parameter','stat'), names_sep = '_',values_to = 'values') %>%
  mutate(parameter = case_when(parameter == 'sd' ~ 'sd_(Intercept)',
                               parameter == 'start' ~ 'mean_(Intercept)',
                               parameter == 'duration' ~ 'duration_(Intercept)')) %>%
  ggplot(aes(x = n_recap_active_moult, y = moult_capture_prob, fill = values, z = values)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.4,.2)) + theme_classic() + geom_point(data = filter(summary_df, !(parameter %in% c('beta_star','finite_sd','sigma_mu_ind')) & Rhat < 1.05), aes(x = n_recap_active_moult, y = moult_capture_prob), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,23) + xlab('individuals with active moult recaptures') + ylab('relative capture probability of moulting individuals')
ggsave('figures/constant_sampling_bias_T2_variants_surface_plots.png')

ggplot(pred_lines, aes(col = as.factor(n_recap_active_moult), x = moult_capture_prob, y = duration_bias_preds)) + geom_line(lwd=1) + facet_wrap(~model) + scale_color_manual(values = viridisLite::viridis(n=5)) 


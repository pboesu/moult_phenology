library(dplyr)
library(ggplot2)
library(moultmcmc)

scenario_lookup =  readRDS('data/simulated_datasets/siskin_sampling_mk2/scenario_lookup.rds')

summary_df <- readr::read_rds('simulation_outputs/siskin_bias_mk2_summary_df.rds')

summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.01)) %>% ungroup() %>% group_by(model) %>% summarize(n_conv = sum(converged))

#pull out unconverged model for new array job / needs to be broken down by model type...
summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.05)) %>% filter(converged==FALSE) %>% pull(scenario) %>% paste(collapse=',')


summary_df %>% filter(Rhat > 1.05 ) %>% group_by(model,parameter) %>% summarize(n_failed=n())

#
summary_df %>%
  filter(parameter != 'beta_star') %>%
  ggplot(aes(x=rate_recap_active_moult,y=Rhat, col = parameter)) +
  geom_point() + facet_grid(model~parameter)

summary_df %>%
  filter(parameter != 'beta_star') %>%
  ggplot(aes(x=n_recap_active_moult,y=Rhat, col = parameter)) +
  geom_point() + facet_grid(model~parameter)#

summary_df %>% group_by(model,scenario) %>% summarize(converged = all(Rhat < 1.05)) %>% filter(converged==FALSE)

summary_df %>% ggplot(aes(x = n_recap_active_moult, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% ggplot(aes(x = rate_recap_active_moult, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% ggplot(aes(x = tau_sd, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

summary_df %>% filter(Rhat < 1.05) %>% ggplot(aes(x = tau_sd, y = estimate, ymax = uci, ymin = lci, col = model, shape = Rhat < 1.05)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y')

library(mgcv)
library(gratia)
duration_fit <- gam((estimate-78)/78 ~ model + te(n_recap_active_moult, tau_sd, by = model, k = 4, m = 1), data = filter(summary_df, parameter == 'duration_(Intercept)' & Rhat < 1.05))
draw(duration_fit, constant = coef(duration_fit)[1], dist = 0.25)

duration_prop_fit <- gam((estimate-78)/78 ~ model + te(I(n_recap_active_moult/n_ind), tau_sd, by = model, k = 4), data = filter(summary_df, parameter == 'duration_(Intercept)' & Rhat < 1.05))
draw(duration_prop_fit, constant = coef(duration_fit)[1], dist = 0.25)

start_fit <- gam((estimate-197)/78 ~ model + te(n_recap_active_moult, tau_sd, by = model, k = 4), data = filter(summary_df, parameter == 'mean_(Intercept)' & Rhat < 1.05))
draw(start_fit, constant = coef(duration_fit)[1], dist = 0.25)

sd_start_fit <- gam((estimate-9)/9 ~ model + te(n_recap_active_moult, tau_sd, by = model, k = 4, m=1), data = filter(summary_df, parameter == 'sd_(Intercept)' & Rhat < 1.05))
draw(sd_start_fit, constant = coef(duration_fit)[1], dist = 0.25)

grid_res = 100
pred_grid = expand.grid(tau_sd = seq(0,8, length.out = 2*grid_res), n_recap_active_moult = seq(0,35,length.out=grid_res), model = c('uz3','uz5','uz3r','uz3ra','uz5r','uz5ra'))
pred_grid$duration_bias <- predict(duration_fit, newdata = pred_grid)
pred_grid$start_bias <- predict(start_fit, newdata = pred_grid)
pred_grid$sd_start_bias <- predict(sd_start_fit, newdata = pred_grid)

pred_lines = expand.grid(tau_sd = seq(0.1,8, length.out = 2*grid_res), n_recap_active_moult = c(1,3,5,10,20), model = c('uz3','uz3r'))
pred_lines$duration_bias_preds <- predict(duration_fit, newdata = pred_lines)

col_breaks = c(seq(-0.22,-0.02,by = 0.02),0.02,0.04,0.06,0.08,0.1,.12)
col_labels = col_breaks
col_labels[1] = paste('<', col_breaks[1])
col_labels[length(col_breaks)] = paste('>', col_breaks[length(col_breaks)])
col_labels

duration_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = tau_sd, fill = duration_bias, z = duration_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.4,.2)) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'duration_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = tau_sd), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('') + ylab('tau_sd')
duration_bias_plot


start_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = tau_sd, fill = start_bias, z = start_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'mean_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = tau_sd), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('individuals with active moult recaptures') + ylab('')

sd_start_bias_plot <- ggplot(pred_grid, aes(x = n_recap_active_moult, y = tau_sd, fill = sd_start_bias, z = sd_start_bias)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = c(seq(-0.4,-0.1,by = 0.05),-0.05,0.05)) + theme_classic() + geom_point(data = filter(summary_df, parameter == 'sd_(Intercept)' & Rhat < 1.05), aes(x = n_recap_active_moult, y = tau_sd), inherit.aes = FALSE, pch = 3, col = 'black') + xlim(1,25) + xlab('') + ylab('')

cowplot::plot_grid(duration_bias_plot + theme(legend.position = 'none',strip.background.y = element_blank(),
                                              strip.text.y = element_blank()),
                   start_bias_plot + theme(legend.position = 'none',strip.background.y = element_blank(),
                                           strip.text.y = element_blank()),
                   sd_start_bias_plot + theme(legend.position = 'none'),
                   nrow = 1)

#plot might be nicer when you reshape the data first
pred_grid %>% 
  filter(model %in% c('uz3','uz3ra','uz5','uz5ra')) %>%
  tidyr::pivot_longer(duration_bias:sd_start_bias, names_to = c('parameter','stat'), names_sep = '_',values_to = 'values') %>%
  mutate(parameter = case_when(parameter == 'sd' ~ 'sd_(Intercept)',
                               parameter == 'start' ~ 'mean_(Intercept)',
                               parameter == 'duration' ~ 'duration_(Intercept)'),
         pretty_par = case_when(parameter == 'sd_(Intercept)' ~ 'Start date std. deviation',
                                parameter == 'mean_(Intercept)' ~ 'Start date',
                                parameter == 'duration_(Intercept)' ~ 'Duration'),
         model_pretty = factor(case_when(model == 'uz3' ~ 'Standard model (T3)',
                                         model == 'uz3ra' ~ 'Recaptures model (T3R)',
                                         model == 'uz5' ~ 'Standard model (T5)',
                                         model == 'uz5ra' ~ 'Recaptures model (T5R)'),
                               levels = c('Standard model (T3)','Recaptures model (T3R)','Standard model (T5)','Recaptures model (T5R)'))) %>%
  ggplot(aes(x = n_recap_active_moult, y = tau_sd, fill = values, z = values)) +
  geom_tile() +
  scale_y_continuous(breaks = seq(0,0.1*78,by = 0.02*78), labels = seq(0,0.1, by = 0.02)) + 
  facet_grid(model_pretty~pretty_par) +
  scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.22,.12), labels = col_labels) +
  theme_classic() +
  guides(fill = guide_colorbar(barheight = 15))+
  geom_point(data = filter(summary_df, !(parameter %in% c('beta_star','finite_sd','sigma_mu_ind')) & Rhat < 1.05 & model %in% c('uz3','uz3ra','uz5','uz5ra')) %>%
               mutate(pretty_par = case_when(parameter == 'sd_(Intercept)' ~ 'Start date std. deviation',
                                             parameter == 'mean_(Intercept)' ~ 'Start date',
                                             parameter == 'duration_(Intercept)' ~ 'Duration'),
                      model_pretty = factor(case_when(model == 'uz3' ~ 'Standard model (T3)',
                                                      model == 'uz3ra' ~ 'Recaptures model (T3R)',
                                                      model == 'uz5' ~ 'Standard model (T5)',
                                                      model == 'uz5ra' ~ 'Recaptures model (T5R)'),
                                            levels = c('Standard model (T3)','Recaptures model (T3R)','Standard model (T5)','Recaptures model (T5R)'))),
             aes(x = n_recap_active_moult, y = tau_sd), inherit.aes = FALSE, pch = 3, col = 'black', size=1) +
  xlim(0,30) + xlab('Individuals with active moult recaptures') +
  ylab(latex2exp::TeX('Unmodelled variation in moult duration   $\\sigma_\\tau/\\tau$')) +
  ggtitle('Non-constant sampling bias')
ggsave('figures/siskin_sampling_bias_T3_vs_T3R_surface_plots.png', width = 6.5, height = 7)

pred_grid %>% 
  tidyr::pivot_longer(duration_bias:sd_start_bias, names_to = c('parameter','stat'), names_sep = '_',values_to = 'values') %>%
  mutate(parameter = case_when(parameter == 'sd' ~ 'sd_(Intercept)',
                               parameter == 'start' ~ 'mean_(Intercept)',
                               parameter == 'duration' ~ 'duration_(Intercept)')) %>%
  ggplot(aes(x = n_recap_active_moult, y = tau_sd, fill = values, z = values)) + geom_tile() + facet_grid(model~parameter) + scale_fill_steps2(name='relative bias',breaks = col_breaks, limits = c(-0.4,.2)) + theme_classic() + geom_point(data = filter(summary_df, !(parameter %in% c('beta_star','finite_sd','sigma_mu_ind')) & Rhat < 1.05), aes(x = n_recap_active_moult, y = tau_sd), inherit.aes = FALSE, pch = 3, col = 'black') + scale_x_continuous(limits = c(1,40)) + xlab('individuals with active moult recaptures') + ylab('tau_sd')
ggsave('figures/siskin_sampling_bias_T35_variants_surface_plots.png')

ggplot(pred_lines, aes(col = as.factor(n_recap_active_moult), x = tau_sd, y = duration_bias_preds)) + geom_line(lwd=1) + facet_wrap(~model) + scale_color_manual(values = viridisLite::viridis(n=5)) 


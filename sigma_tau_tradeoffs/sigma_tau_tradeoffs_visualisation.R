library(dplyr)
library(moultmcmc)
library(ggplot2)

backfits <- lapply(list.files('simulation_outputs/sigma_tau_ratio/', pattern = 'sigma_tau', full.names = T), readRDS)
#TODO: rerun and calculate correlation between sigma, mu and tau when summarising the models

bind_rows(backfits) %>% 
  filter(model %in% c( "T1",  "T1L",   "T2",  "T2L",   "T3")) %>%
  mutate(data_type = substr(type, 1,1),
         lumped = grepl('L',model)) %>%
  mutate(sim_value = case_when(parameter == 'mean_(Intercept)' ~167,
                               parameter == 'duration_(Intercept)' ~pop_duration,
                               parameter == 'sd_(Intercept)'~ pop_duration*sigma_tau_ratio)) %>%
  mutate(sim_value_adj = case_when(parameter == 'mean_(Intercept)' ~ pop_duration,
                                   TRUE ~ sim_value)) -> backfits_df

unconverged <- backfits_df %>% group_by(model,set,sigma_tau_ratio) %>% summarize(converged = max(Rhat) < 1.01 & max(estimate) < 250)
table(unconverged$model, unconverged$converged)

ggplot(unconverged,aes(x=sigma_tau_ratio, y=converged, col = model))+ geom_point(position = position_jitter(width = 0, height = 0.4))

summary(glm(converged ~ model:sigma_tau_ratio, family=binomial, data = unconverged))



pretty_names = tibble(parameter = c("mean_(Intercept)", "duration_(Intercept)", 
                                    "sd_(Intercept)"),
                      pretty_par = c("Start date", "Duration", "Start date std. deviation"))



table(backfits_df$model)

backfits_df %>% 
  left_join(pretty_names) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = sigma_tau_ratio)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') +
  xlab("Model") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), lty = 2) + scale_color_viridis_c(begin=0.1,end=0.9)

backfits_df %>% 
  left_join(pretty_names) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = sigma_tau_ratio, groups = factor(sigma_tau_ratio))) +
  geom_pointrange(position = position_dodge(0.8), size = 0.2) + facet_wrap(~parameter, scales = 'free_y') +
  xlab("model") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), lty = 2)+ scale_color_viridis_c(end=0.9) + theme_classic()


cv_plot <- backfits_df %>% 
  left_join(pretty_names) %>%
  left_join(unconverged) %>% filter(converged == TRUE & lumped == FALSE) %>%
  filter(!is.na(pretty_par)) %>%
  group_by(model,set,sigma_tau_ratio) %>%
  mutate(estimate_adj = ifelse(parameter == "mean_(Intercept)", estimate[parameter == 'duration_(Intercept)'], estimate)) %>% #CV not meaningful for start date b/c scale of mean arbitrary, so use duration as reference
  ungroup %>%
  group_by(model,parameter,sigma_tau_ratio) %>%
  summarise(mean_cv = mean(sd/estimate_adj), pretty_par = unique(pretty_par), data_type = unique(data_type), lumped = unique(lumped)) %>%
  ggplot(aes(x = sigma_tau_ratio, y = mean_cv, col = data_type)) + geom_point() + 
  #geom_line() + 
  facet_grid(data_type~pretty_par) +
  xlab(latex2exp::TeX('$\\sigma/\\tau$')) +
  ylab("Posterior CV") +
  theme(legend.position = "bottom") + 
  #geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)+
  #scale_color_viridis_d(end=0.9) +
  scale_color_manual(values=unname(palette.colors(n=3,"Okabe-Ito")))+
  theme_classic() +
  ylim(0,1)#truncate extreme outcomes
cv_plot
  
backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  #filter(sigma_tau_ratio < 0.5) %>%
  left_join(pretty_names) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  ggplot(aes(x = sigma_tau_ratio, y = estimate, ymin = lci, ymax = uci, col = data_type, pch = lumped)) +
  geom_pointrange() + facet_grid(data_type~parameter, scales = 'free_y') +
  xlab(latex2exp::TeX('$\\sigma/\\tau$')) +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), lty = 2) + theme_classic()
ggsave('figures/sigma_tau_intercepts_results_raw.png')

bias_plot <- backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  #filter(sigma_tau_ratio < 0.5) %>%
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par) & lumped == FALSE) %>%
  group_by(model,parameter,sigma_tau_ratio) %>%
  summarise(mean_bias = mean((estimate-sim_value)/sim_value_adj), pretty_par = unique(pretty_par), data_type = unique(data_type), lumped = unique(lumped)) %>%
  ggplot(aes(x = sigma_tau_ratio, y = mean_bias, col = data_type)) +
  geom_point() +
  #geom_line() +
  facet_grid(data_type~parameter) +
  xlab(latex2exp::TeX('$\\sigma/\\tau$')) +
  ylab("Relative bias")  + 
  #scale_color_viridis_d(end=0.9) + 
  theme_classic()+
  scale_color_manual(values=unname(palette.colors(n=3,"Okabe-Ito")))+
  theme(legend.position = "bottom")# + ylim(-1,1)
bias_plot
ggsave('figures/sigma_tau_intercepts_bias.png')


cowplot::save_plot('figures/sigma_tau_bias_precision.png',
cowplot::plot_grid(bias_plot + theme(legend.position = 'none'),
                   cv_plot+ theme(legend.position = 'none'),
                   #cowplot::get_legend(bias_plot),
                   nrow = 2,#3,
                   #rel_heights = c(1,1,0.2),
                   labels = c('A','B',''),
                   axis = 'l',
                   align = 'v'),
base_height = 8,
base_width = 6
)

                   
abs_bias_plot2d <- backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  #filter(estimate < 500) %>%
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par)) %>%
  mutate(bias = (estimate-sim_value)/sim_value_adj) %>%
  ggplot(aes(x = sigma_tau_ratio, y = pop_duration, col = (abs(bias)), shape = lumped, lty = lumped)) +
  geom_point(position = position_jitter(0.01)) +
  #geom_line() +
  facet_grid(type~parameter) +
  xlab(latex2exp::TeX('$\\sigma/\\tau$')) +
  ylab(latex2exp::TeX('moult duration $\\tau$ (days)'))  + 
  scale_color_viridis_c(end=0.95, limits = c(0,0.5), oob=scales::squish) + 
  theme_classic()+
  theme(legend.position = "bottom")
abs_bias_plot2d

#t3 model gam
backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE & type==3) %>%
  #filter(estimate < 500) %>%
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par)) %>%
  mutate(abs_bias = abs(estimate-sim_value)/sim_value_adj,
         parameter = factor(parameter)) -> t3_abs_bias
backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE & type==2L) %>%
  #filter(estimate < 500) %>%
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par)) %>%
  mutate(abs_bias = abs(estimate-sim_value)/sim_value_adj,
         parameter = factor(parameter)) -> t2l_abs_bias

library(mgcv)
t3_abs_bias_fit <- gam(abs_bias ~ te(sigma_tau_ratio,pop_duration, by = parameter, k=4) + parameter, data = t3_abs_bias)
plot.gam(t3_abs_bias_fit, pages=1)

t2l_abs_bias_fit <- gam(abs_bias ~ te(sigma_tau_ratio,pop_duration, by = parameter, k=4) + parameter, data = t2l_abs_bias)
plot.gam(t2l_abs_bias_fit, pages=1)


pred_data = expand.grid(pop_duration = seq(35,220,by = 2), sigma_tau_ratio = seq(0,0.7,by = 0.01), parameter = unique(t3_abs_bias$parameter))

pred_data$pred_3 <- predict(t3_abs_bias_fit, newdata = pred_data)
pred_data$pred_2L <- predict(t2l_abs_bias_fit, newdata = pred_data)

pred_data_long = tidyr::pivot_longer(pred_data, pred_3:pred_2L, names_to = 'model_type', values_to = 'pred') %>%
  mutate(model_pretty = case_when(model_type == 'pred_3' ~ 'Type 3',
                                  model_type == 'pred_2L' ~ 'Type 2L'),
         par_pretty = case_when(grepl('duration', parameter) ~ 'Duration',
                                grepl('mean', parameter) ~ 'Start date',
                                grepl('sd', parameter) ~ 'Start date SD'))

weaver_data <- readr::read_csv('../2022_Ostrich_moultmcmc_demo/data/weaver_table4_1.csv') %>%
  rename(pop_duration = 'duration') %>%
  mutate(arid = ifelse(grepl('Sociable|Chestnut',Species),'arid','mesic'))

bp_hosts <- readr::read_csv('../2022_Ostrich_moultmcmc_demo/data/brood_parasites_estrildids.csv') %>%
  rename(pop_duration = 'duration') %>%
  mutate(sigma_tau_ratio = start_date_sd/pop_duration)

wc_passerines <- readr::read_csv('../2022_Ostrich_moultmcmc_demo/data/dimorphic_passerines_western_cape_bonnevie2010.csv') %>%
  rename(pop_duration = 'duration')
  
uk_birds <- readr::read_csv('data/morrison_reanalysis_table2_machine_readable.csv') %>%
  rename(pop_duration = 'mean_duration', model = 'model_type') %>%
  mutate(sigma_tau_ratio = mean_sd_start/pop_duration,
         migrant = ifelse(code2ltr %in% c('WW','WH','BC'),'migrant','resident'))


ggplot(pred_data_long, aes(x = sigma_tau_ratio, y = pop_duration, fill = pred)) +
  geom_tile() + facet_grid(model_pretty~par_pretty) +
  scale_fill_steps2(name = "Relative mean abs. error", limits = c(0,0.45), oob = scales::squish, n.breaks = 8) +
  geom_point(data = weaver_data, fill = 'black',col = 'darkgrey', pch = 21) +
  #geom_point(data = uk_birds, fill = NA, col = 'black', pch = 3) +
  geom_point(data = bp_hosts, pch = 21, fill = 'black', col = 'darkgrey') +
  geom_point(data = wc_passerines, pch = 21, fill = 'black', col = 'darkgrey')+
  theme_classic() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(barwidth = 15))+
  ylab('Moult duration') + 
  xlab('Start date SD / Moult duration')
ggsave('figures/sigma_tau_tradeoff_T2L_T3_SA_passerines.png', width = 8)

ggplot(pred_data_long, aes(x = sigma_tau_ratio, y = pop_duration, fill = pred)) +
  geom_tile() + facet_grid(model_pretty~par_pretty) +
  scale_fill_steps2(name = "Relative mean abs. error", limits = c(0,0.45), oob = scales::squish, n.breaks = 8) +
  #geom_point(data = weaver_data, fill = 'black',col = 'darkgrey', pch = 21) +
  #geom_point(data = uk_birds, fill = NA, col = 'black', pch = 3) +
  #geom_point(data = bp_hosts, pch = 21, fill = 'black', col = 'darkgrey') +
  #geom_point(data = wc_passerines, pch = 21, fill = 'black', col = 'darkgrey')+
  theme_classic() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(barwidth = 15))+
  ylab('Moult duration') + 
  xlab('Start date SD / Moult duration')
ggsave('figures/sigma_tau_tradeoff_T2L_T3.png', width = 8)


bias_plot2d <- backfits_df %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  #filter(estimate < 500) %>%
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par)) %>%
  mutate(bias = (estimate-sim_value)/sim_value_adj) %>%
  ggplot(aes(x = sigma_tau_ratio, y = pop_duration, col = bias, shape = lumped, lty = lumped)) +
  geom_point(position = position_jitter(0.02)) +
  #geom_line() +
  facet_grid(type~parameter) +
  xlab(latex2exp::TeX('$\\sigma/\\tau$')) +
  ylab("pop duration")  + 
  scale_color_gradient2(limits = c(-0.5,0.5), oob=scales::squish) + 
  theme_classic()+
  theme(legend.position = "bottom")
bias_plot2d

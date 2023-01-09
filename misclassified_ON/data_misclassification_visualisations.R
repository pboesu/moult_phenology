library(dplyr)
library(moultmcmc)
library(ggplot2)

backfits <- lapply(list.files('simulation_outputs/misclass', pattern = 'misclass', full.names = T), readRDS)

reference_level = tibble(parameter = c("mean_(Intercept)", "duration_(Intercept)", 
                                       "sd_(Intercept)"),
                         sim_value = c(167, 75, 29)) %>%
  mutate(sim_value_adj = ifelse(parameter == 'mean_(Intercept)', 75, sim_value))

pretty_names = tibble(parameter = c("mean_(Intercept)", "duration_(Intercept)", 
                                    "sd_(Intercept)"),
                      pretty_par = c("Start date", "Duration", "Start date std. deviation"))

bind_rows(backfits) %>% 
  filter(prop_misclass < 0.5 & model %in% c( "T1",  "T1L",   "T2",  "T2L",   "T3")) %>%
  mutate(data_type = substr(type, 1,1),
         lumped = grepl('L',model))-> backfits_df

table(backfits_df$model)

unconverged <- backfits_df %>% group_by(model,set,prop_misclass) %>% summarize(converged = max(Rhat) <= 1.01)
table(unconverged$model, unconverged$converged)

ggplot(unconverged,aes(x=prop_misclass, y=converged, col = model))+ geom_point(position = position_jitter())

summary(glm(converged ~  model:prop_misclass, family=binomial, data = unconverged))

filter(unconverged, converged == FALSE) %>% pull(set) %>% unique()

backfits_df %>% group_by(model, parameter, prop_misclass) %>% summarise(estimate = mean(estimate), lci = mean(lci), uci = mean(uci)) %>% 
  ungroup() %>%
  left_join(pretty_names) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = prop_misclass)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') +
  xlab("Proportion of misclassified non-moult records") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2) + scale_color_viridis_c(begin=0.1,end=0.9)

backfits_df %>% 
  left_join(pretty_names) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = prop_misclass, groups = factor(prop_misclass))) +
  geom_pointrange(position = position_dodge(0.6)) + facet_wrap(~parameter, scales = 'free_y') +
  xlab("model") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)+ scale_color_viridis_c(end=0.9)


cv_plot <- backfits_df %>% 
  left_join(pretty_names) %>%
  filter(!is.na(pretty_par)) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  group_by(model,set,prop_misclass) %>%
  mutate(estimate_adj = ifelse(parameter == "mean_(Intercept)", estimate[parameter == 'duration_(Intercept)'], estimate)) %>% #CV not meaningful for start date b/c scale of mean arbitrary, so use duration as reference
  ungroup %>%
  group_by(model,parameter,prop_misclass) %>%
  summarise(mean_cv = mean(sd/estimate_adj),
            lci_cv = mean_cv - sd(sd/estimate_adj),
            uci_cv = mean_cv + sd(sd/estimate_adj),
            pretty_par = unique(pretty_par), data_type = unique(data_type), lumped = unique(lumped)) %>%
  ggplot(aes(x = prop_misclass, y = mean_cv, ymin = lci_cv, ymax = uci_cv, col = data_type, shape = lumped, lty = lumped)) +
  geom_pointrange(position = position_dodge(0.0075), linetype=1) +
  geom_line() +
  facet_wrap(~pretty_par, scales = 'free_y') +
  xlab("Proportion of misclassified non-moult records") +
  ylab("Posterior CV") +
  theme(legend.position = "bottom") + 
  scale_shape_manual(values=c(1,16)) +
  scale_linetype_manual(values = c(3,1))+
  #geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)+
  #scale_color_viridis_d(end=0.9) +
  scale_colour_manual(values=unname(palette.colors(n=3)))+
  theme_classic()
cv_plot
  
backfits_df %>%
  filter(prop_misclass < 0.5) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  left_join(pretty_names) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  ggplot(aes(x = prop_misclass, y = estimate, ymin = lci, ymax = uci, col = data_type, pch = lumped)) +
  geom_pointrange(position = position_dodge(0.01)) + facet_grid(data_type~parameter, scales = 'free_y') +
  xlab("Proportion of misclassified non-moult records") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), data = filter(reference_level, parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")), lty = 2) + theme_classic()
ggsave('figures/misclass_intercepts_results_raw.png')

bias_plot <- backfits_df %>%
  filter(prop_misclass < 0.5) %>%
  left_join(unconverged) %>% filter(converged == TRUE) %>%
  left_join(pretty_names) %>%
  left_join(reference_level) %>%
  filter(!is.na(pretty_par)) %>%
  group_by(model,parameter,prop_misclass) %>%
  summarise(mean_bias = mean((estimate-sim_value)/sim_value_adj),
            lci_bias = mean_bias - sd((estimate-sim_value)/sim_value_adj),
            uci_bias = mean_bias + sd((estimate-sim_value)/sim_value_adj),
            pretty_par = unique(pretty_par), data_type = unique(data_type), lumped = unique(lumped)) %>%
  ggplot(aes(x = prop_misclass, y = mean_bias, ymin = lci_bias, ymax = uci_bias, col = data_type, shape = lumped, lty = lumped)) +
  geom_pointrange(position = position_dodge(0.0075), linetype=1) +
  geom_line() +
  facet_wrap(~pretty_par, scales = 'free_y') +
  xlab("Proportion of misclassified non-moult records") +
  ylab("Relative bias")  + 
  #scale_color_discrete(name = '', labels = c('T1','T2','T3')) +
  scale_colour_manual(name = '', labels = c('T1','T2','T3'),values=unname(palette.colors(n=3)))+
  scale_shape_manual(values=c(1,16), labels = c('Standard model','Lumped model'), name = '') +
  scale_linetype_manual(values = c(3,1), labels = c('Standard model','Lumped model'), name ='')+
  theme_classic()+
  theme(legend.position = "bottom")
bias_plot
ggsave('figures/misclass_intercepts_bias.png')


cowplot::save_plot('figures/misclass_bias_precision.png',
cowplot::plot_grid(bias_plot + theme(legend.position = 'none'),
                   cv_plot+ theme(legend.position = 'none'),
                   cowplot::get_legend(bias_plot),
                   nrow = 3,
                   rel_heights = c(1,1,0.2),
                   align="v",
                   axis='l',
                   labels = c('A','B','')),
base_height = 5
)

                   
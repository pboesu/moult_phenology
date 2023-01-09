library(dplyr)
library(moultmcmc)
library(ggplot2)

backfits <- lapply(list.files('simulation_outputs/euring', pattern = 'integrated', full.names = T), readRDS)
names(backfits) <- stringr::str_extract(list.files('simulation_outputs/euring', pattern = 'integrated', full.names = T), '[0-9]+')

reference_level = tibble(parameter = c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)"),
                         sim_value = c(167, 75, 29))

pretty_names = tibble(parameter = c("mean_(Intercept)", "duration_(Intercept)", 
                                    "sd_(Intercept)"),
                      pretty_par = c("Start date", "Duration", "Start date std. deviation"))

# bind_rows(backfits) %>% group_by(model, parameter) %>% summarise(estimate = mean(estimate), lci = mean(lci), uci = mean(uci)) %>% filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
#   ungroup() %>%
#   left_join(pretty_names) %>%
#   ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = parameter)) + geom_pointrange() + facet_wrap(~pretty_par, scales = 'free_y') +
#   xlab("Proportion of birds with feather scores in active moult category") +
#   ylab("Posterior mean +/- 95% CI") +
#   theme_classic(base_size = 20, base_family = "Formata-CondensedLight") + theme(legend.position = "none") +
#   scale_colour_manual(values = c("#D54B0B","#A2559D", "#A6053F"))
# ggsave('figures/euring_poster_integration.png', dpi = 600, width = 12, height = 7)

#+ geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)
#+ 

bind_rows(backfits[1:18]) %>% group_by(model, parameter) %>% summarise(estimate = mean(estimate), lci = mean(lci), uci = mean(uci)) %>% 
  ungroup() %>%
  left_join(pretty_names) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = parameter)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') +
  xlab("Proportion of birds with feather scores in active moult category") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "none") + geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)

bind_rows(backfits) %>% 
  left_join(pretty_names) %>%
  ggplot(aes(x = model, y = estimate, ymin = lci, ymax = uci, col = parameter)) + geom_pointrange() + facet_wrap(~parameter, scales = 'free_y') +
  xlab("Proportion of birds with feather scores in active moult category") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "none") + geom_hline(aes(yintercept = sim_value), data = reference_level, lty = 2)

backfits_df <- bind_rows(backfits, .id = 'set') %>%
  tidyr::separate(col = model, into = c("model_type", "rej_rate"), sep = '_', remove = FALSE) %>%
  mutate(rej_rate = ifelse(is.na(rej_rate), ifelse(model_type == 'uz1', 100, 0), as.numeric(rej_rate))) %>%
  mutate(prop_scored = (100-rej_rate)/100)
  
backfits_df %>%
  left_join(pretty_names) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  ggplot(aes(x = prop_scored, y = estimate, ymin = lci, ymax = uci, col = model_type)) + geom_pointrange(position = position_dodge(0.15)) + facet_wrap(~parameter, scales = 'free_y') +
  xlab("Proportion of moult records with feather scores") +
  ylab("Posterior mean +/- 95% CI") +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value), data = filter(reference_level, parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")), lty = 2) + theme_classic()
ggsave('figures/euring_intercepts_results_raw.png')

backfits_df %>%
  left_join(pretty_names) %>%
  left_join(reference_level) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  group_by(pretty_par,model_type,prop_scored) %>%
  summarize(mean_estimate = mean(estimate), lci = quantile(estimate, prob=0.025), uci = quantile(estimate, prob=0.975), sim_value = unique(sim_value)) %>%
  ggplot(aes(x = prop_scored, y = mean_estimate, ymin = lci, ymax = uci, col = model_type)) + geom_pointrange(position = position_dodge(0.05)) + facet_wrap(~pretty_par, scales = 'free_y') +
  xlab("Proportion of moult records with feather scores") +
  ylab("Posterior mean +/- 95% CI") + theme_classic() +
  theme(legend.position = "bottom") + geom_hline(aes(yintercept = sim_value),  lty = 2) 

bias_plot <- backfits_df %>%
  left_join(pretty_names) %>%
  left_join(reference_level) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  mutate(sim_value_adj = case_when(parameter == 'mean_(Intercept)' ~ 75,
                                   TRUE ~ sim_value)) %>%
  group_by(pretty_par,model_type,prop_scored) %>%
  summarize(mean_bias= mean(estimate-sim_value)/sim_value_adj, lci = quantile((estimate-sim_value)/sim_value_adj, prob=0.025), uci = quantile((estimate-sim_value)/sim_value_adj, prob=0.975)) %>%
  ggplot(aes(x = prop_scored, y = mean_bias, ymin = lci, ymax = uci, col = model_type)) +
  geom_hline(aes(yintercept = 0),  lty = 2, col = 'grey') +
  geom_pointrange(position = position_dodge(0.075)) +
  facet_wrap(~pretty_par) +
  xlab("Proportion of moult records with continuous scores") +
  ylab("Relative bias") + theme_classic() +
  scale_color_manual(name = 'Model', labels = c('T1','T12','T2','T2S'),
                     values = unname(palette.colors(n=4))) + 
  theme(legend.position = "bottom")
bias_plot

cv_plot <- backfits_df %>%
  left_join(pretty_names) %>%
  left_join(reference_level) %>%
  filter(parameter %in% c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)")) %>%
  mutate(sim_value_adj = case_when(parameter == 'mean_(Intercept)' ~ 75,
                                   TRUE ~ sim_value)) %>%
  group_by(pretty_par,model_type,prop_scored) %>%
  summarize(mean_cv= mean(sd/sim_value_adj), lci = quantile(sd/sim_value_adj, prob=0.025), uci = quantile(sd/sim_value_adj, prob=0.975)) %>%
  ggplot(aes(x = prop_scored, y = mean_cv, ymin = lci, ymax = uci, col = model_type)) + geom_pointrange(position = position_dodge(0.075)) + facet_wrap(~pretty_par) +
  xlab("Proportion of moult records with continuous scores") +
  ylab("CV") +
  theme_classic() +
  scale_color_manual(name = 'Model', labels = c('T1','T12','T2','T2S'),
                     values = unname(palette.colors(n=4))) + 
  theme(legend.position = "bottom")
cv_plot

cowplot::save_plot('figures/t12_data_integration_bias_precision.png',
                   cowplot::plot_grid(bias_plot + theme(legend.position = 'none'),
                                      cv_plot+ theme(legend.position = 'none'),
                                      cowplot::get_legend(bias_plot),
                                      nrow = 3,
                                      rel_heights = c(1,1,0.2),
                                      labels = c('A','B',''),
                                      axis = 'l',
                                      align = 'v'),
                   base_height = 5, base_width = 5
)

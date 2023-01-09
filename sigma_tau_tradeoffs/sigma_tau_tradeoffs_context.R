#sigma tau tradeoff context

uk_birds <- readr::read_csv('data/morrison_reanalysis_table2_neat.csv') %>%
  tidyr::separate(mean_start, c('mean_start', 'mean_start_se'), sep = "[ ()]+") %>%
  tidyr::separate(mean_duration, c('mean_duration', 'mean_duration_se'), sep = "[ ()]+") %>%
  tidyr::separate(mean_sd_start, c('mean_sd_start', 'mean_sd_start_se'), sep = "[ ()]+") %>%
  mutate(across(starts_with("mean_"), as.numeric))

readr::write_csv(uk_birds, 'data/morrison_reanalysis_table2_machine_readable.csv')

sigma_tau_ratio = expand.grid(mean_sd_start = seq(8,20,by =0.5), mean_duration = seq(35,90,by=1)) %>%
  mutate(ratio = mean_sd_start/mean_duration)

ggplot(uk_birds, aes(x = mean_duration, y = mean_sd_start)) + geom_tile(data = sigma_tau_ratio, aes(fill = ratio)) + geom_point() +  scale_fill_viridis_c()
ggplot(uk_birds, aes(x =mean_sd_start/mean_duration)) + geom_histogram(binwidth = 0.05) + geom_rug()

ggplot(uk_birds, aes(y = mean_duration, x = mean_sd_start/mean_duration)) + geom_point()

#quick look at mammal data
library(readxl)
library(dplyr)
library(ggplot2)
library(moultmcmc)
#set colourblind safe defaults for ggplot
options(ggplot2.discrete.colour= unname(palette.colors(n = 8, "Okabe-Ito")))
options(ggplot2.discrete.fill= unname(palette.colors(n = 8, "Okabe-Ito")))

#fox data is deposited on https://doi.org/10.5061/dryad.djh9w0w3n under a CC0 license
fox <- read_xlsx('data/mammals/fox/fox_molts_data_for_Dryad.xlsx') %>%
  mutate(yday = lubridate::yday(date))

ggplot(fox, aes(x = yday, y = molt_cat)) + geom_point() + geom_vline(xintercept = 221)

ggplot(fox, aes(x = yday, y = molt)) + geom_point() + geom_vline(xintercept = 221)
ggplot(fox, aes(x = yday, y = snow)) + geom_point() + geom_vline(xintercept = 221)

#split into spring/autumn and recode
spring <- filter(fox, yday < 221) %>%
  mutate(molt_fac = factor(case_when(molt_cat == 2 ~ 'winter',
                               molt_cat == 3 ~ 'moult',
                               molt_cat == 1 ~ 'summer'),
                            levels = c('winter','moult','summer')),
         molt_fac_num = as.numeric(molt_fac),
         ind_year = factor(paste0(individual,'_',year)))
#saveRDS()

ggplot(spring, aes(x=yday, y = molt_fac, col = year)) + geom_point(position = position_jitter(width = 0, height=0.1)) + facet_wrap(~individual)

table(spring$individual,spring$year)

t1_spring <- moultmcmc(moult_column = 'molt_fac_num',
          date_column = 'yday',
          type = 1,
          data = spring,
          chains = 2, cores = 2)         
moult_plot(t1_spring)

spring %>% group_by(ind_year) %>% summarise(ind_recap = n()) %>% summary()

t1r_spring <- moultmcmc(moult_column = 'molt_fac_num',
                       date_column = 'yday',
                       id_column = 'ind_year',
                       type = 1,
                       data = spring,
                       chains = 2, cores = 2)   
compare_plot(t1_spring,t1r_spring, names = c('T1','T1R')) + theme_classic()
ggsave('figures/Arctic_fox_T1R_T1_param_comp.png', width = 7, height = 4)

#compairs plot to assess if corr between duration and start intercept is reduced
png(filename = 'figures/Arctic_fox_T1R_T1_compairs.png',
    width = 800, height = 800, pointsize= 24)
moultmcmcExtra::compairs_plot(t1_spring, t1r_spring, pars = c("mean_(Intercept)", "duration_(Intercept)", "sd_(Intercept)"), names = c("Std. Model (T1)","Recap. Model (T1R)"), col = palette.colors(n = 2, "Okabe-Ito"))
dev.off()

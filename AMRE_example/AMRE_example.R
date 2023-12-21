# Worked example - fitting molt models to American Redstart data

# The following code walks through fitting Bayesian molt models to real-world observations from American Redstart (Mumme et al. 2021).
# The example assumes that the data have been downloaded from the corresponding Dryad repository (https://datadryad.org/stash/dataset/doi:10.5061/dryad.63xsj3v0x) and extracted to a suitable directory.
# The data are then read in and the primary scores are converted to percentage feather mass grown (PFMG) using generic feather mass indices from Bonnevie 2010b.


library(dplyr)
library(ggplot2)
library(moult)
library(moultmcmc)

#generic feather mass indices for a passerine with nine primaries (Bonnevie 2010b)
fmi_passer10 <- c(8.0, 8.6, 9.2, 10.0, 11.4, 12.4, 13.0, 13.5, 14.0) 

#read AMRE data
# replace path with relevant path to the downloaded data
amre_all <- readr::read_csv('../data/digitized/Mumme2021/AllRawData.csv') %>% 
  filter(Species == 'AMRE') %>% # discard all other species
  tidyr::unite(col = 'moult_str', P1:P9, sep = '', remove = F) %>% # collate primary scores
  mutate(pfmg = moult::ms2pfmg(moult_str, fmi_passer10), # convert to PFMG
         ring_yr = as.factor(paste(Suffix, YEAR, sep = "_"))) # create identifier for within-year recaptures of individuals


#We can then visualise the data, highlighting the undersampling of birds at intermediate molt stages.


amre_all %>% ggplot(aes(x = pfmg)) +
  geom_histogram(binwidth = 0.1) +
  theme_classic() +
  ylab('Number of records') +
  xlab('PFMG')


#We can also visulaise the seasonal progression of molt progress and highlight the recaptures.


amre_all %>% ggplot(aes(x = Julian, y = pfmg, group = ring_yr)) +
  geom_line(alpha=0.3) +
  geom_point(alpha = 0.3) +
  theme_classic()+
  xlab('Days since Jan 01') +
  ylab('PFMG')

# We then fit both a standard type 3 Underhill-Zucchini molt phenology model, and an extended type 3 recaptures model to demonstrate how both models perform in the face of the molt-dependent sampling bias. 
# The standard T3 model fits in a few seconds, therecaptures model is computationally much more intensive and takes approximately 5-10 minutes to fit.

amre_t3 <- moultmcmc(moult_column = 'pfmg',
                      date_column = 'Julian',
                      data = amre_all,
                      type = 3,
                     iter = 1000,
                     chains = 2,
                     cores = 2)# The standard model fits in c. 5 seconds
amre_t3r <- moultmcmc(moult_column = 'pfmg',
                      date_column = 'Julian',
                 id_column = "ring_yr",
                      data = amre_all,
                      type = 3,
                 iter = 1000,
                 chains = 2,
                 cores = 2)#the recaptures model fits in c. 6 minutes


# We can then plot the model fits.

moult_plot(amre_t3) + ggtitle('T3 model')
moult_plot(amre_t3r) + ggtitle('T3R model')

# The model plots provide a relatively straightforward visual check of biased estimates. Poor coverage of the observations by the 95% molt interval shows that the T3 model fits the data poorly, whereas the molt interval for the T3R fit encompasses the observations well.

# Additional usage examples for the moultmcmc package can be found in the package vignettes at https://pboesu.r-universe.dev/moultmcmc.
# Additional case studies with real-world data and code examples can be found in Boersch-Supan et al. 2023.


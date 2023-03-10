sample_one_year <- function(pop_start = 150,
                            pop_duration = 65,
                            pop_sd = 10,
                            duration_sd = 1,
                            n_individuals = 1000,
                            n_occasions = 104, 
                            sample_per_occ = 5,
                            rel_p_moult = 0.2,
                            rel_p_new = 1,
                            rel_p_old = 1,
                            sampling_dates = sample(365, size = n_occasions),
                            removal_sampling = FALSE,
                            verbose=FALSE
){
  if(removal_sampling == TRUE){
    stopifnot(n_individuals >= n_occasions*sample_per_occ)
  }
  individuals = data.frame(ID = factor(formatC(seq_len(n_individuals), width = 6, format = "d", flag = "0")),
                           start_date = rnorm(n_individuals, mean = pop_start, sd = pop_sd),
                           duration = rnorm(n_individuals, mean = pop_duration, sd = duration_sd),
                           p_det = 1)
  
  
  lookup <- moultmcmcExtra::siskin_capture
  data_out = list()
  
  for (i in seq_along(sampling_dates)){
    day = sampling_dates[i]
    
    #calculate moult status
    individuals$pfmg <- case_when(
      day <= individuals$start_date ~ 0,
      day >= individuals$start_date + individuals$duration ~ 1,
      TRUE ~ (day - individuals$start_date)/individuals$duration)
    #update detection propbs
    if(is.numeric(rel_p_moult)){
      if(between(rel_p_moult, 0, 1)){
    individuals$p_det <- case_when(
      individuals$p_det == 0 ~ 0,#removed bird stays removed
      individuals$pfmg == 0 ~ rel_p_old,
      individuals$pfmg == 1 ~ rel_p_new,
      TRUE ~ rel_p_moult
    )
    sample_size_weight =  1
    } else {stop('rel_p_moult out of range')}
      } else {
      if(rel_p_moult == "siskin") {
        #load moultmcmcExtra dataset, use fuzzyjoin to set pdet if not already 0, continue as before
        
        individuals <- fuzzyjoin::difference_left_join(individuals, lookup, max_dist = 1/1000, distance_col = 'matching_diff') %>% #this is really slow!
          group_by(ID) %>%
          slice_min(matching_diff, n=1, with_ties = FALSE) %>%
          rename(pfmg = 'pfmg.x') %>%
          mutate(p_det = case_when(
            p_det == 0 ~ 0, 
            pfmg == 0 ~ rel_p_old,
            pfmg == 1 ~ rel_p_new,
            TRUE ~ p_cap)) %>%
          select(-pfmg.y, -matching_diff, -p_cap)#remove joined columns
        #individuals
        sample_size_weight = sum(individuals$p_det)/nrow(individuals)
      } else {
        stop('undefined sampling bias scheme')
      }}
    if(verbose) print(ceiling(sample_per_occ*sample_size_weight))
    individuals_captured <- sample(individuals$ID, size = ceiling(sample_per_occ*sample_size_weight), prob = individuals$p_det)
    data_out[[i]] <- filter(individuals, ID %in% individuals_captured) %>% 
      mutate(yday = day,
             p_moult = rel_p_moult)
    if (removal_sampling == TRUE) individuals$p_det[individuals$ID %in% individuals_captured] <- 0
  }
  
  bind_rows(data_out) %>% 
    mutate(pop_start = pop_start,
           pop_duration = pop_duration,
           pop_sd = pop_sd,
           duration_sd = duration_sd) -> annual_sample_df
  return(annual_sample_df)
}


library(rethinking);library(tidyverse); library(furrr)

bayes_bd = function(df, all_segs, n_cores = 4, variable = 'under_lim', multi_level = FALSE, mu_sig = 10, sig_sig = 0.5){
  ##Constraining data to above 0 and below speed limit. Under current Yulo processing there should be no 0 speeds
  df = filter(df,
             !is.na(imp_speed),
             imp_speed >0
    ) %>%
    mutate(speed_limit = ifelse(is.na(speed_limit), 60, speed_limit))



  df = mutate(df,
             under_lim = speed_limit - imp_speed,
             under_lim = ifelse(under_lim<0, 0, under_lim)
  ) 

  ##By default the Bayesian analysis estimates a distribution of the difference between the speed limit and imputed speed
  ##This can produce erroneous results in some contexts, especially when speed limits vary along the route and thus in the prior
  
  ##making b_var the variable (under_lim or imputed speed) as stipulated in row
  df = rename(df, b_var = !!variable)
  #Creating prior distibution for hour
  hr_means_raw = df %>%
    #filter(under_lim >= 0 ) %>% 
    group_by(hour) %>% 
    summarise(
      mu_raw = mean(b_var),
      sd_raw = sd(b_var),
      n_obvs = n()
    ) %>% ungroup() %>% arrange(hour) %>%
    mutate(
      sd_raw = ifelse(is.na(sd_raw), median(sd_raw, na.rm = T), sd_raw) ##rare cases where only one observation available
      )
  if(anyNA(hr_means_raw$sd_raw)){
    print(filter(hr_means_raw, is.na(sd_raw)))
    stop()
  }
  limits = select(df, speed_limit, osm_id) %>% unique()
  
  ##applying Bayesian model. The multilevel model is applied to each segment. The seg_hr model is applied to each hour and each segment
  if(multi_level){
    post = purrr::map(all_segs, function(id){
      ml_model_func(id, df, hr_means_raw, mu_sig, sig_sig)
      }) %>%
      bind_rows() %>% unique()
    }else{
    post = purrr::map(unique(df$hour), function(h){
      seg_hr_model(h, df, all_segs, hr_means_raw, mu_sig, sig_sig)
      }) %>% 
      bind_rows() %>% unique()
  }
  ##Constraining the posterior sample to speedlimits and returning it
  post = left_join(post, limits, by = 'osm_id') %>%
    mutate(
      speed_limit = replace_na(speed_limit, median(speed_limit, na.rm = T))
     )
  return(post)
}




seg_hr_model = function(h, df, all_segs, hr_means_raw, mu_sig = 10, sig_sig = 0.5, catch_error=TRUE){
    post = purrr::map(unique(all_segs), function(id){
      
      dat = filter(df, hour == h, osm_id == id) %>% 
        select(b_var, hour)
      dat_list = list(
        b_var = dat$b_var,
        mu_raw    = hr_means_raw$mu_raw[hr_means_raw$hour == h],
        sd_raw    = hr_means_raw$sd_raw[hr_means_raw$hour == h],
        mu_sig = mu_sig,
        sig_sig = sig_sig
      )
      n_obvs = length(dat_list$b_var)
      ##data to return in case of model failure or missing data
      fallback = tibble(
                     'osm_id' = id,
                     'mean' = dat_list$mu_raw,
                     'sd' = dat_list$sd_raw,
                     'hour' = h,
                     'n_obvs' = n_obvs
        )
      
      
      if(n_obvs > 1){
        err = try({
          model = quap(
          alist(
            b_var ~ dnorm(mu, sigma),
            mu ~ dnorm(mu_raw, mu_sig),
            sigma ~ dnorm(sd_raw, sig_sig)
          ), data = dat_list
        )
        })
        ##Sometimes model will failure to converge. In this instance we assume the prior
        if(class(err) == "try-error" & catch_error){
          cfs = fallback
        }else{
          
          cfs = precis(model, depth= 2) 
          cfs = tibble(
                       'osm_id' = id,
                       'mean' = cfs$mean[1],
                       'sd' = cfs$mean[2],
                       'hour' = h,
                       'n_obvs' = n_obvs
          )
        }
      }else{
        ##returning prior as posterior where there is no data
        cfs = fallback
        
      }
      #print(cfs)
      return(cfs)

    }) %>% bind_rows()
    return(post)
}

ml_model_func = function(id, df, hr_means_raw, mu_sig = 40, sig_sig = 20, catch_error= TRUE){
  
      print(sprintf("starting seg %s with mu_sig: %s and sig_sig: %s", id, mu_sig, sig_sig))
      dat = filter(df, osm_id == id) %>% 
        select(b_var, hour)
      
      
      dat_list = list(
        b_var     = dat$b_var,
        mu_raw    = hr_means_raw$mu_raw,
        sd_raw    = hr_means_raw$sd_raw,
        hr = dat$hour + 1,
        mu_sig = mu_sig,
        sig_sig = sig_sig
      )
      
      n_obvs = length(dat_list$b_var)
      ##data to return in case of model failure or missing data
      fallback = tibble(
        'osm_id' = id,
        'mean' = dat_list$mu_raw,
        'sd' = dat_list$sd_raw,
        'hour' = hr_means_raw$hour,
        'n_obvs' = hr_means_raw$n_obvs
      )
      
      if(n_obvs > 1){
        err = try({
          model = quap(
            alist(
              b_var ~ dnorm(mu, sigma),
              mu[hr] ~ dnorm(mu_raw, mu_sig),
              sigma[hr] ~ dnorm(sd_raw, sig_sig)
            ), data = dat_list
          )
        }
        )
        ##Sometimes model will failure to converge. In this instance we assume the prior
        if(class(err) == 'try-error' & catch_error){
          cfs = fallback
        }else{
          cfs = precis(model, depth= 2) 
          cfs = tibble(
                       'osm_id' = id,
                       'mean' = cfs$mean[1:24],
                       'sd' = cfs$mean[25:48],
                       'hour' = hr_means_raw$hour,
                       'n_obvs' = hr_means_raw$n_obvs
          )
        }
      }else{
        cfs = fallback
      }
      #print(cfs)
      return(cfs)

}


## This estiamtes the median and interquartile range by drawing from the posterior distribution
gen_iq_bayes = function(df, under_lim = T){
  int_q = lapply(1:nrow(df), function(i){
    
    samp = rnorm(1000, df$mean[i], df$sd[i])
    samp = ifelse(samp<0, 0, samp)
    samp = ifelse(samp > df$speed_limit[i], df$speed_limit[i] -1, samp)
    sl =  df$speed_limit[i]
    
    UQ_ = quantile(samp, 0.25) %>% ifelse(.>sl, sl, .)
    med_ = quantile(samp, 0.5) %>% ifelse(.>sl, sl, .)
    LQ_ = quantile(samp, 0.75) %>% ifelse(.>sl, sl, .)
    if(under_lim){
      out = tibble(
        UQ = sl - UQ_, 
        median = sl - med_,
        LQ = sl - LQ_
      )
    }else{
      out = tibble(
        UQ =  LQ_, 
        median = med_,
        LQ =  UQ_
      )
    }
    return(out)
  }) %>% bind_rows() %>%
    cbind(df, .)
  return(int_q)
}


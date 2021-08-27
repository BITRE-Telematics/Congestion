

stanserver_bd = function(df, all_segs, variable = 'under_lim', mu_sig = 10, sig_sig = 2, multi_level = F){
  
    
    server = "http://localhost:5365/stanserver"
  
  
  
  
  
  df = filter(df,
              !is.na(imp_speed),
              imp_speed >0
  ) %>%
    mutate(speed_limit = ifelse(is.na(speed_limit), 60, speed_limit))
  
  
  
  df = mutate(df,
              under_lim = speed_limit - imp_speed,
              under_lim = ifelse(under_lim<0, 0, under_lim)
  ) 
  df = rename(df, b_var = !!variable)
  
  hr_means_raw = df %>%
    
    group_by(hour) %>% 
    summarise(
      mu_raw = mean(b_var),
      sd_raw = sd(b_var),

      var = var(b_var),
      n_obvs = n()
    ) %>% ungroup() %>%arrange(hour)
  
  
  
  limits = select(df, speed_limit, osm_id) %>%unique()
  
  
 
  if(!(multi_level)){
  post = lapply(unique(df$hour), function(h){
    lapply(unique(all_segs), function(id){
      print(sprintf("starting seg %s at bd %s", id, h))
      dat = filter(df, hour == h, osm_id == id) %>% 
        select(b_var, hour)
     
      dat_list = list(
        b_var = dat$b_var,
        mu_raw    = hr_means_raw$mu_raw[hr_means_raw$hour == h],
        sd_raw    = hr_means_raw$sd_raw[hr_means_raw$hour == h],
        mu_sig = mu_sig,
        sig_sig = sig_sig,
        N = length(dat$b_var)
      )
      file = sprintf("%s_%s", h, id)
      jsonlite::toJSON(dat_list, auto_unbox = T) %>% readr::write_lines(file)
      if(dat_list$N > 1){
        post = httr::POST(server, body = list(myFile = httr::upload_file(file)), httr::add_headers(model = "segmeans")) %>% content()
        
        cfs = tibble(
          'osm_id' = id,
          'mean' = post$Mu_post,
          'sd' = post$Sig_post,
          'hour' = h,
          'n_obvs' = dat_list$N
        )
      }else{
        cfs = tibble(
          'osm_id' = id,
          'mean' = dat_list$mu_raw,
          'sd' = dat_list$sd_raw,
          'hour' = h,
          'n_obvs' = dat_list$N
        )
      }
      print(cfs)
      file.remove(file)
      return(cfs)
      
    }) %>% bind_rows()
  }) %>% bind_rows() %>% unique()
  }else{
    post =  lapply(unique(all_segs), function(id){
        print(sprintf("starting seg %s", id))
        dat = filter(df, osm_id == id) %>% 
          select(b_var, hour)
        
        dat_list = list(
          b_var = dat$b_var,
          hr = dat$hour +1,
          mu_raw    = hr_means_raw$mu_raw,
          sd_raw    = hr_means_raw$sd_raw,
          mu_sig = mu_sig,
          sig_sig = sig_sig,
          N = length(dat$b_var),
          N_hr = 24
         
        )
        ##stan requires at least one observation for each index, this is a failsafe
        map(1:24, function(h){
          if(!h %in% dat_list$hr){
            dat_list$b_var = c(dat_list$b_var, dat_list$mu_raw[h])
            dat_list$hr = c(dat_list$hr, h)
          }
        })
        
        
        file = sprintf("%s_%s", h, id)
        jsonlite::toJSON(dat_list, auto_unbox = T) %>% readr::write_lines(file)
        if(N > 1){
          post = httr::POST(server, body = list(myFile = httr::upload_file(file)), httr::add_headers(model = "segmeans_ml")) %>% content()
          
          cfs = tibble(
            'osm_id' = id,
            'mean' = unlist(post$Mu_post),
            'sd' = unlist(post$Sig_post),
            'hour' = hr_means_raw$hour,
            'n_obvs' = hr_means_raw$n_obvs
          )
        }else{
          cfs = tibble(
            'osm_id' = id,
            'mean' = dat_list$mu_raw,
            'sd' = dat_list$sd_raw,
            'hour' = h,
            'n_obvs' = hr_means_raw$n_obvs
          )
        }
        print(cfs)
        file.remove(file)
        return(cfs)
        
      }) %>% bind_rows()
   
  }
  
  
  
  post = left_join(post, limits, by = 'osm_id') %>%
    mutate(
      speed_limit = replace_na(speed_limit, median(speed_limit, na.rm = T))
    )
  return(post)
  }




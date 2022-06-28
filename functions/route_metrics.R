library(tidyverse); library(ggrepel); library(hms)

 

metrics = list.files("./times", full.name = T, pattern = "_times.csv") %>%
  purrr::map(function(f){
    d = read_csv(f, col_types = cols(
      all = col_logical()
     
    ))
    all_full = all(d$all)
    d = filter(d, !is.na(med_est)) %>%
      mutate(
        iq_r = LQ_est - UQ_est  ##UQ_est is estimated travel time at upper quartile of speed, not upper quartile of travel times
      )
    out = tibble(
      `route_name` = f %>% str_remove_all("./times/") %>% str_remove_all('_times.csv'),
      `all times` = all_full,
      `min median travel time` = min(d$med_est),
      `min median travel hour` = d$hour[which.min(d$med_est)],
      `max median travel time` = max(d$med_est),
      `max median travel hour` = d$hour[which.max(d$med_est)],
      `min iq travel time`     = min(d$iq_r),
      `min iq travel hour`     = d$hour[which.min(d$iq_r)],
      `max iq travel time`     = max(d$iq_r),
      `max iq travel hour`     = d$hour[which.max(d$iq_r)],
      `total obvs`             =  sum(d$n_obvs, na.rm = T)
      
    ) %>%
      mutate(
        `Peak time to best ratio` = `max median travel time`/`min median travel time`,
        `Mean excess time ratio`  = mean(d$med_est/`min median travel time`),
        `Mean excess time ratio - weighted`  = weighted.mean(d$med_est/`min median travel time`, d$n_obvs),
        `Peak iq to best ratio`   = `max iq travel time`/`min iq travel time`,
        `Mean excess iq ratio`    = mean(d$iq_r/`min iq travel time`),
        `Mean excess iq ratio - weighted`    = weighted.mean(d$iq_r/`min iq travel time`, d$n_obvs),
        `Peak time to best ratio` = round(`Peak time to best ratio`, 3),
        `Peak iq to best ratio`   = round(`Peak iq to best ratio`, 3), 
        `Mean excess time ratio`  = round(`Mean excess time ratio`, 3),
        `Mean excess iq ratio`    = round(`Mean excess iq ratio`, 3),
        `Mean excess time ratio - weighted`  = round(`Mean excess time ratio - weighted`, 3),
        `Mean excess iq ratio - weighted`    = round(`Mean excess iq ratio - weighted`, 3),
        `max median travel time`  = as_hms(`max median travel time`) %>% round_hms(1),
        `min median travel time`  = as_hms(`min median travel time`) %>% round_hms(1),
        `max iq travel time`      = as_hms(`max iq travel time`) %>% round_hms(1),
        `min iq travel time`      = as_hms(`min iq travel time`) %>% round_hms(1)
      ) %>%
      relocate(`route_name`,
               `min median travel time` ,
               `max median travel time` ,
               `Peak time to best ratio` ,
               `min iq travel time` ,
               `max iq travel time` ,
               `Peak iq to best ratio` 
      )
    
    f_bw = f %>% str_replace_all('times', 'bw') 
    
    
    if(nrow(out) == 0){
      print(sprintf("Check %s - no rows", f))
    }
    return(out)
  }
  ) %>%bind_rows()


seg_dat = list.files("./bw", full.name = T, pattern = "_bw.csv") %>%
  purrr::map(function(f){
    read_csv(f, col_types = cols(osm_id = col_character())) %>%
      select(osm_id, distance) %>%
      mutate(`route_name` = f %>% str_remove_all('./bw/') %>% str_remove_all('_bw.csv'))
  }
  ) %>% bind_rows() %>% unique()





route_dists = seg_dat %>% 
  select(route_name, distance, osm_id) %>%
  unique() %>%
  group_by(`route_name`) %>%
  summarise(distance =sum(distance, na.rm =T))


metrics = left_join(metrics, route_dists, by = 'route_name', copy = TRUE)


route_geoms = read_csv("route_maps/geometries.csv", col_names = c('route_name', 'route_geom')) %>%
  distinct(route_name, .keep_all = T) %>%
  filter(route_name %in% metrics$route_name)
write_csv(route_geoms, "route_maps/geometries.csv")

###writing csv outputs
write_csv(metrics, "metrics/route_metrics.csv")

write_csv(seg_dat, "metrics/segment_summary.csv")

breakdown_table = list.files("./times", full.name = T, pattern = '_times.csv') %>%
  purrr::map(function(f){
    df = read_csv(f) %>%
      mutate(route_name = str_remove(f, "_times.csv") %>% str_remove('./times/')) %>%
      select(-n_hour_ids, -all, -bd, -bd_label)
  }) %>%bind_rows()
write_csv(breakdown_table, "metrics/route_times.csv")

##Bi direction graphs

route_parameters = read_csv("route_parameters.csv")
breakdown_table = breakdown_table %>% 
  left_join(route_parameters, by = "route_name") %>%
  left_join(route_geoms, by = "route_name") %>%
  mutate(
    short_name = sprintf(
      "%s_%s",
      str_split(route_name, ' - ', simplify = T)[,1],
      city
      )
  )

walk(unique(breakdown_table$short_name), function(n){
  bidirectional_plot(n, breakdown_table)
  }
)

##Bidirectional maps
walk(unique(breakdown_table$short_name), function(n){
  print(n)
  try({
    route_geom_list = filter(breakdown_table, short_name == n)$route_geom %>% unique()
    bidirectional_map(n, route_geom_list, mb_token, map_style, dpi = 150)
  })
})

###City maps


map_data = left_join(metrics, route_parameters, by = c("route_name" ='route_name')) %>%
  left_join(route_geoms, by = "route_name") %>%
  mutate(route_geom = st_as_sfc(route_geom))

walk(unique(route_parameters$city), function(m){
  plot_city(m, map_data)
  }
)

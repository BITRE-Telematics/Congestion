
##designed to be iterated through row wise with a data frame read from route_parameters.csv, such that "row" is a single row dataframe
route_func = function(
    row,
    year,
    url_format = "http://127.0.0.1:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson&overview=full",
    map_style = "mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt",
    n_cores = 5,
    dpi = 150,
    month_breakdown = F
    ){
  
  
  route_name = row$route_name
  start = row$start 
  finish= row$finish 
  headers = names(row)
  headers %<-% unlist(row)
  
  middle= row$middle %>% str_replace_all("\\|", ";") %>%str_remove_all(" ")
  
  
  #Getting route between points from OSRM
  route = getURL(
    sprintf(url_format, start, middle ,finish) %>% str_remove_all("NA;") %>% str_remove_all(" ")
                 ) %>% 
    jsonlite::fromJSON()
  
  #make map of route for displau
  route_geom = extract_geom(route)
  st_crs(route_geom) = 4269
  
  
  route_map = display_map(route_name, route_geom, mb_token, map_style)
  ggsave(plot = route_map, file = sprintf('congestions/route_maps/display_maps/%s_route.PNG', route_name),
         width = 20, height = 20, units = "cm", dpi = dpi)
  ##Saving the route geomteries for later city maps
  write_csv(tibble('route_name' = route_name, geom = st_as_text(route_geom)), "route_maps/geometries.csv", append = T)
  
  ##Extracting segment ids and distances
  ##the group_by call sums together distances where the routing engine has returned split distances for the purpose of verbal directions
  
  segs = route$routes$legs[[1]]$steps %>% bind_rows() %>%
    group_by(name) %>%
    summarise(distance  = sum(distance, na.rm =T))
    
 
  ##Pulling data from the database. Since this takes time it checks if data is already on the disk for a given segment.
  data_file = sprintf('data/%s_data.csv.gz', route_name)
  if(!file.exists(data_file)){
  cl = makeCluster(n_cores, type = "FORK")
  d = parLapply(cl, segs$name, function(osm_id){
    get_dat(osm_id, year = year)
    }) %>% bind_rows()
  stopCluster(cl)
  write_csv(
    d, 
    gzfile(data_file)
      )
  
  }else{
   d = read_csv(data_file, col_types = cols('osm_id' = col_character(), 'speed_limit' = col_double()))
   d = filter(d, osm_id %in% segs$name)
   new_segs = segs$name[!(segs$name %in% d$osm_id)]
   cl = makeCluster(n_cores, type = 'FORK')
   d2 = parLapply(cl, new_segs, function(osm_id){
     get_dat(osm_id, year = year)
   }) %>% bind_rows()
   stopCluster(cl)
   d = bind_rows(list(d, d2))
   write_csv(
     d, 
     gzfile(data_file)
   )
   rm(d2)
   }
  

  d = filter(d, !is.na(imp_speed))
  
  ##filtering low speeds if required
  if(!is.na(row$min_speed)){
    d = filter(d, imp_speed > row$min_speed)
  }
  
  ##filtering direction
  if(!is.na(row$dir)){
    d = filter_dir(d, row$dir)
  }
  
  ##check for na speedlimits and replacing with assumed values. The analysis assumes vehicles always obey speed limits
  
  na_sl = filter(d, is.na(speed_limit))
  unique(na_sl$osm_id)
  unique(na_sl$class)
  d = replace_na_limits(d)
  
  ## deriving median and interquartile ranges for each segment by hour using the methods stipulated in the parameters
  sum_sp = summarise_speed(d, row, segs, min_speed = row$min_speed)
  all_segs = length(unique(segs$name)) == length(unique(sum_sp$osm_id))
  sum_sp = sum_sp %>%
    full_join(segs, by = c('osm_id' = 'name'))
  
  if(!row$bayes){
    sum_sp = impute_missing(sum_sp)
  }
  
  write_csv(sum_sp, sprintf('bw/%s_bw.csv', route_name))
  
  ##Using the derived median and interquartile ranges for segments to estimate travel times
  sum_times =sum_route(sum_sp, all_segs, route_name) %>%
    mutate(bd = hour, bd_label = paste0(hour, ":00"),
           bd_label = ifelse(nchar(bd_label) == 4, paste0(0, bd_label), bd_label))

  write_csv(sum_times, sprintf("times/%s_times.csv", route_name))
  
  ##Plotting times
  times_plot = plot_times(df = sum_times, name = route_name)
  
  ggsave(plot = times_plot, filename = sprintf('graphs/%s_times.PNG', route_name),
         width  = 140, height = 80, unit  = "mm")
  

  ##Producing map and gif breakdowns of the route to aid report writing
  
   
  
  route_sf = filter(roads, osm_id %in% segs$name)
  #print(unique(route_sf$name))
  
  
  
  sp_df = left_join(sum_sp, route_sf, by = 'osm_id')%>%
    mutate(bd = hour, bd_label = paste0(hour, ":00"),
           bd_label = ifelse(nchar(bd_label) == 4, paste0(0, bd_label), bd_label))
  
  
  map_route(df = sp_df, name = sprintf("%s.PNG",route_name), dir = "route_maps/breakdown_maps")
}
##applies the desired method for each segment
summarise_speed = function(d, row, segs, min_speed = 5){
  if(row$bayes){
    multi_level = if_else(is.null(row$multi_level), F, row$multi_level)
    variable = ifelse(row$bayes_raw_speeds, "imp_speed", "under_lim")
    if(require(rethinking)){
      sum_sp = bayes_bd(df = d, all_segs = segs$name, variable = variable, multi_level = multi_level) %>%
      gen_iq_bayes(under_lim = !row$bayes_raw_speeds) ## draws from posterior distribution to derive interquartile range and median
    }else{
      ##local solution to allow machines that don't have stan installed to run model remotely
      sum_sp = stanserver_bd(df = d, all_segs = segs$name, variable = variable) %>%
       gen_iq_bayes(under_lim = !row$bayes_raw_speeds) 
    }
  }else{
    ##raw median and interquartile options
    sum_sp = extract_range(d)
  }
  if(row$adjust_low){
    sum_sp = adjust_low(sum_sp)
  }
  if(!is.na(row$min_speed)){
    sum_sp = sum_sp %>% mutate(
      LQ = ifelse(LQ<row$min_speed, row$min_speed, LQ)
    )
  }

 return(sum_sp)
}


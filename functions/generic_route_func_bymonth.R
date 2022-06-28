
##designed to be iterated through row wise with a data frame read from route_parameters.csv, such that "row" is a single row dataframe
##It replicates generic_route_func.R but also interates by month, to capture trends over the year
route_func_by_month = function(
    r,
    year,
    url_format = "http://127.0.0.1:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson&overview=full",
    map_style = "mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt",
    n_cores = 5,
    dpi = 150
    ){
  
  
  route_name = r$route_name
  start = r$start 
  finish= r$finish 
  headers = names(row)
  headers %<-% unlist(row)
  
  middle= r$middle %>% str_replace("\\|", ";")
  
  
  route_name = r$route_name
  start = r$start 
  finish= r$finish 
  headers = names(row)
  headers %<-% unlist(row)
  
  middle= r$middle %>% str_replace_all("\\|", ";") %>%str_remove_all(" ")
  
  
  #Getting route between points from OSRM
  route = getURL(
    sprintf(url_format, start, middle ,finish) %>% str_remove_all("NA;") %>% str_remove_all(" ")
  ) %>% 
    jsonlite::fromJSON()
  
  
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
  if(!is.na(r$min_speed)){
    d = filter(d, imp_speed > r$min_speed)
  }
  ##filtering direction
  if(!is.na(r$dir)){
    d = filter_dir(d, r$dir)
  }
  ##check for na speedlimits and replacing with assumed values. The analysis assumes vehicles always obey speed limits
  
  na_sl = filter(d, is.na(speed_limit))
  unique(na_sl$osm_id)
  unique(na_sl$class)
  d = replace_na_limits(d)
  d$month = lubridate::as_datetime(d$datetime) %>% lubridate::month()
  
  
  sum_sp = purrr::map(unique(1:12), function(m){
    out = filter(d, month  == m) %>% 
      summarise_speed(r, segs) %>% 
      left_join(segs, by = c('osm_id' = 'name')) %>%
      mutate(
        month = m,
        all_segs = length(unique(segs$name)) == length(unique(osm_id))
        ) %>%
      impute_missing()
  }
  )

    
  
  
    
  
  write_csv(bind_rows(sum_sp), sprintf('bw/by_month/%s_bw_bymonth.csv', route_name))
   
  sum_times = purrr::map(sum_sp, function(sum_sp_){
    sum_route(sum_sp_, sum_sp_$all_segs[1], route_name) %>%
    mutate(bd = hour, bd_label = paste0(hour, ":00"),
           bd_label = ifelse(nchar(bd_label) == 4, paste0(0, bd_label), bd_label),
           month = sum_sp_$month[1]
           )
    }
  )
  
  
  times_plot = plot_times(df = sum_times %>% bind_rows, name = route_name, by_mon = T)
  
  ggsave(plot = times_plot, filename = sprintf('graphs/by_month/%s_times.PNG', route_name),
         width  = 140, height = 80, unit  = "mm")
  
  write_csv(sum_times %>% bind_rows(), sprintf("times/by_month/%s_times_bymonth.csv", route_name))
  
}


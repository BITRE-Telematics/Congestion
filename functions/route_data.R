library(tidyverse); library(RNeo4j); library(httr); library(ggmap)
library(yaml); library(jsonlite); library(inlegend); library(snapbox)
library(RCurl)
library(parallel)
library(sf)
library(gganimate)

if(!exists('roads')){
  roads = st_read("../shapefiles/AustraliaRoads.geojson")
}

extract_geom = function(route){
  geometry = route$routes$geometry[[1]][[1]] %>% st_linestring() %>% st_as_text() %>% st_as_sfc()
  return(geometry)
}

##Pulls raw data for analysis from BITRE telematics database
##Direction code is complicated by ongoing restructuring
get_dat = function(osm_id, year = year(lubridate::now())){
  query = "MATCH (s:Segment{osm_id:$OSM_ID})<-[oa:ON]-(o:Observation)<-[:OBSERVED_AT]-(t:Trip)<-[:EMBARKED_ON]-(v:Vehicle)
  WITH s, o, t, v, oa
  where o.datetimedt.year = $YEAR AND o.datetime > 0
  RETURN  
  o.imputed_speed as imp_speed,
  o.datetime as datetime,
  o.datetimedt.week as week,
  o.datetimedt.year as year,
  o.datetimedt.dayOfWeek as dayOfWeek,
  o.datetimedt.hour as hour,
  o.datetimedt.timezone as timezone,
  s.osm_id as osm_id,
  s.highway as class,
  s.speed_limit as speed_limit,
  s.forward as direction,
  CASE 
                             WHEN o.forward IS NOT NULL
                             THEN o.forward
                             WHEN oa.forward IS NULL
                             THEN abs(s.forward - o.azimuth) < 90 OR abs(s.forward - o.azimuth) > 270
                             ELSE 
                             	oa.forward 
                             	END 
                             as forward
  "
  df = cypher(g, query, OSM_ID = osm_id, YEAR = year) %>% unique()
  return(df)
}


##Pulls raw interquartile range
extract_range= function(df){
  df %>% group_by(osm_id, hour) %>%
    filter(!is.na(imp_speed), imp_speed>0) %>%
    mutate(
      imp_speed = ifelse(imp_speed - speed_limit > 0, speed_limit, imp_speed)
      ) %>%
    summarise(
      UQ = quantile(imp_speed, 0.75),
      median = quantile(imp_speed, 0.5),
      LQ = quantile(imp_speed, 0.25),
      n_obvs = n()
    ) %>% ungroup()
}

##Estimates travel times given segment speeds
sum_route = function(df, all_segs, route_name){
  
  df$LQ = ifelse(df$LQ == 0, df$median, df$LQ)## for a few troublesome segments
  out = df %>% 
    group_by(hour) %>% 
    summarise(
      n_hour_ids = length(unique(osm_id)),
      LQ_est  = sum((distance/1000)/(LQ/3600)) %>% ifelse(is.infinite(.), NA, .), #the ifelse controls for when the speed is zero
      med_est = sum((distance/1000)/(median/3600)) %>% ifelse(is.infinite(.), NA, .),
      UQ_est  = sum((distance/1000)/(UQ/3600)) %>% ifelse(is.infinite(.), NA, .),
      LQ_est_char     = humanFormat::formatSeconds((LQ_est)),
      med_est_char = humanFormat::formatSeconds((med_est)),
      UQ_est_char     = humanFormat::formatSeconds((UQ_est)),
      n_obvs = sum(n_obvs)
    ) %>% mutate(
      all = all_segs
    )
  if(!all(out$all)){
    print(sprintf("%sMissing hour values - manually impute", route_name))
  }
  return(out)
}


##determines whether a given observation/segment has an azimuth within 90 degrees of the stipulated route direction
bifurcate = function(azi, dir){
  diff = abs(dir-azi)
  (diff <= 90 | diff >= 270)
  
}

filter_dir = function(df, dir){
  df = mutate(df,
    direction = ifelse(forward, direction, direction -180),
    direction = ifelse(direction < 0, direction + 360, direction)
  )
  filter(df, bifurcate(direction, dir))
}
##Optional function to adjust segments with very low speeds at certain times.
adjust_low = function(df, min_obvs = 10){
  df = purrr::map(0:24, function(h){
    out = filter(df, hour == h) %>%
      mutate(
        UQ = ifelse(n_obvs < min_obvs, median(UQ, na.rm = T), UQ),
        median = ifelse(n_obvs < min_obvs, median(median, na.rm = T), median),
        LQ = ifelse(n_obvs < min_obvs, median(LQ, na.rm = T), LQ)
      )
  }) %>% bind_rows()
    
}
replace_na_limits = function(d){
  ##Deliberately excludes failsafe so failure means checking the data
  d = d %>% mutate(
    speed_limit = ifelse(is.na(speed_limit) & class == "motorway", 100, speed_limit),
    speed_limit = ifelse(is.na(speed_limit) & class == "motorway_link", 80, speed_limit),
    speed_limit = ifelse(is.na(speed_limit) & class == "primary", 60, speed_limit),
    speed_limit = ifelse(is.na(speed_limit) & class == "primary_link", 60, speed_limit),
    speed_limit = ifelse(is.na(speed_limit) & class == "trunk", 80, speed_limit),
    speed_limit = ifelse(is.na(speed_limit) & class == "trunk_link", 80, speed_limit)
  )
  return(d)
}

impute_missing = function(df){
  missing = filter(df, is.na(median)) %>% select(-LQ, -median, -UQ)
  
  missing = bind_rows(list(
    filter(missing, !is.na(hour)),
    create_hr_df(filter(missing, is.na(hour)))
  ))
  
  df = filter(df, !is.na(median))
  
  hour_meds = df %>% group_by(hour) %>%
    summarise(
      LQ = median(LQ, na.rm = T),
      median = median(median, na.rm = T),
      UQ = median(UQ, na.rm = T)
    )
  missing = left_join(missing, hour_meds, by = 'hour')
  return(bind_rows(list(df, missing)))
}

create_hr_df = function(df){
  purrr::map(0:23, function(h){
    mutate(df, hour = h, n_obvs = 0)
  }) %>% bind_rows()
}

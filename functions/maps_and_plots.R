library(patchwork); library(ggspatial)
##Maps gifs and hour breakdowns of speeds for analysis
map_route = function(df, name, by_bd = T, dir = '.', scale_name = "Median Speed", low = 'red', high = 'gold'){
  df$geometry = st_transform(df$geometry, 4269)
  bounds = get_bounds(df$geometry)
  df$geometry = st_transform(df$geometry, 3857)
  m = ggplot() +
    layer_mapbox(
      st_bbox(
        bounds, crs = 4269
      ),
      scale_ratio = 0.75,
      mapbox_api_access_token = mb_token,
      map_style = map_style
    ) +
    geom_sf(
      data = df,
      aes(colour = median, geometry = geometry, group = osm_id),
      inherit.aes = FALSE,
      size = 1
      ) +
    scale_color_continuous(low = low, high = high, breaks = c(0, 25, 50, 75, 100), name = scale_name) +
    theme(text = element_text(family = "Century Gothic"))
  if(by_bd){
    
    s = m + facet_wrap(~bd_label, ncol = 6)
    a = m + transition_states(bd_label) + #enter_fade() + exit_shrink() +
      ggtitle(sprintf("%s", '{closest_state}')) +
      theme(plot.title = element_text(size = 20, hjust = 0.5)) 
    a = animate(a, renderer = gifski_renderer(),
                width = 800,
                height = 800,
                units = 'px',
                duration = 24,
                nframes = length(unique(df$bd_label)) *2
    )
    ggsave(plot = s, file = sprintf('%s/%s', dir, name), width = 30, height  = 12, units = "cm")
    anim_save(filename = gsub('.png|.PNG', '.gif', name) %>% sprintf('%s/%s', dir, .))
  }else{
    ggsave(name %>% sprintf('%s/%s', dir, .), plot = m, width = 30, height = 30, units = "cm")
  }
  
}

##creates a square bounding box around the route
get_bounds = function(geometry, buffer = 0.5){ ##proportional margin around the distance from centroid
  
  bounds = st_bbox(geometry)
  cent = c(
    bounds$xmax - 0.5*(bounds$xmax-bounds$xmin),
    bounds$ymax - 0.5*(bounds$ymax - bounds$ymin)
  )
  
  x_breadth = geosphere::distGeo(c(bounds$xmin, cent[2]), c(bounds$xmax, cent[2]))/2
  y_breadth = geosphere::distGeo(c(cent[1], bounds$ymin), c(cent[1], bounds$ymax))/2
  
  max_breadth = max(x_breadth, y_breadth) * (1+buffer)
  
  xmax = geosphere::destPoint(cent, 90, max_breadth)[1]
  xmin = geosphere::destPoint(cent, 270, max_breadth)[1]
  ymax = geosphere::destPoint(cent, 0, max_breadth)[2]
  ymin = geosphere::destPoint(cent, 180, max_breadth)[2]
  
  return(c(xmax = xmax, xmin = xmin, ymax = ymax, ymin = ymin))
}

##displays the route for the report
display_map = function(route_name, route_geom, mb_token, map_style, dpi = 150){
  st_crs(route_geom) = 4269
   map_out = ggplot() +
    layer_mapbox(
      st_bbox(
        get_bounds(route_geom), crs = 4269
      ),
      scale_ratio = 0.75,
      mapbox_api_access_token = mb_token,
      map_style = map_style
    ) +
    geom_sf(
      data = st_transform(route_geom, 3857),
      aes(geometry = geometry),
      inherit.aes = F,
      colour = 'dark blue',
      size = 2
    ) + theme_cropped_map() +
      ggspatial::annotation_scale(
        Location = "b1",
        pad_x = unit(4.5, 'cm'),
        pad_y = unit(0.1, 'cm'),
        height = unit(0.15, 'cm'),
        text_cex = 0.6
          )
  ggsave(filename = sprintf("route_maps/display_maps/%s.PNG", route_name),
         map_out, height = 110, width = 110, units = 'mm', dpi = dpi)
}

##Same display map but both directions
bidirectional_map = function(route_name, route_geom_list, mb_token, map_style, dpi = 150){
  route_geom_list[[1]] = route_geom_list[[1]] %>% st_as_sfc(crs = 4269)
  route_geom_list[[2]] = route_geom_list[[2]] %>% st_as_sfc(crs = 4269)
  ggplot() +
    layer_mapbox(
      st_bbox(
        get_bounds(route_geom_list[[1]]), crs = 4269
      ),
      scale_ratio = 0.4,
      mapbox_api_access_token = mb_token,
      map_style = map_style
    ) +
    geom_sf(
      data = st_transform(
        route_geom_list[[1]],
        3857),
      aes(geometry = geometry),
      inherit.aes = F,
      colour = 'dark blue',
      size = 2
    ) +
    geom_sf(
      data = st_transform(
        route_geom_list[[2]],
        3857),
      aes(geometry = geometry),
      inherit.aes = F,
      colour = 'dark blue',
      size = 2
    ) +
    theme_cropped_map() +
    ggspatial::annotation_scale(
      Location = "b1",
      pad_x = unit(4.5, 'cm'),
      pad_y = unit(0.1, 'cm'),
      height = unit(0.15, 'cm'),
      text_cex = 0.6
    )
  ggsave(sprintf("route_maps/display_maps/bidirectional/%s.PNG", route_name),
         height = 110, width = 110, units = 'mm', dpi = dpi)
}

##Plots times over the course of a day and by month if so chosen
plot_times = function(df, name, max_y = NULL, rm_incomplete =F, by_mon = F, ncol = 3){
  if(rm_incomplete){
    df = filter(df, all)
  }
  plt = ggplot(df, aes(x = bd, y = round(med_est/60, 2), ymax = round(UQ_est/60, 2), ymin = round(LQ_est/60, 2))) + 
    geom_ribbon(fill = '#79D1f4', alpha = 1) +
    geom_line(colour = '#0a2240') +
    scale_x_continuous(limits = c(0, 23),
          breaks = seq(0, 23, 4),
          labels = paste0(seq(0, 23, 4), ":00")
          ) +
    ggthemes::theme_tufte() +
    labs(x = 'Hour of day', y = 'Travel time (minutes)', title = name)
  if(by_mon){
    plt = plt + facet_wrap(~month, ncol = ncol)
  }
  if(is.null(max_y)){
    return (plt + expand_limits(y=0))
  }else{
    return(plt + scale_y_continuous(limits = c(0, max_y)))
  }
}
##Same as above but comines directions from route
bidirectional_plot = function(n, breakdown_table){
  plot_dat = filter(breakdown_table, short_name == n) %>%
    mutate(
      route_name = str_remove(route_name, "^.* - ")
    )
  max_y = max(plot_dat$LQ_est, na.rm = T)/60
  max_y = if_else(max_y%%10 > 5,
                  round(max_y/10)*10,
                  ifelse(max_y%%10 == 5,
                         max_y,
                         trunc(max_y/10)*10 +5
                  )
          )  
  
  plots = purrr::map(unique(plot_dat$route_name), function(r){
    plot_times(
      filter(plot_dat, route_name == r) %>% mutate(bd = hour),
      name = r, max_y = max_y)
  })
  bi_plot = wrap_plots(plots, ncol = 1)
  ggsave(plot = bi_plot, filename = sprintf('graphs/bidirectional/%s_times.PNG', n),
         width  = 140, height = 160, unit  = "mm")
}
##Maps all routes in a city and colours them according to a metrucs
plot_city = function(m, map_data, dpi = 150){
  map_data_ = filter(map_data, city == m)
  bbox_st = st_bbox(
    get_bounds(map_data_$route_geom, buffer = 0.5),
    crs = 4269
  )
  
  st_crs(map_data_$route_geom) = 4269
  
  centroids = st_centroid(map_data_$route_geom) %>%
    st_coordinates() %>% data.frame()
  map_data_ = bind_cols(list(map_data_, centroids))
  
  datalab = map_data_ %>%select(`route_name`, X, Y) 
  datalab$`route_name` = str_split(datalab$`route_name`, ' - ') %>% purrr::map(function(s){s[1]}) %>% unlist()
  datalab = group_by(datalab, `route_name`) %>% summarise(X = X[1], Y = Y[1])
  
  
  m_out_time_mbox = ggplot() + 
    layer_mapbox(bbox_st, scale_ratio = 0.75,
                 mapbox_api_access_token = mb_token,
                 map_style = 'mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt') +
    layer_spatial(
      data = map_data_,
      aes(colour = `Mean excess time ratio`, geometry = route_geom, group = `route_name`), inherit.aes = FALSE, size = 1) +
    scale_color_continuous(low = "gold", high = "red", name = 'Mean Excess\nTime Ratio') +
    geom_spatial_label_repel(
      data = datalab, aes(label = `route_name`, x = X, y = Y),
      size = 5, family = 'serif', fill = alpha(c("white"), 0.6), label.size = NA
    ) +
    theme(text = element_text(family = "serif")) +
    theme_cropped_map() + inlegend::inset_legend_light()
  
  ggsave(plot = m_out_time_mbox, file = sprintf('city_maps/%s_route_times.PNG', m),
         width = 20, height = 20, units = "cm", dpi = dpi)
  
  
  m_out_range_mbox = ggplot()+
    layer_mapbox(bbox_st, scale_ratio = 0.75,
                 mapbox_api_access_token = mb_token,
                 map_style = 'mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt') +
    layer_spatial(data = map_data_, aes(colour = `Mean excess iq ratio`, geometry = route_geom, group = `route_name`), inherit.aes = FALSE, size = 1) +
    geom_spatial_label_repel(
      data = datalab, aes(label = `route_name`, x = X, y = Y),
      size = 5, family = 'serif', fill = alpha(c("white"), 0.6), label.size = NA
    ) +
    scale_color_continuous(low = "gold", high = "red", name = 'Mean Excess\nUncertainty Ratio') +
    theme(text = element_text(family = "serif")) +
    theme_cropped_map() + inlegend::inset_legend_light()
  ggsave(plot = m_out_range_mbox, file = sprintf('city_maps/%s_route_ranges.PNG', m),
         width = 20, height = 20,units = "cm", dpi = dpi)
  
  
}



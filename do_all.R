source("functions/route_data.R")
source("functions/keys_and_connections.R")
source("functions/generic_route_func.R")
source('functions/bayesian_func.R')
source('functions/stanserver.R')
source('functions/maps_and_plots.R')
##creating folder structure for outputs
folders = c(
	'data',
	'graphs',
	'bw',
	'metrics',
	'city_maps',
	'route_maps',
	'times',
	'graphs/bidirectional',
	'route_maps/breakdown_maps',
	'route_maps/display_maps',
	'route_maps/display_maps/bidirectional'
	)
purrr::walk(folders, function(d){
  if(!dir.exists(d)){
    dir.create(d)
  }
})


route_parameters = read_csv("route_parameters.csv", col_types = cols(
  route_name = col_character(),
  start = col_character(),
  finish = col_character(),
  middle = col_character(),
  dir = col_double(),
  bayes = col_logical(),
  bayes_raw_speeds = col_logical(),
  multi_level = col_logical(),
  city = col_character()
    )         
  )

purrr::walk(purrr::transpose(route_parameters), function(r){
  try({
    print(r$route_name)
    route_func(r, year = 2020)
    write_lines(r$route_name, "done.txt", append = T)
  })
})
##Creates outputs for report including maps, times plots with comparable axes and maps with both direction of routes 
source("functions/route_metrics.R")


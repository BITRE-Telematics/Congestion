source("functions/keys_and_connections.R")
source("functions/route_data.R")
source("functions/generic_route_func.R")
source('functions/bayesian_func.R')
source('functions/stanserver.R')
source('functions/maps_and_plots.R')
source('functions/generic_route_func_bymonth.R')
##This provides select outputs by month for when one assumes considerable variation within a year
##for instance during covid lockdowns. It assumes a bayesian approach for all routes.
folders = c(
  'data',
  'graphs',
  'graphs/by_month',
  'bw',
  'bw/by_month',
  'metrics',
  'city_maps',
  'route_maps',
  'times',
  'times/by_month',
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
) %>% mutate(
  bayes = TRUE
)

purrr::walk(purrr::transpose(route_parameters), function(r){
  try({
    print(r$route_name)
    route_func_by_month(r, year = 2020)
    write_lines(r$route_name, "done_by_month.txt", append = T)
  })
})



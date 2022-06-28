##a geojson of the roads data used in the Yulo processing and OSRM routing
if(!exists('roads')){
  roads = sf::st_read("../../shapefiles/AustraliaRoads.geojson")
}
## credentials for the telematics databased, a yaml with the fields username, password and ipporthttp 
creds = yaml::yaml.load_file("keys/neo4jcredsWIN.yaml")

if (!require(neo4bitre)){
  remotes::install_github('bitre-telematics/neo4bitre')
}

g = neo4j_api$new(
  creds$ipporthttp,
  user = creds$username,
  password = creds$password,
  db = creds$db
  )




##a mapbox api token
mb_token = readr::read_lines('keys/mapboxtoken.txt')
map_style = "mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt"

##the format for queries to OSRM. Address must be altered as necessary.
url_format = "http://127.0.0.1:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson"
#url_format = "http://10.0.0.143:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson"

##a geojson of the roads data used in the Yulo processing and OSRM routing
if(!exists('roads')){
  roads = st_read("../shapefiles/AustraliaRoads.geojson")
}
## credentials for the telematics databased, a yaml with the fields username, password and ipporthttp 
creds = yaml.load_file("keys/neo4jcredsWIN.yaml")

g = startGraph(sprintf("%s/db/data", creds$ipporthttp), username = creds$username, password = creds$password)


## a google maps api key
g_key = read_lines("keys/apikey.txt")[1] 

register_google(key = g_key)

##a mapbox api token
mb_token = read_lines('keys/mapboxtoken.txt')
map_style = "mapbox://styles/geowonk/cklpsnqf95jw017olf76p1vbt"

##the format for queries to OSRM. Address must be altered as necessary.
url_format = "http://127.0.0.1:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson"
#url_format = "http://10.0.0.143:5000/route/v1/driving/%s;%s;%s?steps=true&geometries=geojson"

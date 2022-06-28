![BITRE logo](logo.png)
This directory analysis code for the BITRE Urban freight congestion report. It does not include any data and is provided solely to illustrate the means of analysis. 

Parameters for selected routes are in `route_parameters.csv` and are iterated through by `do_all.R`. This applies functions in `functions`. 
The parameters are:

- **route name** : The name of the route
- **start** : The start point of the route in the form `<LON>,<LAT>`
- **end** : The end point of the route in the form `<LON>,<LAT>`
- **middle** : Any intermediate points to ensure the correct route in the form `<LON1>,<LAT1>|<LON2>,<LAT2>`
- **dir** : An optional azimuth (compass bearing between 0 and 360) where the direction of travel on the route is within 90 degrees at all times. This is used to identify direction on two-way segments
- **bayes** : Whether Bayesian method is to be used rather than raw data, needed on some less observed routes
- **bayes_raw_speed** : Whether Bayesian method, if used, should be using imputed speed, or divergence from speed limit 
- **multi_level** : Whether the Bayesian method should be multilevel or not
- **min_speed** : An optional minimum speed accepted for observations. This filters data and, in the case of Bayesian results, shapes the samples drawn from the posterior
- **adjust_low** : Whether segments with very low observation umbers should be adjusted to averages for the route where they are providing distorted results
- **city** : The name of the city the route is in, for producing maps and aiding later analysis
- **mon_weight** : Where a month is partially missing the remaining data can be weighted by the volumes of another month of assumed similar volumes

There are two Bayesian methods, both of which are very elementary. The default, non multilevel model is
<blockquote>
b_var <sub>SEGMENT, HOUR</sub> ~ Normal(mu <sub>SEGMENT, HOUR</sub>, sigma <sub>SEGMENT, HOUR</sub>)

mu ~ Normal(mu <sub>HOUR</sub>, 10)

sigma ~ Normal(sigma <sub>HOUR</sub>, 0.5) 
</blockquote>
Where the posterior distribution of speeds for a given segment and hour is derived using a prior distribution drawn from all segments at that hour.
The multilvel model is
<blockquote>
b_var <sub>SEGMENT [HOUR]</sub> ~ Normal(mu <sub>SEGMENT [HOUR]</sub>, sigma <sub>SEGMENT [HOUR]</sub>)

mu ~ Normal(mu <sub>[HOUR]</sub>, 10)

sigma ~ Normal(sigma <sub>[HOUR]</sub>, 0.5)
</blockquote>
Where the data for each segment is indexed into 24 clusters for each hour and given 24 priors drawn from all segments on the route at that hour.

`do_all_by_month.R` performs the same analysis as `do_all.R` but provides seperate estimates by month. This is useful when one suspects strong changes over the course of the year, for example because of the 2020 COVID-19 lockdowns. It only used the Bayesian estimation.

`functions/keys_and_connections.R` imports a number of API keys and sets varaibles that must be adjusted as necessary.

There is an experimental docker implementation with Rocker because of the use of linux dependent parallel code. Provisional use is
`docker build -t Congestion ./`
`sudo docker run --rm --name="Congestion" -v $PWD:/Congestion -v Congestion` 
then
`sudo docker exec Congestion RScript do_all.R`
But the data directories may need manual specification

Replicating the results of the report requires access to the BITRE telematics database, and a mildly altered version of Open Source Routing Machine with the data used available as a geojson as in `keys_and_connectons.R`. If the user has a large amount of telematics data they may seek to replicate the entire Yulo framwork. The code also assumes a unix like system for parallel functions.

It is licenced under a Creative Commons licence - Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
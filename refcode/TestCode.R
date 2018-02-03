source("R/AddCensusDemographics.R")

api_key <-"d9174b06d5ec03a39d5f89f411614913e72556e6"
map <- st_read("testing/data/raw/ArlingtonMA/ArlingtonMA_VoterPrecinct.shp")
state <- 25
county <- c(17)

MapWithDemographics <- AddCensusDemographics(map,api_key,state,county)

st_write(MapWithDemographics,dsn="testing/data/output/",layer="MapWithDemographics",driver = "ESRI Shapefile")

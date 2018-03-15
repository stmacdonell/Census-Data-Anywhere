source("R/AddCensusDemographics.R")

#set up directories we'll need
dir.create("data/cache/")
dir.create("data/cache/ArlingtonMA/")
dir.create("data/test")

#set api_key
api_key <-"d9174b06d5ec03a39d5f89f411614913e72556e6"

#read the map
unzip("shiny/TestingData/ArlingtonMA_VoterPrecinct.zip",exdir = "data/cache/ArlingtonMA/")
map <- st_read("data/cache/ArlingtonMA/ArlingtonMA_VoterPrecinct.shp")

#set parameters
state <- 25
county <- 17

#add census demographics
MapWithDemographics <- AddCensusDemographics(map,api_key,state,county)

#output map
st_write(MapWithDemographics,dsn="data/test/",layer="MapWithDemographics",driver = "ESRI Shapefile")

#clean up directory
unlink("data/cache",recursive = TRUE)
unlink("data/test",recursive = TRUE)

library(sf)

#read in precinct map
dir.create("data/cache/")
unzip("data/raw/PrecinctShapefiles/SBE_PRECINCTS_20120901.zip",exdir = "data/cache/")
map <- st_read("data/cache/SBE_PRECINCTS_09012012.shp",stringsAsFactors = FALSE)

#read in block map
dir.create("data/cache/blkshape/")
unzip("data/raw/NCblk/nhgis0022_shape.zip",exdir = "data/cache/blkshape/")
unzip("data/cache/blkshape/nhgis0022_shape/nhgis0022_shapefile_tl2010_370_block_2010.zip",exdir = "data/cache/blkshape/")
blkmap <- st_read("data/cache/blkshape/NC_block_2010.shp",stringsAsFactors = FALSE)

####function starts below


map <- st_transform(map,st_crs(blkmap))

#get a copy of only the spatial data
blkmapFull <- blkmap
blkmap <- st_geometry(blkmap)
mapFull <- map
map <- st_geometry(map)

# ##cleans things up
# blkmap <- gBuffer(blkmap, byid = TRUE)
# map <- gBuffer(map,byid = TRUE)
#
# #make new maps for intersection
# blkmap <- as(blkmap,'SpatialPolygons')
# map <- as(map,'SpatialPolygons')

#add indexes to determine
#blkmap <- SpatialPolygonsDataFrame(blkmap, data.frame(block=paste0("b",1:length(blkmap))),match.ID=F)
#map <- SpatialPolygonsDataFrame(map, data.frame(precinct=paste0("p",1:length(map))),match.ID=F)

#which blocks intersect each precinct
#gIntersection and raster::intersection take way to long on everything
#it is much faster to just figure out what intersects at all, and then only intersect those
mapi <- st_intersects(map,blkmap,sparse = TRUE, prepared = TRUE)

#dir.create("data/cache/vtdmunge")
#save.image(file="data/cache/vtdmunge/intersected.RData")

#what is the area of each block
blkareas <- sapply(1:length(blkmap),function(x){
  st_area(st_geometry(blkmap[x]))
})

start <- proc.time()
#how much of each intersected block is within each precinct
percent.of.block.intersecting <- sapply(1:1000,function(x){

  percent.of.area.interescting <- sapply(mapi[[x]],function(y){
    intersection <- st_intersection(map[x],blkmap[y])
    #what is the area of the intersection
    if(is.null(intersection)){
      return(0)
    }else{
      return(st_area(intersection)/blkareas[y])
    }
  })
})
end <- proc.time()
end-start

#dir.create("data/cache/vtdmunge")
#save(percent.of.block.intersecting,file = "data/cache/vtdmunge/PercentOfBlocksIntersecting.RData")

map <- mapFull
blkmap <- blkmapFull

#read block data
dir.create("data/cache/blkdata")
unzip("data/raw/NCblk/nhgis0022_csv.zip",exdir="data/cache/blkdata")
blkdata <- read.csv("data/cache/blkdata/nhgis0022_csv/nhgis0022_ds172_2010_block.csv",stringsAsFactors = FALSE)

blkmap@data <- cbind((1:length(blkmap)),blkmap@data)
colnames(blkmap@data)[1] <- "PolyIndex"

GISJOIN <- list(blkdata$GISJOIN, blkmap@data$GISJOIN)

match <- mclapply(1:length(GISJOIN[[1]]),
                  FUN=function(x){GISJOIN[[1]][x]%in%GISJOIN[[2]]},
                  mc.cores = 2)
match <- unlist(match)
sum(blkdata$H73001[!match])
#should be zero

#drop unneeded data and merge into the shapefile
blkdata <- blkdata[,c(1,40:dim(blkdata)[2])]
blkmap@data <- merge(blkmap@data,blkdata,by="GISJOIN",all.x=TRUE,all.y=FALSE)

#put back in original order
blkmap@data <- blkmap@data[order(blkmap@data$PolyIndex),]

#for each precinct add the portion (%of block area included in the precinct) of the population
#from each block to the precinct
temp <- apply(as.matrix(1:length(mapi)), MARGIN=1,  FUN=function(x){
  percent.of.block.intersecting[[x]] %*% as.matrix(blkmap@data[mapi[[x]],(20:dim(blkmap@data)[2])])
})

map@data <- cbind(map@data,t(temp))

colnames(map@data)[5:dim(map@data)[2]] <- colnames(blkmap@data)[20:dim(blkmap@data)[2]]

dir.create("data/cache/vtdmunge")
save(map,file="data/cache/vtdmunge/mapWithPop.RData")

rm(list=ls(all=TRUE))

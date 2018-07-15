
# Download of static Google Maps and import to a GIS:
# by: T. Hengl (http://spatial-analyst.net)

# setwd("C:/Temp")
library(RgoogleMaps)
library(rgdal)
library(RSAGA)
library(R.utils)

download.file("http://cran.r-project.org/src/contrib/RgoogleMaps_1.1.6.tar.gz", destfile=paste(getwd(), "/", "RgoogleMaps.tar.gz", sep=""))
gunzip("RgoogleMaps.tar.gz", overwrite=TRUE, remove=TRUE)
# under windows you need to get 7z software to unzip *.tar archives:
download.file("http://downloads.sourceforge.net/sevenzip/7za465.zip", destfile=paste(getwd(), "/", "7za465.zip", sep="")) 
unzip("7za465.zip")
system("7za e -ttar RgoogleMaps.tar XY2LatLon.R -r -aos")
source("XY2LatLon.R")

NL.RD <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m +no_defs"
google.prj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
                                           
#----------------------------------------------------------------
# PREPARE A MAP OF THE AREA OF INTEREST
#---------------------------------------------------------------- 
 
# CBS Wijk en buurtkaart 2008
download.file("http://www.cbs.nl/NR/rdonlyres/3D8F1FF7-E6BA-430D-AF3B-9F16CB6647B9/0/buurt_2008_gen1.zip", destfile=paste(getwd(), "/", "NL_buurtkaart2008.zip", sep=""))
for(j in list(".shp", ".shx", ".dbf")){
fname <- zip.file.extract(file=paste("gem_2008_gen", j, sep=""), zipname="NL_buurtkaart2008.zip")
file.copy(fname, paste("./gem_2008_gen", j, sep=""), overwrite=TRUE)
}
unlink("NL_buurtkaart2008.zip")
NLprovs <- readOGR("gem_2008_gen.shp", "gem_2008_gen")
proj4string(NLprovs) <- CRS(NL.RD)
NLprovs.ll <- spTransform(NLprovs, CRS("+proj=longlat +datum=WGS84"))

#----------------------------------------------------------------
# GET A STATIC IMAGE
#----------------------------------------------------------------

# Get the Maximum zoom level:
mzoom <- MaxZoom(latrange=NLprovs.ll@bbox[2,], lonrange=NLprovs.ll@bbox[1,], size=c(640, 640))[[1]]
mzoom
# Center of the study area:
lonc <- mean(NLprovs.ll@bbox[1,])
latc <- mean(NLprovs.ll@bbox[2,])
lonc; latc
# Get a satellite image of the Netherlands:
MyMap <- GetMap.bbox(center=c(latc, lonc), zoom=mzoom, destfile="netherlands.png", maptype ="roadmap")
tmp <- PlotOnStaticMap(MyMap, lon=lonc, lat=latc, pch="+", cex=2, col="red", verbose=1)

# Get a single province:
prov1.ll <- NLprovs.ll[1,]
lonc <- prov1.ll@polygons[[1]]@labpt[1]
latc <- prov1.ll@polygons[[1]]@labpt[2]
mzoom <- MaxZoom(latrange=prov1.ll@bbox[2,], lonrange=prov1.ll@bbox[1,], size=c(640, 640))[[1]]
MyMap <- GetMap.bbox(center=c(latc, lonc), zoom=mzoom, destfile="s-Gravenhage.png", maptype ="satellite")
tmp <- PlotPolysOnStaticMap(MyMap, lon=lonc, lat=latc, pch="+", cex=2, col="red")


#----------------------------------------------------------------
# Convert to a SpatialGridDataFrame
#----------------------------------------------------------------

png.map <- readGDAL("s-Gravenhage.png")
# get the bounding box coordinates in LatLon:
bbox.MyMap <- data.frame(XY2LatLon(MyMap, X=png.map@bbox[1,]-640/2, Y=png.map@bbox[2,]-640/2)) 
coordinates(bbox.MyMap) <- ~lon+lat
proj4string(bbox.MyMap) <- CRS("+proj=longlat +ellps=WGS84")
# bounding box in Google Maps CRS:
bbox.google.prj <- spTransform(bbox.MyMap, CRS(google.prj))
bbox.google.prj
# copy the bounding box coordinates:
png.map@bbox[1,1] <- bbox.google.prj@coords[1,1]
png.map@bbox[2,1] <- bbox.google.prj@coords[1,2]
png.map@bbox[1,2] <- bbox.google.prj@coords[2,1]
png.map@bbox[2,2] <- bbox.google.prj@coords[2,2]
# cell size:
png.map@grid@cellsize <- round(c((png.map@bbox[1,2]-png.map@bbox[1,1])/640, (png.map@bbox[2,2]-png.map@bbox[2,1])/640), 1)
png.map@coords[1,1] <- png.map@bbox[1,1]+png.map@grid@cellsize[1]/2
png.map@coords[1,2] <- png.map@bbox[2,1]+png.map@grid@cellsize[2]/2
png.map@coords[2,1] <- png.map@bbox[1,2]-png.map@grid@cellsize[1]/2
png.map@coords[2,2] <- png.map@bbox[2,2]-png.map@grid@cellsize[2]/2
# cell offset:
png.map@grid@cellcentre.offset <- c(png.map@bbox[1,1]+png.map@grid@cellsize[1]/2, png.map@bbox[2,1]+png.map@grid@cellsize[2]/2)
# attach the correct prj:
proj4string(png.map) <- CRS(google.prj)
str(png.map)
# export to a GIS format:
write.asciigrid(png.map[1], "pngmap_B1.asc", na.value=-1)
# writeGDAL(netherlands[c(1,2,3)], "netherlands.tif", drivername="GTiff", type="Byte", options="INTERLEAVE=PIXEL")

# resample image to local coordinate system:
rsaga.esri.to.sgrd(in.grids="pngmap_B1.asc", out.sgrd="pngmap_B1.sgrd", in.path=getwd())
rsaga.geoprocessor(lib="pj_proj4", 2, param=list(SOURCE_PROJ=paste('"', google.prj, '"', sep=""), TARGET_PROJ=paste('"', NL.RD, '"', sep=""), SOURCE="pngmap_B1.sgrd", TARGET="pngmap_B1_NLRD.sgrd", TARGET_TYPE=0, INTERPOLATION=1))


#----------------------------------------------------------------
# Optional: ESTIMATE NUMBER OF TILES TO COVER THE WHOLE COUNTRY
#----------------------------------------------------------------

# You need to first install and register Python on your machine!
download.file("http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/globalmaptiles.py", destfile=paste(getwd(), "/", "globalmaptiles.py", sep=""))

# Usage: globalmaptiles.py [-profile 'mercator'|'geodetic'] zoomlevel lat lon [latmax lonmax]
system(paste("python.exe globalmaptiles.py", mzoom, NLprovs.ll@bbox[2,1], NLprovs.ll@bbox[1,1], NLprovs.ll@bbox[2,2], NLprovs.ll@bbox[1,2]))

# end of script!
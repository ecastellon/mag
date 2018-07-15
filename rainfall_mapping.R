                                                                           
# title         : rainfall_mapping.R
# purpose       : spatio-temporal interpolation exercise using Bilogora data set;
# reference     : Hengl, T., A. AghaKouchack, and M. Perc(ec-Tadic' 2010. Methods and data sources for spatial prediction of rainfall. In: "Rainfall: State of the Science", edited by F. Y. Testik and M. Gebremichael. Geophysical Monograph Series. American Geophysical Union. doi: 10.1029/2010GM000999
# producer      : Prepared by T. Hengl
# last update   : In Amsterdam, NL, June 2010.
# inputs        : input data is available at [http://spatial-analyst.net/book/Bilogora];
# outputs       : geotiff images projected in the UTM33N system;
# remarks 1     : First download and install FWtools [http://fwtools.maptools.org];

library(rgdal)
library(geoR)
library(gstat)
library(pscl)
utm33 <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# ------------------------------------------------------------
# 1. Download and formating of data
# ------------------------------------------------------------

# download and uzip the data:
download.file("http://spatial-analyst.net/book/system/files/bilogora1km.zip", destfile=paste(getwd(), "bilogora1km.zip", sep="/"), mode='wb', method='wget')
unzip("bilogora1km.zip")
download.file("http://spatial-analyst.net/book/system/files/LST2008Bilogora.zip", destfile=paste(getwd(), "bilogora1km.zip", sep="/"), mode='wb', method='wget')
unzip("LST2008Bilogora.zip")
# winrar file:
download.file("http://spatial-analyst.net/book/system/files/RADAR2008Bilogora.rar", destfile=paste(getwd(), "RADAR2008Bilogora.rar", sep="/"), mode='wb', method='wget')
# create an output directory for maps:
Sys.chmod(getwd(), mode="7777") # allow read/write
dir.create(path="RN"); 
system(paste("C:\\PROGRA~2\\WinRAR\\UnRAR e RADAR2008Bilogora.rar ", getwd(), "/RN/", sep=""))

# read DEM:
grids1km <- readGDAL("dem.asc")
names(grids1km) <- "dem"
proj4string(grids1km) <- CRS(utm33)
# create dummy grids (Lat/Lon):
grids.ll <- spTransform(grids1km, CRS("+proj=longlat +datum=WGS84"))
grids1km$LAT <- grids.ll@coords[,2]
grids1km$LON <- grids.ll@coords[,1]
# str(grids1km@data)
gridcell <- grids1km@grid@cellsize[1]

# derive TWI:
writeGDAL(grids1km["dem"], "dem.sdat", "SAGA")
rsaga.geoprocessor(lib="ta_hydrology", module=15, param=list(DEM="dem.sgrd", C="tmp.sgrd", GN="tmp.sgrd", CS="tmp.sgrd", SB="twi.sgrd", T=120))  # floating point
grids1km$twi <- readGDAL("TWI.sdat")$band1
# radar images:
grids1km$RN10501 <- readGDAL("RN/RN1_2008_05_01.tif")$band1
grids1km$RN10430 <- readGDAL("RN/RN1_2008_04_30.tif")$band1

# convert country boundaries to lines:
rsaga.geoprocessor(lib="shapes_lines", module=0, param=list(LINES="borders.shp", POLYGONS="countries.shp"))
# import country borders:
borders <- readOGR(".", "borders")

# read station data to R:
GSOD.2008.bilogora <- read.table("GSOD_2008_bilogora.csv", sep=";", header=TRUE)
str(GSOD.2008.bilogora)
# reformat values:
GSOD.2008.bilogora$DATE <- as.Date(GSOD.2008.bilogora$DATE)
# select 1st of May 2008:
GSOD.20080501 <- subset(GSOD.2008.bilogora, GSOD.2008.bilogora$DATE==as.Date("2008-05-01")&!is.na(GSOD.2008.bilogora$PREC))
# str(GSOD.20080501)  # 99 points
coordinates(GSOD.20080501) <- ~LON+LAT
proj4string(GSOD.20080501) <- CRS("+proj=longlat +datum=WGS84")
GSOD.20080501 <- remove.duplicates(GSOD.20080501)
GSOD.20080501.XY <- spTransform(GSOD.20080501, CRS(utm33))

GSOD.plt1 <- bubble(GSOD.20080501.XY, "PREC", col="black", pch=21, main="PREC on 2008-05-01", sp.layout=list(list("sp.lines", col="grey", borders), list("sp.points", col="black", pch="+", cex=1.2, subset(GSOD.20080501.XY, GSOD.20080501.XY$PREC==0))))

# aggregate values for whole year:
GSOD.2008year <- aggregate(GSOD.2008.bilogora[!is.na(GSOD.2008.bilogora$PREC),c("PREC","LAT","LON")], by=list(GSOD.2008.bilogora$STNID[!is.na(GSOD.2008.bilogora$PREC)]), FUN=mean)   
coordinates(GSOD.2008year) <- ~LON+LAT
proj4string(GSOD.2008year) <- CRS("+proj=longlat +datum=WGS84")
GSOD.2008year <- remove.duplicates(GSOD.2008year)
GSOD.2008year.XY <- spTransform(GSOD.2008year, CRS(utm33))
GSOD.2008year.XY$PRECm <- round(ifelse(GSOD.2008year.XY$PREC==0, NA, GSOD.2008year.XY$PREC*length(levels(as.factor(GSOD.2008.bilogora$YEARMODA)))), 0)
GSOD.2008year.XY <- subset(GSOD.2008year.XY, !is.na(GSOD.2008year.XY$PRECm)) 
GSOD.plt2 <- bubble(GSOD.2008year.XY, "PRECm", col="black", do.sqrt=FALSE, pch=21, main="Summary PREC for 2008", sp.layout=list("sp.lines", col="grey", borders))
windows(width=14, height=8)
print(GSOD.plt1, split=c(1,1,2,1), more=TRUE)
print(GSOD.plt2, split=c(2,1,2,1), more=FALSE)

# plot RN1 monthly images:
month.list <- dir(path=getwd(), pattern=glob2rx("RN1_2008_*.tif"), recursive=FALSE, full.names=TRUE)
RN1month <- readGDAL(month.list[1])
names(RN1month) <- format(as.Date(paste("2008-", 1, "-01", sep="")), "%b")
for(j in 2:length(month.list)){ RN1month@data[,format(as.Date(paste("2008-", j, "-01", sep="")), "%b")] <- readGDAL(month.list[j], silent=TRUE)$band1 }
windows(width=14, height=6)
spplot(RN1month, at=seq(0,9,9/40)^3, col.regions=grey(rev(seq(0,1,0.025))), sp.layout=list("sp.lines", col="white", borders))
dev.off() 
# derive total rainfall estimated by radar:
grids1km$RN1 <- rowSums(RN1month@data[,format(as.Date(paste("2008-", 1:12, "-01", sep="")), "%b")], na.rm=FALSE, dims=1) 
spplot(grids1km["RN1"], at=seq(5,15,(15-5)/40)^3, col.regions=grey(rev(seq(0,1,0.025))), sp.layout=list("sp.lines", col="white", borders))


# plot RN1 images for a single day:
download.file("http://spatial-analyst.net/book/system/files/RN1_2008_05_01.zip", destfile=paste(getwd(), "RN1_2008_05_01.zip", sep="/"), mode='wb', method='wget')
unzip("RN1_2008_05_01.zip", exdir=paste(getwd(), "2008_05_01", sep="/"))
RN1.list <- dir(path=paste(getwd(), "2008_05_01", sep="/"), pattern=glob2rx("RN1*.tif"), recursive=TRUE, full.names=TRUE)
RN1 <- readGDAL(RN1.list[1])
names(RN1) <- "h1"
for(j in 2:length(RN1.list)){ RN1@data[,paste("h", j, sep="")] <- readGDAL(RN1.list[j], silent=TRUE)$band1 }
windows(width=14, height=6)
spplot(RN1, at=seq(0,3.5,3.5/40)^3, col.regions=grey(rev(seq(0,1,0.025))), sp.layout=list("sp.lines", col="pink", borders))
dev.off() 

# overlay grids and GSOD stations:
GSOD.20080501.ov <- overlay(grids1km, GSOD.20080501.XY) 
GSOD.20080501.ov@data <- cbind(GSOD.20080501@data, GSOD.20080501.ov@data)

# ------------------------------------------------------------
# 2. Mechanical interpolator (local polynomial)
# ------------------------------------------------------------

sp.list <- list(list("sp.lines", col="black", borders), list("sp.points", col="red", pch="+", GSOD.20080501.XY, cex=1.2))

# we can predict "PREC" values as a function of XYZ:
PREC.loess <- loess(log1p(PREC)~LON+LAT, data=GSOD.20080501.ov@data, span=0.4)
summary(PREC.loess)
PREC.ls <- predict(PREC.loess, grids1km@data, se=TRUE)
grids1km$PREC.ls <- expm1(PREC.ls$fit)
# spplot(grids1km["PREC.ls"], at=seq(0,4,4/40), col.regions=grey(rev(seq(0,1,0.025))), main="Local polynomial", sp.layout=sp.list) 

# gstat:
PREC.svar <- variogram(log1p(PREC)~1, GSOD.20080501.XY)
# plot(PREC.svar)
# initial variogram:
PREC.ivgm <- vgm(nugget=0, model="Exp", range=sqrt(diff(grids1km@bbox["x",])^2 + diff(grids1km@bbox["y",])^2)/4, psill=var(log1p(GSOD.20080501.XY$PREC)))
PREC.vgm <- fit.variogram(PREC.svar, model=PREC.ivgm)
PREC.vgm

# ------------------------------------------------------------
# 3. Geostatistics in geoR
# ------------------------------------------------------------

# geoR:
PREC.geo <- as.geodata(GSOD.20080501.XY["PREC"]) 
PREC.geo$data <- PREC.geo$data+0.1 # add measurement error otherwise transformation fails!
# plot(PREC.geo)
# fit variogram using likfit:
PREC.svar2 <- variog(PREC.geo, lambda=0, max.dist=200000, messages=FALSE)
par(mfrow=c(1,2))
plot(PREC.svar$dist, PREC.svar$gamma, xlab="distance", ylab="semivariance", main="gstat"); lines(variogramLine(PREC.vgm, maxdist=2e+05), lwd=2); text(PREC.svar$dist, PREC.svar$gamma, PREC.svar$np, cex=0.8, pos=4) 
PREC.vgm2 <- likfit(PREC.geo, lambda=0, messages=FALSE, ini=c(PREC.vgm$psill[2],PREC.vgm$range[2]), cov.model="exponential")
PREC.vgm2
# this carries much more information!
env.model <- variog.model.env(PREC.geo, obj.var=PREC.svar2, model=PREC.vgm2)
plot(PREC.svar2, envelope=env.model, main="geoR"); lines(PREC.vgm2, lwd=2)
legend("topleft", legend=c("Fitted variogram (ML)"), lty=c(1), lwd=c(2), cex=0.7)
dev.off()

# Ordinary kriging
# prepare prediction locations:
locs <- pred_grid(c(grids1km@bbox[1,1]+gridcell/2, grids1km@bbox[1,2]-gridcell/2), c(grids1km@bbox[2,1]+gridcell/2, grids1km@bbox[2,2]-gridcell/2), by=gridcell)
# ordinary kriging:
PREC.ok2 <- krige.conv(PREC.geo, locations=locs, krige=krige.control(obj.m=PREC.vgm2))  # it will automatically back-transform the values!

# compare local polynomial and OK:
PREC.ls <- PREC.ok2
mt <- as.matrix(grids1km["PREC.ls"])
PREC.ls$predict <- as.vector(mt[,dim(mt)[2]:1])
par(mfrow=c(1,2))
image(PREC.ls, loc=locs, col=gray(seq(1,0.1,l=30)), xlab="X", ylab="Y")
title(main="Local polynomial"); contour(PREC.ls, add=TRUE, nlev=8)
points(PREC.geo[[1]], pch="+", cex=.7)
image(PREC.ok2, loc=locs, col=gray(seq(1,0.1,l=30)), xlab="X", ylab="Y")
title(main="Ordinary kriging predictions"); contour(PREC.ok2, add=TRUE, nlev=8)
points(PREC.geo[[1]], pch="+", cex=.7)
dev.off()

# copy values to SPGD:
mt <- matrix(PREC.ok2$predict, ncol=grids1km@grid@cells.dim[1])
grids1km$PREC.ok <- as.vector(mt[,dim(mt)[2]:1])

# ------------------------------------------------------------
# 4. Regression-kriging in gstat
# ------------------------------------------------------------

# Zero-inflated regresion:
GSOD.20080501.ov$PREC.i <- as.integer(GSOD.20080501.ov$PREC*10)
PREC.zip <- zeroinfl(PREC.i~dem+twi+RN10501, data=GSOD.20080501.ov@data)
summary(PREC.zip)
hist(residuals(PREC.zip))  # residuals are close to normal
PREC20080501.zip <- predict(PREC.zip, grids1km@data, type="response")
max.PREC <- max(grids1km$RN10501, na.rm=TRUE)
grids1km$PREC.reg <- ifelse(PREC20080501.zip/10> max.PREC, max.PREC, PREC20080501.zip/10)

GSOD.20080501.ov <- overlay(grids1km, GSOD.20080501.XY) 
GSOD.20080501.ov@data <- cbind(GSOD.20080501@data, GSOD.20080501.ov@data)
# fit a variogram:
sel <- !is.na(GSOD.20080501.ov$PREC.reg)
plot(variogram(log1p(PREC)~PREC.reg, GSOD.20080501.ov[sel,]))
PREC.rvgm <- fit.variogram(variogram(log1p(PREC)~PREC.reg, GSOD.20080501.ov[sel,]), model=vgm(nugget=0.2, model="Exp", range=100000, psill=0.8))
PREC.rvgm
# plot(variogram(log1p(PREC)~PREC.reg, GSOD.20080501.ov[sel,]), PREC.rvgm)
# make predictions:
grids1km$PREC.rk <- expm1(krige(log1p(PREC)~PREC.reg, locations=GSOD.20080501.ov[sel,], newdata=grids1km, model=PREC.rvgm)$var1.pred)
grids1km$PREC.rk <- ifelse(grids1km$PREC.rk> max.PREC, max.PREC, grids1km$PREC.rk)
windows(width=10, height=10)
spplot(grids1km[c("PREC.reg", "PREC.rk", "PREC.ls", "PREC.ok")], at=c(seq(0,max(GSOD.20080501$PREC),max(GSOD.20080501$PREC)/40), max.PREC), col.regions=grey(c(rev(seq(0,1,0.025)), 0)), sp.layout=sp.list)
# writeGDAL(grids1km["PREC.rk"], "PREC_rk.sdat", "SAGA")

# mean annual precipitation:
GSOD.2008.ov <- overlay(grids1km, GSOD.2008year.XY) 
GSOD.2008.ov@data <- cbind(GSOD.2008year.XY@data, GSOD.2008.ov@data)
sel <- !is.na(GSOD.2008.ov$RN1)&!is.na(GSOD.2008.ov$PRECm)
# regression model:
summary(lm(log1p(PRECm)~dem+twi+RN1, GSOD.2008.ov@data))

plot(variogram(log1p(PRECm)~dem+twi+RN1, GSOD.2008.ov[sel,]))
PRECm.rvgm <- fit.variogram(variogram(log1p(PREC)~PREC.reg, GSOD.2008.ov[sel,]), model=vgm(nugget=0.05, model="Lin"))
PRECm.rvgm
# generate predictions:
grids1km$PRECm.rk <- expm1(krige(log1p(PRECm)~dem+twi+RN1, locations=GSOD.2008.ov[sel,], newdata=grids1km, model=PRECm.rvgm)$var1.pred)
# plot maps next to each other:
windows(width=10, height=6)
spplot(grids1km[c("RN1", "PRECm.rk")], at=seq(20,50,(50-20)/40)^2, col.regions=grey(c(rev(seq(0,1,0.025)), 0)), sp.layout=sp.list)



# end of script;



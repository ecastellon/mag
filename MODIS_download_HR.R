
# title         : MODIS_download_HR.R
# purpose       : Download and resampling of MODIS LST images for Croatia;
# reference     : Hengl, T. 2009. A Practical Guide to Geostatistical Mapping, 2nd Edt. University of Amsterdam, www.lulu.com, 291 p. ISBN: ISBN 978-90-9024981-0
# producer      : Prepared by T. Hengl
# last update   : In Amsterdam, NL, 4 Jul 2010.
# inputs        : year of interest;
# outputs       : daily/nigth images of LST (8-days period);
# remarks 1     : To run this script, you need to obtain and install the MODIS resampling tool from https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool;
# remarks 2     : You also need to obtain the WGET from http://users.ugent.be/~bpuype/wget/  --- simply copy the wget exe to windows system folder;

library(rgdal)
library(RCurl)
ILWIS <- "C:\\Progra~1\\N52\\SetupIlwis\\IlwisClient.exe -C"
# Obtain the MODIS tool from: http://lpdaac.usgs.gov/landdaac/tools/modis/index.asp
setwd("D:/MODIS/HR")
# location of the mosiacing tool:
MRT <- 'C:\\Progra~1\\MRT\\bin\\'
workd <- 'D:\\MODIS\\HR\\'
options(download.file.method="auto")

# download the country borders:
download.file("http://www.geomorphometry.org/data/zupanije.zip", destfile=paste(getwd(),"zupanije.zip",sep="/"))
for(j in list(".shp", ".shx", ".dbf")){
fname <- zip.file.extract(file=paste("zupanije", j, sep=""), zipname="zupanije.zip")
file.copy(fname, paste("./zupanije", j, sep=""), overwrite=TRUE)
}
unlink("zupanije.zip")
list.files(getwd(), recursive=T, full=F)

# location of the MODIS 1 km monthly blocks:
MOD11A2 <- "ftp://e4ftl01u.ecs.nasa.gov/MOLT/MOD11A2.005/"
MOD11A2a <- "ftp://anonymous:test@e4ftl01u.ecs.nasa.gov/MOLT/MOD11A2.005/"

# Borders of Croatia:

zupanije <- readShapePoly("zupanije", proj4string=CRS("+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9999 +x_0=5500000 +y_0=0 +ellps=bessel +towgs84=550.499,164.116,475.142,5.80967,2.07902,-11.62386,0.99999445824 +units=m"))
zupanije.UTM <- spTransform(zupanije, CRS("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
zupanije.ll <- spTransform(zupanije, CRS("+proj=longlat +datum=WGS84"))
lon.c <- mean(zupanije.ll@bbox[1,])
lat.c <- mean(zupanije.ll@bbox[2,])
# writeOGR(zupanije.ll, "zupanije.kml", "zupanije", "KML")
XUL <- zupanije.UTM@bbox[1,1]
YUL <- zupanije.UTM@bbox[2,2]
XLR <- zupanije.UTM@bbox[1,2]
YLR <- zupanije.UTM@bbox[2,1]

# dates <- data.frame(dirname=c("2006.01.01", "2006.01.09", "2006.01.17", "2006.01.25", "2006.02.02", "2006.02.10", "2006.02.18", "2006.02.26", "2006.03.06", "2006.03.14", "2006.03.22", "2006.03.30", "2006.04.07", "2006.04.15", "2006.04.23", "2006.05.01", 
# "2006.05.09", "2006.05.17", "2006.05.25", "2006.06.02", "2006.06.10", "2006.06.18", "2006.06.26", "2006.07.04", "2006.07.12", "2006.07.20", "2006.07.28", "2006.08.05", "2006.08.13", "2006.08.21", "2006.08.29", "2006.09.06", "2006.09.14", "2006.09.22",
#  "2006.09.30", "2006.10.08", "2006.10.16", "2006.10.24", "2006.11.01", "2006.11.09", "2006.11.17", "2006.11.25", "2006.12.03", "2006.12.11", "2006.12.19", "2006.12.27"))
# get the list of directories:
items <- strsplit(getURL(MOD11A2), "\n")[[1]]
folderLines <- items[substr(items, 1, 1)=='d']
dirs <- unlist(lapply(strsplit(folderLines, " "), function(x){x[length(x)]}))
dates <- data.frame(dirname=unlist(strsplit(dirs, "\r")))
str(dates)

dates$BLOCK1 <- rep(NA, length(dates$dirname))
dates$BLOCK2 <- rep(NA, length(dates$dirname))
# download the blocks in a loop (year 2006):
for (i in 1:length(dates$BLOCK1)){
getlist <- strsplit(getURL(paste(MOD11A2, dates$dirname[[i]], "/", sep=""), .opts=curlOptions(ftplistonly=TRUE)), "\r\n")[[1]]
BLOCK1 <- getlist[grep(getlist, pattern="MOD11A2.*.h18v04.*.hdf")[1]]
BLOCK2 <- getlist[grep(getlist, pattern="MOD11A2.*.h19v04.*.hdf")[1]]

# write up the file names back to the dates.txt:
for(j in 2:3){
   dates[i,j] <- get(paste("BLOCK", j-1, sep=""))
}

# Download all blocks from the list to a local drive:
# while(!is.na(dates[i,2])&!is.na(dates[i,3])&!is.na(dates[i,4])&!is.na(dates[i,5])&!is.na(dates[i,6])&!is.na(dates[i,7])&!is.na(dates[i,8])&!is.na(dates[i,9])&!is.na(dates[i,10])){
download.file(paste(MOD11A2a, dates$dirname[[i]], "/", BLOCK1,sep=""), destfile=paste(getwd(), "/", BLOCK1, sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD11A2, dates$dirname[[i]], "/", BLOCK2,sep=""), destfile=paste(getwd(), "/", BLOCK2,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)

# remove "." from the file name:
dirname1 <- sub(sub(pattern="\\.", replacement="_", dates$dirname[[i]]), pattern="\\.", replacement="_", dates$dirname[[i]])
# mosaic the blocks:
mosaicname <- file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")
write(paste(workd, BLOCK1, sep=""), mosaicname)
write(paste(workd, BLOCK2, sep=""), mosaicname, append=T)
close(mosaicname)
# generate temporary mosaic:
shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "0 0 0 0 1 0 0 0 0 0 0 0" -o ', workd, 'TmpMosaic.hdf', sep=""))
close(mosaicname)

# resample to UTM 33N:
filename <- file(paste(MRT, "mrt", dirname1, ".prm", sep=""), open="wt")
write(paste('INPUT_FILENAME = ', workd, 'TmpMosaic.hdf', sep=""), filename) 
write('  ', filename, append=TRUE)
# write('SPECTRAL_SUBSET = ( 0 1 0 0 0 0 0 0 0 0 0 )', filename, append=TRUE)
write('SPECTRAL_SUBSET = ( 1 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE)
write('  ', filename, append=TRUE)
write(paste('SPATIAL_SUBSET_UL_CORNER = (', XUL, YUL, ')'), filename, append=TRUE)
write(paste('SPATIAL_SUBSET_LR_CORNER = (', XLR, YLR, ')'), filename, append=TRUE)
write('  ', filename, append=TRUE)
write(paste('OUTPUT_FILENAME = ', workd, 'LST', dirname1, '.tif', sep=""), filename, append=TRUE)
write('  ', filename, append=TRUE)
write('RESAMPLING_TYPE = NEAREST_NEIGHBOR', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PROJECTION_TYPE = UTM', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE)
write(paste(lon.c, lat.c, '0.0'), filename, append=TRUE)
write(' 0 0.0 0.0', filename, append=TRUE)
write(' 0.0 0.0 0.0', filename, append=TRUE)
write(' 0.0 0.0 0.0', filename, append=TRUE)
write(' 0.0 0.0 0.0 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('UTM_ZONE = 33', filename, append=TRUE)
write('DATUM = WGS84', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PIXEL_SIZE = 1000', filename, append=TRUE)
write('  ', filename, append=TRUE)
close(filename)

# Mosaic the images to get the whole area:
shell(cmd=paste(MRT, 'resample -p ', MRT, 'mrt', dirname1, '.prm', sep=""))
# delete all hdf files!
# unlink(paste(getwd(), '/', BLOCK1, sep=""))
# unlink(paste(getwd(), '/', BLOCK2, sep=""))
}
#}

# Check the validity:
# GDALinfo("tmp2000_08_12.250m_16_days_EVI.tif")

# Principal component analysis:

tif.list <- list.files(getwd(), pattern=glob2rx("*.tif"))
mpr.list <- rep(NA, length(tif.list))

# import to ILWIS:
download.file("http://spatial-analyst.net/CRS/UTM33N.csy", destfile=paste(getwd(),"UTM33N.csy", sep="/"))
for(i in 1:length(tif.list)){
mpr.list[i] <- strsplit(tif.list[[i]], "_")[[1]][1]
shell(cmd=paste(ILWIS, "import tiff(", tif.list[[i]], ",", mpr.list[i], ")"), wait=F)
shell(cmd=paste(ILWIS, " setcsy ",  mpr.list[i], ".grf UTM33N.csy -force", sep=""), wait=F)
}

# create a maplist:
shell(cmd=paste(ILWIS, "crmaplist evi_list", paste(mpr.list, collapse=".mpr ")), wait=F)
# run PCA:
shell(cmd=paste(ILWIS, "princcmp evi_list.mpl"),  wait=F)
shell(cmd=paste(ILWIS, "pcevi.mat = MatrixPrincComp(evi_list, 4)"), wait=F)

# end of script!

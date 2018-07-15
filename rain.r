#--2011-06-22
setwd("c:/eddy/wp/lluvia/rd")
source("rainana.r")
DIRLLU <- "c:/almacen/lluvia/"
#
op <- par(no.readonly=T);dev.off()
grafdir <- "c:/eddy/wp/lluvia/graf/"
grafile <- function(sf) paste(grafdir,sf,".pdf",sep="")
grafpdf <- function(file,w=5,h=5,ps=8,ff="Times",title=""){
  if(!missing(file))
    pdf(file=grafile(file),width=w,height=h,family=ff,pointsize=ps,
        title=title,encoding="ISOLatin1")
}
#distribucion
#--masaya
w <- datos.agroclim(paste(DIRLLU,"masaya.llu",sep=""),
                    file="masaya.rda")
#-los datos de 19621016 a 19621018 parece que fueron editados y
#puestos a la misma cantidad 19mm
w <- subset(w,years(w[,1])!=2011)#todos =0 en 2011
z <- acumulado.mesano(w)
#--sebaco
ws <- datos.agroclim(paste(DIRLLU,"sebaco.llu",sep=""),
                     file="sebaco.rda")
zs <- acumulado.mesano(ws)
#--san isidro
ws <- datos.agroclim(paste(DIRLLU,"sanisi.llu",sep=""),
                     file="sanisi.rda")
zs <- acumulado.mesano(ws)
#--quilali
wq <- datos.agroclim(paste(DIRLLU,"quilal.llu",sep=""),
                     file="quilal.rda")
zq <- acumulado.mesano(wq)
#--palacaguina
wp <- datos.agroclim(paste(DIRLLU,"palaca.llu",sep=""),
                     file="palaca.rda")
zp <- acumulado.mesano(wp)
#--jalapa
wj <- datos.agroclim(file.agroclim("jalapa"))
#zj <- acumulado.mesano(wj)
wj <- subset(wj,years(wj[,1])!=2008)#45 años
zj <- acumulado.mesano(wj)
#
library("MASS")
nclass.Sturges(zj$ppano)#7
nclass.scott(zj$ppano)#5
nclass.FD(zj$ppano)#7
#
grafpdf(file="jalhisppano1",w=3,h=3)
par(mai=c(0.3,0.3,0.2,0.1))
hist(zj$ppano,breaks="Sturges",freq=FALSE,main="",xlab="",ylab="",
     plot=T,col=gray(0.96))
dev.off()
#
grafpdf(file="jalhisppano2",w=3,h=3)
par(mai=c(0.3,0.3,0.2,0.1))
hist(zj$ppano,breaks="Scott",freq=FALSE,main="",xlab="",ylab="",
     plot=T,col=gray(0.96))
dev.off()
#
truehist(zj$ppano,nbins="FD",col=gray(0.98),xlab="")
truehist(zj$ppano,col=gray(0.98),xlab="")
truehist(zj$ppdiano,nbins="FD",col=gray(0.98),xlab="")
with(subset(zj$ppmes,mes=="May"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="")})
with(subset(zj$ppmes,mes=="Jul"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="")})
with(subset(zj$ppmes,mes=="Oct"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="")})
with(subset(zj$ppdiames,mes=="May"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="",main="may")})
with(subset(zj$ppdiames,mes=="Jun"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="",main="jun")})
with(subset(zj$ppdiames,mes=="Jul"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="",main="jul")})
with(subset(zj$ppdiames,mes=="Oct"),
     {truehist(ppmm,nbins="FD",col=gray(0.98),xlab="",main="oct")})
#
bw.nrd0(zj$ppano)
bw.SJ(zj$ppano,method="dpi")#158
#
d <- density(zj$ppano,width="SJ-dpi",n=256)#bw=158
grafpdf(file="jalkerppano1",w=3,h=3)
par(mai=c(0.3,0.3,0.1,0.1))
plot(d,ann=F,type="n")
rug(zj$ppano)
lines(d,ann=F)
dev.off()
#
d$x#puntos de evaluacion del kernel
grafpdf(file="jalkerppano2",w=3,h=3)
par(mai=c(0.3,0.3,0.2,0.1))
hist(zj$ppano,breaks="Sturges",freq=FALSE,main="",xlab="",ylab="",
     plot=T,col=gray(0.96),ann=F,xlim=c(400,2400))#despues de d$x
lines(d,ann=F)
dev.off()
#
d <- density(zj$ppano,bw=80,n=256)
grafpdf(file="jalkerppano3",w=3,h=3)
par(mai=c(0.3,0.3,0.2,0.2))
plot(d,ann=F,type="n")
rug(zj$ppano,col=gray(0))
lines(d,ann=F)
dev.off()
par(op)
#
y <- subset(zj$ppdiames,mes=="Oct")
d <- with(y,{density(ppmm,width="SJ-dpi",n=256,window="biweight")})
d <- with(y,{density(ppmm,width="SJ-dpi",n=256,window="gaussian")})#bw=2.31
grafpdf(file="jalppdiaoct",w=3,h=3)
par(mai=c(0.3,0.3,0.2,0.1))
#with(y,{truehist(ppmm,nbins="FD",col=gray(0.96),ann=F,xlim=c(0,50))})
plot(d,ann=F,type="n")
rug(y$ppmm,col="blue")
lines(d,ann=F)
dev.off()
#
d <- with(subset(zj$ppdiames,mes=="Oct"),
          {density(ppmm,width="SJ-dpi",n=256)})
#
curve(dnorm(x,mean=mean(zj$ppano),sd=sd(zj$ppano)),col="blue",lty=2,ann=F)
plot(function(x)dt(x,df=44),-4,4,ann=F,add=T)
lines(d,ann=F)
#


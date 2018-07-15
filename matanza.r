setwd("c:/eddy/wp/matanza/rdata/")
#abr-2008
source("c:/encuestas/programas/rprog/clasequery.r")
#source("c:/encuestas/rprog/clamet.r")
library("lattice")
library("latticeExtra")
library("directlabels")
library("ggplot2")
library("grid")
library("Cairo")
library("RColorBrewer")
library("zoo")
library("dlm")
dlmAIC <- function(outputMLE,k=2,npar=0,dlmMod){
    if(npar==0&(missing(dlmMod)))
        stop("falta parametros")
    if(npar==0)
        npar <- sum(diag(dlmMod(GG))!=0)
    return(2*outputMLE$value+k*npar)
}
library("TSA")
library("pear")
library("mFilter")
library("dvfBm")
#graficas
directoriogrf <- "c:/eddy/wp/matanza/grf/"
filegraf <- function(sf) paste(directoriogrf,sf,".pdf",sep="")
grfserie <- function(sf,serie,gw=2.45,gh=1.8,ejey=""){
  sf <- paste(directoriogrf,sf,".pdf",sep="")
  pdf(file=sf,family="Times",pointsize=8,paper="special",title="",
      width=gw,height=gh)
  par(mai=c(0.33,0.33,0.2,0.08))
  #par(pin=c(gw,gh))
  #par(fin=c(gw+0.55+0.08+0.1,gh+0.33+0.08+0.1))
  #print(par("plt"))
  #print(par("pin"))
  #print(par("fin"))
  plot(serie,main="",xlab="",ylab="")
  symbols(serie,circles=rep(0.2,length(serie)),add=TRUE,
          inches=FALSE,bg="white")
  mtext(ejey,side=3,line=0,adj=0,cex=1)
  dev.off()
}
#grafica serie(regular) con banking
rbank <- function(serie){
  require("lattice")
  i <- is.na(serie)
  #burda correccion por los NA; ideal interpolar primero intentando 45grados
  serie[i] <- mean(serie[!i])
  sd <- diff(serie)
  ny <- length(sd)
  #R extiende el rango en 4% para acomodar los ticks
  invisible(1.04*banking(rep(1,ny),sd)*((max(serie)-min(serie))/ny))
}
#
#olx:orientacion label eje;0 paralelo,1 horizontal
#ltk:long.tick.marks
gserie <- function(na=NA,serie,ov=FALSE,ejey="",gw=2,mb=0.3,ml=0.3,mt=0.15,
           mr=0.1,maw=0,mah=0,ps=8,ole=NA,ltk=NA,ejeytop=T){
  gh <- gw*rbank(serie)
  gwt <- gw+ml+mr+maw
  ght <- gh+mb+mt+mah
  if(!is.na(na)){
    na <- paste(directoriogrf,na,".pdf",sep="")
    pdf(file=na,family="Times",pointsize=ps,paper="special",
        title="",width=gwt,height=ght,encoding="ISOLatin1")
  }
  #par(pin=c(gw,gh))
  par(mai=c(mb,ml,mt,mr))
  par(omi=c(0,0,0,0))
  if(is.na(ole))ole <- par("las")
  if(is.na(ltk))ltk <- par("tcl")
  ylab <- ifelse(ejeytop,"",ejey)
  plot(serie,main="",xlab="",ylab=ylab,type="l",cex.axis=0.7,las=ole,tcl=ltk)
  if(ov) symbols(serie,circles=rep(0.2,length(serie)),add=TRUE,
                 inches=FALSE,bg="white")
  if(ejeytop) mtext(ejey,side=3,line=0,adj=0,cex=0.7)
  if(!is.na(na)) dev.off()
}
#series multiples
gseriem <- function(na=NA,serie,orden=0,ids="",ejey="",gw=2,mb=0.3,
                    ml=0.3,mt=0.15,mr=0.1,meg=0,maw=0,mah=0,
                    ps=8,cexeje=0.7,cexlab=0.7,cextop=0.7,ov=FALSE){
  if(!is.mts(serie)){
    print("no es multiple")
    return(0)
  }
  np <- attr(serie,"dim")[1]
  ns <- attr(serie,"dim")[2]
  if(length(orden)<ns)orden <- 1:ns
  if(length(ids)<ns)ids <- letters[1:ns]
  rg <- rep(0,ns)
  for(i in 1:ns)rg[i] <- rbank(serie[,i])*gw
  #mh <- max(rg)
  gh <- sum(rg)
  #gh <- mh*ns
  gwt <- gw+ml+mr+maw
  ght <- gh+mb+mt+mah+meg*(ns-1)
  if(!is.na(na)){
    na <- paste(directoriogrf,na,".pdf",sep="")
    pdf(file=na,family="Times",pointsize=ps,paper="special",title="",
        width=gwt,height=ght,encoding="ISOLatin1")
  }
  #par(pin=c(gw,gh))
  #par(mai=c(mb,ml,mt,mr))
  hg <- rg
  hg[orden[1]] <- hg[orden[1]]+mt
  hg[orden[ns]] <- hg[orden[ns]]+mb
  #if(ns>2) for(i in 2:(ns-1))hg[i] <- hg[i]+meg
  #hg <- hg/ght
  lyo <- rep(0,2*ns-1)
  lyh <- rep(lcm(meg*2.54),2*ns-1)
  for(i in 1:ns)lyo[2*i-1] <- i
  for(i in 1:ns)lyh[2*i-1] <- lcm(hg[orden[i]]*2.54)
  layout(matrix(lyo,ncol=1),heights=lyh)
  par(mgp=c(2,1,0))
  #par(type="l")
  par(cex.axis=cexeje)
  par(cex.lab=cexlab)
  par(omi=c(0,0,0,0))
  #par(pin=c(gw,rg[orden[1]]))
  par(mai=c(0,ml,mt,mr))
  j <- orden[1]
  plot(serie[,j],main="",xaxt="n",ylab=ids[j],type="l")
  if(ov) symbols(serie[,j],circles=rep(0.2,np),add=TRUE,
                 inches=FALSE,bg="white")
  mtext(ejey,side=3,line=0,adj=0,cex=cextop)
  if(ns>2){
    for(i in 2:(ns-1)){
      #par(pin=c(gw,rg[orden[i]]))
      par(mai=c(0,ml,0,mr))
      j <- orden[i]
      plot(serie[,j],main="",xaxt="n",ylab=ids[j],type="l")
      if(ov) symbols(serie[,j],circles=rep(0.2,np),add=TRUE,
                     inches=FALSE,bg="white")
    }
  }
  #par(pin=c(gw,rg[orden[ns]]))
  par(mai=c(mb,ml,0,mr))
  j <- orden[ns]
  plot(serie[,j],main="",xlab="",ylab=ids[j],type="l")
  if(ov) symbols(serie[,j],circles=rep(0.2,np),add=TRUE,
                 inches=FALSE,bg="white")
  if(!is.na(na)) dev.off()
}
#lattice
tpdf <- canonical.theme("pdf")#opciones pdf
initre <- function(fsize=8,bg=0,stripbg=gray(0.9),color=1){
  pf<-trellis.par.get("fontsize")
  pf$text <- fsize
  trellis.par.set("fontsize",pf)
  pbg <- trellis.par.get("background")
  pbg$col <- bg
  trellis.par.set("background",pbg)
  psbg <- trellis.par.get("strip.background")
  psbg$col <- stripbg
  trellis.par.set("strip.background",psbg)
  ppl <- trellis.par.get("plot.line")
  ppl$col <- color
  trellis.par.set("plot.line",ppl)
}
initrepdf <- function(sf,gw,gh){
  trellis.device("pdf",file=filegraf(sf),width=gw,height=gh,
                 paper="special",family="Times",encoding="ISOLatin1",
                 onefile=TRUE,title="")
  initre()
}
#Cleveland-grafica de las subseries mensuales
#clesta es la funcion principal que llama a ge
#parametros
#fs:fontsize; mb:margen inferior; ml margen izquierdo (lineas)
#wp,hp: ancho,alto del area de grafica en porcentaje de la pagina
#ww: ancho del area destinada a la grafica de cada mes
#como porcentaje del area asignada por layout
#hw: alto del area de la grafica del mes, en unidades del eje Y
#clx: factor de expansion del ancho de linea
#tip: grafica del mes:bar: segmentos verticales; lineas en otro caso
ge <- function(ss,mmes,num,ysc,wvp,hvp,flx,typ="bar"){
  nvp <- paste("vp",num,sep="")
  nd <- length(ss)
  pushViewport(viewport(name=nvp,
                        layout.pos.row=1,layout.pos.col=num,yscale=ysc))
  pushViewport(viewport(x=0.05,y=unit(mmes,"native"),just="left",
                        width=wvp,height=unit(hvp,"native"),
                        yscale=extendrange(ss),
                        xscale=extendrange(1:nd)))
  cvp <- current.viewport()
  upViewport()
  if(typ=="bar"){
    grid.segments(vp=cvp,x0=unit(1:nd,"native"),y0=unit(mmes,"native"),
                x1=unit(1:nd,"native"),y1=unit(ss,"native"),
                gp=gpar(lex=flx))
  }
  else
    grid.lines(vp=cvp,x=unit(1:nd,"native"),y=unit(ss,"native"),
               gp=gpar(lex=flx))
  upViewport()
  invisible(c(nvp,cvp))
}
clesta <- function(serie,nomes=NA,fs=8,mb=2,ml=2,wp=0.9,hp=0.9,ww=0.9,hw=1.1,clx=0.8,tip="bar"){
  stopifnot(class(serie)=="ts")
  stopifnot(attr(serie,"dim")[2]==1)
  stopifnot((fr <- frequency(serie))>1)
  np <- length(serie)/fr
  ff <- factor(rep(1:fr,np))
  if(!is.na(nomes)&(length(nomes)==fr))levels(ff) <- nomes
  mm <- tapply(serie,ff,mean,trim=0.25)
  s.serie <- split(serie,ff)
  ys <- extendrange(c(max(mm)+hw,min(mm)-hw))
  require("grid")
  grid.newpage()
  vpg <- viewport(name="vpg",width=wp,height=hp,
                  x=unit(ml,"lines"),y=unit(mb,"lines"),
                  just=c("left","bottom"),gp=gpar(fontsize=fs),yscale=ys,
                  layout=grid.layout(ncol=fr,widths=unit(rep(1,fr),"null")))
  pushViewport(vpg)
  grid.rect()
  grid.yaxis(name="ey")
  nvp <- list()
  for(i in 1:fr)
    nvp[[i]] <- ge(s.serie[[i]],mm[i],i,ys,wvp=ww,hvp=hw,flx=clx,typ=tip)
  invisible(nvp)
}
#---
cd <- dbase("pecuario")
#matanza anual
cq <- query()
cq@dbase <- "pecuario"
cq@sql <- "select * from totanu"
z <- datos(query("select * from totanu","pecuario"))
w <- datos(query("select * from totind","pecuario"))
#a formato bats
y <- within(z,cbtoan <- round(cbtoan/1000,0))
y <- within(y,millb <- round(milbtoan/1000,0))
y <- within(y,millbxp <- round(milbxpan/1000,0))
y <- within(y,millus <- round(milusan/1000,0))
w <- within(w,cbinan <- round(cbinan/1000,0))
w <- within(w,millbin <- round(milbinan/1000,0))
fu <- file("mata.dat",open="w")
cat(c("\\matanza anual (total e industrial)","\\magfor"),file=fu,sep="\n")
cat(paste("\\","total,millbtot,millbxp,millus,indus,millbin","\n",sep=""),file=fu)
cat(c("\\YEAR","\\YEAR","\\1","\\1","\\1960","\\1","\\2007","\\48"),file=fu,sep="\n")
for(i in 1:48){cat(c("\\",as.integer(y[i,c(1,5:7)]),as.integer(w[i,c(1,3)])),file=fu);cat("\n",file=fu)}
close(fu)
#fin formato bats
#formato para leer con ox-ssfpack
y <- round(z$cbtoan/1000,0)
fu <- file("mtanual.dat",open="w")
cat(c(length(y),1,"\n"),file=fu)
for(i in 1:length(y)) cat(y[i],file=fu,sep="\n")
close(fu)
#fin ox
s.mta <- ts(z$cbtoan,start=1960)
s.mti <- ts(w$cbinan,start=1960)
a <- rep(start(s.mta)[1]:end(s.mta)[1],2)
f <- rep(c("matanza total","matanza industrial"),
         c(length(s.mta),length(s.mti)))
initrepdf("matanza",2.5,3.8)
xyplot(c(s.mta,s.mti)~a|f,type="l",layout=c(1,2),aspect="xy",
       main="",xlab="",ylab="mil cabezas")
dev.off()
#-matanza municipal,industrial
a <- rep(start(s.mta)[1]:end(s.mta)[1],3)
f <- factor(rep(1:3,rep(length(s.mta),3)))
levels(f) <- c("total","industrial","municipal")
initrepdf("matanza",2.5,3)
xyplot(c(round(s.mta/1000,0),round(s.mti/1000,0),
         round((s.mta-s.mti)/1000,0))~a|f,type="l",
       layout=c(1,3),aspect="xy",main="",xlab="",ylab="mil cabezas",
       par.settings=list(text=7),
       scales=list(tck=0.5))
dev.off()
#tasas de crecimiento por año
s.mtc <- diff(log(s.mta))
s.mic <- diff(log(s.mti))
f <- factor(rep(1:2,rep(length(s.mtc),2)),label=c("total","industrial"))
a <- rep(start(s.mtc)[1]:end(s.mtc)[1],2)
initrepdf("tcre",2.3,2)
xyplot(c(s.mtc,s.mic)~a|f,type="h",layout=c(2,1),aspect=1,
       main="",xlab="",ylab="",scales=list(tck=0.5))
dev.off()
#---mensual-industrial---------
z <- datos(query("select * from cbinms","pecuario"))
s.mims <- ts(z$cbinms/1000,start=c(1970,1),freq=12)
#años completos
if((length(s.mims)%%12)!=0){
  s.mims <- window(s.mims,end=c(end(s.mims)[1]-1,12))
}
#
jul79 <- which(time(s.mims)==(1979+6/12))
s.mims[jul79] <- NA #jul.79
#alternativa
s.mims[jul79] <- predict(HoltWinters(window(s.mims,end=c(1979,6)),
                              seasonal="multiplicative"))
#
afin <- end(s.mims)[1]
anos <- afin-start(s.mims)[1]+1
mes <- as.numeric(cycle(s.mims))
meses <- factor(mes)
levels(meses) <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
ano <- factor(rep(start(s.mims)[1]:afin,rep(12,anos))[1:length(s.mims)])
a2 <- rep(start(s.mims)[1]:afin,rep(12,anos))[1:length(s.mims)]
initrepdf("mims",3.5,2.8)
xyplot(s.mims~mes|ano,layout=c(10,ceiling(anos/10)),type="l",aspect="xy",
       main=NULL,xlab="mes",ylab="mil cabezas",strip=strip.custom(par.strip.text=list(cex=0.8)),scales=list(tck=0.5,cex=0.8))
dev.off()
#descomposicion
s.mims[jul79] <- predict(HoltWinters(window(s.mims,end=c(1979,6)),
                              seasonal="multiplicative"))
w <- stl(s.mims,s.window=25,robust=T)
pdf(file=paste(directoriogrf,"midescom.pdf",sep=""),width=4,
    height=4,pointsize=8)
plot(w,labels=c("mil cabezas","estacionalidad","tendencia","residuos"),
     set.pars=list(mar=c(0,5,0,2.5),oma=c(2.5,0,1,0),tck=-0.01,mfrow=c(4,1)))
dev.off()
#grafica Cleveland de componente estacional
monthplot(w,choice="seasonal")#referencia la media
#para usar otra referencia
mp <- function(y)mean(y,trim=0.25)
monthplot(w,choice="seasonal",base=mp)#referencia la mid-media
monthplot(w,choice="seasonal",base=mp,type="h")#rayas
library("robustbase")
mp <- function(y)huberM(y)
#
mes3 <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct",
          "nov","dic")
pdf(file=paste(directoriogrf,"miestacion.pdf",sep=""),width=4,height=3.7,pointsize=8)
par(mar=c(2.5,4,1,1))
monthplot(w,choice="seasonal",base=mean,labels=mes3,ylab="estacionalidad")
dev.off()
#la grafica para valorar s.windows
u <- w$time.series
mu <- tapply(u[,"seasonal"],meses,mean)
rmu <- rep(mu,anos)[1:length(u[,1])]
ru <- u[,"seasonal"]-rmu
rur <- ru+u[,"remainder"]
xyplot(ru+rur~a2|meses)
#el moving range (mean) para analizar la transformacion de potencia
mrango <- function(serie,fr=12){
  n <- length(serie)
  ns <- as.numeric(rep(NA,n))
  for(i in fr:n)ns[i] <- diff(range(serie[(i-fr+1):i]))
  invisible(ts(ns,start=start(serie),frequency=fr))
}
mmedia <- function(serie,fr=12){
  n <- length(serie)
  ns <- as.numeric(rep(NA,n))
  for(i in fr:n)ns[i] <- mean(serie[(i-fr+1):i],na.rm=T)
  invisible(ts(ns,start=start(serie),frequency=fr))
}
#por mes
mm <- tapply(s.mims,meses,mean,trim=0.25)
ss <- matrix(c(rep(0,12),round(mm,0)),byrow=F,ncol=2)
##-Hylleberg
gHylleberg <- function(s.mims){
  afin <- end(s.mims)[1]
  if((length(s.mims)%%12)!=0){
    afin <- end(s.mims)[1]-1
    s.mims <- window(s.mims,end=c(afin,12))
  }
  anos <- afin-start(s.mims)[1]+1
  meses <- factor(rep(1:12,anos),
                  label=c("enero","febrero","marzo","abril","mayo",
                    "junio","julio","agosto","septiembre","octubre",
                    "noviembre","diciembre"))
  ma <- tapply(s.mims,rep(1:anos,rep(12,anos)),mean,na.rm=T)
  s.cimsnor <- s.mims-rep(ma,rep(12,anos))
  mm <- tapply(s.mims,meses,mean,na.rm=T)
  ss <- matrix(c(rep(0,12),round(mm,0)),byrow=F,ncol=2)
  a2 <- rep(start(s.mims)[1]:afin,rep(12,anos))
  xyplot(s.cimsnor~a2|meses,layout=c(3,4),type="h",
         main=NULL,xlab=NULL,ylab=NULL,
         strip=strip.custom(shingle.intervals=ss,strip.levels=c(F,T),
           strip.names=c(T,F),fg=gray(0.75),bg=gray(0.85),
           par.strip.text=list(cex=0.8)),
         scales=list(tck=0.5,cex=0.8))
}
#años completos
if((length(s.mims)%%12)!=0){
  afin <- end(s.mims)[1]-1
  s.mims <- window(s.mims,end=c(afin,12))
  anos <- afin-start(s.mims)[1]+1
  meses <- factor(rep(1:12,anos),
                  label=c("enero","febrero","marzo","abril","mayo",
                    "junio","julio","agosto","septiembre","octubre",
                    "noviembre","diciembre"))
}
ma <- tapply(s.mims,rep(1:anos,rep(12,anos)),mean,na.rm=T)
s.cimsnor <- s.mims-rep(ma,rep(12,anos))
mm <- tapply(s.mims,meses,mean,na.rm=T)
ss <- matrix(c(rep(0,12),round(mm,0)),byrow=F,ncol=2)
a2 <- rep(start(s.mims)[1]:afin,rep(12,anos))
initrepdf("mimesdesv",3,3)
xyplot(s.cimsnor~a2|meses,layout=c(3,4),type="h",
       main=NULL,xlab=NULL,ylab=NULL,
       strip=strip.custom(shingle.intervals=ss,strip.levels=c(F,T),
         strip.names=c(T,F),fg=gray(0.75),bg=gray(0.85),
         par.strip.text=list(cex=0.8)),
       scales=list(tck=0.5,cex=0.8))
dev.off()
##-cleveland
s.cimsnor <- s.mims-rep(mm,anos)
initrepdf("mimesta",3,3)
xyplot(s.cimsnor~a2|meses,layout=c(3,4),type="h",strip=strip.custom(shingle.intervals=ss,strip.levels=c(F,T),strip.names=c(T,F),fg=gray(0.9),bg=gray(1.0),par.strip.text=list(cex=0.8)),main=NULL,xlab=NULL,ylab=NULL,scales=list(tck=0.5,cex=0.8))
dev.off()
#logaritmos
s.w <- log(s.mims)
wm <- tapply(s.w,meses,mean,trim=0.25)
ssw <- matrix(c(rep(0,12),mm),byrow=F,ncol=2)
s.wn <- s.w-rep(wm,anos)
initrepdf("mimestalog",3,3)
xyplot(s.wn~a2|meses,layout=c(3,4),type="h",strip=strip.custom(shingle.intervals=ssw,strip.levels=c(F,T),strip.names=c(T,F),fg=gray(0.9),bg=gray(1.0),par.strip.text=list(cex=0.8)),main=NULL,xlab=NULL,ylab=NULL,scales=list(tck=0.5,cex=0.8))
dev.off()
#-----produccion de carne y rendimiento
#anual
z <- datos(query("select * from totanu","pecuario"))
w <- datos(query("select * from totind","pecuario"))
#-grafica produccion de carne industrial,municipal
s.carne <- ts(round(z$milbtoan/1000,0),start=1960,frequency=1)
s.carnei <- ts(round(w$milbinan/1000,0),start=1960,frequency=1)
s.carnem <- ts(round((z$milbtoan-w$milbinan)/1000,0),start=1960,frequency=1)
a <- rep(start(s.carne)[1]:end(s.carne)[1],3)
f <- factor(rep(1:3,rep(length(s.carne),3)))
levels(f) <- c("total","industrial","municipal")
ylab <- "millón libras"
Encoding(ylab) <- "UTF-8"
initrepdf("carne",2.5,3.8)
xyplot(c(s.carne,s.carnei,s.carnem)~a|f,type="l",
       layout=c(1,3),aspect="xy",main="",xlab="",ylab=ylab,
       strip=strip.custom(par.strip.text=list(cex=0.8)),
       par.settings=list(text=7),
       scales=list(tck=0.5))
dev.off()
#-grafica rendimiento promedio anual
s.ren <- ts(454*z$milbtotan/z$cbtoan,start=1960,frequency=1)
a <- start(s.ren)[1]:end(s.ren)[1]
anos <- "años"
Encoding(anos) <- "UTF-8"
xyplot(s.ren~a,aspect="xy",type="o",xlab=anos,ylab="kg/cabeza",main="",
       cex=0.5)
gserie(na="ren",serie=s.ren,ov=T,ejey="kg/cabeza",ltk=-0.3)
#industrial
z <- datos(query("select * from totind","pecuario"))
s.pci <- ts(z$milbinan/1000,start=1960,frequency=1)
s.rci <- ts(454*z$milbinan/z$cbinan,start=1960,frequency=1)
gserie("rci",s.rci,ejey="kg/cbz",ov=T,ltk=-0.3)
a <- start(s.rci)[1]:end(s.rci)[1]
xyplot(s.rci~a,aspect="xy",type="l",xlab="",ylab="",main="")
#grafica carne-rendimiento
gseriem(na="prci",serie=cbind(s.pci,s.rci),ids=c("millon lb","kg/cabeza"),
        gw=2.5,meg=0,ps=10)
#peso canal,peso en pie
x <- datos(query("select * from pcind","pecuario"))
w <- datos(query("select * from ppind","pecuario"))
s.pcc <- ts(x,start=1975,frequency=1)
s.ppi <- ts(w,start=1975,frequency=1)
gseriem("ppi",s.ppi,ejey="kg/cabeza",meg=0.08,orden=c(3,2,1),
        ids=c("media","macho","hembra"))
gseriem("pcc",s.pcc,ejey="kg/cabeza",meg=0.05,orden=c(3,2,1),
        ids=c("media","macho","hembra"))
#comparacion peso en pie, rendimiento de carne
s.rci2 <- window(s.rci,start=1975)
s.pp <- ts(w$ppind,start=1975,frequency=1)
plot(cbind(s.rci2,s.pp),plot.type="multiple")
s.pp[3] <- 375#correccion arbitraria despues de ver la gráfica
plot(s.pp,s.rci2,xy.labels=F,pch=".",
     main=NULL,xlab="peso en pie kg/cabeza",ylab="carne kg/cabeza")
lines(lowess(s.pp,s.rci2,f=0.7))
i <- (s.pp>395)#para agregar años a datos "extremos"
time(s.pp)[i]
text(s.pp[i],s.rci2[i],c("1990","1991","1992","1999"),cex=0.7,pos=1)
i <- s.rci2>164
time(s.pp)[i]
text(s.pp[i],s.rci2[i],c("2006","2007"),cex=0.7,pos=1)
#grafica pdf
pdf(file=paste(directoriogrf,"renpesopie.pdf",sep=""),width=3,height=2.8,pointsize=8)
par(mar=c(4.8,4,1,1))
plot(s.pp,s.rci2,xy.labels=F,pch=20,main=NULL,xlab="peso en pie kg/cabeza",ylab="carne kg/cabeza",tcl=-0.3,pty=1)
lines(lowess(s.pp,s.rci2,f=0.7))
i <- (s.pp>395)
text(s.pp[i],s.rci2[i],c("1990","1991","1992","1999"),cex=0.7,pos=c(1,2,1,2))
i <- s.rci2>164
text(s.pp[i],s.rci2[i],c("2006","2007"),cex=0.7,pos=c(1,4))
dev.off()
#peso en pie,peso en canal
s.pc <- ts(x$pcind,start=1975,frequency=1)
plot(s.pp,s.pc,xy.labels=F)
lines(lowess(s.pp,s.pc,f=0.7))
#peso canal,rendimiento
plot(s.pc,s.rci2,xy.labels=F)
lines(lowess(s.pc,s.rci2,f=0.7))
#machos matanza industrial
m <- datos(query("select * from machos","pecuario"))
w <- datos(query("select * from totind","pecuario"))
z <- datos(query("select * from totanu","pecuario"))
s.mti <- window(ts(w$cbinan,start=1960,frequency=1),start=1975)
s.pmi <- 100*round(ts(m$manind,start=1975,frequency=1)/s.mti,2)
s.mta <- window(ts(z$cbtoan,start=1960,frequency=1),start=1975)
s.pmm <- 100*(1.0-round(ts(m$mantot,start=1975,frequency=1)/s.mta,2))
gseriem("pmachos",gw=3,serie=cbind(s.pmi,s.pmm),ejey="porcentaje de machos",
        meg=0.08,ids=c("industrial","municipal"),ps=12,ml=0.4)
#-----exportaciones
xano <- datos(query("select * from totanu","pecuario"))
xmes <- datos(query("select * from exportames","pecuario"))
s.xano <- ts(xano$milbxpan,start=1960,frequency=1)
s.prod <- ts(xano$milbtoan,start=1960,frequency=1)
plot(round(s.xano/1000,0),type="o")
gserie("xanocarne",round(s.xano/1000,0),ejey="millon libras",ov=T,ltk=-0.3)
s.xcre <- diff(log(s.xano))
plot(100*round(s.xano/s.prod,2),type="n")
abline(h=50,lty=3)
lines(100*round(s.xano/s.prod,2),type="o")
#grafica exportaciones del mes
s.xmes <- window(ts(xmes$peso,start=1969,frequency=12),end=c(2007,12))
anos <- end(s.xmes)[1]-start(s.xmes)[1]+1
fanos <- factor(rep(start(s.xmes)[1]:end(s.xmes)[1],rep(12,anos)))
meses <- rep(1:12,anos)
initrepdf("xmescarne",3.5,2.8)
xyplot(s.xmes~meses|fanos,layout=c(10,ceiling(anos/10)),type="l",aspect="xy",
       main=NULL,xlab="mes",ylab="millon libras",scales=list(tck=0.5,cex=0.8),
       strip=strip.custom(par.strip.text=list(cex=0.8)))
dev.off()
#exporta anual a formato bats
wb <- within(xano,{peso <- round(milbxpan/1000,0);
                 valor <- round(milusan/1000,0)})
fu <- file("xacarne.dat",open="w")
cat(c("\\exportaciones anuales de carne:millon lb,millon US","\\magfor"),
    file=fu,sep="\n")
cat(paste("\\","peso,valor","\n",sep=""),file=fu)
cat(c("\\YEAR","\\YEAR","\\1","\\1","\\1960","\\1","\\2007","\\48"),
    file=fu,sep="\n")
for(i in 1:48){
  cat(c("\\",as.numeric(wb[i,"peso"]),as.numeric(wb[i,"valor"])),file=fu)
  cat("\n",file=fu)
}
close(fu)
#exporta mes a formato bats
wb <- within(xmes,{peso <- round(peso/1000,2);
                    valor <- round(valor/1000,2)})
fu <- file("xmcarne.dat",open="w")
cat(c("\\exportaciones mensuales de carne","\\magfor"),file=fu,sep="\n")
cat(paste("\\","peso,valor","\n",sep=""),file=fu)
cat(c("\\MONTH","\\YEAR","\\12","\\1","\\1969","\\3","\\2008","\\471"),
    file=fu,sep="\n")
for(i in 1:471){
  cat(c("\\",as.numeric(wb[i,1]),as.numeric(wb[i,2])),file=fu)
  cat("\n",file=fu)
}
close(fu)
#precios mensuales
s.xpmes <- window(ts(round(100*xmes$valor/xmes$peso,0),start=1969,
                     frequency=12),end=c(2007,12))
anos <- end(s.xpmes)[1]-start(s.xpmes)[1]+1
fanos <- factor(rep(start(s.xpmes)[1]:end(s.xpmes)[1],rep(12,anos)))
meses <- rep(1:12,anos)
initrepdf("xmesprecio",3.2,3.5)
xyplot(s.xpmes~meses|fanos,layout=c(10,ceiling(anos/10)),type="l",aspect="xy",
       main=NULL,xlab="mes",ylab="centavo-dolar/libra",
       scales=list(tck=0.5,cex=0.8),
       strip=strip.custom(par.strip.text=list(cex=0.8)))
dev.off()
#descomposicion precios mensuales
w <- stl(window(s.xpmes,start=c(1989,1)),s.window=25,robust=T)
lab <- c("centavo/lb","estacionalidad","tendencia","residuos")
Encoding(lab) <- "UTF-8"
pdf(file=filegraf("xprdescom"),width=4,height=4,pointsize=8)
plot(w,labels=lab,set.pars=list(mar=c(0,5,0,2.5),oma=c(2.5,0,1,0),
                   tck=-0.01,mfrow=c(4,1)))
dev.off()
pdf(file=filegraf("xprestacion"),width=4.8,height=3,pointsize=8)
par(mar=c(0,4.4,0,1),oma=c(2.2,0,1,0),tck=-0.01,lwd=0.5)
monthplot(w,choice="seasonal",base=function(y)mean(y,trim=0.25),type="h",
          labels=c("E","F","M","A","M","J","J","A","S","O","N","D"),
          ylab="estacionalidad",main="",xlab="")
dev.off()
#valor de las exportaciones
s.xvano <- ts(round(xano$milusan/1000,0),start=1960,frequency=1)
ipcusa <- datos(query("select * from ipcusa","pecuario"))#2000=100
ipc <- 100*(ipcusa$ipcusa/ipcusa$ipcusa[48])#2007=100
s.xvanodef <- ts(round(xano$milusan/(10*ipc),0),start=1960,frequency=1)
ua <- rep(start(s.xvano)[1]:end(s.xvano)[1],2)
u <- factor(rep(1:2,rep(48,2)))
levels(u) <- c("nominal","deflactado")
ey <- "millón de dolares"
Encoding(ey) <- "UTF-8"
initrepdf("xpvalor",gh=2,gw=3)
xyplot(c(s.xvano,s.xvanodef)~ua|u,layout=c(2,1),aspect="xy",type="l",
       ylab=ey,main=NULL,xlab="",par.strip.text=list(cex=1),
       par.settings=list(par.ylab.text=list(cex=1)),
       scales=list(tck=0.5,cex=0.8))
dev.off()
#valor unitario (precio en centavos)
u <- round(100*xano$milusan/xano$milbxpan,0)
s.xprecio <- ts(cbind(u/(ipc/100),u),start=1960,frequency=1,
                names=c("deflactado","nominal"))
gseriem("xprecio",s.xprecio,ids=c("deflactado","nominal"),
        ejey="centavos-dolar/lb",gw=3,cexeje=1,cexlab=1.5,cextop=0.8)
#la demanda USA
dema <- datos(query("select * from demanda","pecuario"))
s.dema <- ts(round(dema$base98,0),start=1980,frequency=1)
gserie("demanda",s.dema,ejey="porcentaje",ejeytop=F,ml=0.55,mt=0.1)
#descomposicion exportaciones mensuales--1980:2007
s.xmes <- window(ts(round(xmes$peso/1000,0),start=1969,frequency=12),
                 end=c(2007,12))
w <- stl(s.xmes,s.window=25,robust=T)
lab <- c("millón libras","estacionalidad","tendencia","residuos")
Encoding(lab) <- "UTF-8"
pdf(file=paste(directoriogrf,"xpdescom.pdf",sep=""),width=4,
    height=4,pointsize=8)
plot(w,labels=lab,set.pars=list(mar=c(0,5,0,2.5),oma=c(2.5,0,1,0),
                   tck=-0.01,mfrow=c(4,1)))
dev.off()
#graf desviaciones
u <- gHylleberg(s.xmes)
initrepdf("xpmesdesv",3,3)
plot(u)
dev.off()
#porcentaje de la exportacion
z <- datos(query("select * from totind","pecuario"))
s.xpp <- ts(round(100*(xano$milbxpan/z$milbinan),0),start=1960,
            frequency=1)
gserie("xporcentaje",s.xpp,ejey="porcentaje",gw=3.5,ml=0.55,ejeytop=F)
#**--- experimento con dlm ---**
library("dlm")
z <- datos(query("select * from totanu","pecuario"))
s.mta <- ts(z$cbtoan/1000,start=1960)
cm1 <- function(x){
  po <- dlmModPoly(order=1)
  return(list(FF=po$FF,GG=po$GG,m0=x[1],V=x[2],W=x[3],
              C0=x[4:5]))
}
cm2 <- function(x){
  po <- dlmModPoly(order=2)
  return(list(FF=po$FF,GG=po$GG,m0=x[1:2],V=x[3],W=x[4:5]*diag(2),
              C0=x[6:7]*diag(2)))
}
mx <- matrix(rep(0,length(s.mta)),ncol=1);mx[14][1] <- 1
cm3 <- function(xp,mx){
  return(list(FF=matrix(c(1,0,1),1),GG=matrix(c(1,0,0,1,1,0,0,0,1),3),
              JFF=matrix(c(0,0,1),1),m0=c(120,10,0),C0=1000*diag(3),
              V=xp[1]*diag(1),W=xp[2:4]*diag(3),X=mx))
}
u <- dlmMLE(s.mta,c(400,12,1,1000,1000,1000,1000),cm)
u <- dlmMLE(s.mta,c(1000,10,10,0),cm3,"BFGS",mx=mx)
#--
kn <- odbcConnectExcel("pecuario.xls")
y <- ts(sqlFetch(kn,"milbxpan")/1.0E3,start=c(1960,1),frequency=1)
z <- ts(sqlFetch(kn,"uston"),start=c(1988,1),frequency=12)
#--Nile
bn <- function(x)dlmModPoly(1,dV=x[1],dW=x[2])
fn <- dlmMLE(Nile,parm=c(110,1),build=bn,hessian=T)
fn <- dlmMLE(Nile,parm=rep(100,2),build=bn,lower=rep(1e-8,2),
             hessian=T)
fn <- dlmMLE(Nile,parm=c(110,1),build=bn,hessian=T)
sqrt(diag(solve(fn$hessian)))
nm <- bn(fn$par)
nf <- dlmFilter(Nile,nm)
ns <- dlmSmooth(Nile,nm)
plot(cbind(Nile,nf$m[-1],ns$s[-1]),plot.type="s",
     col=c("black","red","blue"),ylab="level",lwd=c(1,2,2))
pt <- unlist(dlmSvd2var(nf$U.R,nf$D.R))
ft <- pt+V(nm)
pe <- nf$y-nf$a
et <- pe/sqrt(ft)
plot(dropFirst(pe))
ets <- nf$y-dropFirst(ns$s)
etas <- diff(ns$s)
#--airline
w4 <- aggregate(AirPassengers,nfrequency=4)
lw4 <- log(w4)
#
m <- StructTS(lw4,type="BSM")
tsdiag(m)
#
mb <- function(x){
    m <- dlmModPoly()+dlmModSeas(frequency=4)
    V(m) <- exp(x[1])
    W(m)[1,1] <- 0#exp(x[2])
    W(m)[2,2] <- 0#exp(x[3])
    W(m)[3,3] <- 0#exp(x[4])
    return(m)
}
ini <- log(c(1e-4,1e-4,1e-4,1e-4))
ml <- dlmMLE(lw4,ini,mb)
fk <- dlmFilter(lw4,mb(ml$par))
#prediction error variance (Harvey)
rs <- residuals(fk)
u <- sum(rs$res*rs$res)
pev <- (u/47)*(rs$sd[48]*rs$sd[48])
#---------------
#---aves
#---------------
kn <- odbcConnectExcel("pecuario.xls")
tablas <- sqlTables(kn)["TABLE_NAME"]
odbcClose(kn)
#-carne de res (cabezas,carne,xpcan,xpval)
bo <- tablaxls(file="pecuario",tabla="totanu")
zb <- ts(bo$milbtoan,start=c(1960,1),frequency=1)
#-aves
av <- tablaxls(file="pecuario",tabla="avemes")
z <- ts(av$milave,start=c(1990,1),frequency=12)
#--- matanza.anual
aa <- tablaxls(file="pecuario",tabla="aveanual")#1968-1989
wa <- ts(c(aa$ave/1.0E3,as.vector(aggregate(z))),start=c(1965,1),frequency=1)
za <- window(wa/1.E3,start=c(1970,1))
#--
lza <- log(za)
plot(lza,type="o")
abline(v=c(1980,1990,2000,2010),lty=2)
#
level0=lza[1]
slope0=mean(diff(lza))
#--
m <- StructTS(za,type="trend")
tsdiag(m)
#
bv1 <- function(x)dlmModPoly(order=1,dV=exp(x[1]),dW=exp(x[2]))
pin1 <- c(log(1e-6),log(0.1))
fv <- dlmMLE(lza,parm=pin1,build=bv1)
fv$conv
me <- bv1(fv$par)
V(me);W(me)
#
fkv <- dlmFilter(lza,me)
tsdiag(fkv)
qqnorm(residuals(fkv,sd=F))
qqline(residuals(fkv,sd=F))
#-outlier79
xo <- indicadora(lza,am=c(desde=c(1979,1)))
ftr <- matrix(xo,ncol=1)
bv1x <- function(x){
    mo <- bv1(x[1:2])+dlmModReg(ftr,addInt=F)
    V(mo) <- exp(x[1])
    W(mo)[1,1] <- exp(x[2])
    return(mo)
}
fv <- dlmMLE(lza,parm=pin,build=bv1x)
me <- bv1x(fv$par)
V(me);W(me)
fvk <- dlmFilter(lza,me)
fvs <- dlmSmooth(fvk,me)
tsdiag(fvk)
qqnorm(residuals(fvk,sd=F))
qqline(residuals(fvk,sd=F))
#
plot(lza,type="o")
lines(dropFirst(fvs$s[,1]),col="blue")
lines(dropFirst(fvk$a[,1]),col="red")
fvk$a[which(xo==1)+1,2]
#[1] -0.6169665
fvs$s[which(xo==1)+1,2]
#[1] -0.6730279
#-outliers79,73,cambio temporal 89
o73 <- indicadora(lza,am=c(desde=c(1973,1)))
o79 <- indicadora(lza,am=c(desde=c(1979,1)))
delta <- 0.5
t89 <- indicadora(lza,am=c(desde=c(1989,1)))
i <- which(t89==1)
t89[i:(i+10)] <- delta^(0:10)
ftr <- matrix(c(o73,o79,t89),ncol=3)
ftr <- matrix(c(o79,t89),ncol=2)
me <- bv1x(pin)
fv <- dlmMLE(lza,parm=pin,build=bv1x)
me <- bv1x(fv$par)
V(me);W(me)
fvk <- dlmFilter(lza,me)
fvs <- dlmSmooth(fvk,me)
tsdiag(fvk)
qqnorm(residuals(fvk,sd=F))
qqline(residuals(fvk,sd=F))
#--slope
bv2 <- function(x)dlmModPoly(order=2,dV=exp(x[1]),dW=exp(x[-1]))
pin2 <- log(c(1e-6,2,0.2))
fv <- dlmMLE(lza,parm=pin2,build=bv2,hessian=T)
m2 <- bv2(fv$par)
V(m2);W(m2)
fk2 <- dlmFilter(lza,m2)
tsdiag(fk2)
qqnorm(residuals(fk2,sd=F))
qqline(residuals(fk2,sd=F))
#
ftr <- matrix(o79,ncol=1)
bv2x <- function(x) bv2(x)+dlmModReg(ftr,addInt=F)
m2x <- bv2x(pin2)
fvx <- dlmMLE(lza,parm=pin2,build=bv2x,hessian=T)
m2x <- bv2(fvx$par)
V(m2x);W(m2x)
fk2x <- dlmFilter(lza,m2x)
tsdiag(fk2x)
qqnorm(residuals(fk2x,sd=F))
qqline(residuals(fk2x,sd=F))
#--tendencia constante
bv2c <- function(x)dlmModPoly(order=2,dV=exp(x[1]),dW=c(0,exp(x[2])))
pin2c <- log(c(1e-2,0.2))
fv <- dlmMLE(lza,parm=pin2c,build=bv2c,hessian=T)
m2c <- bv2c(fv$par)
V(m2c);W(m2c)
fk2c <- dlmFilter(lza,m2c)
fs2c <- dlmSmooth(fk2c,m2c)
tsdiag(fk2c)
rs <- residuals(fk2c,sd=F)
qqnorm(rs)
qqline(rs)
#outliers
o73 <- indicadora(lza,am=c(desde=c(1973,1)))
o79 <- indicadora(lza,am=c(desde=c(1979,1)))
o89 <- indicadora(lza,am=c(desde=c(1989,1)))
t89 <- shockfunpote(lza,c(desde=c(1989,1)),delta=0.7)
#
t89 <- indicadora(lza,am=c(desde=c(1989,1),hasta=c(1991,1)))
t89[t89==1] <- c(1,0.7,0.3)
t89 <- indicadora(lza,am=c(desde=c(1989,1),hasta=c(1992,1)))
t89[t89==1] <- c(1,0.75,0.5,0.25)
#-shock 79,89
ftr <- matrix(c(o79,t89),ncol=2)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,1,1)
    diag(C0(mtx)) <- c(2,1,1,1)
    V(mtx)[1,1] <- exp(x[2])
    W(mtx)[2,2] <- exp(x[1])
    invisible(mtx)
}
ini <- log(c(1e-1,1e-2))
fm <- dlmMLE(lza,ini,bt2x)
mm <- bt2x(fm$par)
fk <- dlmFilter(lza,mm)
fs <- dlmSmooth(fk,mm)
#
rs <- residuals(fk,sd=F)
tsdiag(fk)
qqnorm(rs)
qqline(rs)
#
plot(lza,type="p")
lines(dropFirst(fs$s[,1]),col="blue")
lines(dropFirst(fk$a[,1]),col="red")
#
ss <- spectrum(rs,span=c(5,7))
ss <- spectrum(rs,method="ar")
sx <- max(ss$spec)
pe <- 1/ss$freq[ss$spec==sx]
#-shock 73,79,89
ftr <- matrix(c(o73,o79,t89),ncol=3)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,1,1,1)
    diag(C0(mtx)) <- c(2,1,1,1,1)
    V(mtx)[1,1] <- exp(x[2])
    W(mtx)[2,2] <- exp(x[1])
    invisible(mtx)
}
ini <- log(c(1e-1,1e-2))
#-shock 73,79,89 nivel variable
ftr <- matrix(c(o73,o79,t89),ncol=3)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,1,1,1)
    diag(C0(mtx)) <- c(2,1,1,1,1)
    V(mtx)[1,1] <- exp(x[2])
    W(mtx)[1,1] <- exp(x[3])
    W(mtx)[2,2] <- exp(x[1])
    invisible(mtx)
}
ini <- log(c(1e-2,1e-2,1e-1))
#-shock 73,79,89 más AR(2)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,1,1,1)
    diag(C0(mtx)) <- c(2,1,1,1,1)
    #V(mtx)[1,1] <- exp(x[6])
    V(mtx)[1,1] <- 1e-8
    W(mtx)[1,1] <- exp(x[2])
    W(mtx)[2,2] <- exp(x[1])
    cic <- dlmModARMA(ar=x[3:4],sigma2=exp(x[5]))
    V(cic)[1,1] <- 0
    m0(cic) <- c(0.4,0.4)
    invisible(mtx+cic)
}
#ini <- c(log(c(1e-2,1e-2)),0.57,-0.41,log(c(0.8,1e-4)))
ini <- c(log(c(1e-2,1e-2)),0.57,-0.41,log(0.8))
#-shock 73,79,89 más trig
#estimado de periodo
#rs residuales del filtro aplicado a shock 73,79,89 nivel variable
#bcm <- function(x)dlmModTrig(tau=x[1],q=1,dW=c(exp(x[2]),exp(x[3])),
#                             dV=x[4])
#ini <- c(6,log(0.1),log(0.1),1)
#fm <- dlmMLE(rs,ini,bcm)
#2*pi/acos(GG(mc)[1,1])
#---modelo ajustado
ftr <- matrix(c(o73,o79,t89),ncol=3)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,0,0,0)
    diag(C0(mtx)) <- c(2e-1,1e-1,1e-1,1e-1,1e-1)
    V(mtx)[1,1] <- exp(x[2])#1e-6
    #W(mtx)[1,1] <- exp(x[2])
    W(mtx)[2,2] <- exp(x[1])
    cic <- dlmModTrig(tau=5,q=1,dV=0,dW=exp(x[3])*c(1,1))
    invisible(mtx+cic)
}
ini <- log(c(1e-2,1e-2,1e-3))
fm <- dlmMLE(lza,ini,bt2x)
fm$conv
#
mm <- bt2x(fm$par)
fk <- dlmFilter(lza,mm)
fs <- dlmSmooth(fk,mm)
#
rs <- residuals(fk,sd=F)
qqnorm(rs)
qqline(rs)
tsdiag(fk,gof.lag=25)
crs <- cumsuma(rs,lag=1)
crs2 <- cumsuma2(rs,lag=1)
#
plot(lza,type="p")
lines(dropFirst(fs$s[,1]),col="blue")
lines(dropFirst(fk$a[,1]),col="red")
#
bv2cx <- function(x) bv2c(x)+dlmModReg(ftr,addInt=F)
m2cx <- bv2cx(pin2c)
fv <- dlmMLE(lza,parm=pin2c,build=bv2cx,hessian=T)
m2cx <- bv2cx(fv$par)
V(m2cx);W(m2cx)
fk2cx <- dlmFilter(lza,m2cx)
fs2cx <- dlmSmooth(fk2cx,m2cx)
tsdiag(fk2cx)
rs <- residuals(fk2cx,sd=F)
qqnorm(rs)
qqline(rs)
plot(lza,type="p")
lines(dropFirst(fk2c$a[,1]),col="blue")
lines(dropFirst(fs2c$s[,1]),col="red")
#--ciclo
bvc <- function(x) dlmModPoly(order=2,dV=exp(x[1]),dW=c(0,exp(x[2])),
                      m0=c(level0,slope0),C0=diag(c(1e-5,2)))

bv <- function(x){
    ten <- dlmModPoly(order=2,dV=1e-7,dW=exp(x[1:2]),
                      m0=c(level0,slope0),C0=2*diag(2))
    cic <- dlmModARMA(ar=ARtransPars(x[4:5]),sigma2=exp(x[3]))
    return(ten+cic)
}
bvc2 <- function(x){
    ten <- dlmModPoly(order=2,dV=1e-6,dW=c(0,exp(x[2])),
                      m0=c(level0,slope0),C0=2*diag(2))
    reg <- dlmModReg(ftr,dV=1e-6,m0=c(-2,-2),C0=1*diag(2),addInt=F)
    GG(reg) <- diag(x[6:7])
    cic <- dlmModARMA(ar=ARtransPars(x[4:5]),sigma2=exp(x[3]))
    return(ten+reg+cic)
}
bvc <- function(x){
    ten <- dlmModPoly(order=2,dV=1e-7,dW=exp(x[1:2]),
                      m0=c(level0,slope0),C0=2*diag(2))
    cic <- dlmModTrig(s=6,dV=exp(x[3]),dW=exp(x[-(1:3)]))
    return(ten+cic)
}
#
init <- c(log(1e-3),log(1e-2),log(1e-1),0.2,0.2)
mart <- bvc2(init)
fvc <- dlmMLE(lza,parm=init,bvc2)
mart <- bvc2(fvc$par)
fkct <- dlmFilter(lza,mart)
tsdiag(fkct)
rs <- residuals(fkct,sd=F)
qqnorm(rs)
qqline(rs)

init <- c(log(3.7),log(0.085),rep(-2,3))
fv <- dlmMLE(lza,init,bvc)
#--matanza mensual
##
av <- tablaxls(file="pecuario",tabla="avemes")
z <- ts(av$milave,start=c(1990,1),frequency=12)
plot(z,type="l")
#
lz <- log(z)
plot(lz,type="l")
#
wl95 <- window(lz,start=c(1995,1))
wl2k <- window(lz,start=c(2000,1))
#--[descompone]
#
u <- decompose(wl95,type="multiplicative")
u$figure
u <- decompose(wl95,type="additive")
u$figure
plot(u)
#
u <- stl(wl95,s.window=25,robust=T)
summary(u)
plot(u)
u.s <- u$time.series[,"seasonal"]
plot(u.s)
#--[trimestral]
wl3 <- aggregate(wl95,nfrequency=4,sum)
m3 <- arima(wl3,order=c(1,1,0),seasonal=list(order=c(0,1,1),4))
plot(wl3,type="p")
lines(wl3-residuals(m3))
#
wm3 <- ts(matrix(wl3,ncol=frequency(wl3),byrow=T),frequency=1,
          start=start(wl3)[1])
plot(wm3,plot.type="single",col=1:4)
legend(x="topright",legend=c("T1","T2","T3","T4"),bty="n",
       fill=1:4)
#---[periodico?]
#prueba de Martin
u <- decompose(wl95,type="additive")#NA 6 inicio, 6 final
u <- stl(wl95,s.window=25,robust=T)
w <- u$time.series[,"seasonal"]+u$time.series[,"remainder"]
(esperiodica(w))
#
#grafica de subseries estacionales
wm <- subseries(wl95)
#
ff <- tsdate(wl95)
u <- data.frame(wl95=wl95,aa=as.integer(strftime(ff,"%Y")),
                mm=months(ff,abbreviate=T),tt=quarters(ff))
levels(u$tt) <- c("Enero-Marzo","Abril-Junio",
                  "Julio-Septiembre","Octubre-Diciembre")
#RColorBrewer::display.brewer.all(n=8,exact.n=F)
colm <- c(brewer.pal(7,"YlOrBr")[4:6],brewer.pal(7,"Greens")[4:6],
          brewer.pal(7,"GnBu")[4:6],brewer.pal(7,"Blues")[4:6])
colm <- c(brewer.pal(8,"YlOrRd")[c(4,6,8)],
          brewer.pal(8,"Oranges")[c(4,6,8)],
          brewer.pal(8,"YlGn")[c(4,6,8)],brewer.pal(8,"GnBu")[c(4,6,8)])
colm <- c(brewer.pal(8,"YlOrRd")[c(4,6,8)],
          brewer.pal(8,"YlGn")[c(4,6,8)],
          brewer.pal(8,"Greens")[c(4,6,8)],
          brewer.pal(8,"YlGnBu")[c(4,6,8)])
names(colm) <- as.character(u$mm[1:12])
#ggplot
#para colores como esperado
pp <- ggplot(data=u,aes(aa,wl95,colour=mm,group=mm,cex=0.5))
pp <- pp+geom_line(size=0.5)+facet_wrap(~tt,ncol=2)
#first.qp no arregla bien la salida cuando se envía a pdf
#posiblemente por el control del tamaño de letra
pp <- direct.label(pp,list(first.points,
                           dl.move("Ene",y=7.1),
                           dl.move("May",y=7.05),
                           dl.move("Jun",y=7.3),
                           dl.move("Jul",y=7.45),
                           dl.move("Sep",y=7.2)))
pp <- pp+scale_colour_manual(name="Meses",value=colm)
pp <- pp+opts(legend.position="none")+xlim(1993,2011)
pp <- pp+xlab("")+ylab("")
#lattice
pp <- xyplot(wl95~aa|tt,groups=mm,type="l",xlim=c(1993.2,2011.5),
             asp=0.8,as.table=T,strip=F,between=list(x=0.2,y=0.5),
             col=colm[order(names(colm))],
             par.settings=ggplot2like(),axis=axis.grid)
direct.label(pp,list(cex=0.8,first.qp))
#---
pdf(width=4,height=4,file="../grf/gpp.pdf",family="Times",
    pointsize=8,paper="special",title="")
par(mai=c(0.33,0.33,0.2,0.08))
pp
dev.off()
#
#Cairo(type="pdf",units="in",width=4.8,height=4,
#      file="../grf/gpp.pdf",family="serif",pointsize=8)
#par(mai=c(0.33,0.33,0.2,0.08))
#---
wa <- aggregate(w,frequency=1)
level0=wa[1]
slope0=mean(diff(wa))
#
mod <- function(x){
    m1 <- dlmModPoly(order=2,dV=exp(x[1]),dW=exp(x[2:3]),
                    m0=c(level0,slope0),C0=diag(c(1e2,1e2)))
    m2 <- dlmModSeas(freq=12,dV=0,dW=c(exp(x[4]),rep(0,10)))
    invisible(m1+m2)
}
ini <- log(c(1e-3,1e-2,1e-3,1e-3))
#--
x <- as.vector(time(za))
y <- as.vector(za)
qplot(x,y,geom=c("line","point"),asp=1)
#--funcion en serie.r
#> (rectangularidad(as.vector(time(za)),as.vector(za),rectmax=2))
#[1] 0.698758
g <- serie.grafica(za,wp=3,asp=0.7,devfile="../grf/avmatanual.pdf",
                   ejey="millón de aves",mai=c(0.33,0.5,0.2,0.2))
g <- serie.grafica(za,wp=3,asp=1,devfile="../grf/prueba2.pdf",
                   ejey="millón de aves",ejeylado=3,ejeylabdir=1,ejeypos=0.1)
g <- serie.grafica(za,wp=3,asp=0.7,devfile="../grf/avmatanual.pdf",
                   ejey="millón de aves",mai=c(0.33,0.5,0.2,0.2),
                   radio=0.2,rlwd=0.1,lwd=1)
#
fza1 <- bkfilter(za,pl=2,pu=10)
fza2 <- bkfilter(za,pl=4,pu=10)
fza3 <- bkfilter(za,pl=4,pu=8)
fza4 <- bkfilter(za,pl=2,pu=8)
#
tza <- window(fza4$trend,start=start(za)+c(3,0),end=end(za)-c(3,0))
op <- par(no.readonly=T)
g <- serie.grafica(za,wp=3,asp=0.7,devfile="../grf/avmatanual2.pdf",
                   ejey="millón de aves",mai=c(0.33,0.5,0.2,0.2),
                   radio=0.3,rlwd=0.1,lwd=1,devoff=FALSE)
lines(tza,col="blue")
dev.off()
par(op)
#---trend-cycle
zat <- window(fza4$trend,start=start(za)+c(3,0),end=end(za)-c(3,0))
zac <- window(fza4$cycle,start=start(za)+c(3,0),end=end(za)-c(3,0))
rzat <- serie.rect(zat)
rzac <- serie.rect(zac)
mait <- c(0.33,0.5,0.1,0.1)
maic <- c(0.33,0.5,0.1,0.1)
w <- 3
rp <- w*c(1,rzat+rzac)
rg <- rp+c(mait[2]+mait[4],mait[1]+mait[3]+maic[1]+maic[3])
Cairo(type="pdf",units="in",width=rg[1],height=rg[2],
      file="../grf/avmatatencyc.pdf",family="Times",pointsize=8)
nf <- layout(matrix(c(1,2)),
             heights=w*c(rzat,rzac)+c(mait[1]+mait[3],maic[1]+maic[3]),
             respect=F)
#mai clave para la definición de las áreas de dibujo
par(mai=mait)
par(mgp=c(2.5,0.8,0))
plot(zat,ann=F)
mtext("millones de aves",side=2,line=par("mgp")[1])
plot(zac,ann=F)
#gris<-gray(0.95)
#serie.plot(zat,pmai=mait,ejey="millones de aves",bgpr=gris,
#           colrect="NA",gcol="white",colejes=gris,colticks=gris)
#serie.plot(zac,pmai=maic,ejey="",bgpr=gris,colrect="NA",gcol="white",
#           colejes=gris,colticks=gris)
#serie.grafica(zat,wp=w,mai=mait,asp=rzat,ejey="millón de aves",devoff=F)
#serie.grafica(zac,wp=w,mai=maic,asp=rzac,ejey="",devoff=T)
dev.off()
#---
zas <- spectrum(zac,detrend=F,demean=F,spans=c(3,5),plot=F)
Cairo(type="pdf",units="in",width=1.6+0.4,height=1.6+0.6,
      file="../grf/avmatacycspec.pdf",family="Times",pointsize=8)
par(mai=c(0.5,0.3,0.1,0.1),tcl=-0.3,mgp=c(2.5,0.8,0),lend="square")
plot(x=zas$freq,y=zas$spec,ann=F,type="n",axes=F)
pu <- par("usr")
rect(pu[1],pu[3],pu[2],pu[4],col=gray(0.95),border=NA)
lines(x=zas$freq,y=zas$spec,type="h",lwd=0.8)
box(lwd=0.8)
axis(1,col="transparent",col.ticks=gray(0),lwd.ticks=0.8)
axis(2,col="transparent",col.ticks=gray(0),lwd.ticks=0.8)
mtext("frecuencia",side=1,line=2)
dev.off()
#-----------------
#--ave.carne-anual
#-----------------
zc <- ts(av$millb,start=c(1990,1),frequency=12)
wc <- ts(c(aa$millb,as.vector(aggregate(zc))),start=c(1965,1),frequency=1)
zc <- window(wc/1.E3,start=c(1970,1))
lkr <- log(zc)
zck <- zc*0.4542
#
level0 <- lkr[1]
slope0 <- mean(diff(lkr))
#
plot(zc,type="o",main="millones de libras")
plot(lkr,type="o")
abline(v=c(1980,1990,2000,2010),lty=2)
title("log-millones de libras")
#outliers
o79 <- indicadora(lkr,am=c(desde=c(1979,1)))
o89 <- indicadora(lkr,am=c(desde=c(1989,1)))
#
t89 <- shockfunpote(lkr,c(desde=c(1989,1)),delta=0.7)
t89 <- indicadora(lkr,am=c(desde=c(1989,1),hasta=c(1992,1)))
t89[t89==1] <- c(1,0.75,0.5,0.25)
#
ftr <- matrix(c(o79,t89),ncol=2)
bt2x <- function(x){
    mtx <- dlmModPoly()+dlmModReg(ftr,addInt=F)
    m0(mtx) <-c(level0,slope0,0,0)
    diag(C0(mtx)) <- c(2e-1,1e-1,1e-1,1e-1)
    V(mtx)[1,1] <- exp(x[2])#1e-6
    #W(mtx)[1,1] <- exp(x[3])
    W(mtx)[2,2] <- exp(x[1])
    cic <- dlmModTrig(tau=5,q=1,dV=0,dW=c(exp(x[3]),0))
    invisible(mtx+cic)
}
ini <- log(c(1e-2,1e-2,2e-1))
fm <- dlmMLE(lkr,ini,bt2x)
mm <- bt2x(fm$par)
fk <- dlmFilter(lkr,mm)
fs <- dlmSmooth(fk,mm)
#
tsdiag(fk)
rs <- residuals(fk,sd=F)
Box.test(rs,lag=14)
qqnorm(rs)
qqline(rs)
#
plot(lkr,type="p")
lines(dropFirst(fs$s[,1]),col="red")
lines(dropFirst(fk$a[,1]),col="blue")
#
plot(dropFirst(fs$s[,2]))
plot(dropFirst(fs$s[,5]))
#
fkr <- bkfilter(lkr,pl=2,pu=8)
fzc <- bkfilter(zc,pl=2,pu=8)
#> (serie.rect(zck))
#[1] 0.8142776
g <- serie.grafica(zc,wp=3,asp=0.8,devfile="../grf/avkrnanual.pdf",
                   ejey="millones de libras",mai=c(0.33,0.5,0.1,0.1),
                   radio=0.3,rlwd=0.1,lwd=1)
g <- serie.grafica(zck,wp=3,asp=0.8,devfile="../grf/avkrnanual2.pdf",
                   ejey="gigagramos",mai=c(0.33,0.5,0.1,0.1),
                   radio=0.3,rlwd=0.1,lwd=1)
#
zcc <- window(fzc$cycle,start=start(zc)+c(3,0),end=end(zc)-c(3,0))
zcs <- spectrum(zcc,detrend=F,demean=F,spans=c(3,5),plot=T)
#rendimiento
zp <- window(round(wc/wa,2),start=c(1970,1))
#> (serie.rect(zp))
#[1] 0.4286017
g <- serie.grafica(zp,wp=3,asp=0.45,devfile="../grf/avpesoanual.pdf",
                   ejey="libra/ave",mai=c(0.33,0.5,0.1,0.1),
                   radio=0,rlwd=0.1,lwd=1)

## descomposición singular
## 2017-04-30
setwd("c:/eddy/wp/pecuario")
library("Rssa")
library(RODBC)
##
kk <- odbcConnect("pecuario")
av <- sqlQuery(kk, "select * from avemes")
hm <- sqlQuery(kk, "select * from eggmes")
odbcClose(kk)
z <- ts(av$milave,start=c(1990,1),frequency=12)
y <- ts(av$millbs,start=c(1990,1),frequency=12)
x <- ts(hm$huevomildoc, start=c(2000,1), frequency=12)
##
ww <- ssa(z)

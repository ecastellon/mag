setwd("c:/eddy/wp/lluvia/rd")
library("chron")
library("RColorBrewer")
library("grid")
source("anallu.r")
#source("claselluvia.r")
x <- datos.dia("sebaco.txt")
x.basicas <- basicas(x$fch,x$pmm)
#grafica dias satisfacen condicion
w <- subset(x,years(fch)!=2003)
d <- subset(w,months(fch)%in%c("May","Jun")&days(fch)%in%1:31)
cc <- brewer.pal(5,"RdYlBu")
dsc <- rep(cc[4],nrow(d))
dsc[d$pmm==0] <- cc[2]
#grafica simple
plot(x=rep((1:61),44),y=rep(yd,each=61),ann=F,type="n",xaxt="n",yaxt="n")
points(x=rep((1:61),44),y=rep(yd,each=61),pch=20,col=dsc,cex=1.5)
axis(1,at=seq(0,61,5),labels=c(as.character(0:31),as.character(1:30))[seq(1,62,5)])
axis(3,at=seq(0,61,5),labels=c(as.character(0:31),as.character(1:30))[seq(1,62,5)])
axis(2)
#
pushViewport(plotViewport(c(5,4,2,2)))
pushViewport(dataViewport(1:61,1:44,name="plotRegion"))
grid.rect(x=rep((1:61)/61,44),y=rep((1:44)/44,each=61),width=1/61,height=1/44,just=c("right","top"),gp=gpar(col=NA,fill=dsc),name="image")
grid.xaxis()
grid.yaxis()
nada<-function(){
#los datos
z<-read.table(file="../sebaco.txt",header=T)
z<-z[(z$year!=2003),]
a<-factor(z$year)
m<-factor(z$mes)
#para acumulado por pentada
b<-rep(1:73,rep(5,73))
#los datos por año
u<-table(a)
g<-NULL
#cuando es bisiesto 29/02 se agrega a la pentada 12
#una razon es que la prob. de lluvia es baja
for(i in 1:nrow(u)){if(u[i]==366)g<-c(g,b[1:59],12,b[60:365]) else g<-c(g,b)}
gf<-factor(g)
#el acumulado por pentada por año
pn<-tapply(z$mm10,a:gf,sum)
#el indice para extraer el subconjuno pentada=xx de todos los años
#en este caso la numero 73
i<-grep(":73",names(pn))
#para la acumulada empirica
library(stepfun)
fnp40<-ecdf(pn[grep(":40",names(pn))])
plot(fnp40)
#los cuantiles
quantile(pn[grep(":30",names(pn))],probs=seq(0,1,0.25))
#limite de confianza basado en la prueba binomial
#los estadisticos de orden aproximados por la normal
eor<-44*0.25+qnorm(0.05)*sqrt(44*0.25*0.75)
eos<-44*0.25+qnorm(0.95)*sqrt(44*0.25*0.75)
#las probabilidades exactas basadas en la binomial
pbinom(6,44,0.25)
pbinom(15,44,0.25,lower.tail=F)
#para identificar los estadisticos de orden correspondientes
sort(pn[grep(":30",names(pn))])
#la racha mas grande de periodos secos en la pentada
mxrs<-tapply(z$seco,a:gf,max)
mxrs[grep(":30",names(mxrs))]
#las pentadas como factores para boxplot
pn2<-pn[grep(":27|:28|:29|:30|:31|:32|:33",names(pn))]
fpn<-factor(paste("p",substr(names(pn2),6,7),sep=""))
boxplot(pn2~fpn)
#para manejar als fechas
library(chron)
fch<-dates(paste(z$dia,z$mes,z$year,sep="/"),format="d/m/y",origin.=c(month=5,day=1,year=1950))

b<-2
a<-switch(b,"uno",
	"dos",
	"tres")
print(a)
}

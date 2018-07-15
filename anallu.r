library("chron")
#
fecha<-function(d,m,a)
  as.Date(paste(a,m,d,sep="-"))
#
es.fecha <- function(x)
  class(x)=="Date"
#
fechav <- function(vf){
  if(is.vector(vf)&length(vf==3))
    fecha(vf[1],vf[2],vf[3])
  else
    NULL
}
#--convierte a vector c(d,m,a)
fecha2vector <- function(x){
  if(es.fecha(x))
    as.integer(unlist(strsplit(format(x,format="%d-%m-%Y"),"-")))
  else
    NULL
}
#--dias diferencia entre dos fechas
#f1, f2 pueden ser Dates o vectores c(d,m,a)
dias.diff <- function(f1,f2){
  if(is.vector(f1))
    f1 <- fechav(f1)
  if(is.vector(f2))
    f2 <- fechav(f2)
  if(all(es.fecha(f1),es.fecha(f2)))
    abs(as.integer(difftime(f2,f1,units="days")))
  else
    NA
}
#fecha días después de otra fecha
#fo puede ser Dates o vector c(d,m,a)
fecha.despues <- function(fo,dias=1){
  if(is.vector(fo))
    fo <- fechav(fo)
  if(es.fecha(fo))
    fo+dias
  else
    NULL
}
#--secuencia de dias a partir de una fecha
sec.dias <- function(fo,dias)
  seq(fo,length=dias,by="days")
#--fecha limite derecho de np pentadas a partir de dia-mes-año
#(f0,f1],(f1,f2], ...
#la salida incluye f0
pentadas <- function(f1,np=73)
  seq(f1-1,by=5,length=np+1)
#--pentadas del año a partir del 1 de enero
#corrige año bisiesto agregando un día a la última pentada
#de febrero
pentadas.ano <- function(a){
  f5 <- pentadas(fecha(a=a,m=1,d=1))
  if(leap.year(a))
    f5[13:length(f5)] <- f5[13:length(f5)]+1
  invisible(f5)
}
#--lista de secuencia de dias a partir del mismo dia-mes
#para varios años (vector anos)
#-ej.:mismo dia de varios años; salida en vector
#dd<-as.Date(unlist(sec.dias.ano(d,m,anos,dias=1)),origin=fecha(1,1,1970))
sec.dias.ano <- function(d,m,anos,dias)
  lapply(lapply(anos,fecha,m=m,d=d),seq,length=dias,by="days")
#--lee archivo .llu de agroclim y lo transforma
#en data.frame; si file, guarda en file
#precipitacion en mm; no en decimas de mm como agroclim
datos.agroclim <- function(estacion,file){
  require("chron")
  datos.linea <- function(x)
    unlist(strsplit(x,split="[[:space:]]+"))[-1]
  datos.ano <- function(u){
    x <- unlist(u)
    malo <- length(x)<2
    if(!malo){
      ano <- as.integer(datos.linea(x[1])[2])
      #validacion?
      malo <- ano<1900
      if(!malo){
        #bi <- (ano%%4==0 & ano%%100!=0)|(ano%%400==0)
        linea <- 0
        pp <- lapply(x[-1],function(z){
          linea <<- linea+1
          mes <- (linea%/%2)+linea%%2
          dias.linea <- 15
          if(linea%%2==0){
            dias.linea <- dias.linea+c(1,-2,1,0,1,0,1,1,0,1,0,1)[mes]
            if(mes==2&leap.year(ano))
              dias.linea <- dias.linea+1
          }
          (as.numeric(datos.linea(z))/10)[1:dias.linea]
        })
        ll <- sapply(pp,length)
        ms <- rep(rep(1:12,each=2),ll)
        dias <- unlist(sapply(tabulate(ms),seq))
        fc <- paste(ano,ms,dias,sep="-")
        dano <- list(fecha=fc,ppmm=unlist(pp))
      }
    }
    if(malo) dano <- NULL
    invisible(dano)
  }
  w <- readLines(estacion)
  registros <- length(w)-1
  k <- registros%%25
  anos <- registros%/%25
  ianos <- rep(1:anos,each=25)
  if(k>0){
    anos <- anos+1
    ianos <- c(ianos,rep(anos,k))
  }
  z <- lapply(split(w[-1],factor(ianos)),datos.ano)
  j <- sapply(z,is.null)
  if(any(j))
    z <- z[!j]
  if(all(j)){
    datos <- NULL
    print("no se leyeron datos")
  }
  else{
    fc <- unlist(sapply(z,"[[","fecha"))
    pp <- unlist(sapply(z,"[[","ppmm"))
    datos <- data.frame(fecha=as.Date(fc),ppmm=pp)
    if(!missing(file))
      save(datos,file=file,compress=TRUE)
  }
  invisible(datos)
}
#--leer de archivos creados con transf.awk
datos.dia<-function(archivo=NULL){
  if(is.null(archivo))stop("falta archivo")
  if(!require(chron))stop("falta libreria chron")
  print(paste("procesando archivo",archivo))
  z<-read.table(file=archivo,header=T)
  f<-dates(paste(z$dia,z$mes,z$year,sep="/"),format="d/m/y")
  print(paste("datos desde",min(f),"hasta",max(f),";",nlevels(years(f)),"años de registro"))
  #fo<-origin(f[1])
  #print(paste("origen para fechas julianas",paste(fo["day"],fo["month"],fo["year"],sep="/")))
  data.frame(fch=f,pmm=z[,4]/10)
}
#--datos en periodo
#dadas dos fechas, devuelve los datos comprendidos entre las dos
#excluyendo el correspondiente a f1 (dato(f1),...,dato(f2)]
#f1, f2 son Dates o vectores c(d,m,a)
datos.intervalo <- function(colppm,colfecha,f1,f2){
  if(!es.fecha(colfecha[1]))
    stop("no es vector de fechas")
  if(is.vector(f1))
    f1 <- fechav(f1)
  if(is.vector(f2))
    f2 <- fechav(f2)
  if(all(es.fecha(f1),es.fecha(f2))){
    #sf <- sec.dias(f1,dias=dias.diff(f1,f2))
    j <- colfecha>f1&colfecha<=f2
    w <- ifelse(any(j),data.frame(fecha=colfecha[j],ppmm=colppm[j]),NULL)
  }
  else
    w <- NULL
  invisible(w)
}
#--acumula datos según periodo (p.ej. pentada)
#periodos es la secuencia de fechas que marcan los
#intervalos para acumular como el resultado de pentadas
#en el acumulado se excluye el dato de la primera fecha
#(abierto por la izquierda)
acumulados.periodo <- function(colfecha,colppm,periodos,labels){
  min <- min(periodos)
  max <- max(periodos)
  j <- colfecha>min&colfecha<=max
  if(any(j)){
    fp <- ifelse(missing(labels),
                 cut(colfecha[j],breaks=periodos),
                 cut(colfecha[j],breaks=periodos,labels=labels))
    w <- tapply(colppm[j],fp,sum)
  }
  else
    w <- NULL
  invisible(w)
}
#--acumula sobre pentadas anuales de todos los datos
acumula.pentadas <- function(df,colfecha=1,colppm=2){
  anos <- years(df[,colfecha])
  w <- split(df,anos)
  lp <- lapply(w,function(x){
    ano <- fecha2vector(x[1,colfecha])[3]
    pen <- pentadas.ano(ano)+1
    fp <- cut(x[,colfecha],pen,labels=paste(ano,1:73,sep="."))
    tapply(x[,colppm],fp,sum)})
  invisible(lp)
}
#--agrega por mes y año los datos de una estacion
#estadisticas básicas de la precipitación diaria
#minppm valor minimo para dejar de ser dia seco
resumen.dia <- function(df,colfecha=1,colppm=2,minppm=0.1){
  if(!is.data.frame(df))
    stop("datos no validos")
  require("lmomco")
  fy <- years(df[,colfecha])
  fm <- months(df[,colfecha])
  h <- df[,colppm]>minppm
  fym <- interaction(fy,fm,drop=TRUE)
  #fivenum de los dias con lluvia
  print(paste("registros de",nlevels(fy),"años"))
  lmomdh <- tapply(df[h,colppm],fy[h],lmoms)
  lmom <- tapply(df[,colppm],fy,lmoms)
  list(regano=nlevels(fy),
       ppano=tapply(df[,colppm],fy,sum),
       dhumano=tapply(h,fy,sum),
       ppmes=tapply(df[,colppm],fym,sum),
       dhumes=tapply(h,fym,sum),
       fivenumdiahum=tapply(df[h,colppm],fy[h],fivenum),
       lambdapp <- sapply(lmom,"[[","lambdas"),
       ratiospp <- sapply(lmom,"[[","ratios"),
       lambdappdhum <- sapply(lmomdh,"[[","lambdas"),
       ratiosppdhum <- sapply(lmomdh,"[[","ratios")
       )
}
#-- percentil y limites de confianza; empírica
#percentil estimado con la distribución empírica
#dp son los datos de precipitación
#qp el percentil
#confianza estimada con la binomial
percentil.empirica <- function(dp,qp=0.5,cc=0.8){
  dp<-sort(dp)
  cu<-quantile(dp,probs=qp)
  nd<-length(dp)
  nc<-(1-cc)/2
  ni<-0
  i<-0
  while(ni<nc)ni<-pbinom(i<-i+1,nd,qp)
  a1<-pbinom(i,nd,qp)
  nc<-1-cc-a1
  ni<-0
  j<-nd
  while(ni<nc)ni<-pbinom(j<-j-1,nd,qp,lower.tail=F)
  list(percentil=cu,lin=dp[i],lsu=dp[j],cco=1-a1-ni)
}
#--	regresa una lista con los datos (por año) en vDat que estan
#	en el periodo que inicia el dia iDia del mes iMes y que se
#	extiende por iP periodos del tipo (dias,semanas,...) de seq.dates
#	el vector de objetos (dates) fechas (oFch) sirve para
#	referenciar las hileras de vDat
datos.periodo<-function(iDia=NULL,iMes=NULL,iP=NULL,fT="days",oFch=NULL,vDat=NULL){
	if(any(is.null(iDia),is.null(iMes),is.null(iP),is.null(oFch),is.null(vDat)))stop("faltan parametros")
	vf<-dates(sapply(levels(years(oFch)),fecha,d=iDia,m=iMes),format="d/m/y")
	lapply(vf,function(x){vDat[oFch%in%seq.dates(from=x,length=iP,by=fT)]})
}
#-- secuencia de iSig dias de mismo año
sec.dias.anu<-function(iDia=1,iMes=1,iSig=5,oFch=NULL){
	if(is.null(oFch))stop("falta indicador de fechas")
	vf<-dates(sapply(levels(years(oFch)),fecha,d=iDia,m=iMes))
	lapply(vf,seq.dates,length=iSig,by="days")
}
#
lim.pentadas<-function(a=NULL){
	if(is.null(a))stop("año?")
	a<-as.numeric(a)
	py<-sapply(a,function(x){fs<-seq.dates(from=fecha(1,1,x)-1,by=5,length.=73)
                                 if(leap.year(x))fs[13:73]<-fs[13:73]+1
                                 fs}
                   )
	py[length(py)+1]<-py[length(py)]+5
	dates(py,format="d/m/y")
}
#
basicas<-function(vFch=NULL,vPmm=NULL){
	if(any(is.null(vFch),is.null(vPmm)))stop("faltan datos")
	fy<-years(vFch)
	fm<-months(vFch)
	tpy<-tapply(vPmm,fy,sum)
	dhy<-tapply((vPmm>0.1),fy,sum)
	tpm<-tapply(vPmm,fy:fm,sum)
	dhm<-tapply((vPmm>0.1),fy:fm,sum)
	fm<-factor(rep(levels(fm),nlevels(fy)))
	list(panu=tpy,dhanu=dhy,
		epanu=c(media=mean(tpy),mediana=median(tpy),de=sd(tpy)),
		edhan=c(media=mean(dhy),mediana=median(dhy),de=sd(dhy)),
		epmes=cbind(media=tapply(tpm,fm,mean),mediana=tapply(tpm,fm,median),de=tapply(tpm,fm,sd)),
		edhms=cbind(media=tapply(dhm,fm,mean),mediana=tapply(dhm,fm,median),de=tapply(dhm,fm,sd)))
}
#
#niveles p?:año
factor.pentadas<-function(fch=NULL){
	if(is.null(fch))stop("fechas?")
	#fch es objeto dates?
	a<-levels(years(fch))
	lp<-paste("p",1:73,sep="")
	lp<-paste(lp,rep(a,rep(73,length(a))),sep=":")
	cut.dates(fch,breaks=lim.pentadas(a),labels=lp)
}
#-- nombres de datos (d) identifican a la pentada p(np):año
#	e.g despues de tapply
datos.pentada<-function(np,d=NULL){
	if(any(is.null(d),is.null(names(d))))stop("datos,nombres?")
	i<-grep(paste("p",np,sep=""),names(d))
	if(any(i))ds<-d[i]
	else{
		ds<-NULL
		print("no hay datos para esa pentada")
	}
	ds
}
#
datos.year<-function(a,d=NULL){
	if(any(is.null(d),is.null(names(d))))stop("datos,nombres?")
	i<-grep(a,names(d))
	if(any(i))ds<-d[i]
	else{
          ds<-NULL
          print("no hay datos para ese año")
	}
	ds
}
#--	dp es datos de la pentada u otro intervalo
#	qp es la frec.acumulada
#	cc coeficiente de confianza para el intervalo
rsmn.cuantil<-function(dp,qp=0.5,cc=0.8){
	if(is.null(dp))stop("datos?")
	dp<-sort(dp)
	cu<-quantile(dp,probs=qp)
	nd<-length(dp)
	nc<-(1-cc)/2
	ni<-0
	i<-0
	while(ni<nc)ni<-pbinom(i<-i+1,nd,qp)
	a1<-pbinom(i,nd,qp)
	nc<-1-cc-a1
	ni<-0
	j<-nd
	while(ni<nc)ni<-pbinom(j<-j-1,nd,qp,lower.tail=F)
	list(cuantil=cu,lin=dp[i],lsu=dp[j],cco=1-a1-ni)
}
#-- pn es la pentada
#	drs trae la estadistica de la pentada en cada año
#-- el resultado es la frecuencia relativa de valores
#	mayores que liminf
frec.max<-function(pn=NULL,drs=NULL,liminf=10){
	if(any(is.null(pn),is.null(drs)))stop("datos,pentada?")
	dp<-datos.pentada(pn,drs)
	sum(dp>liminf)/length(dp)
}
#-- el acumulado en los ultimos iLap lapsos
#	lsDat es la lista de datos en periodo del año
#-- la salida es una lista por año
acumulado.lapso<-function(lDat=NULL,iLap=2){
	if(is.null(lDat))stop("faltan datos")
	cu<-lapply(lDat,cumsum)
	cd<-lapply(cu,diff,lag=iLap)
	no<-rep(NA,iLap)
	lapply(cd,function(x){c(no,x)})
}
#-- conteo de dias consecutivos secos(humedos)
conteo.secos<-function(vDat=NULL,nSeco=0){
	if(is.null(vDat))stop("faltan datos")
	vDat[1]<-ifelse(vDat[1]>nSeco,-1,1)
	for(i in 2:length(vDat)){
		lPs<-(vDat[i-1]>0)
		vDat[i]<-ifelse(vDat[i]>nSeco,ifelse(lPs,0,vDat[i-1])-1,ifelse(lPs,vDat[i-1],0)+1)
	}
	vDat
}
#
racha.max<-function(vDat=NULL,iSig=15){
	if(is.null(vDat))stop("faltan datos")
	iL<-length(vDat)-iSig
	for(i in 1:iL) vDat[i]<-max(vDat[(i+1):(i+iSig)])
	vDat[1:iL]
}
fin.racha<-function(lD,nxdias=15,rmax=5){
	lDs<-lapply(lD,conteo.secos)
	(sapply(lDs,racha.max,iSig=nxdias)<rmax)
}
cum.next<-function(vD,iNx){
	iL<-length(vD)-iNx
	for(i in 1:iL) vD[i]<-sum(vD[(i+1):(i+iNx)])
	vD[1:iL]
}
fin.etp<-function(lD,nxdias=15,etpmx=20){
	return (sapply(lD,cum.next,iNx=nxdias)>etpmx)
}
ini.cum<-function(vD,lapso=2,premin=20){
	return (diff(cumsum(vD),lag=lapso)>premin)
}
fin.bal<-function(lD,indias,nxdias,etpmx){
	lD<-lapply(lD,cumsum)
	lCd<-lapply(lD,diff,lag=indias)
	vCa<-sapply(lCd,function(x){x[-seq(length(x),length(x)-nxdias+1)]})
	lCd<-lapply(lD,diff,lag=nxdias)
	vCd<-sapply(lCd,function(x){x[-c(1:indias)]})
	return ((vCa+vCd)>etpmx)
}
#-- inicio de lluvia
#	inicio probable:algunos dias antes(iDa) con precipitacion minima acumulada(nPmin)
#	falso inicio: algunos dias despues(iDd) en los que ocurre un evento (cFun) que aborta el inicio
#		-una racha de nLim dias secos
#		-la precipitacion es menor que cierta cantidad nLim (como una fraccion de la etp)
#-- iDia,iMes:dia y mes de partida
#	iDias:periodo para el cual se hace la estimacion
#	cFun:criterio para falso inicio
#	iDa:dias de acumulacion del inicio posible
#	nPm:precipitacion acumulada para inicio
#	iDd:dias despues al inicio posible
#	nLim:criterio limite que aborta el inicio
#	vFch con las fechas
#	vPmm con los datos de precipitacion
lluvia.inicio<-function(iDia=1,iMes=5,iDias=30,cFun="racha",iDa=2,nPm=20,iDd=15,nLim=NULL,vFch=NULL,vPmm=NULL){
	if(any(is.null(nLim),is.null(vFch),is.null(vPmm)))stop("faltan datos")
	aDi<-dates(sapply(levels(years(vFch)),fecha,d=iDia,m=iMes))
	lDp<-lapply(aDi,function(x){vPmm[vFch%in%seq.dates(from=x-iDa,length=iDias+iDa,by="days")]})
	vIp<-sapply(lDp,ini.cum,lapso=iDa,premin=nPm)
	iDb<-ifelse(cFun=="balance",iDa,0)
	lDp<-lapply(aDi,function(x){vPmm[vFch%in%seq.dates(from=x-iDb,length=iDias+iDd+iDb,by="days")]})
	vFi<-switch(cFun,racha=fin.racha(lDp,nxdias=iDd,rmax=nLim),
		etp=fin.etp(lDp,nxdias=iDd,etpmx=nLim),
		balance=fin.bal(lDp,indias=iDa,nxdias=iDd,etpmx=nLim))
	aDi<-sapply(aDi,seq.dates,length=iDias,by="days")
	fDi<-as.factor(sapply(aDi,days))
	vIn<-tapply(vIp&vFi,fDi,mean)
	vIf<-tapply(vIp&(!vFi),fDi,mean)
	vIp<-tapply(vIp,fDi,mean)
	vIr<-vIn
	i<-vIp>0
	vIr[i]<-1-vIr[i]/vIp[i]
	list(posible=vIp,falso=vIf,inicio=vIn,risk=vIr)
}
#
markov.estados<-function(vPmm,hoy=1,ayer=1,umbral=0.1){
	if(is.null(vPmm))stop("faltan datos")
	vH<-(vPmm>umbral)
	vA<-vH
	if(hoy==0)vH<-!vH
	if(ayer==0)vA<-!vA
	for(i in length(vH):2){vH[i]<-(vH[i]&vA[i-1])}
	vH[1]<-NA
	vH
}
#
frec.estados<-function(lD=NULL,ehoy,eayer,umbral){
	vE<-sapply(lD,markov.estados,ehoy,eayer,umbral)
	mE<-matrix(vE,ncol=length(lD))
	apply(mE,1,sum)
}
#
prob.estados<-function(lD=NULL,ehoy,eayer,umbral){
	vE<-sapply(lD,markov.estados,ehoy,eayer,umbral)
	mE<-matrix(vE,ncol=length(lD))
	p<-apply(mE,1,mean)
	p[-1]
}
#
probcon.estados<-function(lD=NULL,ehoy=1,eayer=1,umbral=0.1){
	vH<-sapply(lD,function(x){i<-(x>umbral);i[-1]})
	vA<-sapply(lD,function(x){ifelse(eayer==1,i<-(x>umbral),i<-(x<=umbral));i[-length(i)]})
	vH<-vH&vA
	sA<-apply(matrix(vA,ncol=length(lD)),1,sum)
	sA[sA==0]<-1
	sH<-apply(matrix(vH,ncol=length(lD)),1,sum)
	sH/sA
}

library("chron")
#library("hydroTSM")
#---funciones
#fecha<-function(d,m,a)
#es.fecha <- function(x)
#fechav <- function(vf)
#--convierte a vector c(d,m,a)
#fecha2vector <- function(x)
#--dias diferencia entre dos fechas
#f1, f2 pueden ser Dates o vectores c(d,m,a)
#dias.diff <- function(f1,f2)
#fecha d�as despu�s de otra fecha
#fo puede ser Dates o vector c(d,m,a)
#fecha.despues <- function(fo,dias=1)
#--secuencia de dias a partir de una fecha
#sec.dias <- function(fo,dias)
#--fecha limite derecho de np pentadas a partir de dia-mes-a�o
#(f0,f1],(f1,f2], ...
#la salida incluye f0
#pentadas <- function(f1,np=73)
#--pentadas del a�o a partir del 1 de enero
#corrige a�o bisiesto agregando un d�a a la �ltima pentada
#de febrero
#pentadas.ano <- function(a)
#--lista de secuencia de dias a partir del mismo dia-mes
#para varios a�os (vector anos)
#-ej.:mismo dia de varios a�os; salida en vector
#dd<-as.Date(unlist(sec.dias.ano(d,m,anos,dias=1)),origin=fecha(1,1,1970))
#sec.dias.ano <- function(d,m,anos,dias=1)
#--lee archivo .llu de agroclim y lo transforma
#en data.frame; si file, guarda en file
#precipitacion en mm; no en decimas de mm como agroclim
#substring(x64,seq(1,by=4,length=16),seq(4,by=4,length=16))
#datos.agroclim <- function(estacion,filesave,zoo=FALSE)
#--leer de archivos creados con transf.awk
#datos.dia<-function(archivo=NULL)
#--d�as con lluvia
#diashumedos.mes <- function(ppm,fecha,minppm=0.1)
#diashumedos.ano <- function(ppm,fecha,minppm=0.1)
#tabla a�o-mes. A�os sin datos agregados y puestos a 0
#diashumedos.tabla <- function(dd,minpp=0.1)
#--tabla de verdad de los estados posibles
#--o conteo
#d es objeto "zoo"
#orden de la cadena
markov.estados <- function(d,orden=1,frecuencia=TRUE,minpp=0.1){
#--datos en periodo
#dadas dos fechas, devuelve los datos comprendidos entre las dos
#excluyendo el correspondiente a f1 (dato(f1),...,dato(f2)]
#f1, f2 son Dates o vectores c(d,m,a)
#datos.intervalo <- function(colppm,colfecha,f1,f2)
#--datos de mismos dias en diferentes a�os
#df es data frame como el producido por datos.agroclim
#d,m: d�a,mes de inicio de secuencia de dias
#anos: vector de a�os; para c/a�o se extrae los datos de la secuencia
#datos.fecha <- function(df,colppmm=2,colfecha=1,d=1,m=1,dias=1,anos)
#--acumula datos seg�n periodo (p.ej. pentada)
#periodos es la secuencia de fechas que marcan los
#intervalos para acumular como el resultado de pentadas
#en el acumulado se excluye el dato de la primera fecha
#(abierto por la izquierda)
#acumulado.periodo <- function(colfecha,colppm,periodos,labels)
#--acumula sobre pentadas anuales de todos los datos
#acumulado.pentada <- function(df,colfecha=1,colppm=2)
#--agrega por mes y a�o los datos de una estacion
#estadisticas b�sicas de la precipitaci�n diaria
#minppm valor minimo para dejar de ser dia seco
#acumulado.mesano <- function(df,colfecha=1,colppm=2,minppm=0.1)
#-- L-momentos
#antes verificar pues errores
#e.g all values are equal
#Lmom <- function(df,colfecha=1,colppm=2)
#-- percentil y limites de confianza; emp�rica
#percentil estimado con la distribuci�n emp�rica
#dp son los datos de precipitaci�n
#qp el percentil
#confianza estimada con la binomial
#percentil.empirica <- function(dp,qp=0.5,cc=0.8)
#---------------------
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
#--a�o para clase Date
#no hace falta si chron
#otra posibilidad:
#? as.yearmon.Date definido en zoo
#setGeneric("years",
#           function(x)
#           standardGeneric("years"))
#setMethod("years",signature(x="Date"),
#          function(x) as.integer(format(x,"%Y")))
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
#fecha d�as despu�s de otra fecha
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
#--fecha limite derecho de np pentadas a partir de dia-mes-a�o
#(f0,f1],(f1,f2], ...
#la salida incluye f0
pentadas <- function(f1,np=73)
  seq(f1-1,by=5,length=np+1)
#--pentadas del a�o a partir del 1 de enero
#corrige a�o bisiesto agregando un d�a a la �ltima pentada
#de febrero
pentadas.ano <- function(a){
  f5 <- pentadas(fecha(a=a,m=1,d=1))
  if(leap.year(a))
    f5[13:length(f5)] <- f5[13:length(f5)]+1
  invisible(f5[-1])
}
#--lista de secuencia de dias a partir del mismo dia-mes
#para varios a�os (vector anos)
#-ej.:mismo dia de varios a�os; salida en vector
#dd<-as.Date(unlist(sec.dias.ano(d,m,anos,dias=1)),origin=fecha(1,1,1970))
sec.dias.ano <- function(d,m,anos,dias=1)
  as.Date(unlist(lapply(lapply(anos,fecha,m=m,d=d),seq,length=dias,
                        by="days")),origin=fecha(1,1,1970))
#--ruta archivo agroclim
#DIRLLU <- "c:/almacen/lluvia/"
file.agroclim <- function(x,dirllu="c:/almacen/lluvia/")
  paste(dirllu,x,".llu",sep="")
#--lee archivo .llu de agroclim y lo transforma
#en data.frame; si file, guarda en file
#precipitacion en mm; no en decimas de mm como agroclim
#substring(x64,seq(1,by=4,length=16),seq(4,by=4,length=16))
datos.agroclim <- function(estacion,filesave,zoo=FALSE){
  require("chron")
  datos.linea <- function(x)
    unlist(strsplit(x,split="[[:space:]]+"))[-1]
  datos.corregidos <- function(x,y,l){
    #print(paste("OjO: linea ",l,"de a�o",y,"no conforme 16 datos"))
    unlist(sapply(x,function(z){
      nc <- nchar(z)
      if(nc<=4)
        z
      else{
        k <- nc%/%4
        r <- nc%%4
        b <- seq(r+1,by=4,length=k)
        e <- seq(r+4,by=4,length=k)
        if(r>0){
          b <- c(1,b)
          e <- c(r,e)
        }
        substring(z,b,e)}}))
  }
  datos.ano <- function(u){
    x <- unlist(u)
    malo <- length(x)<2
    if(!malo){
      #ano <- as.integer(datos.linea(x[1])[2])
      ano <- as.integer(substring(x[1],8,11))
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
          u <- datos.linea(z)
          if(length(u)<16)
            u <- datos.corregidos(u,ano,linea)
          (as.numeric(u)/10)[1:dias.linea]
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
#  registros <- length(w)-1
#  k <- registros%%25
#  anos <- registros%/%25
  anos <- length(w)%/%25
  ianos <- rep(1:anos,each=25)
#  if(k>0){
#    anos <- anos+1
#    ianos <- c(ianos,rep(anos,k))
#  }
#  z <- lapply(split(w[-1],factor(ianos)),datos.ano)
  z <- lapply(split(w[2:(anos*25+1)],factor(ianos)),datos.ano)
  j <- sapply(z,is.null)
  if(any(j))
    z <- z[!j]
  if(all(j)){
    datos <- NULL
    print("no se leyeron datos")
  }
  else{
    print(paste(anos,"a�os de registros"))
    fc <- unlist(sapply(z,"[[","fecha"))
    pp <- unlist(sapply(z,"[[","ppmm"))
    if(zoo){
      require("zoo")
      datos <- zoo(pp,as.Date(fc))
    }
    else
        datos <- data.frame(fecha=as.Date(fc),ppmm=pp)
    if(!missing(filesave))
      save(datos,file=filesave,compress=TRUE)
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
  print(paste("datos desde",min(f),"hasta",max(f),";",nlevels(years(f)),"a�os de registro"))
  #fo<-origin(f[1])
  #print(paste("origen para fechas julianas",paste(fo["day"],fo["month"],fo["year"],sep="/")))
  data.frame(fch=f,pmm=z[,4]/10)
}
#resumen de los d�as con lluvia
diashumedos.mes <- function(ppm,fecha,minppm=0.1){
    h <- ppm>minppm
    x <- aggregate(h,list(years(fecha),
                          factor(months(fecha,abbr=T),ordered=T)),
                          sum)
    names(x) <- c("a�o","mes","dias")
    invisible(x)
}
#
diashumedos.ano <- function(ppm,fecha,minppm=0.1){
    h <- ppm>minppm
    x <- aggregate(h,list(years(fecha)),sum)
    names(x) <- c("a�o","dias")
    invisible(x)
}
#tabla a�o-mes
diashumedos.tabla <- function(dd,minpp=0.1){
    require("reshape")
    x <- switch(class(dd),
                "data.frame"=diashumedos.mes(dd$ppmm,dd$fecha,minpp),
                "zoo"=diashumedos.mes(coredata(dd),index(dd),minpp),NULL)
    if(is.null(x))
        stop("clase datos desconocida")
    z <- cast(x,a�o~mes,sum,value="dias",margins="grand_col")
    names(z)[14] <- "tota�o"
    #z trae meses (columnas) en orden alfab�tico
    #no conviene hacer match con c("Ene","Feb",...)
    #porque months devuelve en el idioma de la config. local
    k <- match(months(as.Date(paste("2000",1:12,1,sep="-")),abbr=T),
               names(z))
    #en orden temporal
    z[,2:13] <- z[,k]
    names(z)[2:13] <- names(z)[k]
    #a�os sin registro
    aa <- as.integer(levels(z$a�o))
    ra <- range(aa,na.rm=T)
    sa <- seq(ra[1],ra[2])
    j <- !(sa%in%aa)
    if(any(j)){
        mm <- cbind(data.frame(a�o=factor(sa[j],ordered=T)),
                    as.data.frame(matrix(0,nrow=sum(j),ncol=13)))
        names(mm) <- names(z)
        z <- rbind(z,mm)
        k <- match(sa,z$a�o)
        z <- z[k,]
    }
    j <- z$tota�o==0
    print(paste(sum(j),"a�os sin datos"))
    print(z$a�o[j])
    invisible(z)
}
#--tabla de verdad de los estados posibles
#--o conteo
#d es objeto "zoo" con los datos de una ventana de la serie
#orden de la cadena
markov.estados <- function(d,orden=1,frecuencia=TRUE,minpp=0.1){
    require(combinat)
    #matriz de estados: estados en las filas
    #ncol-1 orden de la cadena
    #ncol d�as consecutivos que conforman el estado
    me <- hcube(rep(2,orden+1),translation=-1)
    d <- d>minpp
    dh <- d
    for(j in 1:orden)
        dh <- merge(dh,lag(d,k=j),all=FALSE)
    nc <- 1:ncol(me)
    w <- apply(me,1,function(x){
        u <- dh
        for(j in nc){
            if(x[j]==0) u[,j] <- !u[,j]
        }
        z <- apply(u,1,all)
        if(frecuencia)
            z <- sum(z)
        z
    })
    nom <- apply(me,1,paste,collapse="")
    if(frecuencia)
        names(w) <- nom
    else
        colnames(w) <- nom
    invisible(w)
}
#--la probabilidad calculada como frecuencia relativa de estados
#d objeto "zoo" con los registros de varios a�os
#inicio es la fecha inicial de la ventana c(mes,dia)
#dias incluidos en la ventana
#orden es el orden de la cadena
#la ventana es una secuencia de d�as de un mismo a�o
#p.ej: 10 de mayo a 20 de junio
#la funci�n calcula las frecuencias en la ventana de c/a�o
#y al final calcula el promedio
#not.run.ex.: w <- aggregate(d,by=years(d),"[",3:10)
markov.probabilidad <- function(d,inicio=c(mes=5,dia=10),dias=30,
                                frelativa=TRUE,orden=1,minpp=0.1){
    #require("plyr")
    #fechas de partida
    fi <- as.Date(paste(levels(years(index(d))),
                  inicio[1],inicio[2],sep="-"))
    fw <- lapply(fi,seq,by="day",length.out=dias)
    fe <- sapply(fw,function(x){markov.estados(window(d,x),
                                               orden=orden,minpp=minpp)})
    if(frelativa)
        fe <- apply(fe,1,sum)/(length(fi)*(dias-1))
    invisible(fe)
}
#
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
#--datos de mismos dias en diferentes a�os
#df es data frame como el producido por datos.agroclim
#d,m: d�a,mes de inicio de secuencia de dias
#anos: vector de a�os; para c/a�o se extrae los datos de la secuencia
datos.fecha <- function(df,colppmm=2,colfecha=1,d=1,m=1,dias=1,anos){
  if(missing(anos))
    anos <- years(df[,colfecha])
  sec <- sec.dias.ano(d,m,anos,dias)
  j <- df[,colfecha]%in%sec
  w <- ifelse(any(j),df[j,c(colfecha,colppmm)],NULL)
  noe <- !(sec%in%df[,colfecha])
  if(any(noe))
    w <- list(datos=w,faltan=sec[noe])
  invisible(w)
}
#--acumula datos seg�n periodo (p.ej. pentada)
#periodos es la secuencia de fechas que marcan los
#intervalos para acumular como el resultado de pentadas
#en el acumulado se excluye el dato de la primera fecha
#(abierto por la izquierda)
acumulado.periodo <- function(colfecha,colppm,periodos,labels){
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
acumulado.pentada <- function(df,colfecha=1,colppm=2){
  anos <- years(df[,colfecha])
  w <- split(df,anos)
  lp <- lapply(w,function(x){
    ano <- fecha2vector(x[1,colfecha])[3]
    pen <- pentadas.ano(ano)+1
    pen <- c(fecha(1,1,ano),pen)
    fp <- cut(x[,colfecha],pen,labels=paste(ano,1:73,sep="."))
    tapply(x[,colppm],fp,sum)})
  invisible(lp)
}
#--agrega por mes y a�o los datos de una estacion
#estadisticas b�sicas de la precipitaci�n diaria
#minppm valor minimo para dejar de ser dia seco
acumulado.mesano <- function(df,colfecha=1,colppm=2,minppm=0.1){
  if(!is.data.frame(df))
    stop("datos no validos")
  fy <- years(df[,colfecha])
  fm <- months(as.chron(df[,colfecha]))
  h <- df[,colppm]>minppm
  #fym <- interaction(fy,fm,drop=TRUE)
  print(paste("registros de",nlevels(fy),"a�os"))
  y <- as.integer(levels(fy))
  sy <- seq(min(y),max(y))
  ppano <- tapply(df[,colppm],fy,sum)
  dhumano <- tapply(h,fy,sum)
  j <- dhumano>0
  ppdiano <- ppano
  ppdiano[j] <- round(ppano[j]/dhumano[j],1)
  ppmes <- aggregate(df[,colppm],list(mes=fm,ano=fy),sum)
  names(ppmes)[3] <- "ppmm"
  dhumes <- aggregate(h,list(mes=fm,ano=fy),sum)
  names(dhumes)[3] <- "dhum"
  j <- dhumes[,"dhum"]>0
  ppdiames <- ppmes
  ppdiames[j,"ppmm"] <- round(ppmes[j,"ppmm"]/dhumes[j,"dhum"],1)
  list(regano=nlevels(fy),
       anos=y,
       anosfaltan=sy[!(sy%in%y)],
       ppano=ppano,
       dhumano=dhumano,
       ppdiano=ppdiano,
       ppmes=ppmes,
       dhumes=dhumes,
       ppdiames=ppdiames
       )
}
#-- L-momentos
#antes verificar pues errores
#e.g all values are equal
Lmom <- function(df,colfecha=1,colppm=2){
  require("lmomco")
  #copiado de resumen.dia
  #lmomdh <- tapply(df[h,colppm],fy[h],lmoms)
  #lmom <- tapply(df[,colppm],fy,lmoms)
       #lambdapp <- sapply(lmom,"[[","lambdas"),
       #ratiospp <- sapply(lmom,"[[","ratios"),
       #lambdappdhum <- sapply(lmomdh,"[[","lambdas"),
       #ratiosppdhum <- sapply(lmomdh,"[[","ratios")
}
#-- percentil y limites de confianza; emp�rica
#percentil estimado con la distribuci�n emp�rica
#dp son los datos de precipitaci�n
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

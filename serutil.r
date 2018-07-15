#funciones utiles en series de tiempo
#tasav <- function(ws,lag)
#tasa de variacion
#timef <- function(ws)
#el resultado de time más explícito
#fecha <- function(serie,indice=0,am=integer(0))
#índice de la serie correspondiente a una fecha c(año,pos.ciclo)
#o fecha que corresponde a un índice en la serie
#indicadora <- function(serie,indice=integer(0),am=integer(0))
#variable indicadora de fecha; las posiciones a llenar con 1 se pueden
#indicar dando el indice=(desde,hasta) o bien las fechas de inicio a
#fin en am(desde=(ano,pos.ciclo),hasta=c(ano,pos.ciclo)). Si sólo se
#indicara desde, resulta una indicadora de pulso; si hasta se da fuera
#del rango de fechas de la serie o menor que desde, hasta se pone
#igual a datos
#shockfunpote <- function(sr,ames,delta)
#crea la variable de intervención función potencia=delta a partir de la
#fecha indicada en ames=c(desde=(año,mes))
#subseries <- function(serie)
#desglosa serie anual según la frecuencia
#periodica.sarima <- function(msarima)
#la prueba de McLeod para comprobar si hay autocorrelación periódica
#en los residuales de un modelo sarima
#devuelve el valor de la estadística de prueba y las autocorrelaciones
#de primer lapso de las subseries estacionales
#estadística~chi2(frequency)
#esperiodica <- function(serie)
#la prueba de Martin para autocorrelación periodica
#falta: considerar media robusta para centrar subseries de estación
#acf2: ACF y PACF en misma gráfica
#sarima.for: pronóstico seasonal-arima
#hendersonsim <- function(puntos)
#filtros simétricos de Henderson
#hendersonasi <- function(puntos,ic)
#filtro asimétrico de Henderson
#filtroasi <- function(filtro,ic){
#filtro asimétrico siguiendo la idea de los de Henderson
#filtro son los coeficientes del filtro
#ic sigue siendo el indicador de ruido/señal
#cumsuma <- function(residual,lag=12,omit.lag=TRUE,alfa=0.05,graf=TRUE)
#CUMSUM de residuales
#cumsuma2 <- function(residual,lag=12,omit.lag=TRUE,graf=TRUE)
#CUMSUM residuales**2; estadística F para heterocedasticidad
#inipdf: inicia el device-pdf con algunas opciones
#grfserie: gráfica serie archivo pdf
#serie.grafica <- function(serie,wp=2,hp=2,asp=1,mai=c(0.33,0.33,0.2,0.1),
#                          psize=8,font="Times",bgpr=gray(0.95),
#                          lcol="black",lwd=1,cex.ejes=1,
#                          radio=0,rlwd=1,rfcol="black",rbcol="transparent",
#                          Grid=TRUE,gcol="white",glwd=0.7,
#                          ejey,ejeylado=2,ejeypos,ejeylabdir=0,
#                          device="pdf",devfile,devoff=TRUE){
#-graficar serie con opciones para cuadrícula, color del área de
#-dibujo, etc. Misma funcionalidad de serie.grafica
#serie.plot <- function(serie,pmai=c(0.33,0.33,0.2,0.1),pmgp=c(2.5,0.8,0),
#                       lcol="black",lwd=1,bgpr="white",colrect="black",
#                       radio=0,rlwd=1,rfcol="black",rbcol="transparent",
#                       Grid=TRUE,gcol="black",glty=1,glwd=0.7,
#                       ptcl=-0.3,cex.ejes=1,ejeylado=2,ejeylabdir=0,
#                       colejes="black",colticks="black",
#                       ejey,ejeypos,pfin,ppin)
#rbank: escala para grafica serie con banking
#rectangularidad <- function(x,y,angulo=atan2(1,1),rectmin=0,rectmax=1)
#-algoritmo de Cleveland para seleccionar la rectangularidad
#-que deja los segmentos de la gráfica con una pendiente
#-de, en promedio, ángulo=angulo
#serie.rect <- function(serie,angulo=atan2(1,1),rectmin=0,rectmax=2)
#-cuando serie preprocesa para rectangularidad
#gserie: gráfica serie con banking archivo pdf
#gseriem: series múltiples con banking
#ejemplo series múltiples con xyplot
#initre: inicia parámetros lattice
#initrepdf: inicia device pdf de lattice
#clesta: gráfica de Cleveland para desplegar estacionalidad
#ejemplos monthplot (similar clesta)
#ejemplo gráfica desviaciones mensuales (similar clesta)
#ge: apoyo de clesta
#gHylleberg: gráfica Hylleberg para desplegar estacionalidad
#mrango: moving range para analizar transformacion de potencia
#mmedia: moving mean
#exporta a BATS
#exporta a Ox
#xtramo: exporta a tramo
tasav <- function(ws,lag=1) round(100*(diff(ws,lag=lag)/ws[1:(length(ws)-lag)]),0)
timef <- function(ws) paste(trunc(time(ws)),cycle(ws),sep="-")
tsdate <- function(ws) as.Date(paste(timef(ws),"01",sep="-"))
fecha <- function(serie,indice=0,am=integer(0)){
  tt <- time(serie)
  datos <- length(serie)
  res <- NULL
  if(indice>=1&indice<=datos){
    ff <- tt[indice]
    res <- paste(trunc(ff),cycle(ff),sep="-")
  }
  else{
    if(length(am)==2){
      ff <- am[1]+(am[2]-1)/frequency(serie)
      res <- which(signif(tt,10)==signif(ff,10))
    }
  }
  return(res)
}
indicadora <- function(serie,indice=integer(0),am=integer(0)){
  res <- NULL
  datos <- length(serie)
  tt <- signif(as.numeric(time(serie)),10)
  if(length(am)>1){
    ff <- signif(am[1]+(am[2]-1)/frequency(serie),10)
    indice[1] <- which(tt==ff)
  }
  if(length(am)>3){
    ff <- signif(am[3]+(am[4]-1)/frequency(serie),10)
    indice[2] <- which(tt==ff)
  }
  if(length(indice)==1)
    indice[2] <- indice[1]
  if(indice[2]<indice[1]|indice[2]>datos)
    indice[2] <- datos
  if(indice[1]>=1&indice[1]<=datos){
    res <- integer(datos)
    res[indice[1]:indice[2]] <- 1
  }
  invisible(res)
}
shockfunpote <- function(sr,ames,delta){
    d <- length(sr)
    j <- numeric(d)
    k <- seq(fecha(sr,am=ames),to=d)
    j[k] <- delta^(seq(0,along.with=k))
    invisible(j)
}
#
subseries <- function(serie){
    fs <- frequency(serie)
    n <- fs*(length(serie)%/%fs)
    nws <- ts(matrix(as.numeric(serie)[seq(n)],ncol=fs,byrow=TRUE),
              start=start(serie)[1],frequency=1)
    invisible(nws)
}
sarima.for <- function(xdata,nahead,p,d,q,P=0,D=0,Q=0,S=-1){
    data <- as.ts(xdata)
    constant <- 1:length(data)
    xmean <- matrix(1,length(data),1)
    if (d>0)
        fitit <- arima(data, order=c(p,d,q),
                       seasonal=list(order=c(P,D,Q), period=S),
                       xreg=constant,include.mean=F)
    if (d<.00001)
        fitit <- arima(data, order=c(p,d,q),
                       seasonal=list(order=c(P,D,Q), period=S),
                       xreg=xmean,include.mean=F)
    if (d+D>1)
        fitit <- arima(data, order=c(p,d,q),
                       seasonal=list(order=c(P,D,Q),
                                     period=S))
    if (d>0)
        nureg <- (length(data)+1):(length(data)+nahead)
    if (d<.00001)
        nureg <- matrix(1,length(nahead),1)
    if (d+D>1)
        nureg <- NULL
    fore <- predict(fitit, n.ahead=nahead, newxreg=nureg)

#
# graph:
#
    U <- fore$pred + 2*fore$se
    L <- fore$pred - 2*fore$se
    minx <- min(data,L)
    maxx <- max(data,U)
    ts.plot(data,fore$pred,col=1:2, ylim=c(minx,maxx),
            ylab=deparse(substitute(xdata)))
    lines(fore$pred, col="red", type="p")
    lines(U, col="blue", lty="dashed")
    lines(L, col="blue", lty="dashed")
#
  return(fore)
}
#---A.C.Harvey, p.257
#rs residuales del modelo ajustado con arima
cumsuma <- function(residual,lag=12,omit.lag=TRUE,alfa=0.05,graf=TRUE){
  if(omit.lag)
    residual <- ts(residual[-(seq(lag))],start=start(lag(residual,-lag)),
                   frequency=frequency(residual))
  n <- length(residual)
  crs <- ts(cumsum(residual)/sd(residual),start=start(residual),
            frequency=frequency(residual))
  pa <- ifelse(alfa==0.05,0.948,0.850)
  lcs <- pa*sqrt(n)+2*pa*seq(n)/sqrt(n)
  if(graf){
    plot(crs,main="",xlab="",ylab="cumsum",type="o")
    #lines(smooth(crs))
    lines(list(x=time(crs),y=lcs))
    lines(list(x=time(crs),y=-lcs))
  }
  invisible(list(crs=crs,lcs=lcs))
}
#residuales**2 y estadística para heterogeneidad
cumsuma2 <- function(residual,lag=12,omit.lag=TRUE,graf=TRUE){
  if(omit.lag)
    residual <- ts(residual[-(seq(lag))],start=start(lag(residual,-lag)),
                   frequency=frequency(residual))
  n <- length(residual)
  rs2 <- residual*residual
  crs2 <- ts(cumsum(rs2)/sum(rs2),start=start(residual),
             frequency=frequency(residual))
  h <- round(n/3)
  H <- sum(rs2[(n-h+1):n])/sum(rs2[1:(h+1)])
  if(graf){
    plot(crs2,ylab="cumsum**2",main="",xlab="",type="o")
    #lines(smooth(crs2))
  }
  invisible(list(crs2=crs2,h=h,H=H,pvH=pf(H,h,h,lower.tail=FALSE)))
}
#-----
acf2 <- function(data,maxlag=NULL,titulo,
  mai=par("mai"),oma=c(1,1.2,1,1),mgp=c(1.5,0.6,0)){
  num <-length(data)
  if (is.null(maxlag)) maxlag=ceiling(10*log10(num))
  mp1 <-maxlag+1
  ACF <-acf(data, maxlag, plot=F)$acf[2:mp1]
  PACF <-pacf(data, maxlag, plot=F)$acf
  LAG <-1:maxlag
  minA <-min(ACF)
  minP <-min(PACF)
  U <-2/sqrt(num)
  L <- -U
  minu <-min(minA,minP,L)-.01
  par(mfrow=c(2,1), mai = mai,oma = oma, mgp = mgp)
  if(missing(titulo))
    titulo <- paste("Series: ",deparse(substitute(data)))
  plot(LAG, ACF, type="h",ylim=c(minu,1),
       main=titulo,xlab="")
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,1))
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  invisible(cbind(LAG, ACF, PACF))
}
#-----
#autocorrelación periódica en los residuales de un sarima
#libro de McLeod
periodica.sarima <- function(msarima){
    sr <- subseries(residuals(msarima))
    ac <- apply(sr,2,acf,lag.max=1,plot=FALSE)
    ac1 <- sapply(ac,"[[","acf")[2,]
    tst <- nrow(sr)*sum(ac1*ac1)
    invisible(list(tst=tst,ac1p=ac1))
}
#-----
#es periódica
#en base al artículo de Martin
esperiodica <- function(serie){
    #sin tendencia
    #ma <- tapply(serie,rep(seq(a),each=p),mean)
    #st <- (serie-rep(ma,each=p))
    print("sin tendencia ?")
    #falta control de subseries de mismo tamaño
    n <- length(serie)
    p <- frequency(serie)
    a <- n%/%p
    if(n!=a*p){
        print("estaciones incompletas; se ajusta")
        serie <- ts(as.numeric(serie)[seq(a*p)],start=start(serie),
                    frequency=p)
        n <- a*p
    }
    ms <- rep(seq(p),a)
    me <- tapply(serie,ms,mean,trim=0)
    xt <- (serie>=rep(me,a))*1
    dt <- c(0,as.numeric((diff(xt)!=0)*1))
    Dv <- tapply(dt,ms,sum)
    D <- sum(Dv)
    #
    n1 <- n-1
    D1 <- D/n1
    ut <- 0
    for(i in 1:(p-1)){
        for(j in (i+1):p){
            k <- j-i
            if(k>=1&k<=4){
                w <- sum(dt[2:(n-k)]*dt[(2+k):n])/(n1-k)
            }
            else{
                if(k>=5&k<=7){
                    w <- D1*D1
                }
                else{
                    if(k>=8&k<=11){
                        w <- sum(dt[2:(n+k-p)]*dt[(2-k+p):n])/(n1+k-p)
                    }
                }
            }
            sig <- sqrt(2*(D1-w))
            ut <- ut+abs(Dv[i]-Dv[j])/sig
        }
    }
    invisible(2*ut/(p*(p-1)*sqrt(a)))
}
#--filtro simétrico de Henderson
#tomadas de "An introductory course on time series analysis"
#del Australian Bureau of Statistics
#oJo: error en ecuación: en documento viene c0+c1*x2
hendersonsim <- function(puntos){
    require("PolynomF")
    p9 <- polynom(a=c(-5670,-19323,-4302,57203,99288,78204,34272,
                  8592,1152,64))
    p2 <- polynom(a=c(-4,12,3))
    r <- (puntos-1)/2
    di <- 8*predict(p9,r)
    c0 <- (315*predict(p2,r))/di
    c1 <- 3465/di
    r1 <- (r+1)*(r+1)
    r2 <- (r+2)*(r+2)
    r3 <- (r+3)*(r+3)
    wi <- sapply(seq(-r,r),
                 function(x){x2 <- x*x;
                             (r1-x2)*(r2-x2)*(r3-x2)*(c0-c1*x2)})
    invisible(wi)
}
#--filtros asimétricos de Henderson
#en base a artículo de Doherty
hendersonasi <- function(puntos,ic){
    coef <- function(m){
        r <- 1:m
        i <- (m+1):puntos
        z <- w[r]
        wi <- w[i]
        sw <- sum(wi)
        m2 <- (m+1)/2
        swi <- sum(i*wi)-m2*sw
        lbm <- lb/(1+(lb*m*(m-1)*m2)/6)
        c(numeric(puntos-m),z+sw/m+lbm*swi*(r-m2))
    }
    w <- hendersonsim(puntos)
    lb <- 4/(pi*ic*ic)
    p2 <- 1+(puntos-1)/2
    ha <- lapply(seq(p2,puntos-1),coef)
    ha[[p2]] <- w
    invisible(matrix(unlist(ha),byrow=F,ncol=p2))
}
#--filtro asimétrico siguiendo la idea de los de Henderson
#filtro son los coeficientes del filtro
#ic sigue siendo el indicador de ruido/señal
filtroasi <- function(filtro,ic){
    coef <- function(m,w){
        r <- 1:m
        i <- (m+1):puntos
        z <- w[r]
        wi <- w[i]
        sw <- sum(wi)
        m2 <- (m+1)/2
        swi <- sum(i*wi)-m2*sw
        lbm <- lb/(1+(lb*m*(m-1)*m2)/6)
        c(numeric(puntos-m),z+sw/m+lbm*swi*(r-m2))
    }
    puntos <- length(filtro)
    lb <- 4/(pi*ic*ic)
    p2 <- 1+(puntos-1)/2
    ha <- lapply(seq(p2,puntos-1),coef,w=filtro)
    ha[[p2]] <- filtro
    invisible(matrix(unlist(ha),byrow=F,ncol=p2))
}
#-----
#Lefrancois,B. 1991. Detecting over-influential observations in time
# series. Biometrika 78(1):91-99
overinfluential <- function(serie,maxlag=3,ac=numeric(0)){
  n <- length(serie)
  if(maxlag>(n/2))
    maxlag <- floor(n/2)
  if(length(ac)==0)
    ac <- acf(serie,maxlag=maxlag,plot=FALSE)$acf[-1]
  z <- (serie-mean(serie))/sd(serie)
  z2 <- z*z
  n1 <- n-1
  mio <- matrix(0,nrow=n,ncol=maxlag)
  for(i in 1:maxlag){
    k <- (i+1):(n-i)
    for(j in k)
      mio[j,i] <- (z[j]*(z[j-i]+z[j+i])-ac[i]*z2[j])/(1-z2[j]/n1)
  }
  invisible(mio)
}
#identificación automática de outliers (Gómez y Maravall
outlieraditivo <- function(residual,cota=3.5){
  si <- 1.483*median(abs(residual-median(residual)))
  invisible(which(abs(residual/si)>=cota))
}
#------ graficas
#directoriogrf <- "c:/eddy/wp/matanza/grf/"
filegraf <- function(sf) paste(directoriogrf,sf,".pdf",sep="")
inipdf <- function(file=character(0),w=5,h=5,ps=8,ff="Times",title=""){
  if(length(file)>0)
    pdf(file=filegraf(file),width=w,height=h,family=ff,pointsize=ps,
        title=title,encoding="ISOLatin1")
}
grfserie <- function(serie,sf=character(0),gw=2.45,gh=1.8,ejey="",circulo=0){
  if(devof <- length(sf)>0){
    sf <- paste(directoriogrf,sf,".pdf",sep="")
    pdf(file=sf,family="Times",pointsize=8,paper="special",
        title="",width=gw,height=gh)
    par(mai=c(0.33,0.33,0.2,0.08))
    #radio <- 0.2
  }
  #else
    #radio <- 0
  #par(pin=c(gw,gh))
  #par(fin=c(gw+0.55+0.08+0.1,gh+0.33+0.08+0.1))
  #print(par("plt"))
  #print(par("pin"))
  #print(par("fin"))
  plot(serie,main="",xlab="",ylab="",cex.axis=0.7)
  if(circulo>0){
    symbols(serie,circles=rep(circulo,length(serie)),add=TRUE,
            inches=FALSE,bg="white")
  }
  mtext(ejey,side=3,line=0,adj=0,cex=0.7)
  if(devof)
    dev.off()
}
#grafica serie control de rectangularidad
#devfile: archivo de salida
#device: tipo
#wp,hp ancho y alto (pulgadas) de la región de dibujo
#región de gráfica es calculada
#map:mai
#radio>0 para dibujar círculos
#devoff:controla cierre de salida
serie.plot <- function(serie,pmai=c(0.33,0.33,0.2,0.1),pmgp=c(2.5,0.8,0),
                       lcol="black",lwd=1,bgpr="white",colrect="black",
                       radio=0,rlwd=1,rfcol="black",rbcol="transparent",
                       Grid=TRUE,gcol="black",glty=1,glwd=0.7,
                       ptcl=-0.3,cex.ejes=1,ejeylado=2,ejeylabdir=0,
                       colejes="black",colticks="black",
                       ejey,ejeypos,pfin,ppin){
    op <- par(no.readonly=TRUE)
    par(tcl=ptcl,mgp=pmgp,mai=pmai,cex.axis=cex.ejes,cex.lab=cex.ejes)
    if(!missing(pfin))
        par(fin=pfin)
    if(!missing(ppin))
        par(pin=ppin)
    if(missing(ejeypos))
        ejeypos=par("mgp")[1]
    if(missing(ejey))
        ejey <- deparse(substitute(serie))
    #
    plot(serie,ann=FALSE,axes=FALSE,type="n")
    pu <- par("usr")
    rect(pu[1],pu[3],pu[2],pu[4],col=bgpr,border=colrect)
    if(Grid){
        grid(col=gcol,lty=glty,lwd=glwd)
    }
    if(lwd>0)
        lines(serie,col=lcol,lwd=lwd)
    if(radio>0)
        symbols(serie,circles=rep(radio,length(serie)),add=TRUE,
                inches=FALSE,fg=rfcol,bg=rbcol,lwd=rlwd)
    axis(1,col=colejes,col.ticks=colticks)
    axis(2,col=colejes,col.ticks=colticks,las=ejeylabdir)
    mtext(ejey,side=ejeylado,line=ejeypos)
    par(op)
}
serie.grafica <- function(serie,wp=2,hp=2,asp=1,mai=c(0.33,0.33,0.2,0.1),
                          psize=8,font="Times",bgpr=gray(0.95),
                          lcol="black",lwd=1,cex.ejes=1,
                          radio=0,rlwd=1,rfcol="black",rbcol="transparent",
                          Grid=TRUE,gcol="white",glwd=0.7,
                          ejey,ejeylado=2,ejeypos,ejeylabdir=0,
                          device="pdf",devfile,devoff=TRUE){
    require("Cairo")
    #op <- par(no.readonly=TRUE)
    rp <- wp*c(1,asp)
    rg <- rp+c(mai[2]+mai[4],mai[1]+mai[3])
    ndv <- NA
    if(!missing(devfile))
        ndv <- Cairo(type=device,units="in",width=rg[1],height=rg[2],
                     file=devfile,family=font,pointsize=psize)
    par(fin=rg)
    par(pin=rp)
    par(tcl=-0.3)
    par(mgp=c(2.5,0.8,0))
    par(mai=mai)
    #
    plot(serie,ann=FALSE,axes=FALSE,type="n")
    pu <- par("usr")
    rect(pu[1],pu[3],pu[2],pu[4],col=bgpr,border=NA)
    if(Grid)
        grid(col=gcol,lty=1,lwd=glwd)
    if(lwd>0)
        lines(serie,col=lcol,lwd=lwd)
    if(radio>0)
        symbols(serie,circles=rep(radio,length(serie)),add=TRUE,
                inches=FALSE,fg=rfcol,bg=rbcol,lwd=rlwd)
    par(cex.axis=cex.ejes)
    par(cex.lab=cex.ejes)
    axis(1,col=bgpr,col.ticks=bgpr)
    axis(2,col=bgpr,col.ticks=bgpr,las=ejeylabdir)
    if(missing(ejey))
        ejey <- deparse(substitute(serie))
    if(missing(ejeypos))
        ejeypos=par("mgp")[1]
    mtext(ejey,side=ejeylado,line=ejeypos)
    #
    if(devoff){
        #par(op)
        dev.off()
    }
    invisible(ndv)
}
serie.ggplot <- function(serie,pgeo=c("line"),pasp=0.7,
                         pxlab="",pylab="",pmain=""){
    qplot(x=as.vector(time(serie)),y=as.vector(serie),
          geom=pgeo,asp=pasp,xlab=pxlab,ylab=pylab,main=pmain)
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
#rectangularidad
#algoritmo de Cleveland
#angulo es el correspondiente de la pendiente deseada
#rectmin, rectmax marcan el intervalo de búsqueda de la
#rectangularidad que asegura la pendiente
rectangularidad <- function(x,y,angulo=atan2(1,1),rectmin=0,rectmax=1){
    vd <- diff(y)/diff(range(y))
    hd <- diff(x)/diff(range(x))
    vd2 <- vd*vd
    hd2 <- hd*hd
    pen <- abs(vd/hd)
    m <- try(uniroot(function(x)(weighted.mean(atan(x*pen),
                                               sqrt(hd2+x*x*vd2))-angulo),
            lower=rectmin,upper=rectmax))
    if(class(m)=="try-error")
        rect <- NA
    else
        rect <- m$root
    invisible(rect)
}
serie.rect <- function(serie,angulo=atan2(1,1),rectmin=0,rectmax=2){
    if(class(serie)=="ts")
        rectangularidad(as.vector(time(serie)),as.vector(serie),
                        angulo,rectmin,rectmax)
    else
        stop("no es serie")
}
#
#olx:orientacion label eje;0 paralelo,1 horizontal
#ltk:long.tick.marks
#ov:radio de los circulos; ov=0 solo lineas
gserie <- function(na=NA,serie,ov=0,ejey="",gw=2,mb=0.3,ml=0.3,mt=0.15,
           mr=0.1,maw=0,mah=0,ps=8,bank=0,ole=NA,ltk=NA,ejeytop=T){
  if(bank==0) bank <- rbank(serie)
  gh <- gw*bank
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
  if(ov) symbols(serie,circles=rep(ov,length(serie)),add=TRUE,
                 inches=FALSE,bg="white")
  if(ejeytop) mtext(ejey,side=3,line=0,adj=0,cex=0.7)
  if(!is.na(na)) dev.off()
}
#series multiples
gseriem <- function(na=NA,serie,orden=0,ids="",ejey="",gw=2,mb=0.3,
                    ml=0.3,mt=0.15,mr=0.1,meg=0,maw=0,mah=0,
                    ps=8,cexeje=0.7,cexlab=0.7,cextop=0.7,
                    radio=0.1,bank=0,ov=FALSE){
  if(!is.mts(serie)){
    print("no es multiple")
    return(0)
  }
  np <- attr(serie,"dim")[1]
  ns <- attr(serie,"dim")[2]
  if(length(orden)<ns)orden <- 1:ns
  if(length(ids)<ns)ids <- letters[1:ns]
  rg <- rep(0,ns)
  if(bank==0)
    for(i in 1:ns)rg[i] <- rbank(serie[,i])*gw
  else
    rg <- bank*rep(gw,ns)
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
  if(ov) symbols(serie[,j],circles=rep(radio,np),add=TRUE,
                 inches=FALSE,bg="white")
  mtext(ejey,side=3,line=0,adj=0,cex=cextop)
  if(ns>2){
    for(i in 2:(ns-1)){
      #par(pin=c(gw,rg[orden[i]]))
      par(mai=c(0,ml,0,mr))
      j <- orden[i]
      plot(serie[,j],main="",xaxt="n",ylab=ids[j],type="l")
      if(ov) symbols(serie[,j],circles=rep(radio,np),add=TRUE,
                     inches=FALSE,bg="white")
    }
  }
  #par(pin=c(gw,rg[orden[ns]]))
  par(mai=c(mb,ml,0,mr))
  j <- orden[ns]
  plot(serie[,j],main="",xlab="",ylab=ids[j],type="l")
  if(ov) symbols(serie[,j],circles=rep(radio,np),add=TRUE,
                 inches=FALSE,bg="white")
  if(!is.na(na)) dev.off()
}
#lattice
tpdf <- function(){
    require(lattice)
    canonical.theme("pdf")#opciones pdf
}
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
#tipo="g" desviaciones c.r.a media del año
#en otro caso, desviaciones c.r.a media subserie-mensual
#color1 color primer plano, color2 color del fondo
#color1=gray(0.75),color2=gray(1)
gHylleberg <- function(serie,tipo="g",color1=NULL,color2=NULL)
{
  afin <- end(serie)[1]
  #solo años completos
  #se podría modificar si tipo!="g"
  if((length(serie)%%12)!=0){
    afin <- afin-1
    serie <- window(serie,end=c(afin,12))
  }
  aini <- start(serie)[1]
  anos <- afin-aini+1
  meses <- factor(rep(1:12,anos),
                  label=c("enero","febrero","marzo","abril","mayo",
                    "junio","julio","agosto","septiembre","octubre",
                    "noviembre","diciembre"))
  #se podría modificar para considerar otra estadística
  #como nivel de referencia
  mm <- tapply(serie,meses,mean,na.rm=TRUE)
  if(tipo=="g")
    corr <- rep(tapply(serie,rep(1:anos,each=12),mean,na.rm=TRUE),
                each=12)
  else
    corr <- rep(mm,anos)
  desvserie <- serie-corr
  ss <- matrix(c(rep(0,12),round(mm,0)),byrow=FALSE,ncol=2)
  xmes <- rep(aini:afin,rep(12,anos))
  require("lattice")
  xyplot(desvserie~xmes|meses,layout=c(3,4),type="h",
         main=NULL,xlab=NULL,ylab=NULL,
         strip=strip.custom(
           shingle.intervals=ss,strip.levels=c(FALSE,TRUE),
           strip.names=c(TRUE,FALSE),fg=color1,bg=color2,
           par.strip.text=list(cex=0.8)),scales=list(tck=0.5,cex=0.8)
         )
}
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
#---exporta a BATS
xbats <- function(serie,archivo,variable="",descripcion=""){
  fu <- file(archivo,open="w")
  #una linea descriptiva
  if(descripcion=="")
    descripcion <- deparse(substitute(serie))
  descripcion <- paste("\\",descripcion,sep="")
  if(length(descripcion)!=2)
    descripcion <- c(descripcion[1],"\\")
  cat(descripcion,file=fu,sep="\n")
  if(variable=="")
    variable <- "yt"
  cat(paste("\\",variable,"\n",sep=""),file=fu)
  freq <- frequency(serie)
  nobs <- length(serie)
  sfrq <- paste("\\",freq,sep="")
  nfrq <- switch(freq%%10,"\\YEAR","\\MONTH")
  mesi <- paste("\\",start(serie)[2],sep="")
  anoi <- paste("\\",start(serie)[1],sep="")
  mesf <- paste("\\",end(serie)[2],sep="")
  anof <- paste("\\",end(serie)[1],sep="")
  snob <- paste("\\",nobs,sep="")
  cat(c(nfrq,"\\YEAR",sfrq,mesi,anoi,mesf,anof,snob),file=fu,sep="\n")
  for(i in 1:nobs){
    cat(c("\\",as.numeric(serie[i])),file=fu)
    cat("\n",file=fu)
  }
  close(fu)
}
#--- exporta a Tramo
xtramo <- function(serie,archivo,descripcion=""){
  fu <- file(archivo,open="w")
  #una linea descriptiva
  if(descripcion=="")
    descripcion <- deparse(substitute(serie))
  cat(descripcion,"\n",file=fu,sep="")
  #numobs año-inicio periodo-inicio periodos.por.año
  nobs <- length(serie)
  cat(c(nobs,start(serie)[1],start(serie)[2],
        frequency(serie),"\n"),file=fu,sep=" ")
  for(i in 1:nobs) cat(serie[i],file=fu,sep="\n")
  close(fu)
}
#--- simula localmente estacionario ARMA(1,1)
#- http://www.gmge.org/2012/06/this-is-not-white-noise/
#- set.seed(42)
#  yy <- lsarma11.sim(n = 1000, theta = c(0.5, -1), sigma = "constant")
lsarma11.sim <- function (n, phi = 0, theta = 0, sigma = 1, rand.gen = rnorm, 
    innov = rand.gen(n)) 
{
    u.d <- seq_len(n)/n
    phi.u <- as.function(polynomial(phi))
    theta.u <- as.function(polynomial(theta))
    phi.t <- phi.u(u.d)
    theta.t <- theta.u(u.d)
    if (sigma == "constant") {
        var.arma11 <- (1 + theta.t^2 - 2 * theta.t * phi.t)/(1 - 
            phi.t^2)
        sigma.t <- sqrt(1/var.arma11)
    }
    else {
        sigma.u <- as.function(polynomial(sigma))
        sigma.t <- sigma.u(u.d)
    }
    xx <- innov
    for (ii in 2:n) {
        xx[ii] <- phi.t[ii] * xx[ii - 1] + sigma.t[ii] * innov[ii] + 
            theta.t[ii - 1] * sigma.t[ii - 1] * innov[ii - 1]
    }
    return(ts(xx))
}
#--- box-test localmente estacionario
#- http://www.gmge.org/2012/06/this-is-not-white-noise/
# box.test.ls(yy, lag = 5, K = 2, type = "Ljung-Box")
box.test.ls <- function (x, lag = 1, type = c("Ljung-Box", "Box-Pierce"), fitdf = 0, 
    K = 1, plot = FALSE, ...) 
{
    stopifnot(is.numeric(x), K >= 1, K == round(K), is.logical(plot))
    type <- match.arg(type)
    if (K >= length(x)) {
        stop("Number of windows must be smaller than the length of the time series.")
    }
    N.k <- floor(length(x)/K)
    if (K == 1) {
        Box.test(x, type, lag = lag)
    }
    else {
        METHOD <- paste0("LS ", type, " test; \n Number of windows = ", 
            K, "; non-overlapping window size = ", N.k)
        stats.pvals <- rollapply(x, width = N.k, by = N.k, FUN = function(x) {
            out <- Box.test(x, lag = lag, type)
            return(c(out$statistic, out$p.value))
        }, fill = NA)
        stats.pvals <- stats.pvals[!is.na(stats.pvals[, 1]), 
            ]
        colnames(stats.pvals) <- c("statistic", "p-value")
        if (plot) {
            plot(stats.pvals[, "p-value"], ylim = c(0, max(stats.pvals[, 
                "p-value"])), type = "h", lwd = 2, ylab = "p-value", 
                xlab = "Window")
            abline(0.05, 0, col = 4, lty = 4)
            abline(0, 0, lty = 1, col = "gray")
        }
        STATISTIC <- sum(stats.pvals[, "statistic"])
        names(STATISTIC) <- "X-squared"
        df.total <- lag * K
        names(df.total) <- "df"
        PVAL <- 1 - pchisq(STATISTIC, df.total)
        return(structure(list(statistic = STATISTIC, parameter = df.total, 
            p.value = PVAL, method = METHOD, data.name = deparse(substitute(x))), 
            class = "htest"))
    }
}

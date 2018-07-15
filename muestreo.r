#---funciones
#nmai <- function(cv=0,beta=0.90,error=0,N=0)
#nestratificado <- function(vz=0,sh=numeric(0),nh=numeric(0),
#                           asignacion="arbitraria",wnh=numeric(0))
#asignacionEstratos <- function(n,tipo="neyman",es=NULL,y=numeric(0),
#                               x=numeric(0))
#maiSinReemplazo <- function(id,n)
#msistematicarandom <- function(id,n,pik=NULL)
#msistematica <- function(id,n)
#selesegupm <- function(datos,vidupm,vnumseg,n)
#seleccionReplicada <- function(identidad,n,replicas=4,muestreo="mai")
#estratosProgresion <- function(x,ne=5)
#estratosDalenius <- function(x,b=numeric(0),ne=5,netm=0)
#estratosSigmaGap <- function(x)
#estratosCensado <- function(x,cv=0.05,maxite=10)
#estratosLavalle <- function(x,b=numeric(0),cv=0.05,ne=5,xp=1,
#                                        eps=0.1,maxite=100)
#selpuntos <- function(impar=1,bd=NULL,pd=NULL,nomdpto=character(0))
#slc <- function(dfupm,dpt,estr,n)
#tslc <- function(dfsl,con)
#splitupmseg <- function(upmseg)
#upmsrepetidas <- function(dfo,dfn)
#sustituye <- function(dfs,dfns,dpt,estr,n)
#segmentosel <- function(dfupm,dpt,estr,n,sgexcluidos)
#datadpto <- function(dircob,cober,dpto)
#epapel <- function(segme,intervalo=0,nestratos)
#selsegdpto <- function(dfupm,dpto,sexcluido,nremplazo)
#muestrasegmentos <- function(dfupmseg,tamu,
#                             sexcluidos=character(0))
#selreplicapapel <- function(dpto,ue,estrato,numstratapap,
#                            lstexcluidos,tamustratapap=1)
#muestraestratopapel <- function(ue,numstratpap,
#                                sexcluidos=character(0))
#sustituyeseg <- function(selold,selact,samdpt,dep)
#selsegmento <- function(upms,estrato)
#muestrarenovada <- function(dfs,dfns,dpt,estr,n)
#muestranueva <- function(dfs,dfns,dpt,estr)
#---modificaciones
#...2012-07-05
#se agregan funciones escritas para pri2011
#...2011-06-06
#.en seleccionReplicada sustituido mmai por maiSinReemplazo para la
#selección dentro de cada "zona"
#---
library("sampling")
#tamaño de muestra
nmai <- function(cv=0,beta=0.90,error=0,N=0){
  stopifnot(cv>0,cv<1,error>0,beta>0,beta<1)
  z <- qnorm(1-(1-beta)/2)
  n <- (z/error)^2*(cv*cv)
  if(N>0)
    n <- n/(1+n/N)
  n
}
#estimacion del total
#la varianza objetivo (vz) se puede determinar como
#::((total)*(cv deseado))^2
#::(d/t)^2 donde d:"error" del estimado t:percentil confianza
#::el error se puede determinar como fracción del total
#asignacion:
#::arbitraria:indicar fraccion muestra correspondiente estratos wnh
#::Neyman
#::proporcional
nestratificado <- function(vz=0,sh=numeric(0),nh=numeric(0),
                           asignacion="arbitraria",wnh=numeric(0)){
  stopifnot(vz>0,length(nh)>0,length(nh)==length(sh))
  if(asignacion=="arbitraria"&&length(wnh)!=length(nh))stop("error")
  nsh <- nh*sh
  wnh <- switch(asignacion,neyman=nsh/sum(nsh),
                proporcional=nh/sum(nh),
                wnh)
  ns1 <- sum((nsh*nsh)/wnh)
  ns2 <- sum(nh*sh*sh)
  ns1/(vz+ns2)
}
#asignacion de la muestra a los estratos
asignacionEstratos <- function(n,tipo="neyman",es=NULL,y=numeric(0),
                               x=numeric(0)){
  stopifnot(n>0,tipo%in%c("neyman","proporcional","ppsx"))
  if(tipo=="neyman"&&(length(y)==0||!is.factor(es)))stop("error")
  if(tipo=="proporcional"&&(!is.factor(es)))stop("error")
  if(tipo=="ppsx"&&length(x)==0)stop("error")
  Wh <- switch(tipo,
               neyman=tabulate(es)*tapply(y,es,sd),
               proporcional=tabulate(es),
               ppsx=x)
  n*(Wh/sum(Wh))
}
##distribución entera óptima de Neyman
##Wright, T. 2012 Amer.Stat. 66(4)
a_Neyman <- function(n,y,e){
    ## y var. estratificación
    ## e id. estratos como factor
    wh <- tabulate(es)*tapply(y, es, sd)
    nh <- n*wh/sum(wh)
    ii <- TRUE
    for(nn in nh) ii <- ii && nn == floor(nn)
    if(!ii){
        mx <- max(ceiling(nh))
        nn <- numeric(mx)+2
        for(ii in 2:mx) nn[ii] <- nn[ii-1]+2*ii
        uu <- as.vector(wh%o%(1/sqrt(nn)))
        names(uu) <- rep(levels(es),mx)
        uu <- (uu[order(uu,decreasing=T)])[seq_len(n-nlevels(es))]
        nh <- as.vector(table(names(uu)))+1
    }
    names(nh) <- levels(es)
    invisible(nh)
}
a_Ney <- function(n, y, e){
    wh <- tabulate(es)*tapply(y, es, sd)
    nh <- n*wh/sum(wh)
    ii <- TRUE
    for(nn in nh) ii <- ii && nn == floor(nn)
    if(!ii){
        oe <- order(wh,decreasing=T)
        wh <- wh[oe]
        mx <- ceiling(max(nh))
        hh <- integer(mx) + 2L
        for(ii in 2:mx) hh[ii] <- hh[ii-1] + 2*ii
        ne <- 3
        #ne <- length(nh)
        oo <- integer(ne)
        nj <- 1
        ii <- TRUE
        nh <- wh/sqrt(hh[nj])
        mx <- wh[1]/sqrt(hh[1])
        hx <- length(hh)
        ni <- 1
        while(mx >= nh[ni] & nj <= hx){
            if(ni < ne)
                ni <- ni + 1
            mx <- wh[1]/sqrt(hh[nj])
            nj <- nj + 1
        }
        for(jj in 2:ne){
            nj <- 1
            while(wh[jj]/sqrt(hh[nj]) > mx & nj <= hx){
                nj <- nj + 1
            }
            oo[jj] <- nj
        }
    }
}
#seleccion mai sin reemplazo
maiSinReemplazo <- function(id,n){
  m <- srswor(n,length(id))
  invisible(id[m==1])
}
#seleccion sistematica unidades orden aleatorio
#pps vector de probabilidad de inclusion
msistematica <- function(id,n,pik=NULL){
  N <- length(id)
  if(is.null(pik)) pik <- rep(1,N)
  pik <- inclusionprobabilities(pik,n)
  m <- UPrandomsystematic(pik)
  invisible(id[m==1])
}
#cuando los elementos están ordenados por algún criterio
#geográfico o de otro tipo para que la muestra quede
#dispersa en esa clasificación ordenada
#selección con igual probabilidad
#metodo es la alternativa cuando N no es divisible entre n
msistematicaNorandom <- function(N,n,metodo="circular"){
    id <- 1:N
    ki <- floor(N/n)
    if(N!=ki*n){
        id <- switch(metodo,
                     blancos={ki <- ki+1
                              c(id,rep(NA,ki*n-N))},
                     circular={ki <- ki+1
                               c(id,id[seq(length.out=ki*n-N)])}
                     )
    }
    invisible(id[seq(sample(1:ki,1),by=ki,length.out=n)])
}
#seleccion mai de segmentos
#en el caso de upms divididas en segmentos y
#se quiere hacer mai de segmentos individuales
#los datos traen id de upm y numero de segmentos
#en la upm; en el data.frame de salida, id es el
#numero consecutivo porque upms en diferentes
#departamentos pueden tener el mismo numero id
#vidupm es el nombre de la variable con id de upm
#vnumseg:nombre de varible con numero de segmentos/upm
#OjO Rerencia salida: nota agregada a la ver. 2012-07-25-9:19
#la id de salida es factor. Tener cuidado al hacer referencia
#con el id de la selección a filas en datos.
#Si ss<-selesegupm(dd,"uu","sg"), referencia correcta es
#ww<-dd[levels(ss$id)[ss$id],]
selesegupm <- function(datos,vidupm,vnumseg,n){
  with(datos,
       {
         b <- maiSinReemplazo(1:sum(datos[,vnumseg]),n)
         data.frame(id=rep(rownames(datos),datos[,vnumseg])[b],
                    upm=rep(datos[,vidupm],datos[,vnumseg])[b],
                    seg=unlist(sapply(datos[,vnumseg],
                      function(w)seq(1,w)))[b]
                    )
       }
       )
}
#seleccion replicada
#poblacion dividida en "seudo estratos" o "zonas"
#por el momento seleccion mai en cada "zona"
#la i-esima replica se construye tomando la i-esima seleccion
#de cada "zona"; de esta manera, una réplica tiene elementos de cada
#una de las "zonas", y hay tantas réplicas como tantas selecciones se
#hagan dentro de cada "zona"
#**identidad son los nombres de las unidades de muestreo
#**n es tamaño de la muestra
seleccionReplicada <- function(identidad,n,replicas=4,muestreo="mai"){
  z <- n/replicas
  if(z==round(z)){
    um <- identidad
    N <- length(um)
    rz <- N%%z
    if(rz>0){#agrega "blancos" al final
      um <- c(identidad,rep(NA,z-rz))
      N <- N+z-rz
      print(paste(z-rz,"blancos"))
    }
    Nz <- N/z
    fz <- factor(rep(1:z,each=Nz))
    lz <- split(identidad,fz)
    mr <- unlist(lapply(lz,maiSinReemplazo,replicas))
    ib <- (is.na(mr))
    if(sum(ib)>0)
      print("eliminados",sum(ib),"blancos")
    rp <- rep(1:replicas,z)
    mm <- data.frame(ide=mr[!ib],rep=rp[!ib])
  }
  else
    mm <- NULL
  invisible(mm)
}
#la progresion geométrica
#Gunning,P and Horgan,J. A new algorithm for the construction of
#stratum boundaries in skewed populations.
#Survey Methodology 30(2):159-166 2004
estratosProgresion <- function(x,ne=5){
  k0 <- min(x)
  if(k0<=0)stop("datos con ceros")
  r <- (max(x)/k0)^(1/ne)
  k0*r^seq(0,ne)
}
#las frecuencias acumuladas-Dalenius
estratosDalenius <- function(x,b=numeric(0),ne=5,netm=0){
  if(length(b)==0){
    if(netm==0) netm <- ne*5
    b <- netm
  }
  else
    ne <- length(b)-1
  g <- hist(x,breaks=b,plot=F)
  fc <- cumsum(sqrt(g$counts))
  ic <- max(fc)/ne
  bk <- numeric(ne+1)
  bk[1] <- g$breaks[1]
  bk[ne+1] <- g$breaks[length(g$breaks)]
  for(i in 1:(ne-1))bk[i+1] <- g$breaks[sum(fc<=(i*ic))+1+1]
  bk
}
#la regla sigma-gap mencionada en
#Morrison,R., Giroux,S and Julien,C. Redesign of the canadian
#agriculture surveys. SSC Annual Meeting; june 2003. Proceeding of the
#survey methods section
estratosSigmaGap <- function(x){
  x <- sort(x)
  n <- length(x)
  sx <- sd(x)
  mx <- median(x)
  i <- 2
  while(x[i]<=mx)i <- i+1
  while((i<=n)&((x[i]-x[i-1])<sx))i <- i+1
  ifelse(i<=n,x[i],-1)
}
#algoritmo para separar estrato "excepcional"
#Hidiroglou,M.A. The construction of a self-representing stratum of
#large units in survey design.
#The American Statistician 40(1):27-31. 1986
estratosCensado <- function(x,cv=0.05,maxite=10){
  if(cv>1) cv <- cv/100
  x <- sort(x)
  N <- length(x)
  cx <- (cv*sum(x))^2
  ub <- max(x)
  na <- N
  ok=TRUE
  ite <- 0
  while(ok&&ite<maxite){
    ny <- N-sum(x>ub)
    y <- x[1:ny]
    #ct <- var(y)/cx
    sy <- var(y)
    #nt <- N-ny/(1+ny*ct)
    nt <- N-(ny*cx)/(cx+ny*sy)
    nc <- ifelse(ite==0,ny,ny-1)
    #ub <- mean(y)+sqrt(cx*(ct+nc/(ny*ny)))
    ub <- mean(y)+sqrt(sy+(cx*nc)/(ny*ny))
    tst <- ifelse(ite>0,1-nt/na,1)
    ok <- !(tst>=0&&tst<0.10)
    if(ok) na <- nt
    ite <- ite+1
  }
  invisible(c(cota=ub,ntot=na,censo=sum(x>ub),itera=ite))
}
#el algoritmo de Lavallée,P and Hidiroglou,M. On the stratification of
#skewed populations. Survey Methodology 14(1):33-43 1988
#b puede traer limites(ne+1) iniciales para empezar las iteraciones
estratosLavalle <- function(x,b=numeric(0),cv=0.05,ne=5,xp=1,eps=0.1,maxite=100){
  x <- sort(x)
  N <- length(x)
  nc <- N*(cv*mean(x))^2
  if(length(b)==0){
    b <- estratosProgresion(x,ne)
    b[ne+1] <- x[N]
  }
  else
    ne <- length(b)-1
  ba <- b
  tst <- 1
  ite <- 0
  while(tst>eps&ite<maxite){
    ite <- ite+1
    fb <- cut(x,breaks=b,include.lowest=TRUE)
    wh <- tabulate(fb)/N
    uh <- tapply(x,fb,mean)
    vh <- tapply(x,fb,var)
    wu <- wh*uh
    wv <- wh^2*vh
    A <- sum((wu^xp)[-ne])
    B <- sum((wv/wu^(xp))[-ne])
    F <- sum((wh*vh)[-ne])+nc
    T <- A*(wh/wu^(xp))
    K <- xp*(B*wu^(xp-1)-A*wv*wu^(-xp-1))
    for(h in 1:(ne-2)){
      ah <- F*(T[h]-T[h+1])
      bh <- F*((K[h]-K[h+1])-2*(uh[h]*T[h]-uh[h+1]*T[h+1]))+2*A*B*(uh[h]-uh[h+1])
      gh <- F*(T[h]*(uh[h]*uh[h]+vh[h])-T[h+1]*(uh[h+1]*uh[h+1]+vh[h+1]))-A*B*(uh[h]*uh[h]-uh[h+1]*uh[h+1])
      b[h+1] <- (sqrt(bh*bh-4*ah*gh)-bh)/(2*ah)
    }
    ah <- F*T[ne-1]-A*B
    bh <- F*(K[ne-1]-2*T[ne-1]*uh[ne-1])+2*A*B*uh[ne-1]
    gh <- F*(T[ne-1]*(uh[ne-1]*uh[ne-1]+vh[ne-1])-F)-A*B*uh[ne-1]*uh[ne-1]
    b[ne] <- (sqrt(bh*bh-4*ah*gh)-bh)/(2*ah)
    #th1 <- A*B/F
    #vh1 <- F/th1
    #uh1 <- 0
    #kh1 <- 0
    #for(h in (ne-1):1){
    #  th <- A*wh[h]/wu[h]^xp
    #  kh <- xp*(B*wu[h]^(xp-1)-A*wv[h]/wu[h]^(xp+1))
    #  ah <- F*(th-th1);print(ah)
    #  bh <- F*(kh-kh1-2*(uh[h]*th-uh1*th1))+2*A*B*(uh[h]-uh1)
    #  gh <- F*(th*(uh[h]*uh[h]+vh[h])-th1*(uh1*uh1+vh1))-A*B*(uh[h]*uh[h]-uh1*uh1)
     # b[h+1] <- sqrt(bh*bh-4*ah*gh)/(2*ah)-0.5
     # th1 <- th
     # kh1 <- kh
     # uh1 <- uh[h]
     # vh1 <- vh[h]
    #}
    tst <- max(abs(b-ba))
    ba <- b
  }
  c(limites=b,itera=ite)
}
#para la seleccion de los puntos de la encuesta
#supone que se han leido las coberturas de bloques y puntos
#y que se ha cargado la librería maptools
#ejemplo: c:/encuestas/pri2008/seleccion/spri2008.r
#COBERPUNTOS <- "c:/encuestas/marco/bloquespuntos/puntos2007.shp"
#COBERBLOQUES <- "c:/encuestas/marco/bloquespuntos/bloques2007.shp"
#cb <- readShapePoly(COBERBLOQUES)
#cp <- readShapePoints(COBERPUNTOS)
#-2009-09-21
#.modificada la funcion para que acepte como parametros las coberturas
#.de trabajo. De ahora en adelante la selección del departamento debe
#.hacerse desde el código que llama la función.
#.e.g. selpuntos(impar=0,bd=bq[bq$DPTO==5,],pd=pt[pt$DPTO=5,],nomdpto)
selpuntos <- function(impar=1,bd=NULL,pd=NULL,nomdpto=character(0)){
  stopifnot(!is.null(bd),!is.null(pd),length(nomdpto)>0)
  print(paste("seleccion de",ifelse(impar==0,"pares","impares"),
              "departamento",nomdpto))
  #bd <- cb[cb$DPTO==dp,]
  fbb <- interaction(bd$BLOQUE,bd$ESTRATO,drop=T,lex.order=T)
  be <- table(fbb)#comprobar unicidad bloque.estrato
  j <- (be>1)
  if(sum(j)>0) return(be[j])
  #plot(m,pch=21,col="red")
  #pd <- cp[cp$DPTO==dp,]
  fbp <- interaction(pd$BLOQUE,pd$ESTRATO,drop=T,lex.order=T)
  #table(fbp)
  #la condicion: reemplaza impares cuando impar==1
  psel <- (as.integer(levels(pd$CPUNTO))[pd$CPUNTO]%%2==impar)
  fbpi <- fbp[psel]#fbp y fbpi con el mismo número de niveles
  psb <- table(fbpi)#puntos a seleccionar por bloque.estrato
  #alinea puntos con bloques
  j <- psb>0#psb con todos los niveles;incluso los sin puntos selecc
  w <- match(names(psb)[j],fbb,nomatch=NA)
  #sum(is.na(w))#test todo punto corresponde a un bloque
  npb <- integer(nlevels(fbb))#puntos por bloque
  npb[w] <- psb[j]#puntos a seleccionar en bloque corresp.
  lw <- dotsInPolys(bd,npb,compatible=T)
  lxy <- lw[w]#solo los bloques donde se va a reemplazar
  #prepara reemplazo de coordenadas
  #a.se identifican los registros a modificar;ordinal en el data
  #b.y se agrupan según bloque.estrato donde pertenecen
  lw <- split((1:length(fbp))[psel],fbpi)
  #c.se vinculan registros con bloque.estrato
  w <- match(names(lw),names(psb[j]),nomatch=NA)
  #d.y se alinean con las coordenadas seleccionadas
  lips <- lw[!is.na(w)]
  #y se reemplazan
  cxy <- attr(pd,"coords")
  for(j in 1:length(lxy))cxy[lips[[j]],] <- lxy[[j]]
  attr(pd,"coords") <- cxy
  writePointsShape(pd,nomdpto)
}
#funciones para seleccionar y renovar segmentos
#1.leer tablas pertinentes
#2.seleccion aleatoria de segmentos reemplazantes en cada
#-- estrato. (controlar para no seleccionar alguno de los ya
#-- seleccionados en la encuesta anterior?)
#(funcion slc)
#3.reemplazar los seleccionados en la tabla donde están registrados los
#-- segmentos de la muestra. El reemplazo es aleatorio
#(funcion sustituye)
#ver: c:/encuestas/pri2007/seleccion/seleccion.r
#supone objetos ya creados con
#cd <- dbase("upms")
#cq <- query("select * from upm","upms")
#dfupm:datos de la tabla leidos con cq
#o bien con foreign;
#e.g: u <- read.dbf("c:/encuestas/marco/upm.dbf")
#     names(u) <- tolower(names(u))
#dpt y estr identifican departamento y estrato
#y n el número de segmentos a seleccionar
#e.g: m753 <- slc(u,"75",3,1)
slc <- function(dfupm,dpt,estr,n){
  i <- (dfupm[,"dpt"]==dpt)&(dfupm[,"estr"]==estr)
  nu <- sum(i)
  sg <- dfupm[i,"segmentos"]
  ad <- rep(rep(dpt,nu),sg)
  au <- rep(dfupm[i,"upm"],sg)
  am <- rep(dfupm[i,"mun"],sg)
  ag <- rep(sg,sg)
  sgm <-unlist(sapply(sg,seq))
  m <- srswor(n,length(sgm))
  i <- m!=0
  w <- data.frame(dpt=ad[i],mun=am[i],estr=rep(estr,sum(i)),
                  upm=au[i],segmentos=ag[i],segmento=sgm[i])
  invisible(w[order(w$upm),])
}
#funcion para agregar a la tabla de seleccion
#modificar para utilizar foreign y parametrizar nombre de la tabla
#OjO: supone conexión ODBC
tslc <- function(dfsl,con){
  n <- nrow(dfsl)
  for(i in 1:n){
    ss="insert into selpos2006(dpto,estr,upm,segmentos,segmento) value("
    ss=paste(ss,dfsl[i,1],sep="")
    ss=paste(ss,dfsl[i,2],dfsl[i,3],dfsl[i,4],dfsl[i,5],sep=",")
    ss=paste(ss,")",sep="")
    sqlQuery(con,ss)
  }
}
#construye id upm.segmento a partir de
#los id de upm y la cantidad de segmentos por upm
#idexcluidos: upm.segmento de los excluidos de la poblacion
poblacionsegmentos <- function(upms,segmentosupm,
                               idexcluidos=character(0)){
    ps <- paste(rep(upms,segmentosupm),
                      unlist(sapply(segmentosupm,seq)),sep=".")
    if(length(idexcluidos)!=0)
        ps <- ps[!(ps%in%idexcluidos)]
    invisible(ps)
}
#separa upm.segmento y construye data.frame
splitupmseg <- function(upmseg){
    u <- strsplit(upmseg,split=".",fixed=TRUE)
    data.frame(upm=sapply(u,"[[",1),segmento=sapply(u,"[[",2))
}
#
#para sustituir segmentos seleccionados
#ver: c:/encuestas/pri2008/seleccion/spri2008.r
#supone cargada libreria sampling
#e.g: ns <- sustituye(ns,m753,"75","3",nrow(m753))
#     ns$upm <- as.factor(ns$upm)
#dfs es data.frame con seleccion (copia de la seleccion vigente)
#(ns<- read.dbf("c:/encuestas/pri2007/seleccion/segpri2007.dbf"))
#slc pone a upm como numeric y en la tabla dbf como caracter, por lo
#-- que en ns aparece como factor; se hace el cambio para facilitar la
#-- asignacion en la penúltima instrucción de la función sustituye
#(ns$upm <- as.integer(levels(ns$upm))[ns$upm])
#dfns los nuevos segmentos seleccionados (slc)
#2009-10-06: modificacion para controlar seleccion recurrente
#dfns solo de un estrato (el indicado en estr);generalizar(?)
#dfns debe traer más que la cantidad a sustituir (n)
#n es la cantidad a reemplazar
#2009-10-08
#separadas las instrucciones para identificar upm-segmento ya en la
#muestra
upmsrepetidas <- function(dfo,dfn){
  f <- interaction(dfo$upm,dfo$segmento,drop=T)
  m <- interaction(dfn$upm,dfn$segmento,drop=T)
  invisible(m%in%f)
}
sustituye <- function(dfs,dfns,dpt,estr,n){
  i <- dfs$dpt==dpt&dfs$str==estr
  j <- !upmsrepetidas(dfs[i,],dfns)
  if(sum(j)<n)
    stop("insuficientes para reemplazar")
  nr <- srswor(n,sum(i))
  i[i] <- nr!=0
  v <- c("upm","mun","segmentos","segmento")
  dfs[i,v] <- dfns[which(j)[srswor(n,sum(j))==1],v]
  invisible(dfs)
}
#
#--seleccionar segmentos de upms
#dfupm es la tabla que viene de la cobertura
#dpt y estr identifican departamento y estrato
#n segmentos a seleccionar de dpt.estr
#sgexcluidos upm.segmento a excluir de la seleccion
#porque ya estan en la muestra o porque no deberían estar
#disponibles para la rotación de la muestra
#es para evitar las consecuencias de la funcion upmsrepetidas
#que llama la funcion sustituye
#sgexcluidos es arreglo character con upm.estrato del departamento
#para el que se hace la seleccion
segmentosel <- function(dfupm,dpt,estr,n,sgexcluidos){
  i <- (dfupm[,"dpt"]==dpt)&(dfupm[,"estr"]==estr)
  nu <- sum(i)
  sg <- dfupm[i,"segmentos"]
  au <- rep(dfupm[i,"upm"],sg)
  #upm.segmento
  sgm <- unlist(sapply(sg,seq))
  wus <- paste(au,sgm,sep=".")
  j <- !(wus%in%sgexcluidos)
  #mejorar codigo siguiente
  au <- au[j]
  sgm <- sgm[j]
  ag <- rep(sg,sg)[j]
  ad <- rep(rep(dpt,nu),sg)[j]
  am <- rep(dfupm[i,"mun"],sg)[j]
  #
  m <- srswor(n,length(sgm))
  i <- m!=0
  w <- data.frame(dpt=ad[i],mun=am[i],estr=rep(estr,sum(i)),
                  upm=au[i],segmentos=ag[i],segmento=sgm[i])
  invisible(w[order(w$upm),])
}
#-
#segupmstr:segmentos por upm según el estrato
datadpto <- function(dircob,cober,dpto,
                     segupmstr=c(2,4,1,0.5,1,0,1,0,0,0,0,1)){
    cb <- tabladbf(dircob,cober)
    w <- data.frame(mun=(cb$codigo%/%1e04)%%dpto,estr=cb$estrato,
                 upm=cb$upm,segmentos=integer(nrow(cb)))
    w$segmentos <- round(cb$km2*segupmstr[cb$estrato],0)
    invisible(w)
}
#-estratos de papel
epapel <- function(segme,intervalo=0,nestratos){
    cumseg <- cumsum(segme)
    if(intervalo==0)
        intervalo=round(sum(segme)/nestratos,0)
    we <- cut(cumseg,breaks=intervalo*0:nestratos,labels=1:nestratos)
    i <- is.na(we)
    if(sum(i)>0)
        we[i] <- nestratos
    invisible(we)
}
#
selsegdpto <- function(dfupm,dpto,sexcluido,nremplazo){
    estr <- dfupm[1,"estr"]
    ns <- nremplazo[estr]
    if(ns>0){
        upm <- rep(dfupm$upm,dfupm$segmentos)
        mun <- rep(dfupm$mun,dfupm$segmentos)
        seg <- rep(dfupm$segmentos,dfupm$segmentos)
        sgm <- unlist(sapply(dfupm$segmentos,seq))
        wus <- paste(upm,sgm,sep=".")
        j <- !(wus%in%sexcluido[[as.character(estr)]])
        m <- srswor(ns,sum(j))
        i <- which(j)[m!=0]
        wus <- wus[i]
        w <- data.frame(dpt=rep(dpto,ns),mun=mun[i],
                        estr=rep(estr,ns),upm=upm[i],
                        segmentos=seg[i],segmento=sgm[i])
        w <- w[order(w$upm),]
    }
    else
        w <- NULL
    invisible(w)
}
#
#dfupmseg: columnas upms,segmentos en upm
#tamu: tamaño de la muestra
#sexcluidos: los ids de los segmentos excluidos de la población
#regresa df con columnas: upm,segmento
muestrasegmentos <- function(dfupmseg,colupm="upm",colseg="segmentos",
                             tamu,sexcluidos=character(0)){
    ps <- poblacionsegmentos(dfupmseg[,colupm],dfupmseg[,colseg],
                             sexcluidos)
    ms <- maiSinReemplazo(ps,tamu)
    invisible(splitupmseg(ms))
}
#
#ue:subconjunto de upms del estrato
#numstratapap:numero de estratos de papel en el estrato
#lexcluidos:segmentos excluidos en el departamento; lista por estrato-papel
#tamstratapap:tamaño muestra por estrato de papel
selreplicapapel <- function(ue,dpto,numstratapap,
                            lexcluidos,tamstratapap=1){
    ue$estr <- epapel(ue$segmentos,nestratos=numstratapap)
    w <- ddply(ue,.variables="estr",.fun=selsegdpto,dpto=dpto,
                sexcluido=lexcluidos,
               nremplazo=rep(tamstratapap,numstratapap))
    w$subestr <- w$estr
    w$estr <- estrato
    invisible(w)
}
#
muestraestratopapel <- function(ue,colseg="segmentos",numstratpap,
                                sexcluidos=character(0)){
    ue$estrapap <- epapel(ue[,colseg],nestratos=numstratpap)
    w <- ddply(ue,.variables="estrapap",.fun=muestrasegmentos,
               sexcluidos=sexcluidos,tamu=1)
    invisible(w)
}
#
#orden: puede ser orden a la serpentina
maiepapel <- function(upm,seg,orden,tamu=1,nrep=1){
    upm <- upm[orden]
    seg <- seg[orden]
    ps <- poblacionsegmentos(upm,seg)
    mu <- seleccionReplicada(ps,n=tamu,replicas=nrep)
    invisible(mu)
}
#
sustituyeseg <- function(selold,selact,samdpt,dep){
    ma <- with(samdpt,split(paste(upm,segmento,sep="."),estr))
    sad <- with(subset(selact,dpto==dep),
                     split(paste(upm,segmento,sep="."),estr))
    sod <- with(subset(selold,dpto==dep),
                     split(paste(upm,segmento,sep="."),estr))
    #srid <- with(subset(selold,dpto==dep),split(idseg,estr))
    srid <- with(subset(selact,dpto==dep),split(idseg,estr))
    #snd <- sod
    snd <- sad
    srd <- sod
    for(es in names(ma)){
        ir <- sad[[es]]%in%sod[[es]]
        #ir <- sod[[es]]%in%sad[[es]]
        iss <- srswor(length(ma[[es]]),sum(ir))==1
        #srd[[es]] <- (sod[[es]][ir])[iss]
        srd[[es]] <- (sad[[es]][ir])[iss]
        srid[[es]] <- (srid[[es]][ir])[iss]
        #ir <- sod[[es]]%in%srd[[es]]
        ir <- sad[[es]]%in%srd[[es]]
        snd[[es]][ir] <- ma[[es]]
    }
    wn <- ldply(lapply(snd,splitupmseg))
    names(wn)[1] <- "estr"
    wn[,"idseg"] <- selact[selact$dpto==dep,"idseg"]
    ws <- ldply(lapply(srd,splitupmseg))
    names(ws)[1] <- "estr"
    ws[,"idseg"] <- unlist(srid)
    invisible(list(nueva=wn,sale=ws))
}
#-selecciona de la tabla de managua-upm dentro de
#los "estratos de papel" de un estrato
#OjO:no considera los segmentos que se deberían excluir
#vea: muestraestratopapel
selsegmento <- function(upms,estrato){
  wu <- with(y <- subset(upms,estra==estrato,
                         select=c("epapel","upm","segmentos")),
             split(y,epapel))
  ids <- lapply(wu, function(x)interaction(rep(x$upm,x$segmentos),
                                           unlist(sapply(x$segmentos,seq)),
                                           drop=T,lex.order=T))
  sapply(ids,maiSinReemplazo,1)
}
#
muestrarenovada <- function(dfs,dfns,dpt,estr,n){
  i <- dfs$dpt==dpt&dfs$str==estr
  #j <- !upmsrepetidas(dfs[i,],dfns)
  #if(sum(j)<n)
  #  stop("insuficientes para reemplazar")
  nr <- srswor(n,sum(i))
  i[i] <- nr!=0
  v <- c("upm","mun","segmentos","segmento")
  sv <- dfs[i,v]
  #revisar para eliminar j<-!upmsrepetidas()
  #sn <- dfns[which(j)[srswor(n,sum(j))==1],v]
  sn <- dfns[sample(1:n),v]
  dfs[i,v] <- sn
  invisible(list(nueva=dfs,salen=sv))
}
#
muestranueva <- function(dfs,dfns,dpt,estr){
  i <- dfs$dpt==dpt&dfs$str==estr
  n <- sum(i)
  k <- nrow(dfns)
  v <- c("upm","mun","segmentos","segmento")
  w <- rbind(dfs[i,v],dfns[,v])
  w$id <- c(integer(length=n),seq(k))
  w <- w[order(w$upm),]
  j <- w$id>0
  m <- which(j)
}

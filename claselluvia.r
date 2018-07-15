#-metodos genericos definidos en genericos.r
if(!isGeneric("datos"))
  source("c:/encuestas/programas/rprog/genericos.r")
#define la clase
setClass("clLluvia",
         representation(estacion="character",file="character",datos="ANY"),
         prototype(estacion=character(0),file=character(0),datos=NULL)
         )
setMethod("initialize","clLluvia",
          function(.Object,estacion=character(0),file=character(0)){
            if(length(estacion)>0) .Object@estacion <- estacion
            if(length(file)>0) .Object@file <- file
            require("chron")
            invisible(.Object)
          }
          )
setMethod("show","clLluvia",
          function(object){
            u <- ifelse(length(object@estacion)>0,
                        object@estacion,"no indicada")
            print(paste("estación meteorológica:",u))
          }
          )
#se leen los archivos ASCII transformados de AGROCLIM por el snipet AWK
setMethod("datos",signature(x="clLluvia",file="missing"),
          function(x){
            w <- NULL
            if(file.exists(x@file)){
              	z <- try(read.table(file=x@file,header=T),silent=TRUE)
                if(is(z,"try-error"))
                  print("no se pudo leer archivo")
                else{
                  f<-dates(paste(z$year,z$mes,z$dia,sep="-"),format="y-m-d")
                  u <- levels(years(f))
                  lag <- as.integer(max(u))-as.integer(min(u))+1
                  print(paste("desde",min(f),"hasta",max(f),";",
                            length(u),"años registrados de",
                              lag,"en el lapso"))
                  w <- data.frame(fecha=f,pmm=z[,4]/10)
                }
            }
            else print("no existe archivo")
            return(w)
          }
          )
#los datos en el formato de salida del metodo datos
#los días húmedos son los que ppm>0.1
#promedios anuales, mensuales de precipitación y días con lluvia
setMethod("resumen",signature(x="clLluvia"),
          function(x,datos=NULL){
            if(!is(datos,"data.frame"))
              datos <- x@datos
            if(!is(datos,"data.frame"))
              datos <- datos(x)
            w <- NULL
            if(nrow(datos)>=365){
              fy<-years(datos$fecha)
              fm<-months(datos$fecha)
              tpy<-tapply(datos$pmm,fy,sum)
              dhy<-tapply((datos$pmm>0.1),fy,sum)
              tpm<-tapply(datos$pmm,fy:fm,sum)
              dhm<-tapply((datos$pmm>0.1),fy:fm,sum)
              fm<-factor(rep(levels(fm),nlevels(fy)))
              w <- list(panu=tpy,dhanu=dhy,
                   epanu=c(media=mean(tpy),mediana=median(tpy),de=sd(tpy)),
                   edhan=c(media=mean(dhy),mediana=median(dhy),de=sd(dhy)),
                   epmes=cbind(media=tapply(tpm,fm,mean),
                     mediana=tapply(tpm,fm,median),de=tapply(tpm,fm,sd)),
                   edhms=cbind(media=tapply(dhm,fm,mean),
                     mediana=tapply(dhm,fm,median),de=tapply(dhm,fm,sd)))
            }
            else
              print("datos insuficientes")
            return(w)
          }
          )
#---------
lluvia <- function(estacion=character(0),file=character(0)){
  invisible(new("clLluvia",estacion,file))
}
#fechas que marcan inicio de periodos de longitud específica
#(decadas,pentadas...) a partir de la fecha desde
#-para día en que finaliza el periodo se resta un día; eliminar primer
#elemento del resultado porque queda fuera del rango de fechas
#-eliminar último elemento porque es inicio de un periodo incompleto
#alternativa a seq.dates o seq.Dates para controlar por año bisiesto y
#fracciones de periodos. Año bisiesto, descarta el 29 de febrero;
#fracción de periodo, eliminada de la cola derecha
#desde y hasta chron,numeric o character con fecha inicial (final)
#numeric c(año,mes,dia)
#character "año-mes-dia"
fechas.periodos <- function(desde,hasta,dias=5,sin29feb=TRUE){
  if(!inherits(desde,"dates"))
    desde <- switch(class(desde)[1],
                    numeric=dates(paste(desde,collapse="-"),format="y-m-d"),
                    character=as.chron(desde,format="y-m-d"),
                    NULL)
  if(!inherits(hasta,"dates"))
    hasta <- switch(class(hasta)[1],
                    numeric=dates(paste(hasta,collapse="-"),format="y-m-d"),
                    character=as.chron(hasta,format="y-m-d"),
                    NULL)
  if(is.null(desde)|is.null(hasta)) stop("desde o hasta incorrecto")
  fs <- seq.dates(from=desde,to=hasta,by="days")
  j <- days(fs)==29&unclass(months(fs))==2
  if(sin29feb) fs <- fs[!j]
  fs[seq(1,length(fs),by=dias)]
}
#tomada de P.Burrel
#para mostrar mapa de eventos; "gráfica de crucigrama"
grid.imageFun <- function(nrow,ncol,cols,byrow=TRUE){
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  if(byrow){
    right <- rep(x,nrow)
    top <- rep(y,each=ncol)
  }
  else{
    right <- rep(x,each=nrow)
    top <- rep(y,ncol)
  }
  grid.rect(x=right,y=top,width=1/ncol,height=1/nrow,just=c("right","top"),
            gp=gpar(col=NA,fill=cols),name="image")
}

stopifnot(all(is.element(c("package:assertthat", "package:dplyr",
                           "src:huaca"),
                         search())))

## estimación departamental
## x variables estimar
## dp departamento correspondiente a x
## fx factor de expansión
## wx ponderación de factor de expansión
## nomdp nombres de departamentos?
estima_dp <- function(x, dp, fx, wx=NULL, nomdp=TRUE){
    if(is.null(wx)) wx <- unos(length(fx))
    assert_that(length(x) == length(fx),
                length(x) == length(dp),
                length(x) == length(wx))
    
    ww <- tapply(x * fx * wx, dp, sum, na.rm=TRUE)
    cc <- names(ww)
    ww <- setNames(c(ww, sum(ww, na.rm=TRUE)), c(cc, "00"))
    if(nomdp) names(ww) <- c(nom_dp(cc), "Total")
    ww
}

## porcentaje contribución al estimado
## departamental
pct_est_dp <- function(y, dpt, fx, wx=NULL){
    if(is.null(wx)) wx <- unos(length(fx))
    assert_that(length(fx) == length(wx))
    fx <- fx * wx
    st <- estima_dp(y, dpt, fx, nomdp=FALSE)
    mm <- match(dpt, as.integer(names(st)))
    invisible(pct(y*fx, st[mm]))
}

## DataFrame Character Numeric Logical -> Logical
## prop. devolver las filas de df con datos
## filtrados por sel, que contribuyen más de cot al
## estimado departamental de la variable vbl
out_est_dpt <- function(df, vbl, cot=15, sel=NULL){
    if(is.null(sel)) sel <- !vector(nrow(df))
    vv <- as.name(vbl)
    ii <- filter(df, sel) %>%
        mutate(pct=pct_est_dp(!!vv, dpt, fxc, wf)) %>%
        {.$pct >= cot & !is.na(.$pct)}
    ##           ii=pct>=cot & !is.na(pct))
    ##df$quest %in% xx$quest[ii] & df$cultivo==cul
    ##sel[which(sel)] <- xx$pct >= cot & !is.na(xx$pct)
    which(sel)[ii]
}

##-- estima total H-H
## y datos, wps ponderación prob.selec.
## wnr ponderación no respuesta
## wne ponderación no elegibilidad
est_HH <- function(y, wps, wnr=NULL, wne=NULL){
    if(is.null(wnr)) wnr <- integer(length(y)) + 1L
    if(is.null(wne)) wne <- integer(length(y)) + 1L
    x <- mean(y * wps * wnr * wne, na.rm=TRUE)
    if(is.nan(x)) x <- NA
}

## varianza Hansen-Hurwitz
vza_HH <- function(y, sin0=FALSE){
    #if(sin0) y <- cero_na(y)
    #nn <- length(y)
    #nn <- sum(!is.na(y))
    #y <- y*ww
    #tt <- sum(y,na.rm=T)/nn
    #sum((y-tt)*(y-tt),na.rm=T)/(nn*(nn-1))
    var(y, na.rm=TRUE)/sum(!is.na(y))
}

## desviación estándar
dst_dpt <- function(x,dp,dh,fx,wx=NULL){
    if(is.null(wx)) wx <- !logical(length(fx))
    vv <- aggregate(x*fx*wx,list(dpt=dp,est=dh),vza_HH)
    vv <- tapply(vv$x,vv$dpt,sum)
    setNames(sqrt(c(vv,sum(vv))),c(names(vv),"00"))
}

## coeficiente de variación
cv_dpt <- function(x,dp,dh,fx,wx=NULL){
    dd <- dst_dpt(x,dp,fx,wx)
    hh <- estima_dp(x,dp,fx,wx,nomdp=FALSE)
    mm <- match(names(dd),names(hh))
    setNames(pct(dd,hh[mm]),names(dd))
}

##-- factores
## poda
factor_trim <- function(ef, level, techo = level){
    ef[ef > level] <- techo
    invisible(ef)
}

##
factor_xp <- function(r0, a0, codarea, supest, fp="puntos2016.rda",
                      CCC=NULL){
    stopifnot(file.exists(fp))
    pp <- load(fp)
    if(is.null(CCC))
        CCC <- c(completa=1,noagricola=2,incompleta=3,noinformante=4,
                 rechazo=5,noproductor=6,conflicto=7,otro=8,noacceso=9)
    rx <- r0[,c("quest","c5000")]
    rx["ccc"] <- names(CCC)[rx$c5000]
    mm <- match(rx$quest,as.integer(pun$cpunto))
    rx["dpto"] <- pun$dpt[mm]
    rx["estrato"] <- pun$estrato[mm]
    mm <- match(rx$quest,a0$quest)
    rx["area"] <- a0[mm,codarea]
    ##ss <- c("completa","incompleta","otro","conflicto")
    ss <- c("completa","incompleta")
    rx[!rx$ccc%in%ss,"area"] <- 0
    fxp <- fac_pi(interaction(rx$dpto,rx$estrato,drop=T),rx$area,
                  interaction(supest$dpto,supest$estrato,drop=T),
                  supest$mz/supest$puntos)
    wnr <- factor_nr(factor(rx$dpto),rx$ccc,CCC,responden=ss,
                     noelegible="noagricola",desconocida="noacceso")
    rx["fxp"] <- fxp
    rx["wnr"] <- wnr
    rx["ef"] <- fxp*wnr
    invisible(rx)
}

##--- factor-expansión

## prob. selección = sup_finca/sup_estrato
## NA si resultado no es finito
pr_es_ <- function(supf, supe=0){
    assert_that(supe > 0)
    nn <- supe/supf
    nn[!is.finite(nn)] <- NA
    invisible(nn)
}

## prob. selección = sup_finca/sup_estrato
## vectores con datos de c/"punto" en la muestra
## - dpto: código departamento
## - estrato: código estrato
## - area: superficie finca. NA ó 0 si no hay datos
## columnas del df con superficie de los estratos
## - str_dpt, str_str, str_sup
pr_s_old <- function(dpto, estrato, area,
                  str_dpt, str_str, str_sup, n_dec=7){
    cc <- sprintf("%02i%02i", dpto, estrato)
    ss <- sprintf("%02i%02i", str_dpt, str_str)
    mm <- match(cc, ss)
    ii <- !(is.na(mm) | is.na(area) | area==0)
    wp <- numeric(length(area))
    wp[ii] <- round(area[ii]/str_sup[mm[ii]], n_dec)
    wp[!ii] <- NA_real_
    invisible(wp)
}

## prob. selección = sup_finca/sup_estrato
## vectores con datos de c/"punto" en la muestra
## - dpto: código departamento
## - estrato: código estrato
## - area: superficie finca. NA ó 0 si no hay datos
## df es data.frame con columnas (en este orden)
## departamento, estrato y superficie
pr_s_ <- function(dpto, estrato, area, df=NULL, n_dec=5){
    assert_that(!is.null(df))
    wp <- area
    cc <- sprintf("%02i%02i", dpto, estrato)
    ss <- sprintf("%02i%02i", df[,1], df[,2])
    mm <- match(cc, ss)
    if(any(ii <- (is.na(mm) | is.na(area) | area==0))){
        message("\n!!! ", sum(ii), " probabilidad indefinida")
        wp[ii] <- NA_real_
    }
    ii <- !ii
    wp[ii] <- round(area[ii]/df[mm[ii], 3], n_dec)
    invisible(wp)
}

## ponderación por probabilidad de selección
## = 1 / (tamaño-muestra * prob.selección)
## vectores de c/"punto" de la muestra
## - dpto, estrato como en pr_s_
## - prs: prob. selección, NULL si llama pr_s_
##   pero requiere pasar area
## - df : como en pr_s_ (dpto,estr,mz,puntos)
w_ps_ <- function(dpto, estrato, prs=NULL, area=NULL, df=NULL,
                  n_dec = 7){
    assert_that(!is.null(df))
    ## chk. area (?)
    if(is.null(prs) & !is.null(area)){
        prs <- pr_s_(dpto, estrato, area, df, n_dec)
    }
    nn <- integer(length(dpto))
    mm <- match_2(dpto, estrato, df[,1], df[,2])
    ii <- !is.na(mm)
    if(any(!ii)){
        message("\n...", sum(!ii), " no coincidencia")
    }
    nn[ii] <- 1.0/(prs[ii] * df[mm[ii], 4])
    nn[is.infinite(nn)] <- 0
    ## old
    ##cc <- interaction(dpto, estrato, drop=TRUE)
    ## unsplit porque tapply cambia orden
    ##nn <- unsplit(tapply(prs, cc,
    ##                     function(x) x*length(x),
    ##                     simplify=FALSE), cc)
    ##ii <- !(is.na(nn) | nn==0)
    ##nn[ii] <- round(1.0/nn[ii], n_dec)
    invisible(round(nn, n_dec))
}

##--- ponderación probabilidad de selección
## devuelve sup.estrato/sup.finca o NA si sup.finca=0 o NA
## dq: quest
## da: área_finca
## dc: control-cuestionario
## ne: ctrl. de no_elegible
## na: ctrl. no acceso (elegilibilidad desconocida) o no respuesta
## tab_pt: función devuelve tabla maestra de puntos
## tab_ae: función devuelve tabla área de estratos
## col_pt: función devuelve nombre estandar tabla puntos
## col_ae: función devuelve nombre estandar tabla área estratos
w_ps <- function(dq, da, dc, ne, na, tab_pt, col_pt, tab_ae, col_ae,
                 n_dec = 6){
    pt <- tab_pt()
    ae <- tab_ae()
    ncp <- col_pt()
    nce <- col_ae()
    mm <- match(dq,pt[,ncp["punto"]])
    if(any(ii <- is.na(mm))){
        message("\n... puntos no válidos: ",sum(ii))
        dq <- dq[!ii]
        da <- da[!ii]
        mm <- match(dq,pt[,ncp["punto"]])
    }
    ww <- data.frame(quest=dq,
                     dpto=pt[mm, ncp["departamento"]],
                     estr=pt[mm, ncp["estrato"]],
                     wps=numeric(length(dq)))
    ss <- sprintf("%02i%02i", ww$dpto, ww$estr)
    cc <- sprintf("%02i%02i", ae[,nce["departamento"]],
                  ae[,nce["estrato"]])
    mm <- match(ss,cc)
    if(anyNA(mm)) stop("\n... error-match-puntos-estratos")
    ii <- da>0
    ww[ii,"wps"] <- round(ae[mm[ii], nce["area"]]/da[ii], n_dec)
    ww[dc%in%ne,"wps"] <- 0
    ww[dc%in%na,"wps"] <- NA
    invisible(ww)
}

##--- elegibilidad
## devuelve ponderación por elegibilidad desconocida (no contacto)
## wb ponderación base; = 1 si no es incluida
## cg,dc,cq,ce: clase,control-quest,ctrl.cuest,cod.no_contacto
## e.g
## nn <- setNames(1:8, c(str2vec("completo incompleto noinformante"),
##                      str2vec("rechazo notiene nolocalizada"),
##                      str2vec("pendiente inaccesible")))
## w_ne_(ww$estrato, ww$c5000, cq=nn, ce="inaccesible")
w_ne_ <- function(cg, dc, wb=NULL, cq=NULL, ce=NULL, n_dec = 5){
    stopifnot(!(is.null(ce)|is.null(cq)))
    if(is.null(wb)) wb <- numeric(length(cg)) + 1.0
    nc <- tapply(wb, cg, sum) ## total clase
    ii <- contactado(dc, cq, ce)
    ne <- tapply(wb*ii, cg, sum) ## num. contactados en clase
    if(any(ii <- ne==0)){
        message("clase ", names(ne)[ii],
                " sin contactos. No se ajusta: wne = 1.")
        ne[ii] <- 1.0
    }
    fc <- round(nc/ne, n_dec)
    mm <- match(as.character(cg), names(nc))
    invisible(fc[mm])
}

##-- no respuesta
## ponderación por no respuesta
## cg: clase
## wb: ponderación base; = 1 si no es incluida
## dc,cq: control-quest, cod.ctrl.cuest.
## cr,na,ne: cod. respuesta, no_acceso, no_elegible
w_nr_ <- function(cg, dc, wb=NULL, cq=NULL, cr=NULL, na=NULL, ne=NULL,
                 n_dec = 5){
    stopifnot(!(is.null(cr)|is.null(na)|is.null(ne)|is.null(cq)))
    if(is.null(wb)) wb <- numeric(length(cg)) + 1.0
    ii <- contactado(dc, cq, na) & elegible(dc, cq, ne)
    nc <- tapply(wb*ii, cg, sum) ## total elegibles conocidos
    ii <- ii & si_responde(dc, cq, cr)
    nr <- tapply(wb*ii, cg, sum) ## elegibles responden
    fc <- round(nc/nr, n_dec)
    mm <- match(as.character(cg), names(nc))
    invisible(fc[mm])
}

contactado <- function(cq, nn, no="novisitado"){
    invisible(!cq %in% nn[no])
}

no_contactado <- function(cq, nn, no="novisitado"){
    invisible(cq%in%nn[no])
}

elegible <- function(cq, nn, no_ele="noagricola"){
    invisible(!cq %in% nn[no_ele])
}

no_elegible <- function(cq, nn, no_ele="noagricola"){
    invisible(cq%in%nn[no_ele])
}

no_accesible <- function(cq, nn, no_acc="noacceso"){
    invisible(cq%in%nn[no_acc])
}

si_responde <- function(cq, nn, si=c("completa","incompleta")){
    invisible(cq%in%nn[si])
}

##-- indicadores respuesta
## produce un indicador (%) de tipos de respuesta
## (elegible, contacto, rechazo, respuesta)
## cq: control cuestionario (integer)
## ccq: códigos de control nombrados c(completa=1,...)
## ind: indicador a calcular
ind_res <- function(cq, ccq, ind="respuesta", noacc_elegible=FALSE){
    tt <- table(cq)
    nn <- integer(length(ccq))
    mm <- match(ccq,as.integer(names(tt)))
    ii <- !is.na(mm)
    nn[ii] <- tt[mm[ii]]
    names(nn) <- names(ccq)
    if(!noacc_elegible){
        nn["noacceso"] <- 0
        nn["novisitado"] <- 0
    }
    cc <- c("respuesta","elegible","rechazo","contacto")
    nc <- nn["completo"]+nn["incompleto"]+nn["rechazo"]
    switch(cc[pmatch(ind,cc)],
           elegible=pct(sum(nn)-nn["noagricola"],sum(nn)),
           contacto=pct(nc,nc+nn["noinformante"]+nn["novisitado"]+
                        nn["noacceso"]),
           rechazo=pct(nn["rechazo"],sum(nn)-nn["noagricola"]),
           respuesta=pct(nn["completo"]+nn["incompleto"],
                         sum(nn)-nn["noagricola"]),0)
}

## factor área = sup_estrato/sup_finca
fac_pi <- function(depstr_p, mz_p, depstr, mz_str){
    mm <- match(depstr_p,depstr)
    if(anyNA(mm)){
        stop(paste((depstr_p[is.na(mm)])[1],"no válido"))
    } else{
        ii <- mz_p>0
        mz_p[ii] <- mz_str[mm[ii]]/mz_p[ii]
    }
    invisible(round(mz_p,6))
}

## pondera por no elegibilidad
factor_ne <- function(clase, ctrlq, codq, wb){
    if(missing(wb)) wb <- rep(1,length(clase))
    nc <- tapply(wb,clase,sum)
    ii <- elegible_conocida(ctrlq,codq)
    ne <- tapply(ii*wb,clase,sum)
    fc <- round(nc/ne,3)
    mm <- match(clase,as.integer(names(nc)))
    wb[ii] <- (wb*fc[mm])[ii]
    invisible(wb)
}

## pondera por no-respuesta
factor_nr <- function(clase,ctrlq,codq,
                      responden=c("completa","incompleta"),
                      noelegible="noagricola",
                      desconocida="noacceso",
                      wb){
    if(missing(wb)) wb <- rep(1,length(clase))
    ii <- elegible(ctrlq,codq,noelegible,desconocida)
    nc <- tapply(wb*ii,clase,sum)
    jj <- ii&responde(ctrlq,codq,resp=responden)
    nr <- tapply(wb*jj,clase,sum)
    fc <- round(nc/nr,3)
    mm <- match(clase,factor(names(nc)))
    wb[jj] <- (wb*fc[mm])[jj]
    invisible(wb)
}

## elegibilidad conocida
elegible_conocida <- function(cq,nn,desconocida="novisitado"){
    if(!is.character(cq)) cq <- map_cod(cq,nn)
    invisible(cq!=desconocida)
}

## es elegible
elegible <- function(cq,nn,noelegible="noagricola",
                     desconocida="noacceso"){
    if(!is.character(cq)) cq <- map_cod(cq,nn)
    ii <- elegible_conocida(cq,nn,desconocida)
    invisible(ii&!cq%in%noelegible)
}

## hay respuesta
responde <- function(cq,nn,resp=c("completa","incompleta","otro")){
    if(!is.character(cq)) cq <- map_cod(cq,nn)
    invisible(cq%in%resp)
}

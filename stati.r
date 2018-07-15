## -- estadísticas

## pct. acumulado "doblado"
## si pct>50: 1-pct
## para moutain plot (K.Monti)
## x: vector de datos
## salida: vector listo para graficar
cumpct_doblado <- function(x){
    nn <- rank(x)/(length(x) + 1)
    ii <- nn>0.5
    nn[ii] <- 1-nn[ii]
    round(nn*100)
}

## depth
## media cuando rangos iguales
## min(upper_rank, lower_rank)
## NA -> NA
depth <- function(x){
    ur <- rank(x, na.last="keep")
    mx <- max(ur, na.rm=TRUE) + 1L
    mapply(min, ur, mx-ur)
}

## pct. acumulado folded
pct_depth <- function(x){
    round(100*depth(x)/(lenght(x)+1))
}

## la inversa de depth
## regresa c(inf, sup)
## el promedio si dos con mismo depth
inv_depth <- function(x, depth=1){
    ## TODO chk depth válido según length(x)
    dx <- depth(x)
    
}

## asimetría-Ekström
skew_g <- function(x, r=3, tq=8){
    na <- 2*r - 1L
    n2 <- 2*na
    r1 <- r - 1
    qq <- quantile(x, prob=seq_len(n2 - 1)/n2, type=tq, na.rm=TRUE)
    qq <- c(qq[seq_len(r1)], qq[na],
            qq[seq(to=n2-1, length.out=r1)])
    cj <- rep(1, na)
    cj[r] <- 1 - na
    dj <- c(rep(-1, r1), 0, rep(1, r1))
    sum(cj*qq)/sum(dj*qq)
}

## las funciones de asimetría se ven muy diferentes
## durante simulaciones. Para la aplicación práctica
## sería útil tener una banda de confianza

## asimetría-función
## Critchley-Jones Scand.Jour.Stat. 35:415-437; 2008
## gp valor de asimetría correspondiente a 0 < ps < 1
## ps = 1/2 para estimado escalar de asimetría
## produce matriz [ps, gp]
## TODO otro método de interpolación (?)
## tal vez una búsqueda binaria sea mejor
skew_b <- function(x, ps=NULL){
    bw <- sd(x, na.rm=TRUE)*exp(log(4/(3*length(x)))/5)
    dn <- density(x, bw, na.rm=TRUE)
    ## estimador de Parzen
    fm <- max(dn$y)
    fm2 <- fm * 0.5
    mo <- dn$x[dn$y==fm]
    ## TODO chk ps
    if(is.null(ps)){
        ii <- dn$x >= mo
        fd <- dn$y[ii]
        xr <- dn$x[ii]
        ps <- fd/fm
        if(!any(fd == fm2)){
            xr <- c(xr, approx(fd, xr, xout=fm2)$y)
            fd <- c(fd, fm2)
            ps <- c(ps, 0.5)
        }
        ## interpolación lineal de la parte izquierda
        ii <- dn$x <= mo
        xl <- approx(dn$y[ii], dn$x[ii], xout=fd)$y
    } else{
        if(!any(ps == 0.5)) ps <- c(ps, 0.5)
        fd <- ps * fm
        ii <- dn$x >= mo
        xr <- approx(dn$y[ii], dn$x[ii], xout=fd)$y
        ii <- dn$x <= mo
        xl <- approx(dn$y[ii], dn$x[ii], xout=fd)$y
    }

    gp <- (xr + xl - 2 * mo)/(xr - xl)
    invisible(matrix(c(ps, gp), ncol=2,
                     dimnames=list(NULL, c("ps", "gp"))))
}

## curva de asimetría
## Boshkanov Stat. & Prob. Lett. 77:1111-116; 2007
##
skew_b <- function(x, ps=NULL){
    bw <- sd(x, na.rm=TRUE)*exp(log(4/(3*length(x)))/5)
    dn <- density(x, bw, na.rm=TRUE)
    ## estimador de Parzen
    fm <- max(dn$y)
    fm2 <- fm * 0.5
    mo <- dn$x[dn$y==fm]
    if(is.null(ps)){
    } else{
        stopifnot(ps <= 1, ps >= 0)
        fd <- ps * fm
        ii <- dn$x >= mo
        xr <- approx(dn$y[ii], dn$x[ii], xout=fd)$y
        ii <- dn$x <= mo
        xl <- approx(dn$y[ii], dn$x[ii], xout=fd)$y
    }
    invisible(matrix(c(xr - mo, mo - xl), ncol=2,
                     dimnames=list(NULL, c("x", "y"))))
}

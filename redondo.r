redondear <- function(dd,suma=1,mult=1,metodo="webs",mxit=1000){
    ww <- mult*(dd/sum(dd))
    oo <- floor(min(ww))
    kx <- oo:ceiling(max(ww)+1)
    sp <- c(oo,switch(metodo,webs=(kx+0.5),
                      jeff=kx+1,adam=kx,hill=sqrt(kx*(kx+1))))
    wr <- (sk <- findInterval(ww,sp))+oo-1
    it <- 0
    while(((d <- sum(wr)-suma)!=0)&&((it <- it+1)<mxit)){
        if(d<0){
            kk <- sp[sk+1]/ww
            ii <- which(kk==min(kk))
            wr[ii] <- wr[ii]+1
        }
        else{
            kk <- sp[sk]/ww
            ii <- which(kk==max(kk))
            wr[ii] <- wr[ii]-1
        }
    }
    zz <- -1
    if(it<mxit){
        id <- sp[sk+1]/dd
        di <- sp[sk]/dd
        ss <- min(id)
        tt <- max(di)
        zz <- rep(0,length(wr))
        if(ss==tt){
            zz[id==ss] <- 1
            zz[di==tt] <- -1
        }
    }
    invisible(list(redondo=wr,delta=zz,conv=(it<mxit)))
}

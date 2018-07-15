stopifnot(all(is.element(c("package:assertthat", "package:dplyr",
                           "src:huaca"),
                     search())))

v_lib <- function(){
    library(assertthat)
    library(editrules)
}

## OjO: si x con NA
## extremos (box-plot)
## devuelve TRUE si dato es "extremo"
## x datos
## wx ponderación
## coef en cota = coef * IQR
## sup si los "extremos superiores"
## low si los "extremos inferiores"
out_bp <- function(x, wx=NULL, coef=1.5, sup=TRUE, low=TRUE){
    if(is.null(wx)) wx <- !logical(length(x))
    assert_that(length(x) == length(wx))
    
    ww <- range((boxplot.stats(x * wx, do.conf=FALSE,
                               do.out=FALSE, coef = coef))$stats)
    jj <- logical(length(x))
    if(sup) jj <- x > ww[2]
    if(low) jj <- jj | (x < ww[1])
    invisible(jj)
}

## extremos (box-plot) datos > 0
out_bpp <- function(x, ...){
    ii <- x > 0
    if(any(ii)){
        ii[which(ii)] <- out_bp(x[ii], ...)
    } else message("!!! no hay datos mayor que cero ...")
    invisible(ii)
}

## Numeric Numeric Logical -> NULL (dot char)
## prop. produce dotchart de datos (x) filtrados (sel)
out_dot <- function(x, sel=(x > 0), ...){
    assert_that(length(x) == length(sel))
    dotchart(x[sel], ...)
}

## Numeric Numeric -> Logical
## prop. devuelve TRUE si pct(x, sum(x)) >= cota%
## out_pct_p(1:3)
out_pct_p <- function(x, cota=10){
    assert_that(is.numeric(x))
    x[is.na(x)] <- 0
    pct(x) >= cota
}

## Numeric Factor|Character Numeric -> Logical
## prop.: aplicar out_pct_p por grupo g al vector x
out_pct_gp <- function(x, g=NULL, ...){
    assert_that(is.numeric(x), !is.null(g))
    c(tapply(x, g, out_pct_p, ...), recursive=TRUE)[order(g)]
}

## imprime TRUE si todos válidos
## devuelve logical de los válidos
## vreg los registros válidos
## reg los registros a validar
## v_reg(quest, cpunto)
v_reg <- function(reg, vreg){
    if(is.character(vreg)) vreg <- type.convert(vreg)
    ii <- reg %in% vreg
    print(assertthat::validate_that(all(ii)))
    invisible(ii)
}

## duplicadas sin origen
## devuelve número de boleta duplicada para la
## que no se encuentra boleta origen
## dup vector con números de boletas origen
##     NA si no es duplicada
## ori números de boletas entre las que debería
##     estar la boleta origen
## v_dup(quest, copiade)
v_dup <- function(ori, dup){
    ii <- !is.na(dup)
    jj <- !dup[ii] %in% ori
    if(any(jj)){
        message(sum(jj), " sin origen")
        ww <- ori[which(ii)[jj]]
    } else ww <- NULL
    invisible(ww)
}

## devuelve duplicadas que se autoreferencian
v_auto <- function(ori, dup){
    ii <- !is.na(dup)
    jj <- dup[ii] %in% ori[ii]
    if(any(jj)){
        message(sum(jj), " autoreferencias")
        ww <- ori[which(ii)[jj]]
    } else ww <- NULL
    invisible(ww)
}

## comprueba que hay superficie
## en cuestionarios completos o incompletos
## area: vector de área de fincas
## cc: control de cuestionario
## ccc: códigos control cuestionario
## v_sin_area(aa, c5000, c(completo=1, incompleto=3))
v_sin_area <- function(area, cc, ccc){
    ii <- area==0 & (cc %in% ccc)
    if(any(ii)) message(sum(ii), " sin dato de área de finca")
    invisible(ii)
}

## duplicadas datos iguales
## verifica sólo duplicadas con dato origen
## produce logical indicando las sin problemas
## x datos a validar correspondiente a ori y dup
## ori la fuente de datos
## dup la duplicada NA si no duplicada
## v_dup_igual(c046, quest, copiade)
## (v_dup_igual(c(10,20,11,21),c(1,2,3,4),c(NA,NA,1,2)))
v_dup_igual <- function(x, ori, dup){
    mm <- match(dup, ori)
    ii <- !is.na(mm)
    jj <- x[ii] != x[mm[ii]]
    ii[which(ii)] <- jj
    if(any(jj)) message("\n", sum(jj), " datos diferentes")
    invisible(!ii)
}

## Vector Vector Vector -> DataFrame
## data frame con registros copia-origen
## con datos diferentes en una variable
## x datos en variable
## qst id. registro (quest)
## ori registro origen (copiade)
## v_dup_x(t7$c045, r7$quest, r7$copiade)
v_dup_x <- function(x, qst, ori){
    ij <- v_dup_igual(x, qst, ori)
    if(any(!ij)){
        mm <- match(ori[!ij], qst)
        xx <- data.frame(quest=qst[!ij], copiade=ori[!ij],
                         acopi=x[!ij], aori=x[mm[!is.na(mm)]])
    } else{
        xx <- NULL
        message("!!! ok match duplicado-origen")
    }
    invisible(xx)
}

## "duplica" la variable x
## x alineada con qst y dup y con igual longitud
## (i-ésimo valor de x corresponde con i-ésimo valor de quest)
## qst número de cuestionario alineada con dup
## dup con boleta origen, NA si no es duplicada
## devuelve x con los cambios
## duplicadas de otras duplicadas no se duplican
## duplicar_v(c033, quest, copiade)
duplicar_v <- function(x, qst, dup){
    assertthat::assert_that(length(x) == length(qst),
                            length(x) == length(dup))
    ii <- !is.na(dup)
    mm <- match(dup[ii], qst)
    jj <- !is.na(mm)
    if(any(!jj)) message(sum(!jj), " sin origen")
    
    qq <- v_auto(qst, dup)
    if(!is.null(qq)){
        message("autoreferenciadas no duplican")
        jj <- jj & !qst[ii] %in% qq
    }
    x[which(ii)[jj]] <- x[mm[jj]]
    invisible(x)
}

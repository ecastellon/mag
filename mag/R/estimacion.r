# -*- coding: utf-8 -*-
## --- factores-estimación muestra punto ---

#' Probabilidad selección
#' @description calcula la probabilidad de selección de una unidad de
#'     producción ubicada en un departamento y estrato específico,
#'     mediante el cociente que resulta de dividir la superficie del
#'     estrato entre la correspondiente de la unidad de producción.
#' @param dpto vector con los códigos de departamento donde está
#'     situada la unidad de producción (entero)
#' @param estrato vector con los códigos de los estratos donde está
#'     situada la unidad de producción (entero)
#' @param area vector con la superficie de las unidades de producción
#'     (manzanas)
#' @param dfestrato data.frame con los datos relevantes de los
#'     estratos de los departamentos
#' @param idcol nombres o número de la columna de 'dfestrato', donde
#'     se encuentran las variable que identifican, en este orden, al
#'     departamento, estrato, superficie y número de puntos asignados
#'     al estrato; por omisión, las primeras 4.
#' @param dec número de decimales en el cociente resultante; por
#'     defecto, 6
#' @return vector con la probabilidad calculada o NA si esta no pudo
#'     calcularse
#' @seealso w_prs
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
pr_sel <- function(dpto, estrato, area, dfestrato,
                   idcol = 1:4, dec = 6){
    assert_that(!(missing(dpto) || missing(estrato) ||
                  missing(area) || missing(dfestrato)),
                is.character(idcol) || is.numeric(idcol),
                length(idcol) >= 4,
                msg = "parámetros incompatibles !!!\n")
    
    if (is.character(idcol)){
        id <- match(idcol, names(dfestrato))
        assert_that(!anyNA(mm),
                    msg = "revise idcol: incompatible\n")
    } else {
        id <- seq_len(ncol(dfestrato))
        assert_that(all(idcol %in% id),
                    msg = "revise idcol: incompatible\n")
        id <- idcol
    }

    wp <- area
    ## vincula finca con estrato
    mm <- match_2(dpto, estrato,
                  dfestrato[, id[1]], dfestrato[, id[2]])
    ii <- (is.na(mm) | is.na(area) | area == 0)
    if (any(ii)){
        message("\n!!! ", sum(ii), " probabilidad indefinida")
        wp[ii] <- NA_real_
    }

    ii <- !ii
    wp[ii] <- round(area[ii] / dfestrato[mm[ii], id[3]], dec)
    invisible(wp)
}

#' Factor de expansión
#' @description calcula la ponderación o factor de expansión a partir
#'     de la probabilidad de selección y el tamaño de la muestra,
#'     asociado con una unidad de producción situada en un estrato y
#'     departamento específico
#' @param dpto vector con códigos de los departamentos donde están
#'     situadas las unidades de producción
#' @param estrato vector con códigos de los estratos donde están
#'     situadas las unidad de producción
#' @param dfestrato data.frame con los datos relevantes de los
#'     estratos de los departamentos
#' @param idcol nombres o número de la columna de 'dfestrato', donde
#'     se encuentran las variable que identifican, en este orden, al
#'     departamento, estrato, superficie y número de puntos asignados
#'     al estrato; por omisión, las primeras 4
#' @param prs probabilidades de selección de las unidades de
#'     producción, o NULL si estas se van a calcular. NULL por
#'     omisión.
#' @param area la superficie (manzanas) de las unidades de producción
#'     si se van a calcular las probabilidades de selección de las
#'     unidades de producción
#' @param dec número de decimales de las ponderaciones; 6 por omisión
#' @return vector con los factores de expansión
#' @seealso pr_sel
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
w_prs <- function(dpto, estrato, dfestrato, idcol = 1:4,
                  prs = NULL, area, dec = 6){
    assert_that(!(missing(dpto) || missing(estrato) ||
                  missing(dfestrato)),
                is.character(idcol) || is.numeric(idcol),
                length(idcol) >= 4,
                msg = "datos insuficientes !!!\n")
    assert_that(is.null(prs) && !missing(area),
                msg = "no puede calcularse probabilidad sin area")
    
    if (is.null(prs) && !missing(area)){
        prs <- pr_sel(dpto, estrato, area, dfestrato, idcol, dec)
    }
    
    if (is.character(idcol)){
        id <- match(idcol, names(dfestrato))
        assert_that(!anyNA(mm),
                    msg = "revise idcol: incompatible\n")
    } else {
        id <- seq_len(ncol(dfestrato))
        assert_that(all(idcol %in% id),
                    msg = "revise idcol: incompatible\n")
        id <- idcol
    }

    nn <- integer(length(dpto))
    mm <- match_2(dpto, estrato,
                  dfestrato[, id[1]], dfestrato[, id[2]])
    ii <- is.na(mm)
    if (any(ii)){
        message("\n...", sum(ii), " no coincidencia")
    }

    ii <- !ii
    nn[ii] <- 1.0 / (prs[ii] * dfestrato[mm[ii], id[4]])
    nn[!is.finite(nn)] <- 0

    invisible(round(nn, dec))
}

#' calibración
#' @description devuelve la ponderación que calibra los factores de
#'     expansión por la estimación de una variable cuyo total en la
#'     población es conocido
#' @param x la variable por la cual se calibra
#' @param factor el factor de expansión
#' @param totpob total de la variable conocido de la población
#' @param dec número de decimales del factor calibrado
#' @return vector con igual número de elementos que 'x'
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
w_calibra <- function(x, factor, totpob = numeric(), dec = 6){
    assert_that(length(x) == length(factor),
                ok_num(totpob),
                msg = "parámetros no compatibles\n")
    te <- totpob / sum(x * factor, na.rm = TRUE)
    if (!is.finite(te)){
        te <- 1.0
    }
    numeric(length(x)) + round(te, dec)
}

## test
## zz <- data.frame(mz = c(10,20, 100),estr = 1:3)
## yy <- data.frame(estrato=c(1,1,1, 3,3,3,2,2),mz=rnorm(8),fac=1.0)
## xx <- w_estrato(yy$estrato,yy$mz,yy$fac, zz,c(2,1),2)

#' calibra - estrato
#' @description calibra los factores por el área en fincas de los
#'     estratos de un departamento
#' @param estrato estrato al que pertenece la unidad de producción
#' @param area manzanas de la unidad de producción
#' @param factor factor de expansión de la unidad de producción
#' @param totales data.frame con las columnas que identifican el
#'     estrato y la superficie (manzanas) correspondiente
#' @param idcol vector de la posición o nombre (en el data.frame
#'     'totales') de las columnas, en este orden, estrato y
#'     superficie; por defecto, c(1, 2)
#' @param dec número de decimales en la ponderación; 6 por defecto
#' @return vector con igual número de elementos que 'factor'
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
w_estrato <- function(estrato, area, factor, totales,
                      idcol = c(1L, 2L), dec = 6){

    assert_that(length(estrato) == length(area),
                length(estrato) == length(factor),
                is.data.frame(totales) && length(totales) >= 2,
                is.character(idcol) || is.numeric(idcol),
                length(idcol) == 2,
                msg = "parámetros incompatibles\n")
    
    if (is.character(idcol)){
        id <- match(idcol, names(totales))
        assert_that(!anyNA(mm),
                    msg = "nombre de columnas incompatibles\n")
    } else {
        id <- idcol
        assert_that(all(id %in% seq_len(ncol(totales))),
                    msg = "nombre de columnas incompatibles\n")
    }
    ff <- factor(totales[,id[1]])
    rownames(totales) <- levels(ff)[ff]
    
    ww <- split(data.frame(x = area, y = factor), estrato,
                drop = TRUE)

    zz <- lapply(names(ww),
                 function(z){
                     w_calibra(ww[[z]]$x, ww[[z]]$y,
                               totales[z, id[2]], dec)
                 })
    invisible(unsplit(zz, estrato, drop = TRUE))
}

#' Elegibilidad desconocida
#' @description ponderación por elegibilidad es el factor que se
#'     aplica cuando no fue posible hacer 'contacto' con la unidad de
#'     producción y por tanto no fue posible determinar si es miembro
#'     (elegible) o no, de la población muestreada. La ponderación se
#'     calcula dentro de grupos determinados por una variable
#'     relacionada con la frecuencia de 'contactos' (por ejemplo,
#'     estrato, clase de tamaño de finca, etc.). Se calcula como el
#'     cociente entre el tamaño de la muestra y el número de
#'     'contactos' en la clase.
#' @param clase variable de agrupamiento
#' @param ccc variable con el código de control del cuestionario que
#'     indica si la unidad de muestreo fue 'contactada' o no.
#' @param cnc códigos de control usados para indicar los no
#'     'contactados' y cuya elegibildad es desconocida
#' @param dec número de decimales en la ponderación; 5 por omisión
#' @return vector con ponderaciones (1 o mayor que 1)
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
w_ed <- function(clase, ccc, cnc, dec = 5){

    wb <- numeric(length(clase)) + 1.0
    
    nc <- tapply(wb, clase, sum) ## total clase
    ii <- contactado(ccc, cnc)
    ne <- tapply(wb * ii, clase, sum) ## num. contactados en clase
    if (any(ii <- ne == 0)){
        message("clase ", names(ne)[ii],
                " sin contactos. No se ajusta: w_ne = 1.")
        ne[ii] <- 1.0
    }
    fc <- round(nc / ne, dec)
    mm <- match(as.character(clase), names(nc))
    fc <- fc[mm]
    dim(fc) <- NULL
    invisible(fc)
}

#' no respuesta
#' @description ponderación por no respuesta es el factor que se
#'     aplica por efecto de las unidades de muestreo con elegibilidad
#'     conocida y que rechazaron informar. Se calcula como el cociente
#'     del número de unidades de muestreo con elegibilidad conocida y
#'     el de las que no rechazaron la entrevista.
#' @param clase variable de agrupamiento
#' @param ccc variable con el código de control del cuestionario que
#'     indica si la unidad de muestreo es elegible, 'contactada', y
#'     si rechazó o no, la entrevista
#' @param crs códigos de control usados para indicar los que
#'     respondieron
#' @param cnc códigos de control usados para indicar los no
#'     'contactados'
#' @param cne códigos de control usados para indicar a las unidades de
#'     muestreo no elegibles (no agrícolas)
#' @param dec número de decimales en la ponderación; 5 por omisión
#' @export
#' @author eddy castellón
w_nr <- function(clase, ccc, crs, cnc, cne, dec = 5){

    wb <- numeric(length(clase)) + 1.0
    
    ii <- contactado(ccc, cnc) & elegible(ccc, cne)
    nc <- tapply(wb * ii, clase, sum) ## total elegibles conocidos
    ii <- ii & si_responde(ccc, crs)
    nr <- tapply(wb * ii, clase, sum) ## elegibles responden
    fc <- round(nc / nr, dec)
    mm <- match(as.character(clase), names(nc))
    fc <- fc[mm]
    dim(fc) <- NULL

    invisible(fc)
}

#' contactado
contactado <- function(cq, nc = "novisitado"){
    invisible(!cq %in% nc)
}

#' elegible
elegible <- function(cq, ne = "noagricola"){
    invisible(!cq %in% ne)
}

#' con respuesta
si_responde <- function(cq, rs){
    invisible(cq %in% rs)
}

#' estima - clase
#' @description agrupa los datos por las clases definidas en una
#'     variable (p.ej. departamentos), calcula los estimados por grupo
#'     como la suma de la variable ponderada los factores de expansión
#'     y otras variables, y luego las agrega para formar el total
#'     (p.ej. país) si así es indicado (FALSE por omisión)
#' @param x variable con los datos
#' @param grupo variable de agrupamiento
#' @param factor factor de expansión
#' @param wfx ponderación adicional (p.ej. por no respuesta o
#'     cualquier otra mediante la cual se desee 'ajustar' los
#'     factores)
#' @param nom nombre alternativo al implícito en las clases de
#'     agrupamiento
#' @param total agregar el total? FALSE por omisión
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
estima_gr <- function(x, grupo, factor, wfx, nom, total = FALSE){
    if (missing(wfx)){
        wfx <- integer(length(factor)) + 1L
    }
    
    assert_that(length(x) == length(factor),
                length(x) == length(grupo),
                msg = "longitud de vectores es incompatible!!!\n")
    
    ww <- tapply(x * factor * wfx, grupo, sum, na.rm = TRUE)

    if (!missing(nom)){
        if (length(ww) == length(nom)){
            ww <- setNames(ww, nom)
        } else {
            message(paste("vector de nombres no compatible",
                          "con el de clases\n"))
        }
    }

    if (total){
        ww <- setNames(c(ww, sum(ww, na.rm = TRUE)),
                       c(names(ww), "Total"))
    }
    ww
}

#' estimado - contribución
#' @description encuentra el porcentaje con el que contribuye cada
#'     dato a la estimación del total
#' @param x variable de interés
#' @param factor factor de expansión
#' @param wfx ponderación adicional al factor de expansión
#' @param dec número de decimales en el porcentaje
#' @export
#' @author eddy castellón
pct_estima <- function(x, factor, wfx, dec = 0){
    assert_that(length(x) == length(factor),
                msg = "variable y factor incompatibles\n")
    
    if (missing(wfx)){
        wfx <- integer(length(factor)) + 1L
    } else {
        assert_that(length(factor) == length(wfx),
                    msg = "factor y ponderación incompatibles\n")
    }
    
    invisible(pct(x * factor * wfx, dec = dec))
}

## --- utiles durante estimación ---
#' punto-válido
#' @description verifica cuales números de punto no se encuentran en
#'     la lista de puntos válidos
#' @param npto código de punto
#' @param vpto códigos de puntos válidos
#' @export
#' @author eddy castellón
v_reg <- function(npto, vpto){
    if (is.character(vpto)){
        vpto <- type.convert(vpto)
    }
    invisible(npto %in% vpto)
}

#' autoreferenciadas
#' @description identifica las unidades de producción que tienen dos o
#'     más códigos de cuestionario distinto ('duplicadas') pero que se
#'     autoreferencian
#' @param ori código de cuestionario de la unidad de producción
#'     'origen'
#' @param dup otro código de cuestionario de la misma unidad de
#'     producción ('duplicada')
#' @export
#' @author eddy castellón
v_auto <- function(ori, dup){
    ii <- !is.na(dup)
    jj <- dup[ii] %in% ori[ii]

    ww <- NULL
    if (any(jj)){
        message(sum(jj), " autoreferencias")
        ww <- ori[which(ii)[jj]]
    }
    
    invisible(ww)
}

#' duplicar datos
#' @description para una variable dada, sustituye los datos en las
#'     boletas 'duplicadas' con los correspondientes de las boletas
#'     'origen'
#' @param x variable
#' @param qst códigos de cuestionarios
#' @param dup código del cuestionario origen si la boleta es
#'     'duplicada'; NA si no es duplicada
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
duplicar_v <- function(x, qst, dup){
    assert_that(length(x) == length(qst),
                length(x) == length(dup))
    
    ii <- !is.na(dup)
    mm <- match(dup[ii], qst)
    jj <- !is.na(mm)
    if (any(!jj)){
        message(sum(!jj), " duplicadas sin origen")
    }
    
    qq <- v_auto(qst, dup)
    if (!is.null(qq)){
        message("autoreferenciadas no duplican")
        jj <- jj & !qst[ii] %in% qq
    }
    x[which(ii)[jj]] <- x[mm[jj]]
    invisible(x)
}

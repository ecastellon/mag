setOldClass(c("data.frame"))
setGeneric("names")
setGeneric("get")
setGeneric("existe", function(x, ...) standardGeneric("existe"))
# alternativas a la genérica comment, el atributo es meta (por metadato)
setGeneric("descripcion", function(x, ...) standardGeneric("descripcion"))
setGeneric("meta<-", function(x, metadato, ...) standardGeneric("meta<-"))
setGeneric("meta", function(x, ...) standardGeneric("meta"))
setGeneric("guardar", function(x, name, file, ...) standardGeneric("guardar"))
setGeneric("datos", function(x, file, ...) standardGeneric("datos"))
setGeneric("querysql", function(x, ...) standardGeneric("querysql"))
setGeneric("resumen", function(x, ...) standardGeneric("resumen"))
setGeneric("ltxhtml", function(x, fltx = character(0), tipo = "latex",
                               dec = numeric(0), 
                               ali = character(0), nomr = character(0),
                               clider = 0, glast = TRUE, ultimafila = 0, 
                               ...) {
    standardGeneric("ltxhtml")
})
### 
setMethod("existe", "data.frame", function(x) {
    FALSE
})
# 
setMethod("descripcion", "data.frame", function(x) {
    return("ninguna")
})

setMethod("meta<-", "data.frame",
          function(x, value){
              attr(x, "meta") <- value
              invisible(x)})

setMethod("meta", "data.frame", function(x) {
    ifelse(is.null(cc <- attr(x, "meta")), NA_character_, cc)
})

# 
setMethod("guardar", "data.frame", function(x, name, file) {
    if (missing(file))
        stop("file?")
    if (missing(name)) 
        name <- deparse(substitute(x))
    env <- new.env(hash = FALSE)
    if (file.exists(file)) 
        load(file, envir = env)
    assign(name, x, pos = env)
    save(list = ls(name = env), file = file, envir = env, compress = TRUE)
    rm(env)
    print(paste(name, "transferido a", file))
})

setMethod("datos", "data.frame", function(x, name, file) {
    if (missing(file)) 
        stop("file?")
    if (missing(name)) 
        name <- 1
    if (file.exists(file)) {
        env <- new.env(hash = FALSE)
        dfs <- load(file, envir = env)
        cual <- switch(class(name), character = match(name, dfs), numeric = ifelse(name > 
            0 && name <= length(dfs), name, NA), NA)
        d <- ifelse(is.na(cual), NULL, get(dfs[cual], envir = env))
        if (is.null(d)) 
            print(paste("no existe", name, "en", file))
        rm(env)
    } else {
        print(paste("no existe archivo", file))
        d <- NULL
    }
    invisible(d)
})
# tabla latex ntab del data.frame x dec:decimales en la columna ali:alineacion de
# las columnas nomr:nombre de las hileras de x que definen la columna de
# identificacion clider:columna que determina el orden; 0 orden original >0 orden
# descendente; <0 ascendente glast:cuando orden descendente: la mas grande de
# ultimo?  por aquello de poner totales en la ultima hilera ---algoritmo ordenar
# filas de tabla---2010-02-17 no siempre indicador de país es el más grande
# (promedios) se modifica el algoritmo para considerar esto ultimafila es
# parametro que indica la fila que deberá quedar en la misma posición después de
# ordenar por clider ultimafila=0 se ordenan todas las filas y controla glast ---
# OJO:convertir a metodo de data.frame pp(postproceso):adaptaciones propias
# usando threeparttable **ToDo** -validacion del nombre de archivo
setMethod("ltxhtml", "data.frame", function(x, fltx = character(0), tipo = "latex", 
    dec = numeric(0), ali = character(0), nomr = character(0), clider = 0, glast = 1, 
    pp = character(0), ultimafila = 0) {
    carcorltx <- function(cc) {
        cc <- gsub("%", "\\%", cc, fixed = TRUE)
        cc <- gsub("$", "\\$", cc, fixed = TRUE)
        cc
    }
    require("xtable")
    if (length(nomr) == nrow(x)) 
        rownames(x) <- nomr
    if (abs(clider) %in% (1:ncol(x))) {
        h <- sort.list(x[, abs(clider)], decreasing = (clider > 0))
        if (ultimafila %in% 1:nrow(x)) {
            ultimafila <- which(h == ultimafila)
            h <- c(h[-ultimafila], h[ultimafila])
        } else {
            if (glast) 
                h <- c(h[-1], h[1])
        }
        x <- x[h, ]
    }
    xtb <- xtable(x)
    # falta mas controles a dec y ali ver los metodos en xtable
    if (length(dec) == 0) 
        dec <- rep(0, ncol(x) + 1)
    digits(xtb) <- dec
    if (length(ali) == 0) 
        ali <- c("l", rep("r", ncol(x)))
    align(xtb) <- c("l", rep("r", ncol(x)))
    # version de xtable devuelve string pero si no hay archivo imprime en pantalla
    if (length(fltx) == 0) 
        fltx <- ""
    guardar <- fltx != ""
    if (!guardar) 
        fltx <- tempfile()
    sT <- print(xtb, file = fltx, type = tipo)
    if (!guardar) {
        unlink(fltx)
        fltx <- ""
    }
    if (tipo == "latex" && length(pp) > 0) {
        z <- sapply(strsplit(sT, "\n"), function(x) gsub("^[[:space:]]+", "", x), 
            USE.NAMES = FALSE)
        sT <- z[1:2]
        sT <- c(sT, "%-- modificaciones(E.Castellon)", paste("%--", pp["descripcion"]))
        bL <- (pp["landscape"] == "si")
        if (bL) 
            sT <- c(sT, "\\begin{landscape}")
        # sT <- c(sT,'\\begin{table}[hbt]',pp['fontsize'])
        sT <- c(sT, "\\begin{table}[hbt]")
        sT <- c(sT, "\\centering", "\\begin{threeparttable}")
        sT <- c(sT, paste("\\caption[", pp["toc"], "]", sep = ""))
        sT <- c(sT, paste("{", pp["titulo"], "}", sep = ""))
        sT <- c(sT, paste("\\label{t:", pp["label"], "}", sep = ""))
        bFs <- pp["fontsize"] %in% c("\\tiny", "\\small", "\\large")
        if (bFs) 
            sT <- c(sT, paste("{", pp["fontsize"], sep = ""))
        if (pp["formato"] == "") {
            i <- sapply(z, function(w) any(grep("tabular}{", w)))
            cT <- z[i]
        } else cT <- paste("\\begin{tabular}{", pp["formato"], "}", sep = "")
        sT <- c(sT, cT, "\\toprule")
        sT <- c(sT, carcorltx(pp["cabecera"]))
        cT <- z[sapply(z, function(w) any(grep("^[[:alnum:]]", w)))]
        cT <- gsub("&[[:space:]]+", "&", cT)
        cT <- gsub("&0", "&", cT)
        sT <- c(sT, "\\midrule", cT)
        sT[length(sT)] <- paste("\\midrule", sT[length(sT)])
        sT <- c(sT, "\\bottomrule", "\\end{tabular}")
        if (pp["notas"] != "") 
            sT <- c(sT, "\\begin{tablenotes}", pp["notas"], "\\end{tablenotes}")
        if (bFs) 
            sT <- c(sT, "}")
        sT <- c(sT, "\\end{threeparttable}", "\\end{table}")
        if (bL) 
            sT <- c(sT, "\\end{landscape}")
        if (guardar) 
            cat(sT, file = fltx, sep = "\n")
    }
    invisible(paste(sT, sep = "\n"))
}) 

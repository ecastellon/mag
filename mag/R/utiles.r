# -*- coding: utf-8 -*-

#' @export
save_df <- function(x, name, file) UseMethod("save_df")
#' @export
quitar_0 <- function(x, excepto) UseMethod("quitar_0")

#' Modifica el atributo \code{meta}
#' @description Para agregar alguna descripción significativa de los
#'     datos. Alternativa a la función \code{comment}.
#' @export
#' @examples
#' meta(df) <- "metadata"
#' @author Eddy Castellón
`meta<-` <- function(x, value){
    attr(x, "meta") <- value
    invisible(x)
}

#' Atributo \code{meta}
#' @param x nombre del data.frame u objeto (sin comillas)
#' @return Valor del atributo \code{meta} o \code{NA} si no ha sido asignado
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' meta(name_df) -> attr(name_df, "meta")
#' @author Eddy Castellón
meta <- function(x){
    ifelse(is.null(cc <- attr(x, "meta")), NA_character_, cc)
}

#' metadatos
meta_c <- function(x){
    assert_that(is.character(x) && length(x) == 1)
    meta(eval(as.name(x), parent.frame()))
}

#' Lista de data.frames
#' @description Encuentra los data.frame definidos en un "environment"
#' @param env "environment" donde buscar; por omisión, el desde donde
#'     se llama la función (parent.frame)
#' @importFrom assertthat assert_that
#' @seealso list_dff
#' @export
list_dfe <- function(env){
    if (missing(env)){
        env <- parent.frame()
    } else {
        assert_that(is.environment(env),
                    msg = "argumento debe ser 'environment'")
    }
    cc <- ls(env)
    ii <- vapply(cc, function(x)is.data.frame(eval(as.name(x), env)),
                 TRUE)
    cc[ii]
}

#' data.frames en archivo
#' @description Lista los data.frame almacenados en el archivo \code{file}
#' @param file ruta/nombre del archivo
#' @param meta incluir el atributo \code{meta}?; FALSE por omisión.
#' @return devuelve vector con los nombres de data.frame o un
#'     data.frame con el atributo \code{meta} asociado si fuera pedido.
#' @export
#' @examples
#' list_dff("file_name")
#' @seealso list_dfe
#' @author Eddy Castellón
#' @importFrom assertthat assert_that
list_dff <- function(file, meta = FALSE){
    assert_that(file.exists(file),
                msg = "archivo no existe")
    cc <- load(file)
    mt <- character(length(cc))
    ii <- logical(length(cc))
    for (jj in seq_along(cc)){
        zz <- eval(as.name(cc[jj]))
        ii[jj] <- is.data.frame(zz)
        if (ii[jj]){
            mt[jj] <- ifelse(is.null(me <- attr(zz, "meta")),
                             NA_character_, me)
        }
    }

    if (any(ii)){
        xx <- cc[ii]
        if (meta){
            xx <- data.frame(data = xx, meta = mt[ii],
                             stringsAsFactors = FALSE)
        }
    } else {
        message("\n... ningún data.frame en archivo")
        xx <- NULL
    }
    ## xx <- list_dfe()
    ## if (meta){
    ##     xx <- data.frame(data = xx,
    ##                      meta = vapply(xx, meta_c, "",
    ##                                    USE.NAMES = FALSE),
    ##                      stringsAsFactors = FALSE)
    ## }
    xx
}

#' Guardar data.frame
#' @description Agrega data.frame a un archivo con un nuevo nombre si
#'     así fuera indicado. Los objetos (data.frame y otros)
#'     previamente almacenados en el archivo, son preservados.
#' @param x nombre del data.frame (sin comillas).
#' @param name nombre con que será almacenado el data.frame en el archivo.
#' @param file ruta/nombre del archivo
#' @export
#' @importFrom assertthat assert_that
save_df.data.frame <- function(x, name, file){

    assert_that((!missing(file)) && ok_nombre(file),
                msg = "nombre de archivo no válido?")
    if (missing(name)){
        warning("\n... guarda con el mismo nombre del data.frame")
        name <- deparse(substitute(x))
    } else {
        assert_that(nzchar(name),
                    msg = "nombre inválido")
    }

    env <- new.env()
    if (file.exists(file)){
        load(file, envir=env)
    }
    assign(name, x, pos = env)
    save(list = ls(name = env), file = file, envir = env, compress = TRUE)
    cat(name, "transferido a", file, "\n")
}

#' Agrega objetos a un archivo
#' @description Agrega uno o más objetos (data.frame y otros)
#'     indicados por su nombre (sin comillas) o en un vector (entre
#'     comillas) definidos en un "environment", a un archivo,
#'     conservando los que ya se encontraban en él.
#' @param list vector con los nombres (entre comillas) de los objetos
#' @param file ruta/nombre del archivo
#' @param envir el "environment" donde están definidos los objetos;
#'     por omisión, el "environment" desde donde se llama la función
#'     (parent.frame)
#' @return listado (invisible) de objetos agregados
#' @export
#' @examples
#' save_add(aa, bb, file="xx.rda")
#' save_add(list=c("aa", "bb"), file="xx.rda")
#' save_add(aa, bb, list=c("dd", "ee"), file="xx.rda")
#' @importFrom assertthat assert_that
save_add <- function(..., list = character(), file, env){
    assert_that((!missing(file)) && ok_nombre(file),
                msg = "a cuál archivo? !!!\n")
    if (missing(env)){
        env <- parent.frame()
    } else {
        assert_that(exists(env))
    }

    nm <- as.character((substitute(alist(...))))[-1L]
    assert_that(length(nm) > 0 || length(list) > 0,
                msg = "nada para guardar..!!!\n")
    nm <- c(nm, list)

    ne <- new.env()
    if (file.exists(file)){
        cc <- load(file, envir = ne)
    } else {
        cc <- NULL
    }

    ok <- setNames(!logical(length(nm)), nm)
    for (ss in nm){
        if (exists(ss, envir = env)){
            assign(ss, get(ss, envir = env, inherits = FALSE),
                   envir = ne)
        } else {
            warning("\n!!!", ss, " no existe")
            ok[ss] <- FALSE
        }
    }
    assert_that(any(ok),
                msg = "\n... ninguno por agregar !!!")
    nm <- nm[ok]

    if (is.null(cc)){
        cc <- nm
    } else {
        cc <- union(cc, nm)
    }

    save(list = cc, file = file, compress = TRUE, envir = ne)
    cat(gettextf("%i objetos transferidos a %s\n", length(cc), file))
    invisible(cc)
}

#' Agrega objetos a un archivo. Alias de save_add.
#' @description Agrega uno o más objetos (data.frame y otros)
#'     indicados por su nombre (sin comillas) o en un vector (entre
#'     comillas) definidos en un "environment", a un archivo,
#'     conservando los que ya se encontraban en él.
#' @param list vector con los nombres (entre comillas) de los objetos
#' @param file ruta/nombre del archivo
#' @param envir el "environment" donde están definidos los objetos;
#'     por omisión, el "environment" desde donde se llama la función
#'     (parent.frame)
#' @return listado (invisible) de objetos agregados
#' @export
#' @examples
#' save_add(aa, bb, file="xx.rda")
#' save_add(list=c("aa", "bb"), file="xx.rda")
#' save_add(aa, bb, list=c("dd", "ee"), file="xx.rda")
#' @importFrom assertthat assert_that
add_tof <- function(..., list = character(), file, env){
    save_add(..., list, file, env)
}

#' Leer data.frame y asignarlo a un "environment"
#' @description Lee uno o más data.frame almacenados en un archivo y
#'     los asigna a un "environment" con el mismo nombre que tienen en
#'     el archivo
#' @param ... nombres de data.frame ("sin comillas")
#' @param list vector de caracteres con el o los nombres de los
#'     data.frame que serán transferidos del archivo al "environment"
#' @param file ruta/nombre del archivo
#' @param envi "environment" de destino; por omisión el desde donde
#'     se llama la función (parent.frame)
#' @return listado (invisible) de los data.frame que fueron
#'     encontrados y devueltos
#' @examples
#' read_dff(c("aa", "bb"), file="xx.rda")
#' nwe <- new.env(); read_dff(aa, file = "xx.rda", envir = nwe)
#' @export
#' @importFrom assertthat assert_that
read_dff <- function(..., list = character(), file, envir){
    assert_that(file.exists(file),
                msg = "de cuál archivo?")

    nm <- as.character((substitute(alist(...))))[-1L]
    assert_that(length(nm) || length(list) > 0,
                msg = "nada para leer..!!!\n")
    nm <- c(nm, list)

    if (missing(envir)){
        envir <- parent.frame()
    } else {
        assert_that(exists(envir))
    }

    oo <- load(file)
    cc <- intersect(oo, nm)
    if(length(cc) != length(nm)){
        message("estos ",
                paste0(setdiff(nm, cc), collapse=","),
                " no están en ", file, "\n")
    }
    if (length(cc)){
        for (ss in cc){
            assign(ss, get(ss, mode = "list"), envir = envir)
        }
    }
    invisible(cc)
}

#' Leer data.frame
#' @description Lee data.frame almacenado en archivo para asignarlo a
#'     una variable en el "environment" desde donde se llama la
#'     función
#' @param df nombre del data.frame (comilla o sin comillas)
#' @param fi ruta/nombre del archivo donde está almacenado el
#'     data.frame
#' @return data.frame (invisible)
#' @examples
#' ww <- get_dff(aa, file="xx.rda")
#' @seealso get_dff_c
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
get_dff <- function(df, file = character()){
    assert_that(!missing(df), file.exists(file),
                msg = "falta nombre de archivo o data.frame")

    oo <- load(file)
    df <- as.character(substitute(df))
    if (is.element(df, oo)){
        x <- get(df, inherits = FALSE)
    } else {
        message("\n...data.frame ", df, " no existe !!!")
        x <- oo
    }
    invisible(x)
}

#' Leer data.frame
#' @description Lee data.frame almacenado en archivo para asignarlo a
#'     una variable en el "environment" desde donde se llama la
#'     función. Es la versión de get_dff para llamar desde una función
#'     a la cual el parámetro df ha sido pasado como tipo character.
#' @param df nombre del data.frame (entre comillas)
#' @param fi ruta/nombre del archivo donde está almacenado el
#'     data.frame
#' @return data.frame (invisible)
#' @examples
#' ww <- get_dff(aa, file="xx.rda")
#' @seealso get_dff
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
get_dff_c <- function(df, file = character()){
    assert_that(!missing(df), file.exists(file),
                msg = "falta nombre de archivo o data.frame")

    oo <- load(file)
    if (is.element(df, oo)){
        x <- get(df, inherits = FALSE)
    } else {
        message("\n...data.frame ", df, " no existe !!!")
        x <- oo
    }
    invisible(x)
}

#' quitar ceros
#' @description elimina las filas de un data.frame que tienen cero en
#'     todas las columnas, exceptuando las indicadas. Las columnas de
#'     caracteres no se toman en cuenta.
#' @param df data.frame
#' @param excepto nombre de las columnas exceptuadas del filtro
#' @return data.frame sin los registros filtrados
#' @examples
#' quitar_0(ww, excepto="quest") -> data.frame sin los registros donde
#'     todas las columnas (exceptuando la nombrada "quest") llevan
#'     dato = 0
#' @export
#' @import dplyr
#' @author eddy castellón
quitar_0.data.frame <- function(df, excepto){
    ii <- vapply(df, is.numeric, TRUE)
    if (any(ii)){
        ss <- names(df)[ii]
        if (!missing(excepto)){
            assert_that(is.numeric(excepto) || is.character(excepto),
                        msg = "excepto numerico o caracter")
            if (is.character(excepto)){
                ss <- setdiff(ss, excepto)
            } else { ## chk cardinal
                ss <- ss[setdiff(seq_along(df)[ii], excepto)]
            }
        }
        df <- filter_at(df, vars(ss), any_vars(. > 0))
    }
    df
}

## --- departamentos-municipios ---

#' Departamento-municipio
#' @description la ruta de acceso del archivo con los códigos de
#'      departamento y municipio es leída de la variable DEPMUN
#'      almacenada en .Renviron
#' @param x nombre del archivo o no indicado
#' @importFrom assertthat assert_that
#' @author eddy castellón
file_depmun <- function(x = character()){
    if (!length(x)){
        x <- Sys.getenv("DEPMUN")
    }
    assert_that(nzchar(x) && file.exists(x),
                msg = "archivo no existe !!!\n")
    invisible(x)
}

#' Municipios
#' @description devuelve data.frame con los códigos y nombres de los
#'     municipios
#' @param file nombre del archivo; si no es indicado se lee de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return data.frame con dos columnas: la primera con el código de
#'     municipio, la segunda con el nombre del municipio
#' @importFrom assertthat assert_that
#' @export
#' @author eddy castellón
df_muni <- function(file = character(), dfmun = "dmuni"){
    assert_that(length(file) <= 1,
                msg = "nombre del archivo con códigos")
    fdm <- file_depmun(file)
    invisible(get_dff_c(dfmun, fdm))
}

#' Códigos oficiales
#' @description la lista de códigos oficiales
#' @param file nombre del archivo; si no es indicado lo toma de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     supone que la primera columna lleva los códigos
#' @seealso df_muni
#' @export
#' @author eddy castellón
cod_municipios <- function(file = character(), dfmun = "dmuni"){
    x <- df_muni(file, dfmun)
    as.integer(x[,1])
}

#' Nombres
#' @description nombre del departamento o municipio
#' @param cod códigos de los departamentos o municipios
#' @param x data.frame con los datos de municipios o departamentos
#' @author eddy castellón
nombre_cod <- function(cod = integer(), x){

    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod),
                msg = "códigos no válidos")
    if (ii){
        cod <- as.integer(cod)
    }

    dm <- vector("character", length(cod))
    mm <- match(cod, x[,1])
    ii <- is.na(mm)
    dm[!ii] <- x[mm[!ii], 2]
    if (any(ii)){
        dm[ii] <- NA_character_
        warning("!! NA códigos no válidos\n")
    }
    dm
}

#' Municipios
#' @description nombres oficiales de municipios
#' @param cod códigos de municipios (entero)
#' @param file nombre del archivo; si no es indicado lo toma de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     supone que la segunda columna lleva los nombres
#' @return lista de nombres de municipios indicados en \code{cod}, o la
#'     de todos si no se indican los códigos
#' @seealso df_muni
#' @export
#' @author eddy castellón
municipios <- function(cod, file = character(), dfmun = "dmuni"){
    
    x <- df_muni(file, dfmun)
    if (missing(cod)){
        dm <- as.character(x[,2])
    } else {
        dm <- nombre_cod(cod, x)
    }
    dm
}

#' municipios - departamento
#' @description lista de nombres de municipios en un departamento
#' @param dpto código (entero) del departamento
#' @param nombre TRUE devuelve nombres (por omisión), FALSE devuelve
#'     códigos
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return lista de nombres o códigos de los municipios de un
#'     departamento
#' @seealso municipios, df_muni, file_depmun
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
municipios_dp <- function(dpto = numeric(), nombre = TRUE,
                          file = character(), dfmun = "dmuni"){
    assert_that(ok_num(dpto) && length(dpto) == 1,
                msg = "sólo un código de departamento\n")
    
    x <- df_muni(file, dfmun)
    
    cc <- as.integer(x[,1])
    ii <- (cc %/% 100L) == dpto
    if (any(ii)){
        if (nombre){
            nm <- x[ii, 2]
        } else {
            nm <- cc[ii]
        }
    } else {
        message("código de departamento no válido\n")
        nm <- NA_character_
    }
    nm
}

#' Codigo municipio
#' @description código compuesto de municipio
#' @param mu código de municipio (entero)
#' @param dp código departamento (entero)
#' @return vector de enteros con el código compuesto de municipio
#' @author eddy castellón
cod_muni_ <- function(mu, dp){
    
    nm <- length(mu)
    nd <- length(dp)
    if (nm != nd){
        warning("!! vectores de códigos con desigual número\n")
    }

    x <- as.integer(dp * 100L + mu)
    ii <- !ok_muni(x)
    if (any(ii)){
        x[ii] <- NA_integer_
        warning("NA código no válido\n")
    }
    invisible(x)
}

#' Codigo municipio
#' @description genera el código compuesto de municipio a partir del
#'     código de departamento y código de municipio dentro de
#'     departamento
#' @param mu código de municipio
#' @param dp código departamento
#' @return código tipo entero, NA si no es municipio válido
#' @examples cod_muni(5, 5) -> 505
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_muni <- function(mu = integer(), dp = integer()){
    ii <- ok_int_chr(mu)
    jj <- ok_int_chr(dp)
    assert_that(ii || ok_num(mu), jj || ok_num(dp),
                msg = "!!! códigos no válidos\n")
    
    if (ii){
        mu <- as.integer(mu)
    }
    if (jj){
        dp <- as.integer(dp)
    }
    cod_muni_(mu, dp)
}

#' Código de municipio
#' @description función no vectorizada que devuelve código de
#'     municipio a partir de su nombre
#' @param nom nombre del municipio
#' @param dfmun data.frame con códigos-nombres de municipio oficiales;
#'     la primera columna lleva el código, la segunda el nombre
#' @return código de municipio si es válido; si no, NA
#' @author eddy castellón
cod_muni_nom <- function(nom = character(), dfmun){
    if (grepl("[[:alpha:]]+", nom)){#al menos una letra
        nom <- str_sin_tilde(tolower(str_bien_formada(nom)))
        mm <- pmatch(nom, tolower(as.character(dfmun[,2])),
                     duplicates.ok = FALSE)
        cod <- ifelse(is.na(mm), NA_integer_, dfmun[mm, 1])
    } else {#solo digitos
        cod <- NA_integer_
    }
    cod
}

#' Codigo municipio
#' @description devuelve código de municipio a partir de su nombre.
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return código de municipio si es válido; si no, NA
#' @seealso df_muni()
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_muni_nombre <- function(nom = character(), file = character(),
                            dfmun = "dmuni"){
    assert_that(ok_chr(nom),
                msg = "código tipo caracter")
    x <- df_muni(file, dfmun)

    cod <- vapply(nom, cod_muni_nom, 0, x, USE.NAMES = FALSE)
    if (anyNA(cod)){
        warning("hay nombres no válidos\n")
    }
    cod
}

#' código departamento - municipio
#' @description devuelve el código de departamento que corresponde a
#'     nombre de municipio
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @export
#' @author eddy castellón
cod_dpto_muni <- function(nom = character(), file = character(),
                          dfmun = "dmuni"){
    cm <- cod_muni_nombre(nom, file, dfmun)
    ii <- !is.na(cm)
    cm[ii] <- cm[ii] %/% 100L
    cm
}

#' nombre departamento - municipio
#' @description devuelve nombre del departamento al que pertenece el
#'     municipio
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @author eddy castellón
nom_dpto_muni <- function(nom, file = character(),
                          dfmun = "dmuni", dfdpt = "ddpto"){
    x <- cod_dpto_muni(nom, file, dfmun)
    ii <- !is.na(x)
    nd <- vector("character", length(x))
    nd[ii] <- departamentos(x[ii], file, dfdpt)
    if (any(!ii)){
        nd[!ii] <- NA_character_
    }
    nd
}

#' Municipio válido
#' @description valida código de municipio
#' @param cod código extendido (departamento-municipio), caracter o
#'     numero
#' @param file nombre del archivo con data.frame del municipio; si no
#'     es indicado, se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return vector lógico con igual número de elementos que cod
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
ok_muni <- function(cod = integer(), file = character(),
                    dfmun = "dmuni"){
    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod), msg = "código tipo entero")
    if (ii){
        cod <- as.integer(cod)
    }
    cod %in% cod_municipios(file, dfmun)
}

## --- departamentos ---
#' Departamentos
#' @description códigos y nombres de los departamentos
#' @param file nombre del archivo con data.frame departamentos; si no
#'     es indicado se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return data.frame; primera columna el código oficial, y la segunda
#'     el nombre
#' @export
#' @author eddy castellón
df_dpto <- function(file = character(), dfdpt = "ddpto"){
    
    assert_that(length(file) <= 1,
                msg = "sólo nombre del archivo\n")
    fdm <- file_depmun(file)
    invisible(get_dff_c(dfdpt, fdm))
}

#' Codigos departamentos
#' @description lista de códigos de departamento
#' @param file nombre del archivo con data.frame departamentos; si no
#'     es indicado se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @export
#' @author eddy castellón
cod_departamentos <- function(file, dfdpt = "ddpto"){
    x <- df_dpto(file, dfdpt)
    as.integer(x[,1])
}

#' Departamentos
#' @description lista de departamentos correspondientes a los códigos
#'     o lista completa si estos no se indican
#' @param file nombre del archivo con data.frame departamentos; si no
#'     es indicado se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return lista de nombres de departamentos indicados en \code{cod},
#'     o la de todos si no se indican los códigos
#' @seealso df_dpto
#' @export
#' @author eddy castellón
departamentos <- function(cod, file = character(),
                         dfdpt = "ddpto"){
    x <- df_dpto(file, dfdpt)

    if (missing(cod)){
        dm <- as.character(x[,2])
    } else {
        dm <- nombre_cod(cod, x)
    }
    dm
}

#' código departamento
#' @description devuelve código del departamento que corresponde al
#'     nombre del departamento
#' @param nom nombre del departamento
#' @param file nombre del archivo con los data.frame
#' @param dfdpt nombre del data.frame con datos de departamentos; por
#'     omisión 'ddpto'
#' @author eddy castellón
cod_dpto_nom <- function(nom, dfdpt = "ddpto"){

    if (grepl("[[:alpha:]]+", nom)){#al menos una letra
        nom <- str_sin_tilde(tolower(str_bien_formada(nom)))
        
        mm <- pmatch(nom, tolower(dfdpt[,2]),
                     duplicates.ok = FALSE)
        if (is.na(mm)){
            cod <- NA_integer_
        } else {
            cod <- dfdpt[mm, 1]
        }
    } else {#solo digitos
        cod <- NA_integer_
    }
    cod
}

#' Código departamento
#' @description devuelve código de departamento a partir del nombre
#'     del departamento
#' @param nom nombre del municipio o nombre del departamento
#' @param file nombre del archivo con data.frame del municipio; si no
#'     es indicado, se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return código de departamento si es código válido; si no, NA
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_dpto_nombre <- function(nom = character(), file = character(),
                            dfdpt = "ddpto"){

    assert_that(ok_chr(nom),
                msg = "código tipo caracter o entero")

    dd <- df_dpto(file, dfdpt)
    
    cod <- vapply(nom, cod_dpto_nom, 0, dd, USE.NAMES = FALSE)
    if (anyNA(cod)){
        warning("hay nombres no válidos\n")
    }
    cod
}

#' Departamento válido
#' @description valida código de departamento
#' @param cod código de departamento
#' @param file donde están los códigos oficiales. Si se omite,
#'     lo lee desde .Renviron
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
ok_dpto <- function(cod = integer(), file = character(),
                    dfdpt = "ddpto"){
    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod), msg = "código tipo entero")

    if (ii){
        cod <- as.integer(cod)
    }
    cod %in% cod_departamentos(file, dfdpt)
}

## --- miscelánea ---

#' mismo tipo
#' @description vectores del mismo tipo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo tipo
#' @author eddy castellón
p_type <- function(x, y){
    typeof(x) == typeof(y)
}

#' Alias p_type mismo tipo
#' @description vectores del mismo tipo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo tipo
#' @author eddy castellón
eq_type <- function(x, y){
    typeof(x) == typeof(y)
}

#' mismo modo
#' @description vectores del mismo modo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo modo
#' @author eddy castellón
p_mode <- function(x, y){
    mode(x) == mode(y)
}

#' Alias p_mode mismo modo
#' @description vectores del mismo modo?
#' @param x vector
#' @param y vector
#' @return TRUE si x e y son del mismo modo
#' @author eddy castellón
eq_mode <- function(x, y){
    mode(x) == mode(y)
}

#' Desagregar palabras
#' @description Desgrega las palabras (token) separadas por coma o
#'     espacios que se encuentran en una ristra de caracteres, y las
#'     devuelve en un vector. Evita el tedio de entrecomillar cada
#'     elemento de un vector.
#' @param str palabras encerradas por comillas separadas por coma o
#'     espacios
#' @return vector de caracters con las palabras individuales
#' @export
#' @examples
#' tok_str("aa bb,cc") -> c("aa", "bb", "cc")
#' @author eddy castellón
tok_str <- function(str){
    strsplit(str, split="[[:space:],]+")[[1L]]
}

## Character -> Character
## remueve extensión de nombre de archivo
## tools::file_path_sans_ext("ABCD.csv")
#' File extension
#' @description remueve la extensión de nombre de archivo
#' @param file nombre del archivo
sin_ext <- function(file){
    sub("\\..*$", "", file)
}

## VectorOfCharacter Character -> Character
## implementación recursiva (ÔjÔ ¿nest_char?, ¿bracket_chr?)
##' Anidar entre caracter
##' @description
##'     A una cadena de caracteres le agrega, al inicio y al final, un
##'     caracter, p.ej. para encerrar entre paréntesis. La
##'     implementación es recursiva cuando se aplica a un vector de
##'     caracteres
##' @param x vector tipo character
##' @param cc vector con los caracteres delimitadores; "()" por default
##' @return string
##' @export
##' @examples
##' nest_str(c("aa", "bb", "cc")) -> "aa(bb(cc))"
##' nest_str(c("aa", "bb"), c("[", "]")) -> "aa[bb]"
##' nest_str(c("", "bb"), c("[", "]")) -> "[bb]"
nest_str <- function(x, cc = c("(", ")")){
    if (length(x) == 1){
        x
    } else {
        paste0(x[1], cc[1], nest_str(x[-1], cc), cc[2])
    }
}

#' Nombre propio
#' @description pone en mayúsculas las primeras letras de las palabras
#' @param x palabra
#' @examples
#' a_propio('juan calero') -> 'Juan Calero'
#' @export
#' @importFrom assertthat assert_that
a_propio <- function(x = character()){
    assert_that(ok_chr(x),
                msg = "x no es tipo character")
    gsub("\\b([a-z])","\\U\\1", tolower(x), perl = TRUE)
}

## elimina los espacios al inicio y al final de x
## equivalente a str_trim de stringr
## Character -> Character
## str_podada('ja   ja') -> 'ja   ja'
## str_podada(' ja  ja ') -> 'ja  ja'

#' Poda espacios
#' @description quita los espacios antes y después de una frase
#' @param x frase
#' @export
#' @importFrom assertthat assert_that
str_podada <- function(x = character()){
    assert_that(ok_chr(x),
                msg="x no es tipo character")
    regmatches(x, regexpr("\\b.*\\b", x, perl = TRUE))
}

#' Poda espacios
#' @description quita los espacios antes y después de una frase
#' @param x frase
#' @export
#' @importFrom assertthat assert_that
podar_str <- function(x = character()){
    assert_that(ok_chr(x),
                msg="x no es tipo character")
    regmatches(x, regexpr("\\b.*\\b", x, perl = TRUE))
}

## sustituye dos o más caracteres tipo espacio (\n, \t, \s)
## por un espacio y poda extremos
## Character -> Character
## str_bien_formada("ja  ja ja ") -> "ja ja ja"

#' Espacios de más
#' @description elimina dos o más espacios consecutivos
#' @param x frase
#' @importFrom assertthat assert_that
str_bien_formada <- function(x = character()){
    assert_that(ok_chr(x),
                msg="x no es tipo character")
    str_podada(gsub("[[:space:]]+", " ", x))
}

#' Espacios de más
#' @description elimina dos o más espacios consecutivos
#' @param x frase
#' @export
#' @importFrom assertthat assert_that
sin_espacio_extra <- function(x = character()){
    assert_that(ok_chr(x),
                msg="x no es tipo character")
    podar_str(gsub("[[:space:]]+", " ", x))
}

## no requiere stringr

#' Sin tilde
#' @description sustituye letras con acento por las equivalentes sin
#'     acento
#' @param x palabra con o sin acentos
#' @export
#' @importFrom assertthat assert_that
str_sin_tilde <- function(x = character()){
    assert_that(ok_chr(x),
                msg = "x no es tipo character")
    vv <- c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
            "Á"="A", "É"="E", "Í"="I", "Ó"="O", "Ú"="U")
    while (any((mm <- regexpr("[ÁáÉéÍíÓóÚú]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
}

#' Caracter
#' @description es vector de caracteres con elementos
#' @param x vector
#' @export
ok_chr <- function(x){
    is.character(x) && length(x)
}

#' Número
#' @description es vector modo numérico y con elementos
#' @param x vector
#' @export
ok_num <- function(x){
    is.numeric(x) && length(x)
}

#' Entero
#' @description es vector tipo entero
#' @param x vector
#' @export
ok_int <- function(x){
    is.integer(x) && length(x)
}

#' Enteros-letra
#' @description es vector cuyos elementos son dígitos-alfanuméricos
#' @param x vector
#' @export
ok_int_chr <- function(x){
    ok_chr(x) && !any(grepl("[^0-9]", x))
}

#' Nombre
#' @description es vector alfanumérico con elementos cuyo primer
#'     caracter es letra
#' @param x nombre
#' @return TRUE si caracteres alfanuméricos empezados por letra
#' @export
ok_nombre <- function(x){
    ok_chr(x) && all(grepl("^[[:alpha:]]\\w*", x))
}

#' NA a cero
#' @description los elementos NA de un vector de modo numérico
#'     convertidos a 0
#' @param x vector tipo entero o doble
#' @export
na0 <- function(x){
    if (is.numeric(x)) {
        x[is.na(x)] <- ifelse(typeof(x) == "integer", 0L, 0.0)
    }
    invisible(x)
}

#' cero a NA
#' @description mapea los elementos igual a cero de un vector de modo
#'     numérico a NA
#' @param x vector tipo entero o doble
#' @export
cero_na <- function(x){
    if (is.numeric(x)){
        x[x == 0 & !is.na(x)] <- NA
    }
    invisible(x)
}

#' vector NA
#' @description crea vector con elementos iniciados a NA; el número de
#'     elementos es indicado explícitamente, o implícitamente pasando
#'     un vector con el mismo número de elementos
#' @param n número de elementos o vector con 'n' elementos
#' @param tipo el tipo de vector
#' @param toNA iniciar a NA? TRUE por omisión
#' @export
#' @author eddy castellón
vector_NA <- function(n = 1, tipo = "integer", toNA = TRUE){
    if (length(n) > 1){
        n <- length(n)
    }
    vv <- vector(tipo, n)
    
    if (toNA){
        is.na(vv) <- seq_along(vv)
    }
    invisible(vv)
}

#' buscar-remplazar
#' @description busca elementos de 'busca' entre los de 'buscaen' con
#'     la función match, y remplaza los correspondientes en 'x' con
#'     los de 'remplazo'. El número de elementos en 'x' debe ser igual
#'     al de 'busca', y el de 'buscaen' al de 'remplazo'; también
#'     deben ser del mismo modo 'x' y 'remplazo', y 'busca' y
#'     'buscaen'. x = NULL o missing crea vector inicializado a NA o 0
#'     según parámetro opcional toNA.
#' @param x vector
#' @param busca vector con los elementos a buscar
#' @param buscaen vector donde se buscan los elementos
#' @param remplazo vector con los elementos que remplazarán los
#'     correspondientes en 'x'
#' @param msg TRUE por omisión; FALSE suprime mensajes de advertencia
#' @param ... para indicar cómo iniciar x cuando x = NULL: toNA =
#'     FALSE inicia a 0
#' @export
#' @author eddy castellón
remplazar <- function(x = NULL, busca, buscaen, remplazo,
                      msg = TRUE, ...){
    if (missing(buscaen) || missing(remplazo) || missing(busca)){
        warning("no hubo remplazo ...\n")
    } else {
        ## inicializar con mismo tipo si falta
        if (is.null(x) || missing(x)) {
            x <- vector_NA(n = length(busca), tipo = typeof(remplazo),
                           ...)
        }
        assert_that(p_mode(x, remplazo),
                    p_mode(busca, buscaen),
                    length(x) == length(busca),
                    length(buscaen) == length(remplazo),
                    msg = "vectores no compatibles\n")
        mm <- match(busca, buscaen)
        ii <- is.na(mm)
        if (any(ii) && msg){
            warning("\n... ", sum(ii), " no calzan...")
        }
        ii <- !ii
        if (any(ii)){
            if (msg){
                message(sum(ii), " remplazos\n")
            }
            x[ii] <- remplazo[mm[ii]]
        }
    }
    invisible(x)
}

#' porcentaje
#' @description calcula el porcentaje con el que un dato contribuye al
#'     valor que se da como base
#' @param x datos
#' @param base la base del porcentaje; por omisión el total de los
#'     datos
#' @param dec el número de decimales con el que se devuelve el
#'     porcentaje; por omisión, igual a cero
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
pct <- function(x, base, dec = 0){
    if (missing(base)){
        base <- sum(x, na.rm = TRUE)
    } else {
        assert_that(length(base) == 1,
                    msg = "base debe ser única")
    }

    if (base == 0 | is.na(base)){
        pp <- NA_real_
        warning("base es igual a cero")
    } else {
        pp <- round(100 * x / base, dec)
    }
    pp
}

#' fracción
#' @description calcula el cociente y redondea
#' @param x numerador
#' @param y denominador
#' @param dec decimales en el resultado
#' @return cociente o NA si y = 0
#' @export
#' @author eddy castellón
frac <- function(x, y, dec = 2){
    assert_that(is.numeric(x), is.numeric(y),
                msg = "no son números")
    if (length(x) != length(y)) warning("\n!!! x, y diferente longitud")
    r <- vector_NA(length(x), "double")
    ii <- y != 0
    r[ii] <- round((x / y)[ii], dec)
    r
}

#' factor a caracter
#' @description transforma vector tipo factor a caracter
#' @param x vector tipo factor
#' @return invisible
#' @export
fac2char <- function(x){
    if (is.factor(x)){
        ww <- levels(x)[x]
    } else {
        message("argumento no es factor...")
        ww <- x
    }
    invisible(ww)
}

#' factor a entero
#' @description transforma vector tipo factor a enteros; a los
#'     elementos que no pueden convertirse los deja en NA
#' @param x vector tipo factor
#' @return invisible
#' @export
fac2int <- function(x){
    if (is.factor(x)){
        w_f <- type.convert(gsub("[^0-9]", NA_integer_, levels(x)[x]))
    } else{
        message("argumento no es factor...")
        w_f <- x
    }
    invisible(w_f)
}

#' match-alternativa
#' @description match con reporte de cuántos no hacen match
#' @param x como en match
#' @param y como en match
#' @param msg con mensaje? TRUE por defecto
#' @export
#' @author eddy castellón
parear <- function(x, y, msg = TRUE){
    assertthat::assert_that(mode(x) == mode(y),
                            msg = "modos diferentes")
    mm <- match(x, y, nomatch = NA, incomparables = NULL)
    if(msg && any(is.na(mm))){
        warning("sin pareja: ", sum(is.na(mm)), " de ", length(x))
    }
    mm
}

#' match-2
#' @description Hace match de las variables que resultan de
#'     \code{paste0(x1, x2)} y \code{paste0(y1, y2)}
#' @param x1 primera variable asociada a los registros
#' @param x2 segunda variable
#' @param y1 variable correspondiente a x1
#' @param y2 variable correspondiente a x2
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
match_2 <- function(x1, x2, y1, y2){
    assert_that((length(x1) == length(x2)),
                (length(y1) == length(y2)),
                msg = "vectores de tamaño distinto !!!\n")
    invisible(match(interaction(x1, x2, drop = TRUE),
                    interaction(y1, y2, drop = TRUE)))
}

#' Alias de operador in
#' @description remplazo de operador in
#' @param x vector busca
#' @param table vector donde buscar elementos de x
#' @return TRUE para cada elemento de x encontrado en table
#' @export
#' @author eddy castellón
en <- function(x, table){
    match(x, table, nomatch = 0) > 0
}

## DataFrame|List -> Integer
## produce el vector de índices de orden que
## generan las columnas cc del data.frame o lista df
#' ordenar
#' @description produce el vector de índices para ordenar las filas
#'     del data.frame o lista conforme a las columnas indicadas, o el
#'     orden original si estas no se indican
#' @param df data.frame
#' @param cc columnas (nombres o números que las identifican,
#'     utilizadas para generar el índice
#' @return vector de enteros
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
order_df <- function(df, cc){
    assert_that(is.list(df),
                msg = "data.frame o lista")
    ordenar <- TRUE
    if (missing(cc)){
        nn <- seq_along(df)
    } else if (is.numeric(cc)){
        nn <- as.integer(cc)
        assert_that(setequal(intersect(seq_along(df), nn),
                             nn),
                    msg = "columnas_orden no válidas")
    } else if (is.character(cc)){
        assert_that(setequal(cc, intersect(cc, names(df))),
                    msg="columnas_orden no válidas")
        nn <- match(cc, names(df))
    } else {
        ordenar <- FALSE
    }

    if (ordenar){
        nn <- do.call("order", as.list(unname(df[nn])))
    } else if (is.data.frame(df)){# en la secuencia original
        nn <- seq_len(nrow(df))
    } else {# si es lista
        nn <- seq_along(df)
    }
    nn
}

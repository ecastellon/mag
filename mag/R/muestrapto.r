##--- selección muestra de puntos ---
## v.0.0 20180508

## Character Character -> Character
## prop. devuelve nombre del archivo shape (sin extensión)
## de la cobertura del departamento dpt que se encuentra
## en la ruta dsn
## shp_dpt("masaya", "upms")
#' Shape list
#' @description devuelve el nombre del archivo shape de la cobertura
#'      del departamento. Asume que el nombre del archivo inicia con
#'      el nombre o las primeras letras del nombre del departamento.
#' @param dpt nombre del departamento
#' @param dsn ruta de acceso
#' @return nombre sin extensión
#' @importFrom assertthat assert_that
#' @export
shp_name <- function(dpt = character(), dsn = character()){
    assert_that(ok_nombre(dpt), ok_nombre(dsn),
                msg = "revisar parámetros")
    sh <- list.files(dsn, paste0("^", dpt, ".*\\.shp$"))
    assert_that(length(sh) > 0,
                msg="verificar nombre y ruta de acceso")
    if(length(sh) > 1){
        sh <- sh[1]
    }
    cat("shape:", sh, "!!!\n")
    sin_ext(basename(sh))
}

## Character Character -> SpatialDataFrame
## prop. leer file shape sh en la ruta dsn
#' Shape read
#' @description lee el archivo de una cobertura shape
#' @param shp nombre del archivo shape
#' @param dsn ruta de acceso al archivo
#' @return SpatialDataFrame
#' @export
#' @import rgdal
#' @importFrom assertthat assert_that
shp_read <- function(shp = character(), dsn = character()){
    assert_that(ok_nombre(shp), ok_nombre(dsn),
                msg = "revisar parámetros")
    if (grepl("\\.shp$", shp)){
        shp <- sin_ext(shp)
    }

    assert_that(file.exists(file.path(dsn, paste0(shp, ".shp"))))
    invisible(readOGR(dsn = dsn, layer = shp, stringsAsFactors = FALSE))
}

#' Department's blocks
#' @description extrae la cobertura de bloques de muestreo de un
#'     departamento, de la cobertura de bloques del país
#' @param dpt nombre (caracteres) o código (entero) del departamento
#' @param cob cobertura de bloques del país (SpatialPolygonsDataFrame)
#' @return la cobertura de bloques del departamento (invisible)
#' @export
#' @importFrom assertthat assert_that
shp_dpto <- function(dpt, cob){
    assert_that((!missing(cob)) && is_spatial_poly(cob),
                msg = "cob no es cobertura de polígonos")
    if (is.character(dpt)){
        dpt <- cod_dpto_nombre(dpt)
    }
    assert_that(is.numeric(dpt),
                dpt %in% cob@data$dpto,
                msg = "revisar nombre del departamento")
    invisible(cob[cob@data$dpto == dpt,])
}

#' Shape save
#' @description guarda una cobertura en archivo shape
#' @param spd nombre del SpatialDataFrame
#' @param capa nombre del archivo shape
#' @param dsn ruta de acceso; directorio de trabajo por omisión
#' @export
#' @import rgdal
#' @importFrom assertthat assert_that
shp_save <- function(spd, capa = character(), dsn = getwd()){
    assert_that(is_spatial(spd), ok_nombre(capa), file.exists(dsn),
                msg = "revisar ruta o nombre de archivo")
    capa <- sin_ext(basename(capa))
    writeOGR(spd, dsn = dsn, layer = capa,
             driver="ESRI Shapefile",
             check_exists = TRUE, overwrite_layer = TRUE)
}

## Character -> DataFrame
#' Shape data
#' @description lee los datos asociados a la cobertura shape
#' @param shp nombre del arhcivo shape
#' @param dsn ruta de acceso
#' @return data.frame
#' @export
#' @import foreign
shp_data <- function(shp = character(), dsn = character()){
    assert_that(ok_nombre(shp), ok_nombre(dsn),
                msg = "revisar parámetros")

    if(!grepl("\\.dbf$", shp)){
        shp <- paste0(shp, ".dbf")
    }
    sh <- file.path(dsn, shp)
    assert_that(file.exists(sh),
                msg = "archivo de características no existe")
    invisible(read.dbf(sh, as.is = TRUE))
}

## Character -> Character
## lista de atributos del shape
#' Shape features
#' @description devuelve los nombres de las columnas (variables) de la
#'      tabla de atributos de la cobertura
#' @param shp nombre del archivo shape
#' @param dsn ruta de acceso
#' @return NULL si error de lectura del archivo de datos
#' @export
shp_atributos <- function(shp, dsn){
    ww <- shp_data(shp, dsn)
    if (is.data.frame(ww)){
        x <- names(ww)
    } else {
        x <- NA_character_
    }
    x
}

## cols: columnas en cob que pasan en cobertura de salida
## fmtp: formato para el número de punto
#' Sampling points
#' @description muestreo espacial de puntos
#' @param cob cobertura: objeto SpatialPolygonsDataFrame
#' @param cols columnas ('features') en el data.frame asociado a la
#'      cobertura, que pasan a la cobertura de salida
#' @param fmtp formato para dar nombre a los puntos; '%02i%03i' por
#'      defecto: código de departamento y ordinal dentro de
#'      departamento
#' @return objeto SpatialPointsDataFrame (invisible) con las
#'      coordenadas de los puntos seleccionados
#' @export
#' @import sp
#' @importFrom maptools dotsInPolys
#' @importFrom assertthat assert_that
sample_points <- function(cob, cols = c("codigo", "estrato"),
                               fmtp = "%02i%03i"){
    assert_that(inherits(cob, "SpatialPolygonsDataFrame"),
                msg = "cob no es SpatialPolygonsDataFrame")
    ww <- cob@data
    assert_that(all(tolower(cols) %in% tolower(names(ww))),
                msg = "revisar nombre de columnas")
    pd <- dotsInPolys(cob, ww$puntos)
    names(pd@data) <- tolower(names(pd@data))
    mm <- match(pd@data$id, rownames(ww))
    if(anyNA(mm))
        stop("no-corresponde-puntos-seleccionado-bloques")
    ##ss <- c("codigo", "estrato", "bloque")
    dp <- cbind(pd@data, ww[mm, cols])
    oo <- order(dp$codigo)
    dp[oo, "cpunto"] <- sprintf(fmtp, ww$dpto[1],
                                seq_along(oo))
    pd@data <- dp
    pd@proj4string <- CRS(proj4string(cob))
    invisible(pd)
}

#' Replicated points
#' @description crear indicador de réplica (submuestra)
#' @param x data.frame con los datos de la cobertura de puntos
#' @param nrep número de réplicas
#' @param colrep columna con identificación del estrato
#' @param orden columnas que se van a utilizar para ordenar la
#'     asignación de las réplicas
#' @return vector con los números de réplicas asignadas
#' @export
replica <- function(x, nrep = 5,
                    colrep = "estrato", orden = NULL){

    assert_that(is.data.frame(x),
                msg = "no es data frame")
    assert_that(nrow(x) == nrep * (nrow(x) %/% nrep),
                msg = "muestra no es múltiplo de réplicas")

    if(is.null(orden)){#ordena por todas las columnas
        orden <- length(x)
    }
    id <- seq_len(nrow(x))
    nn <- order_df(x[orden])
    xx <- cbind(x[nn, orden, drop = FALSE], id = id[nn])

    zz <- Reduce(rbind,
                 lapply(split(xx, xx[,colrep]),
                        function(x){
                            st <- nrow(x) %/% nrep
                            ni <- sapply(sample(seq_len(nrep), nrep), seq,
                                         by = nrep, length.out = st)
                            dim(ni) <- c(nrow(x), 1)
                            cbind(x[ni,], replica = rep(seq_len(nrep),
                                                        each = st))
                        }))
    mm <- match(id, zz$id)
    invisible(zz$replica[mm])
}

#' Spatial class
#' @description es objeto de clase spatial (librería sp)
#' @param obs objeto
is_spatial <- function(obs){
    inherits(obs, "Spatial", FALSE)
}

#' SpatialPolygon class
#' @description es objeto SpatialPolygonsDataFrame
#' @param obs objeto
is_spatial_poly <- function(obs){
    inherits(obs, "SpatialPolygonsDataFrame", FALSE)
}

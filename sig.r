## funciones para administrar los puntos de muestreo
## y la cartografía relacionada
p_fun <- function(){
    library(assertthat)
    library(foreign)
}

## agrega columna a un d.f
## inicializa a 0 si numeric
## "" si alfanumérico
df_add_col <- function(df, nom, tipo="integer"){
    assert_that(!nom %in% names(df))
    df[nom] <- ifelse(tipo=="character", character(nrow(df)),
                      integer(nrow(df)))
    invisible(df)
}

## cambia algunos datos en una columna de d.f
## df: data.frame
## col_busca: con la que se hace el match
## col_cambia: donde se harán los cambios
## val_busca: buscados en col_busca
## val_new: nuevos datos
df_cambia_datos <- function(df, col_busca, col_cambia,
                            val_busca, val_new){
    assert_that(!is.na(pmatch(typeof(df[, col_busca]),
                              typeof(val_busca)) &
                       pmatch(typeof(df[, col_cambia]),
                              typeof(val_new))))
    mm <- match(val_busca, df[, col_busca])
    if(anyNA(mm)){
        df <- do.call("[", list(val_busca, is.na(mm)))
        message("\n!!! sin cambios: ", sum(is.na(mm)),
                " buscados no se encuentran en ", col_busca)
    } else df[mm, col_cambia] <- val_new
    invisible(df)
}

## modifica o agrega un atributo
## a la tabla de atributos de la cobertura
## dbf_cob: tabla dbf de atributos de la cobertura
## (esto es para ARCGIS cambiará para GRASS)
## col_bus: nombre columna para hacer match con val_bus
## col_atr: columna donde cambiar val_atr
## val_bus: los puntos para los que se cambia atributo
## val_atr: nuevo valor de atributos
## write_cob: guarda actualización
p_cambia_atr_cob <- function(dbf_cob, col_bus, col_atr,
                             val_bus, val_atr, write_cob=FALSE){
    assert_that(all(file.exists(dbf_cob),
                length(val_bus) == length(val_atr)))
    ww <- try(read.dbf(dbf_cob, as.is=TRUE))
    assert_that(!is.na(pmatch(typeof(val_bus), typeof(ww[, col_bus]))))
    
    if(col_atr %in% names(ww)){
        assert_that(!is.na(pmatch(typeof(ww[, col_atr]),
                                     typeof(val_atr))))
    } else ww <- df_add_col(ww, col_atr, typeof(val_atr))

    ## si hay error devuelve vector
    ww <- df_cambia_datos(ww, col_bus, col_atr, val_bus, val_atr)
    if(is.data.frame(ww) & write_cob) write.dbf(ww, dbf_cob)
    invisible(ww)
}

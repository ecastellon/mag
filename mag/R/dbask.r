# -*- coding: utf-8 -*-
## --- funciones para consultar base de datos ---
#' @export
set_odb <- function(ob, ...) UseMethod("set_odb")
#' @export
get_data <- function(ob,...) UseMethod("get_data")
#' @export
get_atr <- function(x, atr) UseMethod("get_atr")
#' @export
db_constr <- function(ob) UseMethod("db_constr")
#' @export
db_open <- function(ob) UseMethod("db_open")
#' @export
db_info <- function(ob) UseMethod("db_info")
#' @export
db_tablas <- function(ob, msg) UseMethod("db_tablas")
#' @export
db_columnas <- function(ob, tabla) UseMethod("db_columnas")
#' @export
db_qry <- function(ob, xs, ...) UseMethod("db_qry")
#' @export
db_fetch <- function(ob, tabla, ...) UseMethod("db_fetch")
#' @export
db_save <- function(ob, df, tb, nr, ...) UseMethod("db_save")
#' @export
db_drop <- function(ob, tb) UseMethod("db_drop")
#' @export
tabla_exists <- function(ob, tb) UseMethod("tabla_exists")
#' @export
is_open <- function(ob) UseMethod("is_open")

## ListOfListOfCharacter,Character,Character,Character Character ->
## Character
#' expresion SQL
##' @description produce expresión SQL a partir de listas con
##'     estructura list(a=c(TABLA, alias), k=c(campos), as=c(alias de
##'     campos)) y, si se requiere, clausulas where, inner join,
##'     left(right) join, order by y group by. Si es un join, se
##'     requieren dos tablas y la cláusula on completada por whr
##'     (where). El tipo de join se puede indicar sólo con las
##'     primeras letras; e.g in(nner)
##' @param lak lista con vectores tabla-alias, campos de la tabla y
##'     nombres de columnas del data.frame que resulte de la consulta
##' @param whr cláusula where
##' @param ord cláusula order by
##' @param gby cláusula group by
##' @param joi cláusula join
##' @return expresión de consulta SQL
##' @examples
##' xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
##'           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
##'      whr="a.c1=b.c1")
##'   select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a,prib b
##'       where a.c1=b.c1
##'
##' xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
##'           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
##'      whr="a.c1=b.c1", joi="le")
##'   select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a left
##'       join prib b on a.c1=b.c1
##'
##' xsql(list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a", "b")))
##'   select b.c1 as a,b.c2 as b from prib b
##' @export
##' @import magrittr
##' @importFrom assertthat assert_that
xsql <- function(lak = NULL, whr = "", ord = "", gby = "", joi = ""){
    assert_that(!is.null(lak))
    if (!is.list(lak[[1]])) lak <- list(lak)
    una_t <- length(lak) == 1
    ## más de una tabla es un join
    assert_that((nzchar(whr) || nzchar(joi)) == !una_t,
                msg="where o join si más de una tabla")
    joins <- c("left join", "inner join", "right join")
    if(nzchar(joi)){
        joi <- pmatch(joi, joins)
        assert_that(!is.na(joi), nzchar(whr),
                    length(lak) == 2,
                    msg = "join correcto entre 2 tablas")
    }
        
    ## lista de campos, alias y tablas correctos
    assert_that(
        all(vapply(lak, function(x)length(x[["k"]]) > 0,
                   TRUE)),
        all(vapply(lak, function(x)length(x[["a"]]) == 2,
                   TRUE)),
        all(vapply(lak, function(x) is.null(x[["as"]]) |
                                    length(x[["k"]]) ==
                                    length(x[["as"]]), TRUE)))
    sq <- vapply(lak,
                 function(x){
                     if (!is.null(x[["as"]])){
                         x[["k"]] <- paste(x[["k"]], x[["as"]],
                                           sep=" as ")
                     }
                     sp <- ifelse(nzchar(x[["a"]][2]), ".", "")
                     paste(x[["a"]][2], x[["k"]], sep=sp,
                           collapse=",")
                 }, "", USE.NAMES=FALSE) %>%
        paste(collapse=",") %>% paste("select", ., "from",
                                      paste(lak[[1]][["a"]],
                                            collapse=" "))
    ## join
    if(!una_t){
        if(nzchar(whr)){
            sq <- vapply(lak[-1], function(y)paste(y[["a"]], collapse=" "),
                         "", USE.NAMES=FALSE) %>% paste(collapse=",") %>%
                paste(sq, ., sep=",") %>%
                paste("where", whr)
        } else{
            if(!is.na(joi)){
                sq <- paste(sq, joins[joi],
                            paste(lak[[2]][["a"]], collapse=" "),
                            "on", whr)
            }
        }
    }
  
    ## TODO validar expresiones
    if(nzchar(ord)) sq <- paste(sq, "order by", ord)
    if(nzchar(gby)) sq <- paste(sq, "group by", gby)
    sq
}

## Character, VectorOfCharacter, VectorOfCharacter -> Character
##' Versión simplificada de xsql
##' @description devuelve expresión SQL a partir de TABLA, CAMPOS y
##'     ALIAS de campos
##' @param tabla nombre de tabla (entre comillas)
##' @param campos vector de caracteres con nombres de campos
##' @param as vector de caracteres con los alias de campos
##' @examples
##' xsql_s("pri", c("a","b"), c("x", "y"))
##'   "select a.a as x,a.b as y from pri a"
##' xsql_s("pri", c("a","b")) "select a.a,a.b from pri a"
##' @export
##' @importFrom assertthat assert_that
xsql_s <- function(tabla, campos, as = NULL, alias){

    assert_that(!(missing(tabla) || missing(campos)),
                ok_nombre(tabla),
                is.character(campos) && all(nzchar(campos)),
                msg = "cuáles campos de cuál tabla?")

    if (!is.null(as)){
        assert_that(is.character(as) && all(nzchar(as)) &&
                    length(campos) == length(as),
                    msg = "algún nombre de campo no es válido\n")
    }

    if (missing(alias)){
        alias <- ""
    } else {
        assert_that(ok_nombre(alias),
                    msg = "alias de tabla no es válido\n")
    }
    xsql(list(a=c(tabla, alias), k=campos, as=as))
}

##' Parámetro lista de \code{xsql}
##' @description facilita construir lista para llamar a \code{xsql}
##' @param db nombre de tabla (entre comillas)
##' @param km vector de caracteres con nombre de campos
##' @param al nombre de alias de la tabla
##' @param as vector de caracteres con nombre de columnas
##' @return lista tabla-campos apta para llamar a función \code{xsql}
##' @export
##' @importFrom assertthat assert_that
lxs <- function(db, km, al="a", as=NULL){
    if(!is.null(as)){
        assertthat::assert_that(is.character(as),
                                length(km) == length(as))
    }
    list(list(a=c(db, al), k=km, as=as))
}

## VectorOfCharacter -> character ' SQL union
##' SQL union
##' @description Construye la expresión \code{union} de dos o más
##'     expresiones SQL. \code{union} incluye sólo una vez cada
##'     registro en el resultado de la consulta; \code{union all} los
##'     incluye a todos. Para identificar los registros que resulten
##'     de mandar a ejecutar las subconsultas, a estas se le puede
##'     agregar un campo que devuelva una "constante". En el
##'     resultado, la columna tendrá el nombre \code{nomlab}.
##' @param x vector de caracteres cuyos elementos son expresiones SQL
##' @param all \code{union all}?; TRUE por defecto
##' @param nomcol nombre de la columna si se quiere identificar las
##'     subconsultas
#' @param idcon vector de caracteres o de enteros que servirán para
#'     etiquetar las subconsultas
##' @return expresión SQL
##' @export
##' @importFrom assertthat assert_that
##' @examples
#' xsql_u(c("xsqla", "xsqlb", "xsqlc")) ->
#'        "xsqla union all (xsqlb union all (xsqlc))"
xsql_u <- function(x, all = TRUE, nomcol = character(),
                   idcon = character()){
    nn <- length(x)
    assert_that(is.character(x),  nn > 1,
                msg = "más de una expresión SQL")

    ## nombra las subconsultas para auxiliar la identificación de los
    ## registros resultantes de cada una
    if (length(nomcol)){
        assert_that(ok_nombre(nomcol),
                    msg = "falta nombre de índices de consulta")
        if (!length(idcon)){
            idcon <- seq_len(nn)
        } else {
            assert_that(length(idcon) == nn,
                        msg = "número de nombres y de subconsultas")
            ## para darles nombres
            if (is.character(idcon)){
                idcon <- sQuote(idcon)
            }
        }
        ## se agregan a la expresión como constantes
        x <- vapply(seq_along(x),
                     function(z){
                         cc <- paste0("\\1,", nomcol, "=",
                                      idcon[z], "\\2")
                         sub("(.+)(\\s+from.+)", cc, x[z], perl = TRUE)
                     },
                     "a", USE.NAMES = FALSE)
    }

    nest_str(c(paste(x[-nn],
                    paste0("union", ifelse(all, " all", ""))),
              x[nn]))
}

#' Clase S3 odb
#' @description Crea e inicializa un objeto de clase odb. Con las
#'     propiedades de este objeto se construye la cadena de conexión
#'     ('connection string') que permite 'conectarse' o 'abrir' la
#'     base de datos para que acepte las instrucciones de consulta.
#'     
#'     Para cada servidor de base de datos (SQL server, Excel, Access,
#'     MySQL, SQLite, etc.) hay una 'connection string' específica
#'     según la aplicación que sirve de intermediario con el servidor.
#'
#'     Con las librería RODBC y RODBCDBI se usan 'driver' o
#'     controladores ODBC, administrados por la aplicación
#'     (\url{c:/windows/system32/odbcad32.exe}) administrador de datos
#'     ODBC. El menú 'controladores' muestra los controladores ODBC
#'     instalados en el equipo. Si no aparece el indicado hay que
#'     descargar e instalar el 'software' que lo instala.
#'
#'     En \url{https://www.connectionstrings.com} y otras similares,
#'     están descritas las 'connection string' más comunes. Estas
#'     llevan uno o más parámetros junto con el valor que les
#'     corresponde. Por ejemplo, la conexión ODBC mínima a libros
#'     Excel versión 2007 requiere los parámetros 'Driver' y 'Dbq': el
#'     primero para indicar el controlador y el segundo para la ruta
#'     de acceso al libro:
#'     'Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)}; DBQ=\url{c:/MyExcel.xlsx};'. En
#'     este caso, 'Driver' y 'Dbq' son las propiedades se deben
#'     especificar por medio de un objeto de la clase odb.
#' @param ... captura uno o más de los parámetros necesarios para
#'     especificar la 'connection string' Si no se indica ninguno el
#'     resultado será una lista sin elementos, que después se podrá
#'     llenar con la función 'setter' \code{set_odb}.
#' @return objeto de clase odb
#' @seealso \code{odb_sql}, \code{odb_xcl}
#' @export
odb <- function(...){
    x <- list(...)
    class(x) <- "odb"
    invisible(x)
}

#' Clase odb
#' @description es objeto clase odb?
#' @param x objeto
#' @export
is_odb <- function(x){
    inherits(x, "odb")
}

#' print.odb
#' @description ver los datos en objeto odb. Si hay palabra clave se
#'     oculta.
#' @param x objeto odb
#' @export
print.odb <- function(x){
    xx <- x
    if (is.element("pwd", names(xx))){
        xx["pwd"] <- strrep("*", nchar(xx$pwd))
    }
    class(xx) <- "list"
    print(xx)
}

#' str.odb
#' @description ver la estructura de objeto odb. Si hay palabra clave,
#'     se oculta
#' @param x objeto odb
#' @export
str.odb <- function(x){
    print(x)
}

#' Modificar propiedades de objeto odb
#' @description modifica o agrega una o más propiedades a un objeto
#'     odb. Las propiedades previamente definidas son actualizadas a
#'     los nuevos valores.
#' @param ob objeto odb
#' @param ... nuevas propiedades o modificaciones a las ya definidas
#' @return objeto odb
#' @examples
#' ob <- odb(server = "{SQL Server}", database = "db")
#' ## agrega user y modifica database
#' ob <- set_odb(ob, database = "ndb", user = "me")
#' @export
set_odb.odb <- function(ob, ...){
    cc <- names(ob)
    xx <- list(...)
    zz <- c(ob[setdiff(cc, names(xx))], list(...))
    class(zz) <- class(ob)
    invisible(zz)
}

#' get property
#' @description devuelve una propiedad
#' @param x objeto odb
#' @param atr atributo o propiedad
#' @export
get_atr.odb <- function(x, atr){
    ifelse(is.element(atr, names(x)), x[[atr]], NA_character_)
}

#' odb SQL Server
#' @description inicializa objeto de clase odb para conectar con un
#'     servidor que corre SQL Server
#' @param driver "{SQL Server}" por defecto.
#' @param server dirección IP del servidor
#' @param database nombre de la base de datos
#' @param uid nombre del usuario
#' @param pwd contraseña del usuario
#' @return objeto de clase odb
#' @export
db_sql <- function(driver = "{SQL Server}", server = NULL,
                   database = NULL, uid = NULL, pwd = NULL){
    if (is.null(uid)){
        uid <- Sys.getenv("SQLUID")
    }
    if (is.null(pwd)){
        pwd <- Sys.getenv("SQLPWD")
    }

    assert_that(nzchar(uid), nzchar(pwd),
                msg = "falta clave o nombre para tener acceso")
    
    ob <- odb(driver = driver, server = server, database = database,
              uid = uid, pwd = pwd)
    ## validar
    nm <- names(ob)
    for (ss in c("driver", "server", "database", "uid", "pwd")){
        if (!is.element(ss, nm)){
            warning(sprintf("%s no indicado\n", ss))
        }
    }
    invisible(ob)
}

#' odb excel
#' @description inicializa objeto de clase odb para conectar con un
#'     libro excel.
#' @param file ruta de acceso al archivo
#' @param version7 versión 2007 de excel?. TRUE por defecto.
#' @param ronly acceso de sólo lectura?. TRUE por defecto.
#' @return objeto de clase odb
#' @importFrom assertthat assert_that
#' @export
db_xcl <- function(file, version7 = TRUE, ronly = TRUE){

    ff <- tryCatch(normalizePath(file, mustWork = TRUE),
                   error = function(e){
                       stop("\n...archivo no existe!!")})

    dr <- "{Microsoft Excel Driver (*.xls"
    id <- "790"
    if (version7){
        dr <- paste0(dr, ", *.xlsx, *.xlsm, *.xlsb")
        id <- NULL
    }
    dr <- paste0(dr, ")}")

    invisible(odb(Driver = dr, DriverId = id, Dbq = ff,
                  DefaultDir = normalizePath(dirname(ff)),
                  ReadOnly = ronly))
}

#' Connection string
#' @description construye la 'connection string' para tener acceso a
#'     la base de datos, usando las propiedades de un objeto odb
#' @param ob objeto de clase odb
#' @return connection string
#' @export
db_constr.odb <- function(ob){
    cc <- c(ob, recursive = TRUE)
    paste0(paste0(names(cc), "="), cc, collapse=";")
}

#' Conectar a servidor
#' @description llama la función \code{odbcDriverConnect} de RODBC
#'     para conectarse a la base de datos
#' @param ob objeto odb apropiado al driver
#' @return objeto RODBC para tener acceso a la base de datos; NULL
#'     si no se pudo hacer la conexión
#' @seealso \code{is_rodbc}
#' @export
#' @import RODBC
db_open.odb <- function(ob){
    constr <- db_constr(ob)
    invisible(tryCatch(
        odbcDriverConnect(connection=constr,
                          tabQuote = c("[", "]")),
        error = function(e){
            message("\n...ERROR", constr)
        },
        warning = function(e) message("\n...ADVERTENCIA de conexión")))
}

#' Validar RODBC
#' @description es objeto RODBC?. Para validar si la conexión mediante
#'     RODBC es válida
#' @param ob objeto RODBC
#' @export
is_rodbc <- function(x){
    inherits(x, "RODBC")
}

#' Canal abierto
#' @description objeto RODBC en uso?
#' @param ob objeto de conexión RODBC
#' @export
is_open.RODBC <- function(rob){
    !inherits(db_info(rob), "try-error")
}

#' database info
#' @description información básica sobre la base de datos y del
#'     "driver"
#' @param ob objeto RODBC
#' @return vector tipo "character"
#' @import RODBC
db_info.RODBC <- function(ob){
    ww <- tryCatch(odbcGetInfo(ob),
                   error = function(e) e)
    invisible(ww)
}

#' Alias de \code{sqlTables} de RODBC
#' @description simplifica el resultado de \code{sqlTables} que
#'     devuelve los nombres de las tablas de una base de datos
#' @param ob objeto RODBC para conectar con el servidor de datos
#' @param msg cuántas tablas? TRUE por defecto
#' @return nombres de las tablas en la base de datos o NULL si error
#'     durante la consulta
#' @export
#' @import RODBC
db_tablas.RODBC <- function(ob, msg = TRUE){
    xx <- tryCatch(sqlTables(ob, tableType = "TABLE"),
                   error = function(e){
                       message("\n!!! ERROR")
                       NULL})
    if (!is.null(xx)){
        xx <- xx[["TABLE_NAME"]]
        if (msg){
            message("\n tablas: ", length(xx))
        }
    }
    xx
}

#'Tabla existe
#' @description existe tabla en base de datos?
#' @param ob objeto RODBC
#' @param tb nombre de tabla
#' @export
tabla_exists.RODBC <- function(ob, tb){
    is.element(tolower(tb), tolower(db_tablas(ob)))
}

#' Alias de \code{sqlColumns} de RODBC
#' @description simplifica el resultado de \code{sqlColumns} para
#'     devolver los nombres de las columnas de una tabla
#' @param ob objeto RODBC para conectar con el servidor de datos
#' @param tt nombre de la tabla en la base de datos
#' @return nombre de las columnas en la tabla o NULL si error
#' @export
#' @import RODBC
db_columnas.RODBC <- function(ob, tt){
    xx <- tryCatch(sqlColumns(kk, tt),
                   error = function(e){
                       message("\n!!! ERROR")
                       NULL})
    if (!is.null(xx)){
        xx <- xx[["COLUMN_NAME"]]
    }
    xx
}

#' Ejecutar consulta
#' @description captura los errores y el resultado de consultar la
#'     base de datos con la función \code{sqlQuery} de RODBC
#' @param ob objeto RODBC para conectar con el servidor de datos
#' @param xs expresión SQL
#' @param strfac cadenas de caracteres como factor? FALSE por defecto
#' @param max número máximo de registros por leer; 0, sin límite, por
#'     defecto
#' @param ... otros parámetros de \code{sqlGetResults}
#' @return data.frame (invisible) con el resultado de la consulta, o
#'     mensaje de error si no se pudo hacer
#' @export
#' @import RODBC
db_qry.RODBC <- function(ob, xs, strfac = FALSE, max = 0, ...){
    ww <- tryCatch(
        sqlQuery(ob, xs, errors = TRUE, stringsAsFactors = strfac,
                 max = max, ...),
        error = function(e) e,
        message = function(e)e,
        warning = function(e)e)

    if (!is.data.frame(ww)){
        message("\n!!!...error durante lectura")
    }
    invisible(ww)
}

#' Leer tabla
#' @description captura los errores y el resultado de leer
#'     todos los campos de una tabla con \code{sqlFetch} de RODBC
#' @param ob objeto RODBC para conectar con el servidor de datos
#' @param tb nombre de la tabla
#' @param strfac cadenas de caracteres como factor? FALSE por defecto
#' @param max número máximo de registros por leer; 0, sin límite, por
#'     defecto
#' @param ... otros parámetros de \code{sqlGetResults} o \code{sqlQuery}
#' @return data.frame (invisible) con el resultado de la consulta, o
#'     mensaje de error si no se pudo hacer
#' @export
#' @import RODBC
db_fetch.RODBC <- function(ob, tabla, strfac = FALSE, max = 0, ...){
    ww <- tryCatch(sqlFetch(ob, tabla, stringsAsFactors = strfac,
                            max = max, ...),
                   error=function(e) e)
    
    if (!is.data.frame(ww)){
        message("\n!!!...error durante lectura")
    }
    invisible(ww)
}

#' Guardar tabla
#' @description guarda un data frame como una tabla de la base de
#'     datos, mediante \code{sqlSave} de RODBC
#' @param ob ojeto RODBC
#' @param df nombre del data frame
#' @param tb nombre de la tabla
#' @param rn agrega nombre de filas a la tabla? FALSE por defecto
#' @param ... adicionales para sqlSave
#' @export
db_save.RODBC <- function(ob, df, tb, rn = FALSE, ...){
    ww <- tryCatch(sqlSave(ob, df, tablename=tb, rownames=rn, ...),
                   error = function(e) e)
    if (inherits(ww, "try-error")){
        message("\n!!!... ERROR")
    }
    invisible(ww)
}

#' Eliminar tabla
#' @description elimina una tabla de la base de datos mediante
#'     \code{sqlDrop}
#' @param ob objeto RODBC
#' @param tb nombre de la tabla
#' @export
db_drop.RODBC <- function(ob, tb){
    ww <- tryCatch(sqlDrop(ob, df),
                   error = function(e) e)
    if (inherits(ww, "try-error")){
        message("\n!!!... ERROR")
    }
    invisible(ww)
}

#'save-excel
#' @description un data.frame es convertido a una tabla excel
#' @param df el data.frame
#' @param tabla nombre de la tabla excel
#' @param file nombre del archivo excel
#' @param xv7 versión 7 de excel? TRUE por defecto
#' @export
#' @author eddy castellón
save_xcl <- function(df, tabla, file, xv7 = TRUE){
    oo <- db_xcl(file, version7 = xv7, ronly = FALSE)
    kk <- db_open(oo)
    if (is_rodbc(kk)){
        db_save(kk, df, tabla)
        odbcClose(kk)
    } else {
        message("error conexión\n")
    }
}

#'read-excel
#' @description leer tabla o rango con nombre de excel por medio de
#'     RODBC
#' @param x nombre del rango
#' @param file nombre del archivo
#' @param xv7 versión 7 de excel? TRUE por defecto
#' @return data.frame o NULL si error durante la lectura
#' @export
#' @author eddy castellón
read_xcl <- function(x, file, xv7 = TRUE){
    oo <- db_xcl(file, version7 = xv7, ronly = FALSE)
    kk <- db_open(oo)
    if (is_rodbc(kk)){
        uu <- db_fetch(kk, x)
        odbcClose(kk)
    } else {
        uu <- NULL
    }
    invisible(uu)
}

## --- códigos y consultas base datos encuestas ---
#' fabrica función código
#' @description produce función para generar códigos
#' @param nn parámetro para la parte entera del código
#' @param di número de dígitos que componen el código; 3 por defecto
#' @return función
#' @export
#' @examples
#' ff <- codigo_fac(di = 4)
#' ff(4) -> "c0004"
#' ff(c(20, 100)) -> c("c0020", "c0100")
codigo_fac <- function(nn, di = 3){
    function(nn) sprintf(paste0("%s%0", di, "i"), "c", nn)
}

#' construye código
#' @description produce código
#' @param nn parte entera del código
#' @param di número de dígitos en el código; 3 por defecto
#' @export
#' @examples
#' codigo(100, 4) -> "c0100"
codigo <- function(nn, di = 3){
    sprintf(paste0("%s%0", di, "i"), "c", nn)
}

#' matriz de códigos
#' @description construye una matriz de códigos a partir de un vector
#'     de códigos, o a partir de un vector de enteros si la función
#'     recibe la función que los construye.
#' @param nn vector de enteros o de caracteres (códigos)
#' @param nomcol nombre de las columnas de la matriz
#' @param nomfi nombre de las filas
#' @param por_fila la matriz se llenará¡ por fila por fila? TRUE por
#'     defecto
#' @param cod función que generará los códigos si el parámetro nn es
#'     un vector de enteros
#' @return matriz de códigos
#' @export
#' @examples
#' ff <- codigo_fac(di = 3)
#' xx <- matriz_cod(1:6, nomcol = c("x", "y", "z"), cod = ff)
#' @importFrom assertthat assert_that
matrix_cod <- function(nn, nomcol = NULL,
                       nomfi = NULL, por_fila = TRUE, cod = NULL){
    assert_that((is.numeric(nn) && is.function(cod)) ||
                is.character(nn),
                msg = "si entero falta funcion de codigos")

    if (is.numeric(nn)){
        nn <- cod(as.integer(nn))
    }
    
    ncol <- length(nomcol)
    if (!length(ncol)){
        ncol <- length(nn)
    }

    ## if (!is.null(nomcol)){
    ##     assert_that(length(nomcol) == ncol,
    ##                 msg = "columnas inconsistente con nombres")
    ## }

    matrix(nn, ncol = ncol, byrow = por_fila,
           dimnames = list(nomfi, nomcol))
}

#' Expresion SQL encuestas MAG
#' @description leer cuadros con estructura de códigos
#' 
#'     cxx1 cxx2 cxx3 cxx4
#' 
#'     cxx5 cxx6 cxx7 cxx8
#' 
#'     .... .... .... ....
#' 
#'     o
#' 
#'     cxx1 cxx3 cxx5 cxx7
#' 
#'     cxx2 cxx4 cxx6 cxx8
#' 
#'     donde las columnas son las variables
#'
#'     Cada fila del cuadro corresponde a una consulta SQL, las que
#'     son combinadas en una sola mediante la cláusula 'union
#'     all'. A cada consulta se le agrega un campo 'constante' que
#'     indica a cuál fila del cuadro corresponde, lo que permitirá,
#'     después de mandar a ejecutar la consulta, identificar los
#'     registros extraídos por cada subconsulta.
#' @param nn vector de enteros o de códigos; si es tipo entero, debe
#'     incluirse el parámetro con la función que construye códigos
#' @param tab nombre de la tabla que se va a consultar
#' @param nomvar nombre de las variables
#' @param idr nombre del campo de la tabla con el nombre de los
#'     registros
#' @param nomidr nombre de la columna que traerá los nombres de
#'     registros de donde se extrae la consulta
#' @param uall union all? TRUE por defecto
#' @param nomcol nombre de la columna que llevará la etiqueta de
#'     consulta
#' @param idcon nombres que identifican los registros de cada
#'     consulta; por omisión, los enteros de 1 hasta el número de
#'     consultas
#' @param por_fila secuencia de códigos por fila? TRUE por defecto
#' @param ... parámetros adicionales para matrix_cod
#' @return expresión SQL
#' @export
#' @importFrom assertthat assert_that
xsql_t <- function(nn, tabla, nomvar = character(),
                   idr = NULL, nomidr = "quest", uall = TRUE,
                   nomcol = NULL, idcon = NULL, ...){

    assert_that(!missing(tabla), ok_nombre(tabla),
                ok_chr(nomvar) && all(nzchar(nomvar)),
                msg = "falta nombre de tabla o de variables")

    nc <- length(nn)
    nvar <- length(nomvar)
    ##nvar <- ifelse(!nv, nc, nv)

    nr <- nc %/% nvar
    assert_that(nc == nvar * nr,
                msg = paste("número de elementos no es múltiplo",
                            "de número de variables"))

    if (is.null(idcon)){
        idcon <- seq_len(nr)
    } else {
        assert_that(length(idcon) == nr,
                    msg = paste("número de etiqueta no es igual",
                                "al número de consultas"))
    }
    
    ## if (!nv){
    ##     nomvar <- paste0("V_", seq_len(nvar))
    ## }
    
    ##mk <- matrix_cod(nn, ncol = nvar, nomcol = nomvar, ...)
    mk <- matrix_cod(nn, nomcol = nomvar, ...)
    
    if (!is.null(idr)){
        mk <- cbind(idr, mk)
        colnames(mk)[1] <- nomidr
    }
    
    ## expresiones de consulta
    ## TODO incorporar where
    nc <- colnames(mk)
    ss <- vapply(split(mk, seq_len(nr)),
                 function(x){
                     xsql(lxs(tabla, al = "", km = x, as = nc))
                 },
                 "a", USE.NAMES=FALSE)
    
    xsql_u(ss, uall, nomcol, idcon)
}

#' consulta base de datos encuesta
#' @description manda a ejecutar consulta a la base de datos. Al
#'     data.frame resultante le atribuye metadatos y filtra por filas
#'     los datos no igual a cero si así es solicitado
#' @param ob objeto odb
#' @param qstr una cadena de caracteres con la expresión de consulta
#'     SQL
#' @param meta metadatos
#' @param sin_0 filtra los registros con datos diferentes de 0? FALSE
#'     por defecto
#' @param na_0 convierte columnas numericas de NA a cero? FALSE por
#'     defecto
#' @param max número máximo de registros en el resultado. Todos por
#'     defecto.
#' @param ... parámetros adicionales para la función quitar_0
#' @export
#' @importFrom assertthat assert_that
get_data.odb <- function(ob, qstr, meta, sin_0 = FALSE,
                         na_0 = FALSE, max = 0, ...){
    # ' es expresión válida?
    assert_that(nzchar(qstr),
                msg = "falta expresión de consulta")

    kk <- db_open(ob)
    if (is_rodbc(kk)){
        ww <- db_qry(kk, qstr, max = max)
        odbcClose(kk)
    
        if (is.data.frame(ww)){
            if (na_0){
                ww[] <- lapply(ww, na0)
            }
            if (sin_0){
                ww <- quitar_0(ww, ...)
            }
            if ((!missing(meta)) && is.character(meta)){
                attr(ww, "meta") <- meta
            }
        }
    }
    invisible(ww)
}

#**OjO**
#-metodos genericos definidos en genericos.r
if(!isGeneric("datos"))
    sys.source("c:/eddy/code/r/genericos.r",
               envir=attach(NULL, name="genericos"))
if(!exists("strrep", mode="function")){
    strrep <- function(x, times) paste0(replicate(times, x),
                                      collapse="")
}
#- /construye expresion sql a partir de argumentos si hace falta/
#- lak es lista con lista de alias y tablas y codigos en select:
#  list(list(a=(alias,tabla),k=(codigos)),list(a=(alias,tabla),k=(codigos))...)
#  ejemplo:
#    list(list(a=c("pri2004t1","t1"),k=c("quest","c0001"),
#                                        as=c("quest","genero")),
#         list(a=c("pri2004t2","t2"),k=c("quest","c0341")))
#- expresion where: e.g:"t1.quest=t2.quest"
#- expresion order: e.g:"t1.quest"
#- expresion by: e.g:"cnty"
## xsql <- function(lak=NULL, cwh="", cord="", cby=""){
##   sq <-""
##   if(!is.null(lak)){
##     z<-sapply(lak,
##               function(y){
##                 if(!(is.null(y[["as"]])|length(y[["as"]])==0))
##                   y[["k"]] <- paste(y[["k"]],y[["as"]],sep=" as ")
##                 paste(y[["a"]][2],y[["k"]],sep=".",collapse=",")
##               })
##     sq<-paste("select",paste(z,collapse=","),"from")
##     z<-sapply(lak,function(y)paste(y[["a"]],collapse=" "))
##     sq<-paste(sq,paste(z,collapse=","))
##   }
##   if(!(cwh==""|length(cwh)==0|is.null(cwh)))
##     sq<-paste(sq,"where",cwh)
##   if(cord!="")
##     sq<-paste(sq,"order by",cord)
##   if(cby!="")
##     sq<-paste(sq,"group by",cby)
##   invisible(sq)
## }

## ListOfListOfCharacter,Character,Character,Character Character -> Character
## produce expresión SQL a partir de listas list(a=c(TABLA, alias),
## k=c(campos), as=c(alias de campos)) con clausulas where, inner
## join, left(right) join order by y group by. Si es un join, se
## requieren dos tablas y la cláusula on completada por whr; el tipo
## de join se puede indicar sólo con las primeras letras; e.g in(nner)
##
## xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
##           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
##      whr="a.c1=b.c1") ->
##   "select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a,prib b
##    where a.c1=b.c1"
##
## xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
##           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
##      whr="a.c1=b.c1", joi="le") ->
##   "select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a left
##    join prib b on a.c1=b.c1"
##
## xsql(list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a", "b"))) ->
##   "select b.c1 as a,b.c2 as b from prib b"
##
xsql <- function(lak=NULL, whr="", ord="", gby="", joi=""){
    assertthat::assert_that(!is.null(lak))
    if(!is.list(lak[[1]])) lak <- list(lak)
    una_t <- length(lak) == 1
    ## mas de una tabla es un join
    assertthat::assert_that((nzchar(whr) && nzchar(joi)) == !una_t)
                            ##msg="where o join si más de una tabla")
    joins <- c("left join", "inner join", "right join")
    if(nzchar(joi)){
        joi <- pmatch(joi, joins)
        assertthat::assert_that(!is.na(joi), nzchar(whr),
                                length(lak) == 2)
                                ##msg="join correcto entre 2 tablas")
    }
        
    ## lista de campos, alias y tablas correctos
    assertthat::assert_that(
                    all(vapply(lak, function(x)length(x[["k"]]) > 0,
                               TRUE)),
                    all(vapply(lak, function(x)length(x[["a"]]) == 2,
                               TRUE)),
                    all(vapply(lak, function(x) is.null(x[["as"]]) |
                                                length(x[["k"]]) ==
                                                length(x[["as"]]), TRUE)))
    sq <- vapply(lak,
                 function(x){
                     if(!is.null(x[["as"]])){
                         x[["k"]] <- paste(x[["k"]], x[["as"]],
                                           sep=" as ")}
                     paste(x[["a"]][2], x[["k"]], sep=".",
                           collapse=",")}, "", USE.NAMES=FALSE) %>%
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
  
    ##TODO validar expresiones
    if(nzchar(ord)) sq <- paste(sq, "order by", ord)
    if(nzchar(gby)) sq <- paste(sq, "group by", gby)
    sq
}

## versión simplificada de xsql
## Character, VectorOfCharacter, VectorOfCharacter -> Character
## produce expresión SQL a partir de TABLA, CAMPOS y ALIAS
## xsql_s("pri", c("a","b"), c("x", "y")) ->
##   "select a.a as x,a.b as y from pri a"
## xsql_s("pri", c("a","b")) -> "select a.a,a.b from pri a"
xsql_s <- function(db, km, as=NULL){
    assertthat::assert_that(is.character(km),
                            is.character(db))
    if(!is.null(as)){
        assertthat::assert_that(is.character(as),
                                length(km) == length(as))
    }
    xsql(list(a=c(db, "a"), k=km, as=as))
}

## construye lista para llamar a xsql
lxs <- function(db, km, al="a", as=NULL){
    if(!is.null(as)){
        assertthat::assert_that(is.character(as),
                                length(km) == length(as))
    }
    list(list(a=c(db, al), k=km, as=as))
}

## VectorOfCharacter -> Character
## produce la expresion "union all" de varias expresiones SQL
## xsql_union(c("a", "b", "c")) -> "a union all(b union all(c))"

## implementación con while
## xsql_union <- function(x){
##     nn <- length(x)
##     xx <- NULL
##     while(nn > 1){
##         xx <- paste0("union all(", x[nn], " ", xx, ")")
##         nn <- nn - 1
##     }
##     paste(x[1], xx)
## }

## implementación recursiva
## VectorOfCharacter Character -> Character
## genera expresión anidada, encerrada entre (),
## de elementos de VectorOfCharacter
## str_ani(c("aa", "bb", "cc")) -> aa(bb(cc))
str_ani <- function(x){
    if(length(x) == 1){
        x
    } else{
        paste0(x[1], "(", str_ani(x[-1]), ")")
    }
}

bend_char <- function(x="()"){
    c(sub("(\\b.)(.*)", "\\1", x), sub("(.*)(.\\b)", "\\2", x))
}

delim_str <- function(x, del="()"){
    cc <- bend_char(del)
    paste0(cc[1], x, cc[2])
}

nest_str <- function(x, cc=c("(",")")){
    if(length(x) == 1){
        x
    } else {
        paste0(x[1], cc[1], nest_str(x[-1], cc), cc[2])
    }
}

## nest_str <- function(x, del="()"){
##     if(length(x) == 1){
##         x
##     } else{
##         paste0(x[1], delim_str(nest_str(x[-1], del), del))
##     }
## }

## VectorOfCharacter -> character
## union no incluye registros "repetidos"
## union all incluye todos los registros
## xsql_u(c("a", "b", "c")) -> a union all (b union all (c))
xsql_u <- function(x, all=TRUE){
    nn <- length(x)
    assertthat::assert_that(is.character(x),  nn > 1)
    str_ani(c(paste(x[-nn],
                    paste0("union", ifelse(all, " all", ""))),
              x[nn]))
}

##
setServer <- function(ser="mag") assign("SERVER", ser, envir=.GlobalEnv)
getServer <- function() get("SERVER", envir=.GlobalEnv)

dbOpen <- function(connstr){
    invisible(tryCatch(
                  odbcDriverConnect(connection=connstr),
                  error=function(e) message("\n...ERROR no se pudo conectar"),
                  warning=function(e) message("\n...ERROR de conexión"),
                  finally=function(e) NULL))
}

dbSqlserver <- function(server="mag", dbase="encuestas"){
    stopifnot(nzchar(server))
    if(pmatch(server, "local", nomatch=0) == 1){
        connstr <- paste0("DRIVER={SQL Server Native Client 11.0};",
                          "Server=(localDB)\\MSSQLLocalDB;",
                          "trusted_connection=yes;",
                          "Database=", dbase, ";")
    } else{
        cc <- c("DRIVER=", "Server=", "Uid=", "pwd=", "Database=")
        if(server == "mag"){
            ss <- c("{SQL Server}", "10.22.166.42", "erick",
                    "erick83$$$", dbase)
        } else{
            ss <- c("{SQL Server}", "10.22.168.199", "ecastellon",
                    "ecdatamag%20", dbase)
        }
        connstr <- paste0(cc, ss, collapse=";")
    }
    dbOpen(connstr)
}

#--odbcReConnect genera error cuando el Sql-server local
dbReOpen <- function(channel){
    if(inherits(channel, "RODBC")){
        kk <- try(odbcReConnect(channel), silent=TRUE)
        if(inherits(kk,"try-error")){
            kk <- dbOpen(conn=attr(channel, "connection.string"))
        }
    } else{
        kk <- NULL
        message("\n... ERROR-no-RODBC-channel")
    }
    invisible(kk)
}

dbTablas <- function(kk){
    invisible(sqlTables(kk, tableType="TABLE")[["TABLE_NAME"]])
}

dbColumnas <- function(kk,tt){
    invisible(sqlColumns(kk,tt)[["COLUMN_NAME"]])
}

dbQry <- function(ssq, ser="mag", dat="encuestas", null=0){
    ww <- tryCatch({conn <- dbSqlserver(ser, dat)
        sqlQuery(conn, ssq, nullstring=NA_real_)},
        error=function(e)message("\n...error-lectura!!!"),
        message=function(e)e,
        warning=function(e)e,
        finally=odbcClose(conn))
    invisible(ww)
}

vfpdbc <- function(dir)
    odbcDriverConnect(
        paste("DRIVER={Microsoft Visual FoxPro Driver};SourceType=DBC;SourceDB=",
              dir,sep=""),case="tolower")

tabladbc <- function(dir,tabla){
  kn <- vfpdbc(dir)
  w <- sqlFetch(kn,tabla)
  odbcClose(kn)
  invisible(w)
}

condbf <- function(dir)
    odbcDriverConnect(paste("DRIVER={Microsoft Visual FoxPro Driver};SourceType=DBF;SourceDB=",dir,sep=""),case="tolower")

tabladbf <- function(dir, tabla){
  kn <- condbf(dir)
  w <- sqlFetch(kn, tabla)
  odbcClose(kn)
  invisible(w)
}

guardadbf <- function(datos, tabla, dir, vartipos){
  wd <- getwd()
  setwd(dir)
  kn <- condbf(getwd())
  sqlSave(kn, datos, tablename=tabla, rownames=F, varTypes=vartipos)
  odbcClose(kn)
  setwd(wd)
}

borradbf <- function(dir,tabla){
  wd <- getwd()
  setwd(dir)
  kn <- condbf(getwd())
  sqlDrop(kn, tabla)
  odbcClose(kn)
  setwd(wd)
}

## Cuando se crea una tabla en Excel mediante sqlSave
## si la tabla es nueva automáticamente se crean dos tablas
## con el mismo nombre: la tabla del sistema con sufijo $ y
## la tabla "normal" sin sufijo alguno. A la tabla del sistema
## está asociada la "hoja" de Excel, con el mismo nombre pero
## sin el sufijo. También se crea un rango con el nombre de la
## tabla, que cubre las celdas donde se depositan los datos (el
## rango es ¿la tabla del sistema o la tabla normal?. Se deduce
## que es la del sistema porque el rango está asociado a una hoja,
## de manera que si cambia de nombre a la hoja, el rango nombrado
## también registra el cambio de nombre. ¿Tabla del sistema es
## el equivalente de la base de datos?).

## Cuando se elimina una tabla con sqlDrop se elimina la tabla
## normal pero no la del sistema ni tampoco el rango nombrado, y entonces
## si se intenta volver a grabar otro d.f en la misma tabla, manda el
## error de que la tabla ya existe. Con RODBC no es posible descartar la
## tabla del sistema. Por eso, para reusar el nombre de la tabla, lo que
## se puede hacer es abrir Excel y eliminar manualmente el rango nombrado
## y la hoja o, simplemente, renombrar la hoja para que el
## nombre de la tabla no continúe asociado a ella. Por alguna razón,
## en algunas circunstancias sqlDrop dos veces consecutivas para
## descartar la tabla

## TODO equivalencia de extensión xlsx o xls con
## el tipo de conexión Excel2007 o Excel
xlscon <- function(filexls, xlsx=FALSE, ro=TRUE){
    ## kn <- ifelse(xlsx,
    ##              odbcConnectExcel2007(filexls, readOnly=ro),
    ##              odbcConnectExcel(filexls, readOnly=ro))
    if(xlsx){
         kn <- odbcConnectExcel2007(filexls, readOnly=ro)
    } else{
        kn <- odbcConnectExcel(filexls, readOnly=ro)
    }
    if(!inherits(kn, "RODBC")){
        message("error-conexión")
    }
    kn
}

## TODO mejorar manejo de errores
## Character, Logical -> Character
## produce listado de tablas en el archivo fxls
tablas_xls <- function(fxls, xlsx=TRUE){
    kk <- xlscon(fxls, xlsx)
    uu <- dbTablas(kk)
    odbcClose(kk)
    uu
}

## TODO administrar excepciones si tablas_xls error
## extensión de fxls
## Character, Character, Logical -> Logical
## responde ¿existe la tabla en fxls?
existe_xls <- function(tabla, fxls, xlsx=TRUE){
    assert_that(file.exists(fxls),
                is.character(tabla))
    uu <- tablas_xls(fxls, xlsx)
    tabla %in% uu
}

tablaxls <- function(filexls, tabla, xlsx=F){
    kn <- xlscon(filexls, xlsx)
    w <- NULL
    if(inherits(kn, "RODBC"))
        w <- tryCatch(sqlFetch(kn, tabla),
                      error=function(e)message("error-lectura"),
                      finally=odbcClose(kn))
    invisible(w)
}

guardaxls <- function(datos, tabla, filexls,
                      nomfilas=FALSE, xlsx=TRUE){
    if(xlsx)
        kn <- odbcConnectExcel2007(filexls, readOnly=FALSE)
    else
        kn <- odbcConnectExcel(filexls, readOnly=FALSE)
    sqlSave(kn, datos, tablename=tabla, rownames=nomfilas)
    odbcClose(kn)
}

borraxls <- function(tabla, filexls, xlsx=TRUE){
    if(existe_xls(tabla, filexls, xlsx)){
        kn <- xlscon(filexls, xlsx, ro=FALSE)
        sqlDrop(kn, tabla)
        odbcClose(kn)
    }
}

## consulta base datos y agrega metadato descriptivo
## produce d.f
## tb: tabla de la base de datos de donde leer
## qstr: lista o string de consulta
##   si lista: vectores construir consulta con xsql
##   qstr[[1]] vector de campos
##   qstr[[2]] nombres columnas de salida (opcional)
## meta: el atributo descriptivo del d.f
## db: nombre de la base de datos
## e.g get_dat("dbf", "select ...", "datos de ...")
## get_dat("dbf", list(c("c001","c002"),c("cam1","cam2")),"...")
get_dat <- function(tb, qstr="", meta="", db="encuestas",
                    sin_0=FALSE, ...){
    if(is.list(qstr)){
        oo <- list(list(a=c(tb,"a"), k=qstr[[1]]))
        if(length(qstr) > 1 && is.character(qstr[[2]])){
            oo[[1]]["as"] <- list(as=qstr[[2]])
        }
        qstr <- xsql(oo)
    }
    ww <- dbQry(qstr, getServer(), dat=db)
    if(!is.data.frame(ww)){
        message("error-lectura")
    } else{
        if(sin_0) ww <- quitar_0(ww, ...)
        attr(ww,"meta") <- meta
    }
    invisible(ww)
}

## leer cuadros con estructura
##   cxxx cxxx cxxx cxxx (codigos)
##   cxxx cxxx cxxx cxxx
##   .... .... .... ....
## donde las columnas son las variables
## tb es nombre de la tabla de datos
## mk es matriz con los códigos de las columnas
## de la tabla en la base de datos
##   cult semb prod umed (variables)
##   cxxx cxxx cxxx cxxx
##   cxxx cxxx cxxx cxxx
## el nombre de las columnas de mk se convierten
## en los nombres de las variables y si se le dan
## nombre a las filas estos se convierten en una
## variable adicional
## kf es la columna que identifica a los registros
## el resultado es d.f con
##   nrow = num.registros_en_base_datos*num.filas_cuadro
##   ncol = 1(2 si nombre de filas de matriz) + num.columnas_cuadro
## o NULL si problemas en la lectura
## ww <- lec_dat(tb=DAT, db=DBF,
##              mk=matriz_cod(ss, ncol=numc, nomcol=nomc,
##                            nomfi=nomh, cod=cod),
##              nomfila=nomv, ...)
lec_dat <- function(tb, mk, kf="quest", db="encuestas",
                    nomfila="nom_fil", sin_0=FALSE, meta="", ...){
    stopifnot(is.matrix(mk))
    nr <- nrow(mk)
    nc <- colnames(mk)
    if(!is.null(nc)) nc <- c(kf, nc)
    ss <- vapply(split(mk, seq_len(nr)),
                 function(x)xsql(lxs(tb, km=c(kf, x),
                                     as=nc)), "a",
                 USE.NAMES=FALSE)%>%
        paste(collapse=" union all(")%>%
        paste0(strrep(")", nr-1))
    
    ww <- dbQry(ss, getServer(), dat=db)
    ##dbQry debería enviar mensaje y NULL
    if(is.data.frame(ww)){
        ##if(sin_0) ww <- quitar_0(ww, excepto="quest")
        if(!is.null(rownames(mk))){
            ww[nomfila] <- rep(rownames(mk), each=nrow(ww) %/% nr)
        }
        if(sin_0) ww <- quitar_0(ww, ...)
        attr(ww, "meta") <- meta
    } else ww <- NULL
    invisible(ww)
}

## df sin las hileras con datos en columna = 0
## excepto: nombres de columnas excluidas en filtrado
## quitar_0(ww, excepto="quest")
quitar_0 <- function(df, excepto="quest"){
    ss <- setdiff(names(df)[vapply(df, is.numeric, TRUE)],
                  excepto)
    filter_at(df, vars(ss), any_vars(. > 0))
}

#=== manejo de códigos
codigo_fac <- function(nn, di=3){
    function(nn) sprintf(paste0("%s%0", di, "i"), "c", nn)
}

codigo <- function(nn, di=3){
    sprintf(paste0("%s%0", di, "i"), "c", nn)
}

##codigo <- codigo_fac(nn,di=3)
es.codigo <- function(cc) is.character(cc) && (substr(cc, 1, 1)=="c")

## matriz de códigos para hacer consulta (ver get_dat)
matriz_cod <- function(nn, ncol=length(nn), nomcol=NULL,
                       por_fila=TRUE, nomfi=NULL, cod=NULL){
    assertthat::assert_that((is.null(cod) & is.character(nn)) |
                            (is.numeric(nn) & !is.null(cod)))
    if(!is.null(cod)) nn <- cod(nn)
    matrix(nn, ncol=ncol, byrow=por_fila,
           dimnames=list(nomfi, nomcol))
}

ex_val <- function(opi, ope="==", opd, codigo){
    if(!es.codigo(opi)&&is.numeric(opi)) opi <- codigo(opi)
    if(ope=="><") ope <- c(">","<")
    if(ope=="+="&&length(opd)==1){
        opi <- paste0(opi,collapse="+")
        ope <- "=="
        if(!es.codigo(opd) && is.numeric(opd)) opd <- codigo(opd)
    }
    paste0(opi, ope, opd)
}

##=== otros
## Vector Vector Vector -> Vector
## copia elemento de x en i-ésima posición a la j-ésima
## la j-ésima posición en dup indica el i-ésimo en x; es NA
## si el j-ésimo elemento no se modifica
## ide es una clave (key) que nombra los elementos de x
## para referenciarlos en dup
## x_dup(c(1, 2, 3), c(11, 12, 13), c(NA, 13, NA))
## -> c(1, 3, 3)
## x_dup(c(1, 2, 3), c(13, 12, 11), c(NA, 13, NA))
## -> c(1, 1, 3)
## x_dup(c(1, 2, 3), c(11, 12, 13), c(13, 13, NA))
## -> c(3, 3, 3)

## x_dup(c045, quest, copiade)
x_dup <- function(x, ide, dup) x_modi(x, dup, ide, x, msg=FALSE)

## Problema: en las encuestas de monitoreo, cuando se trata de la
## producción de granos, una misma UP (boleta) tiene registros de uno
## o más cultivos:
## quest cultivo sembrada
## 0001        1        2
## 0001        2        1
## 0002        1        1
## Algunos de los registros son "duplicados" de otro porque uno o más
## "puntos" corresponden a la misma UP. Uno o más registros pueden ser
## "copia" del mismo "origen"
## quest copiade
##  0001      NA
##  0002    0001
## Durante la toma de datos y la digitación, los registros "origen" se
## digitan (se supone) y debería también digitarse el registro
## "copia". El problema es como asegurar que estos últimos sean copia
## fiel de los primeros
## DataFrame DataFrame -> DataFrame
## prop. crear data frame con todos los registros (origen y copia),
## "clonando" los registros "origen" de conformidad con los indicado
## en la tabla origen-copia
## Ejemplo:
##   quest cultivo  area
## 1     1       1     1
## 2     1       2     2
## 3     2       1     3
## 4     2       2     4
## 5     3       1     5
##   quest copiade
## 1     1      NA
## 2     2      NA
## 3     3      NA
## 4     4       1
## 5     5       2
## 6     6       1
## 7     7      NA
## 8     8       7
## df_dup(grano, copia) ->
##    quest cultivo  area
##  1     1       1     1
##  2     1       2     2
##  3     2       1     3
##  4     2       2     4
##  5     3       1     5
##  6     4       1     1
##  7     4       2     2
##  8     5       1     3
##  9     5       2     4
## 10     6       1     1
## 11     6       2     2
df_dup <- function(dd, dc){
    id <- dd[[1]]
    or <- dc[[2]]
    cp <- dc[[1]]

    ii <- !is.na(or)
    xx <- lapply(which(ii), function(x){
        if(any(jj <- id == or[x])){
            uu <- dd[jj,]
            uu[,1] <- cp[x]
            uu} else NULL}) %>% Reduce(rbind,.)
    
    invisible(rbind(dd[id %in% cp[!ii],], xx))
    ## ww <- NULL
    ## for(ii in nn){
    ##     uu <- dd[dd[[1]] == dc[ii,][[2]],]
    ##     uu[,1] <- dc[ii,][[1]]
    ##     ww <- rbind(ww, uu)
    ## }
}

## --- de uso general ---

## Character -> Character
## remueve extensión de nombre de archivo
## tools::file_path_sans_ext("ABCD.csv")
sin_ext <- function(nf){
    sub("\\..*$", "", nf)
}

## DataFrame|List -> Integer
## produce el vector de índices de orden
## que generan las columnas cc del data.frame o lista df
ind_orden <- function(df, cc = NULL){
    assert_that(is.list(df))
    ordenar <- TRUE
    if (is.numeric(cc)){
        nn <- as.integer(cc)
        assert_that(setequal(intersect(seq_along(df), nn),
                             nn),
                    msg="columnas_orden no válidas")
    } else if (is.character(cc)){
        assert_that(setequal(cc, intersect(cc, names(df))),
                    msg="columnas_orden no válidas")
        nn <- match(cc, names(df))
    } else if (is.null(cc)){# ordena por todas las columnas
        nn <- seq_along(df)
    } else{
        ordenar <- FALSE
    }

    if (ordenar){
        nn <- do.call("order", as.list(unname(df[nn])))
    } else if (is.data.frame(df)){# en la secuencia original
        nn <- seq_len(nrow(df))
    } else{
        nn <- seq_along(df)
    }
    nn
}

## Integer | Vector -> Vector
## un vector de unos de longitud nn (escalar)
## o de la misma longitud del vector nn
unos <- function(nn = 0){
    if(length(nn) > 1) nn <- length(nn)
    integer(nn) + 1L
}

vec_uno <- function(nn=0) integer(nn) + 1L

na0 <- function(x){
    if(is.numeric(x)) x[is.na(x)] <- 0
    invisible(x)
}

cero_na <- function(x){
    if(is.numeric(x)) x[!is.na(x) & (x==0)] <- NA
    invisible(x)
}

## transforma los NA en 0 en las numericas
## excluir columnas ignoradas, si no, todas
df_na0 <- function(df, excluir=NULL){
    ii <- vapply(df, is.numeric, TRUE)
    cc <- names(df)
    if(any(ii)){
        if(is.numeric(excluir)){
            if(all(excluir > 0 & excluir < ncol(df))){
                excluir <- cc[excluir]
            }
        }
        if(is.character(excluir)){
            if(!all(excluir %in% cc)){
                warning("\n", excluir, " no válido")
            }
            ii <- ii & (cc %in% setdiff(cc, excluir))
        }
        df[,ii][is.na(df[,ii])] <- 0
    } else{
        message("\n no hay columnas numericas")
    }
    invisible(df)
}

## Vector|DataFrame|Matrix Vector -> Matrix
## prop. devuelve matriz con columnas x,y imitando xy.coords
## xy_mat(2:4) -> 2 1
##                3 1
##                4 1
## mm <- data.frame(uu=1:3, bb=letters[1:3], zz=4:6, stringsAsFactors=F)
## mm <- matrix(1:4)
## ## xy_mat(mm)
## xy_mat <- function(x, y=NULL){
##     oo <- getOption("warn")
##     options(warn=-1)
##     xy <- xy.coords(x, y)
##     options(warn=oo)
    
##     x <- xy[[1]]
##     if(is.null(y)){
##         y <- xy[[2]]
##         if(isTRUE(all.equal(y, seq_along(y))) ||
##            all(is.na(y))){
##             y <- unos(length(y))
##         }
##     }
##     matrix(c(x, y), ncol=2, byrow=FALSE)
## }

sum_w <- function(x, w, ...){
    assert_that(is.numeric(x), is.numeric(w),
                length(x) == length(w))
    sum(x * w, ...)
}

## Matrix|DataFrame|List Vector -> Numeric
## prop. calcular producto interior de dos vectores
## x y w. Los dos vectores pueden ser los primeros dos
## vectores columna de matriz o data.frame x o de la lista
## x cuando no se indica w, en cuyo caso w se hace vector de 1.
sum_xw <- function(x, w=NULL, ...){
    ## si data.frame y alguna columna no es numérica
    ## xy.coords -> warning
    ## ÔjÔ si es factor
    oo <- getOption("warn")
    options(warn=-1)
    xy <- xy.coords(x, w)
    options(warn=oo)

    x <- xy[[1]]
    if(is.null(w)){
        w <- xy[[2]]
        ## xy.coords produce secuencia si x unidimensional
        ## o NA si ponderación no es valor numérico
        if(isTRUE(all.equal(w, seq_along(w))) ||
           all(is.na(w))){
            w <- unos(length(w))
        }
    }

    sum(x * w, ...)
}

sum_xwg <- function(df, g, x, w){
    gg <- quo(g)
    xx <- quo(x * w)
    group_by(df, !!gg) %>%
        summarise(suma = sum(!!xx))
}

## weighted.mean(.,., na.rm=TRUE) -> NA si NA en w
## TODO modificar para que elimine los NA en w
mean_w <- function(x, w, ...) stats::weighted.mean(x, w, ...)

es.na0 <- function(x) if(is.numeric(x)) is.na(x)|(x==0) else x

pct <- function(x, base=sum(x), ndig=0) round(100 * x/base, ndig)

num_gt <- function(x, base=0) sum(na0(x) > base)

libs <- function(cc, sep="[[:space:]]"){
    try(sapply(strsplit(cc, split=sep)[[1]],
               library, character.only=T))
}

## TODO
## librerias(plyr, purr, magrittr)
## librerias(...)
#==
save_rm <- function(x, file=NULL, env=parent.frame()){
    stopifnot(is.character(x), !is.null(file))
    if(length(x)==1)
        x <- strsplit(x, split="[[:space:],]")[[1]]
    save(list=x, file=file, envir=env)
    rm(list=x, envir=env)
}

## transfiere lista de objetos en x en env al archivo file
## sobrescribe objetos en file con el mismo nombre
guardar_v <- function(x, file=NULL, env=.GlobalEnv){
    stopifnot(!is.null(file), is.character(x))
    ## necesario para evitar conflicto con cc si en x viene objeto cc
    enn <- new.env(hash=FALSE)
    if(file.exists(file)) cc <- load(file, envir=enn) else cc <- NULL
    for(ss in x) assign(ss, get(ss, envir=env, mode="list",
                                inherits=FALSE), envir=enn)
    if(is.null(cc)) cc <- x else cc <- union(cc, x)
    save(list=cc, file=file, compress=TRUE, envir=enn)
    message(length(cc)," data.frame transferidos a ", file)
    invisible(cc)
}

guardar_rm <- function(x, file, env=.GlobalEnv){
    cc <- guardar_v(x, file, env)
    if(is.character(cc)) rm(list=x, envir=env)
}

## data.frame en file cargados a env
leer_df <- function(lista, file, env=.GlobalEnv){
    stopifnot(is.character(lista))
    if(file.exists(file)){
        oo <- load(file)
        cc <- intersect(oo, lista)
        if(length(cc) != length(lista)){
            message("estos ", paste0(setdiff(lista, cc), collapse=","),
                    " no están en ", file, "\n")
        }
        if(length(cc) > 0){
            for(ss in cc) assign(ss, get(ss, mode="list"), pos=env)
        }
    } else{
        message("archivo ", file, " no existe")
        cc <- NULL
    }
    invisible(cc)
}

## devuelve data.frame almacenado en archivo
## si no lo encuentra devuelve listado de d.f
get_dff <- function(df=NULL, ff=NULL){
    assert_that(is.character(df), length(df)==1,
                file.exists(ff))
    oo <- load(ff)
    ##mget para varios objetos
    if(df %in% oo){
        x <- get(df, inherits=FALSE)
    } else{
        message("\n", df, " no encontrado")
        x <- oo
    }
    invisible(x)
}

## lista de d.f en file
## produce d.f con nombres y attr meta
lista_df <- function(file){
    if(file.exists(file)) oo <- load(file)
    cc <- vapply(oo, function(x){
        ss <- do.call(attr, list(x=as.symbol(x), which="meta"))
        ss[is.null(ss)] <- NA_character_
        ss}, "", USE.NAMES=FALSE)
    data.frame(data=oo, meta=cc)
}

df_lista <- function(ff) lista_df(ff)

## nombres de d.f en file
df_en <- function(file){
    if(file.exists(file)){
        oo <- load(file)
    } else oo <- NA_character_
    oo
}

al_path <- function(que,donde){
    if(!is.element(donde,search())){
        attach(que,name=donde)
    } else message(donde," ya está en la ruta de búsqueda")
}

en_path <- function(que) que %in% search()

out_path <- function(que) if(en_path(que))
                              detach(que, character.only=TRUE)
##=== NSE ===
subset_2 <- function(x, condition){
    cc <- substitute(condition)
    rr <- eval(cc, x, parent.frame())
    x[rr,, drop=FALSE]
}

##=== data-frames ===
## Vector Vector -> Logical
## prop. sustituir operador infijo %in%
en <- function(x, y){
    !is.na(ligar(x, y))
}

## Vector Vector -> Vector
## match con mensaje de cuántos elementos
## en x hacen pareja con los elementos en y
## ligar(1:3, 1:5, msg=TRUE) -> (1, 2, 3)
## ligar(1:3, 1:2, msg=TRUE) -> (1, 2, NA); "2 parejas"
## ligar(1:3, 5:10) -> (NA, NA, NA); "0 parejas"
## ligar(letters[1:3], 1:3) -> (NA, NA, NA); "tipos diferentes"
ligar <- function(x, y, msg=TRUE){
    assertthat::assert_that(mode(x) == mode(y),
                            msg="modos diferentes")
    mm <- match(x, y, nomatch=NA, incomparables=NULL)
    if(msg && any(is.na(mm))){
        message(sum(!is.na(mm)), " parejas de ", length(x))
    }
    mm
}

## match con mensaje
match_m <- function(x, y) ligar(x, y)

atar <- function(x,y) ligar(x, y)

## match por dos variables
match_2 <- function(x1, x2, y1, y2){
    ## misma longitud x1,x2 y y1,y2 ?
    ## mismo tipo ?
    invisible(match(paste0(x1, x2),
                    paste0(y1, y2)))
}

## Vector -> Vector
## vec_NA(tip, n, toNA)
## prop. produce vector de tipo tip con n elementos inicializados
## a NA si toNA
vector_NA <- function(n=1, tip="integer", toNA=TRUE){
    vv <- vector(tip, n)
    if(toNA){
        is.na(vv) <- seq_along(vv)
    }
    invisible(vv)
}

## agrega columna a un d.f
## inicializa a 0 si numeric
## "" si alfanumérico
df_add_col <- function(df, nom, tipo="numeric"){
    assertthat::assert_that(!nom %in% names(df))
    df[nom] <- do.call(tipo, list(nrow(df)))
    invisible(df)
}

## vectorizada de df_add_col para data.frame
## convertir a add_cols para agregar columnas a data.frame o matriz
add_cols <- function(df, nom, tipo="integer", toNA=FALSE){
    ii <- nom %in% names(df)
    if(any(ii)){
        message("excluye ", sum(ii),
                "con mismo nombre de alguna en data.frame")
        nom <- nom[!ii]
    }
    
    nc <- length(nom)
    assertthat::assert_that(nc > 0,
                            msg="no hay columnas por agregar")
    if(nc > length(tipo)) tipo <- rep_len(tipo, nc)
    nr <- nrow(df)
    ww <- lapply(seq_along(nom), function(x){
        vector_NA(nr, tipo[x], toNA)})
    
    names(ww) <- nom
    invisible(cbind(df, ww))
}

## comprueba dos vectores son mismo tipo
eq_tipo_p <- function(x, y) typeof(x) == typeof(y)

eq_tipo_p_old <- function(x, y){
    (is.numeric(x) & is.numeric(y))|
        (is.character(x) & is.character(y))|
        (is.logical(x) & is.logical(y))
}

## modifica los datos de un vector
## según resultado de búsqueda de un vector en otro
## xnw y busen deben estar "alineados"
## y así mismo x y busca
## i.e misma longitud y de modo que
## x[i] es remplazazo por xnw[mm[i]]
x_modi <- function(x, busca, busen=NULL, xnw=NULL, msg=TRUE){
    assert_that(length(x) == length(busca),
                length(busen) == length(xnw),
                eq_tipo_p(x, xnw),
                eq_tipo_p(busca, busen))
    if(is.null(busen) | is.null(xnw)) return(x)
    mm <- match(busca, busen)
    if(any(ii <- is.na(mm))){
        if(msg) message("\n... ", sum(ii), " no enlazan...")
    }
    ii <- !ii
    if(any(ii)){
        if(msg) message(sum(ii), " remplazos")
        x[ii] <- xnw[mm[ii]]
    }
    invisible(x)
}

## sinónimo de x_modi
busca_remplaza <- function(busca, donde, x, remplazo){
    x_modi(x, busca, donde, remplazo)
}

## cambia algunos datos en una columna de d.f
## df: data.frame
## col_busca: con la que se hace el match
## col_cambia: donde se harán los cambios
## val_busca: buscados en col_busca
## val_new: nuevos datos
df_modi_col <- function(df, col_busca, col_cambia,
                        val_busca, val_new){
    n1 <- length(val_new)
    n2 <- length(val_busca)
    if(n1 > n2){
        message("\n... trunca los nuevos")
        val_new[seq_len(n2)]
    }
    ## eficiente?
    if(n1 < n2){
        message("\n... recicla los nuevos")
        val_new <- (rep(val_new, ceiling(n2/n1)))[seq_len(n2)]
    }
    df[col_cambia] <- x_modi(df[,col_cambia], val_busca,
                             df[,col_busca], val_new)
    invisible(df)
}

## nombres de columnas numéricas
numerica <- function(df) names(df)[vapply(df, is.numeric, TRUE)]

## columnas tipo numeric
## nom=FALSE devuelve la posición
col_numeric <- function(df, nom=TRUE){
    ii <- vapply(df, is.numeric, TRUE)
    if(nom) names(df)[ii] else which(ii)
}

## quita elementos de un vector
## drop indica los que se van a quitar
## drop_elem(aa, 1:3)
## drop_elem(aa, c(T,T,F,T))
## drop_elem(aa, c("a", "b"))
drop_elem <- function(x, drop=NULL){
    assertthat::assert_that(is.vector(x), msg="x vector")
    if(!is.null(drop)){
        if(is.character(drop)) drop <- one_of(drop, vars=x)
        if(is.logical(drop)) drop <- which(drop)
        x <- x[-drop]
    }
    x
}

## filtra los filas con al menos un dato > 0 en columnas numéricas
filtrar_n0 <- function(df, excepto=NULL){
    ss <- drop_elem(numerica(df), excepto)
    if(length(ss) > 0)
        df <- filter_at(df, vars(ss), any_vars(. > 0))
    invisible(df)
}

##=== str
fac2char <- function(x){
    if(is.factor(x)){
        ww <- levels(x)[x]
    } else{
        message("argumento no es factor...")
        ww <- x
    }
    invisible(ww)
}

fac2int <- function(x){
    if(is.factor(x)){
        w_f <- type.convert(gsub("[^0-9]", NA_integer_, levels(x)[x]))
    } else{
        message("argumento no es factor...")
        w_f <- x
    }
    invisible(w_f)
}

##== character ==
## "tokens" de x indicados por sep
str2vec <- function(x, sep="[[:space:],;:]+") strsplit(x,sep)[[1]]

## convierte letras iniciales de x en mayúscula
a_propio <- function(x) gsub("\\b([a-z])","\\U\\1", tolower(x),
                             perl=TRUE)

## convierte vocales con acento a vocales sin acento
## Character -> Character
## sin_tilde("ción") -> "cion"
## sin_tilde("cion") -> "cion"
## sin_tilde("sí sí pón") -> "si si pon"

## con library(stringr)
## str_sin_tilde <- function(x){
##     require(stringr)
##     str_replace_all(x, "[ÁáÉéÍíÓóÚú]",
##                     function(ll){
##                         c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
##                           "Á"="A", "É"="E", "Í"="I", "Ó"="O",
##                           "Ú"="U")[ll]})
## }

## regmatches<- sólo modifica la primera ocurrencia del patrón
## regmatches("sísó", gregexpr("[áéíóú]", "sísó")) <- c("i", "o")
## -> "sisi" no "siso" como esperaba
## a pesar de que gregexpr() devuelve c(2, 4)
## tampoco str_replace_all sin llamado a función (e.g de
## str_replace_all)

## no requiere stringr
str_sin_tilde <- function(x){
    assert_that(is.character(x), msg="x no es tipo character")
    vv <- c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
            "Á"="A", "É"="E", "Í"="I", "Ó"="O", "Ú"="U")
    while(any((mm <- regexpr("[ÁáÉéÍíÓóÚú]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
    ## recursiva
    ## mm <- regexpr("[ÁáÉéÍíÓóÚú]", x)
    ## if(mm == -1L){
    ##     return(x)
    ## } else {
    ##     vv <- c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
    ##             "Á"="A", "É"="E", "Í"="I", "Ó"="O", "Ú"="U")
    ##     regmatches(x, mm) <- vv[regmatches(x, mm)[[1]]]
    ##     str_sin_tilde(x)
    ## }
    ## NOT RUN
    ## mm <- gregexpr("[ÁáÉéÍíÓóÚú]", x)
    ## if(mm[[1]][1] > 0) regmatches(x, mm) <- vv[regmatches(x,
    ## mm)[[1]]]
    ## x
}

## elimina los espacios al inicio y al final de x
## equivalente a str_trim de stringr
## Character -> Character
## str_podada("ja   ja") -> "ja   ja"
## str_podada(" ja  ja ") -> "ja  ja"
str_podada <- function(x){
    regmatches(x, regexpr("\\b.*\\b", x, perl=TRUE))
}

## sustituye dos o más caracteres tipo espacio (\n, \t, \s)
## por un espacio y poda extremos
## Character -> Character
## str_bien_formada("ja  ja ja ") -> "ja ja ja"
str_bien_formada <- function(x){
    str_podada(gsub("[[:space:]]+", " ", x))
}

##--- como revalue en plyr
## devuelve los nombres de elementos de a
## "" si elemento en de no encontrado en a
map_cod <- function(de,a){
    mm <- match(de,a)
    ii <- !is.na(mm)
    ww <- character(length(de))
    ww[ii] <- names(a)[mm[ii]]
    ww
}

##==
cod_muni <- function(dp, dm) sprintf("%02i%02i", dp, dm)

## código departamento es válido
dpt_vale <- function(cd, fd="c:/encuestas/marco/depmun.rda"){
    xx <- get_dff("ddpto", fd)
    invisible(as.integer(cd) %in% xx$dpto)
}

## código municipio es válido
mun_vale <- function(cm, fd="c:/encuestas/marco/depmun.rda"){
    xx <- get_dff("dmuni", fd)
    invisible(as.integer(cm) %in% xx$muni)
}

## Character|Integer -> Integer
## el código del departamento a partir del código de municipio
## valida código departamento pero no el de municipio
## cod_dpt("0505") -> 5
## cod_dpt(500) -> 5
## cod_dpt("5510") -> 55
## cod_dpt(5510) -> 55
## cod_dpt(9601) -> NA
cod_dpt <- function(x){
    assert_that(is.character(x)|is.numeric(x))
    if(is.character(x)) x <- type.convert(x)
    ii <- x > 100
    x[ii] <- x[ii] %/% 100
    x[!dpt_vale(x)] <- NA_integer_
    x
}

## Character -> Integer
## el código de departamento o municipio a partir del nombre
## si nombre inválido devuelve NA
## cod_dm("Nueva Segovia", "d") -> 5
## cod_dm("Jalapa", "m") -> 505
## cod_dm("jjj") -> NA
cod_dm <- function(dm, que="departamento",
                   fd="c:/encuestas/marco/depmun.rda"){
    assert_that(is.character(dm),
                msg="dm character")
    assert_that(file.exists(fd),
                msg="archivo departamento-municipios")
    nn <- pmatch(que, c("departamento", "municipio"))
    assert_that(!is.na(nn),
                msg=c("departamento o municipio"))
    load(fd)

    dm <- str_bien_formada(dm) %>% tolower() %>% str_sin_tilde()
    if(nn == 1){
        mm <- pmatch(dm, c("raccn", "raccs"), duplicates.ok=TRUE)
        ii <- !is.na(mm)
        if(any(ii)){
            dm[ii] <- c("raan", "raas")[mm[ii]]
        }
        mm <- pmatch(dm, tolower(ddpto$departamento), duplicates.ok=TRUE)
        ii <- is.na(mm)
        if(any(ii)) message("hay departamento no válido")
        mm[!ii] <- ddpto$dpto[mm[!ii]]
    } else{
        mm <- pmatch(dm, tolower(dmuni$municipio), duplicates.ok=TRUE)
        ii <- is.na(mm)
        if(any(ii)) message("hay municipio no válido")
        mm[!ii] <- dmuni$muni[mm[!ii]]
    }
    mm
}

## Integer, Character -> Character
## devuelve nombre departamento o municipio
## si no existe devuelve NA_character_
## dm vector de códigos de dpto o municipio
## que es "departamento" o "municipio" o pmatch
## nombre_dm(c(5, 0), "dep") -> c("Nueva Segovia", NA)
nombre_dm <- function(dm, que="dep",
                      fd="c:/encuestas/marco/depmun.rda"){
    assert_that(file.exists(fd))
    x <- ifelse(pmatch(que, c("departamento","municipio")) == 1,
                "ddpto", "dmuni") %>% get_dff(fd)
    z <- x_modi(character(length(dm)), as.integer(dm),
                as.vector(x[,1]), as.vector(x[,2]), msg=FALSE)
    if(any(ii <- !nzchar(z))){
        z[ii] <- NA_character_
        message("\n... hay códigos no válidos")
    }
    z
}

## TODO adaptar para nombre_dm
## devuelve nombre departamento o municipio
## - si dpto adjunta cod=99 con nombre Total para indicar país cuando estima
nom_dm <- function(dp, que="dpto", fd="c:/encuestas/marco/depmun.rda"){
    load(fd)
    if(que=="dpto"){
        ddpto <- rbind(ddpto, data.frame(dpto=99,
                                        departamento="Total"))
    } else ddpto <- dmuni
    mm <- match(as.integer(dp), ddpto[,1])
    cc <- character(length(dp))
    ii <- !is.na(mm)
    cc[ii] <- ddpto[mm[ii], 2]
    if(any(!ii)) message("\n... hay códigos inválidos")
    cc
}

nom_mu <- function(dm, dp=NULL){
    dm <- as.integer(dm)
    if(!is.null(dp) & all(dm < 100)) dm <- cod_muni(dp,dm)
    nom_dm(dm, "muni")
}

nom_dp <- function(x) nom_dm(x)

## Integer|Character -> DataFrame
## produce d.f codigo-municipio de código o nombre dp departamento
## municipios(75) ó municipios("Carazo")
##     muni        municipio
## 121 7505       San Marcos
## 122 7510         Jinotepe
## 123 7515          Dolores
## 124 7520         Diriamba
## 125 7525       El Rosario
## 126 7530 La Paz de Carazo
## 127 7535     Santa Teresa
## 128 7540     La Conquista
## ¡TODO vectorizar!
municipios <- function(dp,
                       fd="c:/encuestas/marco/depmun.rda"){
    assert_that(is.character(dp) | is.integer(dp),
                msg="código o nombre de departamento")
    assert_that(file.exists(fd),
                msg="archivo departamento-municipio")
    leer_df("dmuni", fd, env=parent.frame())

    if(is.character(dp)){
        leer_df("ddpto", fd, env=parent.frame())
        mm <- str_bien_formada(dp) %>% str_sin_tilde() %>%
            match(ddpto$departamento)

        assert_that(any(ii), msg="departamento no válido")
        dp <- ddpto$dpto[mm]
    }
    mm <- match(dmuni$muni %/% 100, dp)
    ii <- !is.na(mm)

    assert_that(any(ii), msg="departamento no válido")
    dmuni[ii,]
}

## Character -> Character
## devuelve departamento al que pertenece municipio
## corrige espacios y acentos de la escritura
## dpto_muni("Jalapa") -> "Nueva Segovia"
## dpto_muni("Quilalí") -> "Nueva Segovia"
## dpto_muni("Quilali") -> "Nueva Segovia"
## dpto_muni("San rafael  del norte") -> "Jinotega"
## dpto_muni("Juju") -> NA_character
dpto_muni <- function(mun){
    mun <- str_bien_formada(mun) %>% str_sin_tilde()
    ##mun <- regexpr("\\b.*\\b", mun, perl=TRUE) %>%
    ##    regmatches(mun, .) %>% gsub("[[:space:]]+", " ", .)
    nn <- cod_dm(mun, "m")
    ii <- mun_vale(nn)
    nn[ii] <- nn[ii] %/% 100
    nombre_dm(nn)
}

##--
## devuelve departamentos de los puntos de monitoreo
## cuest es código de cuestionario
## fp nombre archivo maestro de los puntos
## nom devuelve nombre; cod el código
## fd nombre archivo departamento-municipio
dpt_pun <- function(cuest, fp=NULL, noc="cod",
                    cual=c("nombre", "codigo", "código"),
                    fd){
    assert_that(is.numeric(cuest), file.exists(fp))
    oo <- load(fp)
    cc <- do.call("names", list(as.name(oo)))
    ## TODO try? si no encuentra pun, d[ep]
    cq <- grep("pun.+", cc)
    cd <- grep("d[ep].+", cc)
    qq <- as.integer(do.call("[[", list(as.name(oo), cq)))
    nd <- as.integer(do.call("[[", list(as.name(oo), cd)))

    cd <- cero_na(x_modi(integer(length(cuest)), cuest, qq, nd,
                  msg=FALSE))
    if(anyNA(cd)) message(sum(is.na(cd)), " puntos no encontrados")

    if(cual[pmatch(noc, cual)]=="nombre"){
        if(base::missing(fd)){
            cd <- nombre_dm(cd, "dp")
        } else{
            cd <- nombre_dm(cd, "dp", fd)
        }
    }
    invisible(cd)
}

## conversion unidades de medida
## produce data.frame
##   pr=datos transformado a la escala deseada
##   um=escala original actualizada a los cambios
## y dato original
## u escala original de los datos (unidad de medida)
## de unidad que se quiere convertir
## a  unidad a la que se quiere convertir
## fc factor de conversión de um_from a um_to
## e.g convertir_ume(y, u, "libra", "quintal", 0.01)
convertir_ume <- function(y, u, de, a, fc){
    mm <- match(u, de)
    if(any(ii <- !is.na(mm))){
        u[ii] <- a[mm[ii]]
        y[ii] <- y[ii] * fc[mm[ii]]
        message("conversiones ", sum(ii))
    } else message("no hay unidad-medida ", de)
    invisible(data.frame(um=u, pr=y, stringsAsFactors=F))
}

##== colores
## col2rgb("yellow")
## rgb(1.0, 1.0, 0.0)
## rgb(255, 255, 0.0, maxColorValue=255)
GetColorHexAndDecimal <- function(color){
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}
SetTextContrastColor <- function(color){
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}

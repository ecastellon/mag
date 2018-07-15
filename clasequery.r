#**OjO**
#-metodos genericos definidos en genericos.r
if(!isGeneric("datos"))
    sys.source("c:/eddy/code/r/genericos.r",
               envir=attach(NULL,name="genericos"))
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
xsql <- function(lak=NULL,cwh="",cord="",cby=""){
  sq<-""
  if(!is.null(lak)){
    z<-sapply(lak,
              function(y){
                if(!(is.null(y[["as"]])|length(y[["as"]])==0))
                  y[["k"]] <- paste(y[["k"]],y[["as"]],sep=" as ")
                paste(y[["a"]][2],y[["k"]],sep=".",collapse=",")
              })
    sq<-paste("select",paste(z,collapse=","),"from")
    z<-sapply(lak,function(y)paste(y[["a"]],collapse=" "))
    sq<-paste(sq,paste(z,collapse=","))
  }
  if(!(cwh==""|length(cwh)==0|is.null(cwh)))
    sq<-paste(sq,"where",cwh)
  if(cord!="")
    sq<-paste(sq,"order by",cord)
  if(cby!="")
    sq<-paste(sq,"group by",cby)
  invisible(sq)
}
#
dbOpen <- function(connstr){
    invisible(tryCatch(
                  odbcDriverConnect(connection=connstr),
                  error=function(e) message("\n...ERROR no se pudo conectar"),
                  warning=function(e) message("\n...ERROR de conexión"),
                  finally=function(e) NULL))
}
dbSqlserver <- function(server="mag",dbase="encuestas"){
    if(pmatch(server,"local",nomatch=0)==1){
        connstr <- paste0("DRIVER={SQL Server Native Client 11.0};",
                          "Server=(localDB)\\MSSQLLocalDB;",
                          "trusted_connection=yes;",
                          "Database=",dbase,";")
    } else{
        connstr <- paste0("DRIVER={SQL Server};Server=10.22.166.42;",
                          "Uid=erick;pwd=erick83$$$;Database=",dbase,";")
    }
    dbOpen(connstr)
}
#--odbcReConnect genera error cuando el Sql-server local
dbReOpen <- function(channel){
    if(inherits(channel,"RODBC")){
        kk <- try(odbcReConnect(channel),silent=TRUE)
        if(inherits(kk,"try-error")){
            kk <- dbOpen(conn=attr(channel,"connection.string"))
        }
    } else{
        kk <- NULL
        message("\n... ERROR-no-RODBC-channel")
    }
    invisible(kk)
}
dbTablas <- function(kk){
    invisible(sqlTables(kk,tableType="TABLE")[["TABLE_NAME"]])
}
dbColumnas <- function(kk,tt){
    invisible(sqlColumns(kk,tt)[["COLUMN_NAME"]])
}
dbQry <- function(kk,ss,reopen=TRUE){
    ww <- sqlQuery(kk,ss)
    odbcClose(kk)
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
tabladbf <- function(dir,tabla){
  kn <- condbf(dir)
  w <- sqlFetch(kn,tabla)
  odbcClose(kn)
  invisible(w)
}
tablaxls <- function(filexls,tabla,xlsx=F){
    if(xlsx)
        kn <- odbcConnectExcel2007(filexls)
    else
        kn <- odbcConnectExcel(filexls)
    w <- sqlFetch(kn,tabla)
    odbcClose(kn)
    invisible(w)
}
guardadbf <- function(datos,tabla,dir,vartipos){
  wd <- getwd()
  setwd(dir)
  kn <- condbf(getwd())
  sqlSave(kn,datos,tablename=tabla,rownames=F,varTypes=vartipos)
  odbcClose(kn)
  setwd(wd)
}
guardaxls <- function(datos,tabla,filexls,nomfilas=F,xlsx=T){
    if(xlsx)
        kn <- odbcConnectExcel2007(filexls,readOnly=F)
    else
        kn <- odbcConnectExcel(filexls,readOnly=F)
    sqlSave(kn,datos,tablename=tabla,rownames=nomfilas)
    odbcClose(kn)
}
borradbf <- function(dir,tabla){
  wd <- getwd()
  setwd(dir)
  kn <- condbf(getwd())
  sqlDrop(kn,tabla)
  odbcClose(kn)
  setwd(wd)
}

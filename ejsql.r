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
dbOpen <- function(connstr){
    invisible(tryCatch(
                  odbcDriverConnect(connection=connstr),
                  error=function(e) message("\n...ERROR no se pudo conectar"),
                  warning=function(e) message("\n...ERROR de conexión"),
                  finally=function(e) NULL))
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
tablaxls <- function(filexls,tabla,xlsx=F){
    if(xlsx)
        kn <- odbcConnectExcel2007(filexls)
    else
        kn <- odbcConnectExcel(filexls)
    w <- sqlFetch(kn,tabla)
    odbcClose(kn)
    invisible(w)
}
guardaxls <- function(datos,tabla,filexls,nomfilas=F,xlsx=T){
    if(xlsx)
        kn <- odbcConnectExcel2007(filexls,readOnly=F)
    else
        kn <- odbcConnectExcel(filexls,readOnly=F)
    sqlSave(kn,datos,tablename=tabla,rownames=nomfilas)
    odbcClose(kn)
}

                                        #lectura primera forma
  kdb <- dbReOpen(kdb)
  ff <- function(cul){
      cc <- paste0(c("s","u","sp","p","um","d","m"),cul)
      xsql(list(list(a=c("frutalesmarzo","a"),
                     k=c("quest",cc),
                     as=c("quest","sup","ums","spp","prod","ump",
                         "des","mes"))))
  }
  ss <- sapply(1:16,ff)
  cc <- paste(ss,collapse=" union all(")
  cs <- paste0(cc,")))))))))))))))")
  dd <- sqlQuery(kdb,cs,nullstring=0)
  odbcClose(kdb)
  cul <- c("piña","pita","nara","manda","limon","cit","agua",
           "bana","plata","papa","cala","grana","melon","sandi",
           "guay","mango")
  culti <- rep(cul,each=918)
  dd["cultivo"] <- culti
                                        #lectura segunda forma

  kdb <- dbReOpen(kdb)
  ff <- function(cul,nomb){
      cc <- paste0(c("s","u","sp","p","um","d","m"),cul)
      ss <- xsql(list(list(a=c("frutalesmarzo","a"),
                     k=c("quest",cc),
                     as=c("quest","sup","ums","spp","prod","ump",
                         "des","mes"))))
      dd <- sqlQuery(kdb,ss,nullstring=0)
      dd["cultivo"] <- nomb[cul]
      dd
  }
  cul <- c("piña","pita","nara","manda","limon","cit","agua",
           "bana","plata","papa","cala","grana","melon","sandi",
           "guay","mango")
  ld <- lapply(1:16,ff,cul)
  dd <- Reduce(rbind,ld)

# R programs for 
#    Commandeur, J.J.F. & Koopman, S.J. (2007)
#    "An Introduction to State Space Time Series Analysis,"
#    Oxford University Press.
#
# written by: 
#    S.Ono (mail: elsur2005[at]v003.vaio.ne.jp)
# 
# history: 
#    version 0.1 2014/10/02 (Chapter1-Chapter4.1)
#    version 0.2 2014/10/06 (Chapter1-Chapter8.2)
#    version 1.0 2014/10/11
#    version 1.1 2014/10/12
#
# notes:
#    my old-fashioned Hungarian prefixes: 
#      n  : iNteger
#      g  : numeric (g in "sinGle")
#      b  : binary  
#      a  : vector  (not sure where it comes from)
#      m  : matrix
#      l  : list
#      ts : ts object (Time Series)
#      o  : object of any class
#
# # # # # # # # # # # # # # # # # # #
#       some helper routines        #
# # # # # # # # # # # # # # # # # # #
# - - - - - - - - - - - - -
sub.LogLik <- function(tsNu, tsF, nStateVar){
  # purpose: get log likelihood.
  #          See Section 8.4
  # args:    tsNu:      (ts object)      time series of predicion error
  #          tsF:       (ts object)      time series of prediction error variance
  #          nStateVar: (numeric scalar) number of state variables
  # return:  (numeric scalar) log likelihood
  
  # trap: the length should be same
  stopifnot(length(tsF) == length(tsNu))
  
  # remove the first (nStateVar) timepoints
  agNu <- tsNu[-(1:nStateVar)]
  agF  <- tsF[-(1:nStateVar)]
  
  - 0.5 * length(tsNu) * log(2*pi) - 0.5 * sum( log(agF) + (agNu^2 / agF) )
}
# - - - - - - - - - - - - -- -
sub.AIC <- function(gLogLik, nStateVar, nHyperParam){
  # purpose: get AIC
  #          See Section 2.1.
  # args:    gLogLik:     (numeric scalar) log likelihood
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  # return:  (numeric scalar) AIC
  
  - 2 * gLogLik + 2 * ( nStateVar + nHyperParam )
  
}
# - - - - - - - - - - - - -- -
sub.BoxLjung <- function(agStdRes, nStateVar, nHyperParam, nLag=15){
  # purpose: Independence test for standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            stat:     (numeric scalar) Q(nLag) 
  #            critical: (numeric scalar) 5% critical value
  # notes:   Though the C&K book suggests all residuals can be used, 
  #          their ssfpack program removes the first (nStateVar) 
  #          residuals.
  
  # call Box.test
  gStat <- Box.test(
    agStdRes[-(1:nStateVar)], # remove the first (nStateVar) timepoints
    lag     = nLag, 
    type    = "Ljung-Box"
  )$statistic
  
  # get critical value
  gCritical <- qchisq(0.95, nLag-nHyperParam+1)
  
  list(
    stat     = gStat, 
    critical = gCritical
  )
}
# - - - - - - - - - - - - -- -
sub.ResidualAcf <- function(agStdRes, nStateVar, nLag=15){
  # purpose: autocorrelation of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            acf:      (numeric vector) acf (1 ... nLag)
  #            critical: (numeric scalar) 95% confidence limit
  # notes:   Though the C&K book suggests all residuals can be used, 
  #          their ssfpack program removes the first (nStateVar) 
  #          residuals.
  
  list(
    acf = acf(agStdRes[-(1:nStateVar)], plot=FALSE)$acf[-1],
    critical = 2 / sqrt(length(agStdRes))
  )
  
}
# - - - - - - - - - - - - -- -
sub.Homoscedasticity <- function(agStdRes, nStateVar){
  # purpose: Homoscedasticity test of standardized residuals
  #          See the C&K Book, Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            size:  (integer scalar) block size
  #            stat:  (numeric scalar) H(block size)
  #            upper: (numeric scalar) 5% critical value (upper)
  #            lower: (numeric scalar) 5% critical value (lower)
  
  # get leength
  n <- length(agStdRes)
  
  # defint blocksize
  nBlockSize <- round((n - nStateVar)/3)
  gValue     <- sum(agStdRes[(n-nBlockSize+1):n]^2) / 
                  sum(agStdRes[(nStateVar+1):(nStateVar+nBlockSize)]^2)
  
  list(
    size  = nBlockSize,
    stat  = gValue,
    upper = qf(0.975, nBlockSize, nBlockSize), 
    lower = qf(0.025, nBlockSize, nBlockSize)
  )
}
# - - - - - - - - - - - - -- -
sub.JarqueBera <- function(agStdRes, nStateVar){
  # purpose: Normality test of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            stat:     (numeric vector) statistic
  #            critical: (numeric vector) 5% critical value (=5.99)
  # notes:   Though the C&K book suggests all residuals can be used, 
  #          their ssfpack program removes the first (nStateVar) 
  #          residuals.
  
  require(tseries)
  
  list(
    stat     = jarque.bera.test(agStdRes[-(1:nStateVar)])$statistic, 
    critical = qchisq(0.95,2)
  )
}
# - - - - - - - - - - - - -- -
sub.ShowLogLik <- function(
  n, nStateVar, nHyperParam, 
  gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gLL.CK
){
  # purpose: show tables of LogLikelihood(LL) and AIC
  # args:    n:           (numeric scalar) length of time series
  #          nSteteVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          gLLbyFun.DLM:  (numeric scalar) LL computed by dlm
  #          gLLbyErr.DLM:  (numeric scalar) LL computed with predition error by dlm
  #          gLLbyFun.KFAS: (numeric scalar) LL computed by KFAS
  #          gLLbyErr.KFAS: (numeric scalar) LL computed with predition error by KFAS
  #          gLL.CK:      (numeric scalar) LL in the CK Book 
  # return:  NULL
  
  asTemplate <- c(
    "-----------------------------------------------------------------",
    "                   LogLik   [diff]     (/n)      AIC    (/n)",
    "-----------------------------------------------------------------",
    "dlm  (by pred.err) %8.4f            (%6.3f) %7.2f (%6.3f)",
    "     (by output)   %8.4f [%+8.3f] (%6.3f) %7.2f (%6.3f)", 
    "KFAS (by pred.err) %8.4f [%+8.3f] (%6.3f) %7.2f (%6.3f)", 
    "     (by output)   %8.4f [%+8.3f] (%6.3f) %7.2f (%6.3f)",
    "The CK Book        %8.4f [%+8.3f] (%6.3f) %7.2f (%6.3f)",
    "-----------------------------------------------------------------"
  )
  cat(sprintf(
    paste(asTemplate, collapse="\n"), 
    # DLM residual
    gLLbyErr.DLM, 
    gLLbyErr.DLM/n, 
    sub.AIC(gLLbyErr.DLM, nStateVar, nHyperParam),
    sub.AIC(gLLbyErr.DLM, nStateVar, nHyperParam)/n,
    # DLM function
    gLLbyFun.DLM, 
    gLLbyFun.DLM - gLLbyErr.DLM,
    gLLbyFun.DLM/n, 
    sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam),
    sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)/n,
    # KFAS residual
    gLLbyErr.KFAS, 
    gLLbyErr.KFAS - gLLbyErr.DLM,
    gLLbyErr.KFAS/n, 
    sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam),
    sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam)/n,
    # KFAS function
    gLLbyFun.KFAS, 
    gLLbyFun.KFAS - gLLbyErr.DLM,
    gLLbyFun.KFAS/n, 
    sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam),
    sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam)/n,
    # C&K book
    gLL.CK, 
    gLL.CK - gLLbyErr.DLM,
    gLL.CK/n, 
    sub.AIC(gLL.CK, nStateVar, nHyperParam),
    sub.AIC(gLL.CK, nStateVar, nHyperParam)/n
  ))
  cat("\n")
}
# - - - - - - - - - - - - -- -
sub.ShowDiagnostics <- function(
  agStdRes, nStateVar, nHyperParam, nMaxLag, anACLag
){
  # purpose: Show Diagnostics Table
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nMaxLag:     (integer scalar) max lag (for BoxLjung)
  #          anACLag:     (integer vector) two number of lag (for ACF)
  # return:  NULL
  
  # get various statistics
  lBoxLjung <- sub.BoxLjung(agStdRes, nStateVar, nHyperParam, nMaxLag)
  lACF      <- sub.ResidualAcf(agStdRes, nStateVar, nMaxLag)
  lHomo     <- sub.Homoscedasticity(agStdRes, nStateVar)
  lJB       <- sub.JarqueBera(agStdRes, nStateVar)
  
  # gHomoStat: H statistics, or its reciprocal
  gHomoStat <- ifelse(lHomo$stat > 1, lHomo$stat, 1/lHomo$stat)
  
  asTemplate <- c(
    "------------------------------------------------------",
    "                   stat     value  critical satisfied",
    "------------------------------------------------------",
    "independence      Q  (%2d)  %7.3f   %5.2f     %1s",  # BoxLJung, 4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "homoscedasticity  %-3s(%2d)  %7.3f   %5.2f     %1s", # Homo,     5 args
    "normality         N        %7.3f   %5.2f     %1s",   # N,        3 args
    "------------------------------------------------------"
  )
  
  cat(
    sprintf(
      paste(asTemplate, collapse="\n"), 
      # BoxLjung, 4 args
      nMaxLag,    
      lBoxLjung$stat,
      lBoxLjung$critical,
      ifelse(lBoxLjung$stat < lBoxLjung$critical, "+", "-"),
      # ACF, 4 args
      anACLag[1], 
      lACF$acf[anACLag[1]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[1]]) < lACF$critical, "+", "-"),
      # ACF, 4 args
      anACLag[2],
      lACF$acf[anACLag[2]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[2]]) < lACF$critical, "+", "-"),
      # Homo, 5 args
      ifelse(lHomo$stat > 1, "H", "1/H"),
      lHomo$size, 
      gHomoStat, 
      lHomo$upper, 
      ifelse(gHomoStat < lHomo$upper, "+", "-"),
      # N, 3 args
      lJB$stat,  
      lJB$critical,
      ifelse(lJB$stat < lJB$critical, "+", "-")
    )
  )
  cat("\n")
    
}
# - - - - - - - - - - - - -- -
sub.plotFigure_scatterreg <- function(tsData, formula, sTitle, asLegend){
  # purpose: plot scatter plot + regression line (e.g. Figure 1.1)
  # args:    tsData:   (ts object)      time series 
  #          formula:  (formula object) formula for lm()
  #          sTitle:   (string scalar)  title
  #          asLegend: (string vector)  legend of the points and the line
  # return:  NULL
  
  # trap
  stopifnot(is.ts(tsData))
  
  cat("plotting", sTitle, "...\n")
  plot(
    tsData, 
    type = "p",
    pch  = 3,
    xlab = NA, 
    ylab = NA, 
    main = sTitle
  )
  
  oOut.lm <- lm(formula)
  abline(oOut.lm, col="red")
  
  legend(
    "top", 
    legend = asLegend,
    lty = c(-1, 1),
    pch = c(3, -1), 
    col = c("black", "red"),
    cex = .8
  )
}
# - - - - - - - - - - - - -- -
sub.plotFigure_xyscatterreg <- function(tsX, tsY, formula, sTitle, asLegend){
  # purpose: plot x-y scatter plot + regression line (e.g. Figure 5.2)
  # args:    tsY:      (ts object)      time series, dependent variable
  #          tsX:      (ts object)      time series, independent variable
  #          formula:  (formula object) formula for lm()
  #          sTitle:   (string scalar)  title
  #          asLegend: (string vector)  legend of the points and the line
  # return:  NULL
  
  # trap
  stopifnot(is.ts(tsY))
  stopifnot(is.ts(tsX))
  stopifnot(length(tsX) == length(tsY))
  
  cat("plotting", sTitle, "...\n")
  plot(
    tsY, 
    tsX,
    type = "p",
    pch  = 3,
    xlab = NA, 
    ylab = NA, 
    main = sTitle
  )
  
  oOut.lm <- lm(formula)
  abline(oOut.lm, col="red")
  
  legend(
    "top", 
    legend = asLegend,
    lty    = c(-1, 1),
    pch    = c(3, -1), 
    col    = c("black", "red"),
    cex    = .8
  )
}
# - - - - - - - - - - - - -- -
sub.plotFigure_twoseries <- function(tsData1, tsData2, sTitle, asLegend){
  # purpose: plot time series + smoothed series (e.g. Figure 2.1)
  # args:    tsData1:  (ts object) observed time series (black line)
  #          tsData2:  (ts object) smoothed time series (red line)
  #          sTitle:   (string scalar) title
  #          asLegend: (string vector) legends of the lines
  # return:  NULL
  # notes:   Either tsData1 or tsData2 can be NULL.
  #          - When both tsData1 and tsData2 are specified, 
  #            asLegend should be c(label of tsData1, label of tsData2)
  #          - When only tsData1 are specified, 
  #            asLegend should be the label of tsData1
  #          - When only tsData2 are specified, 
  #            asLegend should be the label of tsData2
  #          time(tsData1) can be different with time(tsData2)
  
  # trap
  stopifnot(!is.null(tsData1) | !is.ts(tsData1))
  stopifnot(!is.null(tsData2) | !is.ts(tsData2))
  abExist <- c(
    !is.null(tsData1), 
    !is.null(tsData2)
  )
  stopifnot(any(abExist))
  stopifnot(sum(abExist) == length(asLegend))
  
  cat("plotting", sTitle, "...\n")
  # set frame
  plot(
    if (abExist[1]) { tsData1 } else { tsData2 }, 
    type = "n" ,
    xlab = NA, 
    ylab = NA,
    main = sTitle
  )
  # plot 1
  if (abExist[1]) {
    lines(tsData1, col = "black")
  }
  # plot 2
  if (abExist[2]) {
    lines(tsData2, col = "red")
  }
  
  legend(
    "top", 
    legend = asLegend,
    lty    = 1, 
    col    = c("black", "red"),
    cex    = .8
  )
  
}
# - - - - - - - - - - - - -- -
sub.plotFigure_interval <- function(tsData1, tsData2, tsWidth, sTitle, asLegend){
  # purpose: plot time series + smoothed series + interval (e.g. Figure 8.2)
  # args:    tsData1:  (ts object) observed time series (black line)
  #          tsData2:  (ts object) smoothed time series (red line)
  #          tsWidth:  (ts object) interval width
  #          sTitle:   (string scalar) title
  #          asLegend: (string vector) legends of the lines
  # return:  NULL
  # notes:   tsData1 can be NULL.
  #          - When both tsData1 and tsData2 are specified, 
  #            asLegend should be c(label of tsData1, label of tsData2)
  #          - When only tsData2 are specified, 
  #            asLegend should be the label of tsData2
  #          time(tsData2) should be equal to time(tsWidth)
  
  # trap
  if (!is.null(tsData1)){
    stopifnot(is.ts(tsData1))
    stopifnot(length(asLegend) == 2)
  } else {
    stopifnot(length(asLegend) == 1)
  }
  stopifnot(is.ts(tsData2))
  stopifnot(is.ts(tsWidth))
  stopifnot(time(tsData2) == time(tsWidth))
  
  cat("plotting", sTitle, "...\n")
  # set frame
  if (!is.null(tsData1)){
    plot(
      cbind(tsData1, tsData2+tsWidth, tsData2-tsWidth),
      plot.type = "single",
      type = "n",
      xlab = NA, 
      ylab = NA,
      main = sTitle
    )
  } else {
    plot(
      cbind(tsData2+tsWidth, tsData2-tsWidth),
      plot.type = "single",
      type = "n",
      xlab = NA, 
      ylab = NA,
      main = sTitle
    )
  }  

  # plot 1
  if (!is.null(tsData1)){
    lines(tsData1, col = "black", lty=1)
  }
  
  # plot 2
  lines(tsData2, col = "red", lty=3)
  lines(tsData2+tsWidth, col = "red")
  lines(tsData2-tsWidth, col = "red")

  if (!is.null(tsData1)){
    legend(
      "top", 
      legend = asLegend,
      lty    = c(1,3), 
      col    = c("black", "red"), 
      cex    = .8
    )
  } else {
    legend(
      "top", 
      legend = asLegend,
      lty    = 1, 
      col    = "red", 
      cex    = .8
    )
  }
}
# - - - - - - - - - - - - -- -
sub.plotFigure_residual <- function(tsResidual, sTitle, sLegend){
  # purpose: plot residuals (e.g. Figure 1.3)
  # args:    tsResidual: (ts object)     residual time series
  #          sTitle:     (string scalar) title
  #          sLegend:    (string scalar) legend of the line
  # return:  NULL
  
  stopifnot(is.ts(tsResidual))
  
  cat("plotting", sTitle, "...\n")
  plot(
    tsResidual,
    type  = "l",
    lty   = 2,
    col   = "green",
    xlab  = NA, 
    ylab  = NA,
    main  = sTitle
  )
  
  abline(h=0, col="gray")
  
  legend(
    "top", 
    lty    = 2, 
    legend = sLegend,
    col    = "green",
    cex    = .8
  )

}
# - - - - - - - - - - - - -- -
sub.plotFigure_auxresidual <- function(tsAuxResidual, sTitle, sLegend){
  # purpose: plot auxiliary residuals (e.g. Figure 8.11)
  # args:    tsAuxResidual: (ts object)     auxiliary residual time series
  #          sTitle:        (string scalar) title
  #          sLegend:       (string scalar) legend of the line
  # return:  NULL
  
  stopifnot(is.ts(tsAuxResidual))
  
  cat("plotting", sTitle, "...\n")
  plot(
    tsAuxResidual,
    type  = "l",
    lty   = 2,
    col   = "green",
    xlab  = NA, 
    ylab  = NA,
    main  = sTitle
  )
  
  abline(h=0, col="gray")
  abline(h=-1.96, col="red")
  abline(h=+1.96, col="red")
  
  legend(
    "top", 
    lty    = 2, 
    legend = sLegend,
    col    = "green",
    cex    = .8
  )
  
}
# - - - - - - - - - - - - -- -
sub.plotFigure_ACF <- function(tsData, nMaxLag, sTitle, sLegend){
  # purpose: plot a correlogram 
  # args:    tsData:     (ts object)      time series
  #          nMaxLag:    (integer scalar) max lag
  #          sTitle:     (string scalar)  title
  #          sLegend:    (string scalar)  legend of the line
  # return:  NULL
  
  stopifnot(is.ts(tsData))
  
  cat("plotting", sTitle, "...\n")
  acf(
    ts(tsData, frequency=1), 
    lag.max = nMaxLag,
    lwd     = 12, 
    col     = "green", 
    main    = sTitle,
    xlab    = NA, 
    ylab    = NA , 
    xlim    = c(1,nMaxLag)
  )
  
  legend(
    "top", 
    legend = sLegend, 
    lty    = 1, 
    col    = "green",
    cex    = .8
  ) 
  
}
# # # # # # # # # # # # # # # # # # #
#          main routines            #
# # # # # # # # # # # # # # # # # # #
# - - - - - - - - - - - - -- -
exercise.Chapter1 <- function(){
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]))
  
  # linear regression
  cat("Linear regression: \n")
  oOut.lm <- lm(tsData ~ time(tsData))
  print(oOut.lm)
  
  # plot
  sub.plotFigure_scatterreg(
    tsData, 
    tsData ~ time(tsData),
    "Figure 1.1", 
    c("log UK drivers KSI against time (in months)", "regression line")
  )
  sub.plotFigure_twoseries(
    tsData, 
    NULL, 
    "Figure 1.2", 
    "log UK drivers KSI"
  )
  sub.plotFigure_residual(
    tsData - predict(oOut.lm), 
    "Figure 1.3",
    "residuals"
  )
  sub.plotFigure_ACF(
    as.ts(rnorm(length(tsData))), 
    15, 
    "Figure 1.4", 
    "ACF-random residuals"
  )
  sub.plotFigure_ACF(
    tsData - predict(oOut.lm), 
    15, 
    "Figure 1.5", 
    "ACF-regression residuals"
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter2.1 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sDATAFILENAME   <- "UKdriversKSI.txt"
  gCKLogLik       <- 0.3297597  # AIC in C&K book
  # - - - - - - - - - - - 

  # read data
  dfData <- read.table(sDATAFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 1  # number of state variables
  nHyperParam <- 1  # number of hyper parameters
  
  # init value for (sigma^2_epsilon)
  gInit <- var(tsData)
  
  cat("\n") 
  
  cat("estimated by dlm package: \n")
  funModel <- function(parm){
    dlmModPoly(order = 1, dV = exp(parm), dW = 0)
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM      <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM     <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM     <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2]))
  cat("\n") 
  
  cat("estimated by KFAS package: \n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))), 
    H = matrix(NA)
  )
  # Fitting
  oFitted.KFAS <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  # Filtering & Smoothing
  oEstimated.KFAS   <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / t(sqrt(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS     <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS     <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM, 
    "Figure 2.1", 
    c("log UK drivers KSI", "deterministric level")
  )
  sub.plotFigure_residual(
    tsData - tsSmoState.DLM, 
    "Figure 2.2",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter2.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik    <- 0.6451960  # AIC in C&K book
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 1
  nHyperParam <- 2
  
  # init value for (sigma^2_epsilon, sigma^2_xi)
  agInit     <- c(var(tsData), 0.001) 
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM      <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM     <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM     <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.DLM$W)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))), 
    H=matrix(NA)
  )
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  # Filtering & Smoothing
  oEstimated.KFAS   <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS     <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS     <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.KFAS$model$Q)))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM, 
    "Figure 2.3", 
    c("log UK drivers KSI", "stochastic level")
  )
  sub.plotFigure_residual(
    tsData - tsSmoState.DLM, 
    "Figure 2.4",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter2.3 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "NorwayFinland.txt"
  gCKLogLik <- 0.8468622
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  anTime <- as.integer(dfData[,1])
  stopifnot(anTime == seq(from=anTime[1], to=anTime[1]+length(anTime)-1))
  tsData <- ts(log(dfData[,2]), start=anTime[1])
  n      <- length(tsData)

  # model specs
  nStateVar   <- 1
  nHyperParam <- 2
  
  # init value for (sigma^2_epsilon, sigma^2_xi)
  agInit <- c(var(tsData), 0.001)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM       <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM       <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM       <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.DLM$W)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))), 
    H=matrix(NA)
  )
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  # Filtering & Smoothing
  oEstimated.KFAS   <- KFS(oFitted.KFAS$model, filtering=c("state", "mean"))
  agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.KFAS$model$Q)))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm)\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 10, c(1,4)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM, 
    "Figure 2.5", 
    c("log fatalities in Norway", "stochastic level")
  )
  sub.plotFigure_residual(
    tsData - tsSmoState.DLM, 
    "Figure 2.6",
    "irregular"
  )
  
  # plot for section 8.5, Fig8.5 & Fig8.7 - - - - - 
  sub.plotFigure_twoseries(
    dropFirst(oSmoothed.DLM$s), 
    dropFirst(oFiltered.DLM$f), 
    "Figure 8.5", 
    c("smoothed level", "filtered level")
  )
  sub.plotFigure_twoseries(
    NULL, 
    dropFirst(lPredErr.DLM$res), 
    "Figure 8.7a", 
    "prediction errors"
  )
  sub.plotFigure_twoseries(
    NULL, 
    dropFirst(lPredErr.DLM$sd)^2, 
    "Figure 8.7b", 
    "prediction error variance"
  )
  
  # plot for section 8.6, Fig8.13 - - - - - 
  # observed 
  tsActual <- ts(c(tsData, rep(NA, 5)), start=anTime[1])
  # past
  agLevel.Past <- as.numeric(oFiltered.DLM$a)
  agLevelVar.Past   <- sapply(
    dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R), 
    function(x) x[1,1]
  )
  # future
  oForecast <- dlmForecast(oFiltered.DLM, nAhead = 5)
  agLevel.Future    <- as.numeric(oForecast$f)
  agLevelVar.Future <- unlist(oForecast$Q)

  ## cf. 4 possible sources of past data
  ##
  ## 1) filtered level by DLM
  ## print(oFiltered.DLM$a)  
  ## print(agLevelVar.Past)  
  ##
  ## 2) forecast by DLM
  ## print(oFiltered.DLM$f) # equal to 1) 
  ## not sure how to get variances, but it may be agLevelVar.Past
  ##
  ## 3) one step predictions of states by KFAS
  ## print(oEstimated.KFAS$a) 
  ## print(sapply(oEstimated.KFAS$P, function(x) drop(x)))
  ##
  ## 4) filtered estimate of signal by KFAS
  ## print(oEstimated.KFAS$m) 
  ## print(sapply(oEstimated.KFAS$P_mu, function(x) drop(x)))

  # filtered level
  tsLevel    <- ts(
    c(agLevel.Past, agLevel.Future), 
    start=anTime[1]
  )
  tsWidth    <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(c(agLevelVar.Past, agLevelVar.Future)),
    start=anTime[1]
  )
  sub.plotFigure_interval(
    tsActual, 
    dropFirst(tsLevel), 
    dropFirst(tsWidth), 
    "Figure 8.13", 
    c("log fatalities in Norway", "filtered level and forecasts")
  )

}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter3.1 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  gCKLogLik    <- 0.4140728
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 1
  
  # init value for sigma^2_epsilon
  gInit     <- var(tsData)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    dlmModPoly(order = 2, dV = exp(parm), dW = c(0,0))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=2, Q=list(matrix(0), matrix(0))), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)  -> states are {level, slope}
  # fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  # filtering & smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oEstimated.KFAS$alphahat[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter3.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.6247935
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, zeta)
  agInit     <- c(var(tsData),0.001,0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    dlmModPoly(order = 2, dV = exp(parm[1]), dW = exp(parm[2:3]))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA))), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are {level, slope}
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  # Filtering & Smoothing
  oEstimated.KFAS   <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agQ[1]))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oEstimated.KFAS$alphahat[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1],
    "Figure 3.1",
    c("log UK drivers KSI","stochastic level and slope")
  )
  sub.plotFigure_twoseries(
    NULL, 
    dropFirst(tsSmoState.DLM[,2]),
    "Figure 3.2",
    "stochastic slope"
  )
  sub.plotFigure_residual(
    tsData - tsSmoState.DLM[,1], 
    "Figure 3.3",
    "irregular"
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter3.3 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.6247935
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsData),0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(exp(parm[2]), 0))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM     <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM     <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=2, Q=list(matrix(NA), matrix(0))), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (level, slope)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agQ[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oEstimated.KFAS$alphahat[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1], 
    "Figure 3.4",
    c("log UK drivers KSI", "stochastic level and deterministic slope")
  )
 
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter3.4a <- function(){
  # the first paragraph of section 3.4
  # local linear trend model with stochastic level & slope
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "NorwayFinland.txt"
  gCKLogLik <- 0.7864746
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  anTime <- as.integer(dfData[,1])
  stopifnot(anTime == seq(from=anTime[1], to=anTime[1]+length(anTime)-1))
  tsData <- ts(log(dfData[,3]), start=anTime[1])
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, zeta)
  agInit     <- c(var(tsData),0.001,0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package: \n")
  funModel <- function(parm){
    dlmModPoly(order = 2, dV = exp(parm[1]), dW = exp(parm[2:3]))
  }
  # fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
   
  cat("estimated by KFAS package: \n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA))), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are {level, slopt}
  # fitting
  oFitted.KFAS <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[1]))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oEstimated.KFAS$alphahat[1,2]))
  cat("\n") 
   
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter3.4b <- function(){
  # the rest of section 3.4
  # local linear trend model with deterministic level & stochastic slope
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "NorwayFinland.txt"
  gCKLogLik <- 0.7864746
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  anTime <- as.integer(dfData[,1])
  stopifnot(anTime == seq(from=anTime[1], to=anTime[1]+length(anTime)-1))
  tsData <- ts(log(dfData[,3]), start=anTime[1])
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 2
  
  # init value for variances (epsilon, zeta)
  agInit     <- c(var(tsData),0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package: \n")
  funModel <- function(parm){
    dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(0, exp(parm[2])))
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM     <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package: \n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=2, Q=list(matrix(0), matrix(NA))), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (level, slope)
  # Fitting
  oFitted.KFAS <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_zeta)    = %f\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oEstimated.KFAS$alphahat[1,1]))
  cat(sprintf("hat(nu_1)            = %f\n", oEstimated.KFAS$alphahat[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 10, c(1,4)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1], 
    "Figure 3.5a",
    c("log fatalities Finland", "deterministic level, stochastic slope")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2],
    "Figure 3.5b",
    "stochastic slope"
  )
  sub.plotFigure_residual(
    tsData - tsSmoState.DLM[,1], 
    "Figure 3.6",
    "irregular"
  )
  
  # plot for section 8.5, Figure 8.14 - - - - - 
  # observed
  tsData.Obs <- ts(c(tsData, rep(NA, 5)), start=anTime[1])
  # variance in filter
  lFltVar           <- dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R)
  agLevelVar        <- sapply(lFltVar, function(x) x[1,1])
  # forecast
  oForecast         <- dlmForecast(oFiltered.DLM, nAhead = 5)
  agLevelVar.Future <- unlist(oForecast$Q)
  
  # filtered level
  tsData.Level    <- ts(
    c(oFiltered.DLM$f, oForecast$f),
    start=anTime[1]
  )
  # confidence interval
  tsData.Width    <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(c(agLevelVar, agLevelVar.Future)),
    start=anTime[1]
  )
  
  sub.plotFigure_interval(
    tsData.Obs, 
    window(tsData.Level, start=1972), 
    window(tsData.Width, start=1972), 
    "Figure 8.14", 
    c("log fatalities in Finland", "filtered trend and forecasts")
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter4.1 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.4174873
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 12
  nHyperParam <- 1
  
  # init value for variances (epsilon)
  gInit     <- c(var(tsData))  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm), dW=0)
    m2 <- dlmModSeas(12, dV = 0, dW=rep(0,11)) 
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1})        = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
             SSMseasonal(12, sea.type="dummy"), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (level, seasondummy1, seasondummy2, ...)
  ## print(oModel.KFAS$Q) -> Q is (1,1) matrix of 0
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS    <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1]+tsSmoState.DLM[,2], 
    "Figure 4.2",
    c("log UK drivers KSI", "deterministic level")
  )
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1], 
    "Figure 4.3",
    c("log UK drivers KSI", "deterministic level")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2], 
    "Figure 4.4",
    "deterministic seasonal"
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1]+tsSmoState.DLM[,2]), 
    "Figure 4.5",
    "irregular"
  )

}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter4.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.9369063
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 12
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, omega)
  agInit <- c(var(tsData), 0.001, 0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[3]), rep(0,10)))
    return( m1 + m2 )
  }
  ## print(funModel(log(c(123,456,789))))
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(12, sea.type="dummy", Q=matrix(NA), n=n), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (level, seasondummy1, seasondummy2, ...)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix where diag is (NA,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[1]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1], 
    "Figure 4.6",
    c("log UK drivers KSI", "stochastic level")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2], 
    "Figure 4.7",
    "stochastic seasonal"
  )
  sub.plotFigure_twoseries(
    NULL, 
    ts(window(tsSmoState.DLM[,2], start=c(1969,1), end=c(1969,12)), frequency = 1),
    "Figure 4.8",
    "seasonal 1969"
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1]+tsSmoState.DLM[,2]), 
    "Figure 4.9",
    "irregular"
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter4.3 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.9363361
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 12
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsData), 0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM    <- dlmFilter(tsData, oFitted.DLM)
  lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(12, sea.type="dummy"), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (level, seasondummy1, seasondummy2, ...)
  ## print(oModel.KFAS$Q)  -> Q is (1,1) matrix of NA
  # Fitting  
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit) 
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  # add disturbance smoothing to plot later
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing=c("state", "mean", "disturbance"))
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.KFAS$model$Q[,,1])))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  # plot for section 8.3 - - - - - - - -
  # variance of smoothed states
  lEstVar       <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)[-1]
  agLevelVar    <- sapply(lEstVar, function(x) x[1,1])
  agSeasonVar   <- sapply(lEstVar, function(x) x[2,2])
  agTotalVar    <- agLevelVar + agSeasonVar

  # plot
  # level variance
  sub.plotFigure_twoseries(
    NULL, 
    ts(agLevelVar, frequency=12, start=c(1969, 1)), 
    "Figure 8.1", 
    "level estimation error variance"
  )
  # level interval
  sub.plotFigure_interval(
    tsData, 
    tsSmoState.DLM[,1], 
    ts(
      qnorm(0.05, lower = FALSE) * sqrt(agLevelVar), 
      frequency=12, start=c(1969, 1)
    ), 
    "Figure 8.2", 
    c("log UK drivers KSI", "stochatic level +/- 1.64SE")
  )
  # season
  tsSeasonWidth <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(agSeasonVar), 
    frequency=12, start=c(1969, 1)
  )
  sub.plotFigure_interval(
    NULL, 
    window(tsSmoState.DLM[,2],  start=c(1981,1)),
    window(tsSeasonWidth, start=c(1981,1)), 
    "Figure 8.3", 
    c("deterministic seasonal +/- 1.64SE")
  )
  # signal
  tsTotalWidth <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(agTotalVar), 
    frequency=12, start=c(1969, 1)
  )
  sub.plotFigure_interval(
    NULL, 
    window(tsSmoState.DLM[,1]+tsSmoState.DLM[,2], start=c(1981,1)),
    window(tsTotalWidth, start=c(1981,1)), 
    "Figure 8.4 (by dlm)", 
    c("signal +/- 1.64SE")
  )

  # ploting Figure 8.4 again by KFAS - - - - - - - - - -
  tsSmoState.KFAS <- oEstimated.KFAS$muhat
  agTotalVar.KFAS <- sapply(oEstimated.KFAS$V_mu, function(x) drop(x))

  plot(
    ts(
      cbind(agTotalVar, agTotalVar.KFAS), 
      frequency=12, start=c(1969, 1)
    ), 
    plot.type="single", 
    col = c("black", "red"),
    ylab = "variance", 
    main = "Variance of smoothed signal"
  )
  legend(
    "top", 
    legend = c(
      "dlmSvd2var(dlmSmooth(...)$U.S, dlmSmooth(...)$D.S)", 
      "KFS(...)$V_mu"
    ),
    lty = c(1, 1),
    col = c("black", "red"),
    cex = .8
  )
  ## I compared these two series with the output of ssfpack 
  ## and found ssfpack is similar with KFAS, not with ldm
  ## print(agTotalVar[144:192])
  ## print(agTotalVar.KFAS[144:192])
  
  tsTotalWidth.KFAS <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(agTotalVar.KFAS), 
    frequency=12, start=c(1969, 1)
  )
  sub.plotFigure_interval(
    NULL, 
    window(tsSmoState.KFAS, start=c(1981,1)),
    window(tsTotalWidth.KFAS, start=c(1981,1)), 
    "Figure 8.4 (by KFAS)", 
    c("signal +/- 1.64SE")
  )

  # plot for section 8.5 - - - - - - - -
  sub.plotFigure_auxresidual(
    rstandard(oEstimated.KFAS, type="state"), 
    "Figure 8.11a", 
    "Structural level break t-tests"
  )
  sub.plotFigure_auxresidual(
    rstandard(oEstimated.KFAS, type="pearson"), 
    "Figure 8.11b", 
    "Outlier t-tests"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter4.4 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKinflation.txt"
  gCKLogLik <- 3.198464
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(dfData[,1], frequency=4, start=c(1950, 1))
  n      <- length(tsData)
  # plot(tsData)
  
  # model specs
  nStateVar   <- 4
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, gamma)
  agInit     <- c(var(tsData), 0.00001, 0.00001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(4, dV=0, dW=c(exp(parm[3]), rep(0,2)))
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %e\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_gamma)   = %e\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(mu_208)          = %f\n", oSmoothed.DLM$s[209,1]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(4, sea.type="dummy", Q=matrix(NA), n=n), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (level, seasondummy1, seasondummy2, seasondummy3)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix where diag is (NA,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %e\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[1]))
  cat(sprintf("hat(sigma^2_gamma)   = %e\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(gamma_208)       = %f\n", coef(oEstimated.KFAS)[208,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 10, c(1,4)
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1], 
    "Figure 4.10a",
    c("quarterly price changes in U.K.", "stochastic level")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2], 
    "Figure 4.10b",
    "stchastic seasonal"
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1]+tsSmoState.DLM[,2]), 
    "Figure 4.10c",
    "irregular"
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter5.1a <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.4140728
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  x      <- ts(as.numeric(1:n), frequency=12, start=c(1969, 1))
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 1
  
  # init value for variances (epsilon)
  gInit     <- c(var(tsData))  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
    m2 <- dlmModReg(x, dV=0, addInt = FALSE)
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
             SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (x, level)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix of 0
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter5.1b <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.4457201
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  dfXData <- read.table(sXFILENAME, skip=1)
  x <- ts(dfXData[,1], frequency=12, start=c(1969, 1))
  stopifnot (length(x) == n)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 1
  
  # init value for variances (epsilon)
  gInit     <- var(tsData)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order=1, dV=exp(parm[1]), dW=0)
    m2 <- dlmModReg(x, dV=0, addInt=FALSE)
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
             SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS) -> states are (x, level)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix of 0
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 

  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x, 
    "Figure 5.1",
    c("log UK driver KSI", "deterministic level + beta*log(PETROL PRICE)")
  )
  sub.plotFigure_xyscatterreg(
    tsData, 
    x, 
    tsData ~ x,
    "Figure 5.2", 
    c(
      "log UK driver KSI againsg log PETROL PRICE", 
      "deterministic level + beta*log(PETROL PRICE)"
    )
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x), 
    "Figure 5.3",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter5.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.6456361
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  dfXData <- read.table(sXFILENAME, skip=1)
  x <- ts(dfXData[,1], frequency=12, start=c(1969, 1))
  stopifnot (length(x) == n)
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  gInit     <- c(var(tsData), 0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModReg(x, dV=0, addInt=FALSE)
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
      SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x, level)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix where diag is (0, NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x, 
    "Figure 5.4",
    c("log UK driver KSI", "stochastic level + beta*log(PETROL PRICE)")
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x), 
    "Figure 5.5",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter6.1 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  gCKLogLik    <- 0.4573681
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  x <- ts(rep(0, n), frequency=12, start=c(1969, 1))
  x[170:n] <- 1
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 1
  
  # init value for variances (epsilon)
  gInit <- var(tsData)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm), dW=0)
    m2 <- dlmModReg(x, dV=0, addInt = FALSE)
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(lambda_1)        = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
             SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x, level)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix of 0
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method ="BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(lambda_1)        = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x, 
    "Figure 6.1",
    c("log UK driver KSI", "deterministic level + lambda*(SEATBELT LAW)")
  )
  sub.plotFigure_xyscatterreg(
    tsData, 
    x, 
    tsData ~ x,
    "Figure 6.2", 
    c(
      "log UK driver KSI against SEATBELT LAW", 
      "deterministic level + lambda*(SEATBELT LAW)"
    )
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x), 
    "Figure 6.3",
    "irregular"
  )
  
  # comparison of prediction error variances between ldm and KFAS
  cat("making additional chart ... \n")
  agPEV.DLM <- as.numeric(lPredErr.DLM$sd^2)
  agPEV.DLM[agPEV.DLM > 1] <- NA
  agPEV.KFAS <- as.numeric(t(oEstimated.KFAS$F))
  tsPEV <- ts(cbind(agPEV.DLM, agPEV.KFAS), frequency=12, start=c(1969, 1))
  colnames(tsPEV) <- c("ldm", "KFAS")
  plot(tsPEV, main="prediction error variance")
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter6.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  gCKLogLik    <- 0.6630851
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  x <- ts(rep(0, n), frequency=12, start=c(1969, 1))
  x[170:n] <- 1
  
  # model specs
  nStateVar   <- 2
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsData), 0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModReg(x, dV=0, addInt = FALSE)
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(lambda_1)        = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x, level)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix where diag is (0, NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(lambda_1)        = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x, 
    "Figure 6.4",
    c("log UK driver KSI", "stochastic level + lambda*(SEATBELT LAW)")
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] * x), 
    "Figure 6.5",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter7.1 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.8023777
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x2 <- rep(0, n)
  x2[170:n] <- 1
  X <- ts(
    cbind(dfX1Data[,1], x2),
    frequency=12, start=c(1969, 1)
  )
  colnames(X) <- c("x1", "x2")
  
  # model specs
  nStateVar   <- 14
  nHyperParam <- 1
  
  # init value for variances (epsilon)
  gInit  <- var(tsData)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm), dW=0)
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    m3 <- dlmModReg(X, dV=0, addInt=FALSE)
    return( m1 + m2 + m3)
  }
  ## print(funModel(log(123)))
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(gInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM  <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[2,13]))
  cat(sprintf("hat(lambda_1)        = %f\n", oSmoothed.DLM$s[2,14]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
             SSMseasonal(12, sea.type="dummy") + 
             SSMregression(~ -1 + X, Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x1, x2, level, seasondummies)
  ## print(oModel.KFAS$Q) -> Q is (2,2) matrix where diag is (0, 0)
  # fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(gInit), 
    method = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,4]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(lambda_1)        = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + (tsSmoState.DLM[,13] * X[,1]) + (tsSmoState.DLM[,14] * X[,2]), 
    "Figure 7.1",
    c("log UK driver KSI", "det.level+beta*log(PETROL PRICE)+lambda*(SEATBELT LAW)")
  )
  
  # plot for section 7.3
  sub.plotFigure_ACF(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] + (tsSmoState.DLM[,13] * X[,1]) + (tsSmoState.DLM[,14] * X[,2])), 
    15, 
    "Figure 7.5", 
    "ACF-det.level and seasonal model residuals"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter7.2 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.9825225
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x2 <- rep(0, n)
  x2[170:n] <- 1
  X <- ts(
    cbind(dfX1Data[,1], x2),
    frequency=12, start=c(1969, 1)
  )
  colnames(X) <- c("x1", "x2")
  
  # model specs
  nStateVar   <- 14
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, omega)
  agInit     <- c(var(tsData), 0.0001, 0.0001)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[3]), rep(0,10)))
    m3 <- dlmModReg(X, dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  ## print(funModel(log(c(123, 456, 789))))
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[1,13]))
  cat(sprintf("hat(lambda_1)        = %f\n", oSmoothed.DLM$s[1,14]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(12, sea.type="dummy", Q=matrix(NA), n=n) + 
             SSMregression(~ -1 + X, Q=matrix(0, ncol=2, nrow=2)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x1, x2, level, seasondummies)
  ## print(oModel.KFAS$Q) -> Q is (4,4) matrix where diag is (0,0,NA,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[3]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agQ[4]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,4]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(lambda_1)        = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 15, c(1,12)
  )
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + (tsSmoState.DLM[,13] * X[,1]) + (tsSmoState.DLM[,14] * X[,2]), 
    "Figure 7.2",
    c("log UK driver KSI", "sto.level+beta*log(PETROL PRICE)+lambda*(SEATBELT LAW)")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2], 
    "Figure 7.3",
    c("stochastic seasonal")
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] + (tsSmoState.DLM[,13] * X[,1]) + (tsSmoState.DLM[,14] * X[,2])), 
    "Figure 7.4",
    "irregular"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter7.3 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.9798650
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x2 <- rep(0, n)
  x2[170:n] <- 1
  X <- ts(
    cbind(dfX1Data[,1], x2),
    frequency=12, start=c(1969, 1)
  )
  colnames(X) <- c("x1", "x2")
  
  # model specs
  nStateVar   <- 14
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsData), 0.001)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    m3 <- dlmModReg(X, dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  ## print(funModel(log(c(123, 456, 789))))
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[2,13]))
  cat(sprintf("hat(lambda_1)        = %f\n", oSmoothed.DLM$s[2,14]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(12, sea.type="dummy") + 
             SSMregression(~ -1 + X, Q=matrix(0, ncol=2, nrow=2)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x1, x2, level, seasondummies)
  ## print(oModel.KFAS$Q) -> Q is (3,3) matrix where diag is (0,0,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  # add disturbance smoothing to plot later
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing=c("state", "mean", "disturbance"))
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[3]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,4]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(lambda_1)        = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  # plot
  sub.plotFigure_ACF(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] + (tsSmoState.DLM[,13] * X[,1]) + (tsSmoState.DLM[,14] * X[,2])), 
    15, 
    "Figure 7.6", 
    "ACF-sto.level and det.seasonal model residuals"
  )
  
  # plot for section 8.5
  sub.plotFigure_twoseries(
    NULL, 
    window(agStdPredErr.DLM, start=c(1970,3)),
    "Figure 8.8",
    "standardized one-step prediction errors"
  )
  sub.plotFigure_ACF(
    window(agStdPredErr.DLM, start=c(1970,3)), 
    10, 
    "Figure 8.9", 
    "ACF-standardized one-step prediction errors"
  )
  cat("making Figure 8.10 ... \n")
  hist(
    agStdPredErr.DLM[-(1:nStateVar)], 
    breaks=17,
    main = "Figure 8.10"
  )
  
  sub.plotFigure_auxresidual(
    rstandard(oEstimated.KFAS, type="state"), 
    "Figure 8.12a", 
    "Structural level break t-tests"
  )
  sub.plotFigure_auxresidual(
    rstandard(oEstimated.KFAS, type="pearson"), 
    "Figure 8.12b", 
    "Outlier t-tests"
  )
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter7.4 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKinflation.txt"
  gCKLogLik <- 3.305023
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  tsData <- ts(dfData[,1], frequency=4, start=c(1950, 1))
  n      <- length(tsData)
  
  x1 <- ts(rep(0, n), frequency=4, start=c(1950, 1))
  x2 <- ts(rep(0, n), frequency=4, start=c(1950, 1))
  x1[which(time(x1) == 1975.25)] <- 1
  x2[which(time(x2) == 1979.50)] <- 1
  X <- cbind(x1, x2)
  
  # model specs
  nStateVar   <- 6
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, gamma)
  agInit     <- c(var(tsData), 0.00001, 0.00001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(4, dV=0, dW=c(exp(parm[3]), rep(0,2)))
    m3 <- dlmModReg(X, dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  # print(funModel(log(c(123,456,789))))
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*n*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %e\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_gamma)   = %e\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(lambda1_1)       = %f\n", oSmoothed.DLM$s[2,5]))
  cat(sprintf("hat(lambda2_1)       = %f\n", oSmoothed.DLM$s[2,6]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
             SSMseasonal(4, sea.type="dummy", Q=matrix(NA), n=n) + 
             SSMregression(~ -1 + X, Q=matrix(0, ncol=2, nrow=2)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   -> states are (x1, x2, level, seasondummies)
  ## print(oModel.KFAS$Q) -> Q is (3,3) matrix where diag is (0,0,NA,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %e\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[3]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agQ[4]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,4]))
  cat(sprintf("hat(lambda1_1)       = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(lambda1_1)       = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    n, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*n
  )
  cat("\n") 
  
  cat("diagnostics of standardized prediction error (estimated by dlm):\n")
  sub.ShowDiagnostics (
    agStdPredErr.DLM, nStateVar, nHyperParam, 10, c(1,4)
  )
  
  # plot
  sub.plotFigure_twoseries(
    tsData, 
    tsSmoState.DLM[,1] + tsSmoState.DLM[,5] * X[,1] + tsSmoState.DLM[,6] * X[,2], 
    "Figure 7.7a",
    c("quarterly price changes in U.K.", "sto.level+pulse intervention variables")
  )
  sub.plotFigure_twoseries(
    NULL, 
    tsSmoState.DLM[,2], 
    "Figure 7.7b",
    "stochastic seasonal"
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1] + tsSmoState.DLM[,2] + tsSmoState.DLM[,5] * X[,1] + tsSmoState.DLM[,6] * X[,2]), 
    "Figure 7.7c",
    "irregular"
  )
   
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter8.6a <- function(){
  # log UK driver KSI before SEAT-BELT law
  # stochastic level, stochastic season, log UK petrol price
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.9556575
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  # complete data
  ## tsData <- ts(log(dfData[1:169,1]), frequency=12, start=c(1969, 1))
  ## n      <- length(tsData)
  # short data
  tsShort <- ts(log(dfData[1:169,1]), frequency=12, start=c(1969, 1))
  nShort  <- length(tsShort)
  # explanatory data
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x <- ts(dfX1Data[,1], frequency=12, start=c(1969, 1))
  
  # model specs
  nStateVar   <- 14
  nHyperParam <- 3
  
  # init value for variances (epsilon, xi, omega)
  agInit     <- c(var(tsShort), 0.0001, 0.0001)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[3]), rep(0,10)))
    m3 <- dlmModReg(x[1:nShort], dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  ## print(funModel(log(c(123, 456, 789))))
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsShort, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsShort, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsShort, oFitted.DLM)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*nShort*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agW[2]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[1,13]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsShort ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
      SSMseasonal(12, sea.type="dummy", Q=matrix(NA), n=n) + 
      SSMregression(~ -1 + x[1:nShort], Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   # -> states are (x, level, seasondummies)
  ## print(oModel.KFAS$Q) # -> Q is (3,3) matrix where diag is (0,NA,NA)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[2]))
  cat(sprintf("hat(sigma^2_omega)   = %e\n", agQ[3]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    nShort, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*nShort
  )
  cat("\n") 
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter8.6b <- function(){
  # log UK driver KSI before SEAT-BELT law
  # stochastic level, deterministic season, log UK petrol price
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  gCKLogLik    <- 0.9555823
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  # complete data
  ## tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  ## n      <- length(tsData)
  # short data
  tsShort    <- ts(
    log(dfData[1:169,1]), 
    frequency=12, start=c(1969, 1)
  )
  nShort     <- length(tsShort)
  # explanatory data
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x <- ts(dfX1Data[,1], frequency=12, start=c(1969, 1))
  
  # model specs
  nStateVar   <- 13
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsShort), 0.0001)
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    m3 <- dlmModReg(x[1:nShort], dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsShort, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM    <- funModel(oMLE.DLM$par)
  # Filtering
  oFiltered.DLM <- dlmFilter(tsShort, oFitted.DLM)
  agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
  lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)  
  # Smoothing
  oSmoothed.DLM <- dlmSmooth(tsShort, oFitted.DLM)
  # LL
  gLLbyFun.DLM    <- -oMLE.DLM$value - 0.5*nShort*log(2*pi)
  gLLbyErr.DLM    <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar)
  cat("LL by dlm (to compare with exercise.Chapter8.6c()):", oMLE.DLM$value, "\n")
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat(sprintf("hat(beta_1)          = %f\n", oSmoothed.DLM$s[1,13]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsShort ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
      SSMseasonal(12, sea.type="dummy", Q=matrix(0), n=n) + 
      SSMregression(~ -1 + x[1:nShort], Q=matrix(0)), 
    H=matrix(NA)
  )
  ## print(oModel.KFAS)   # -> states are (x, level, seasondummies)
  ## print(oModel.KFAS$Q) # -> Q is (3,3) matrix where diag is (0,NA,0)
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, simplify=FALSE)
  agStdPredErr.KFAS  <- rstandard(oEstimated.KFAS, type="recursive")
  # LL
  gLLbyFun.KFAS    <- oEstimated.KFAS$logLik
  gLLbyErr.KFAS    <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar)
  cat("LL by KFAS (to compare with exercise.Chapter8.6c()):", oEstimated.KFAS$logLik, "\n")
  # report
  agQ            <- diag(oFitted.KFAS$model$Q[,,1])
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[2]))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,3]))
  cat(sprintf("hat(beta_1)          = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat("\n") 
  
  cat("max loglikelihood and AIC:\n")
  sub.ShowLogLik (
    nShort, nStateVar, nHyperParam, 
    gLLbyFun.DLM, gLLbyErr.DLM, gLLbyFun.KFAS, gLLbyErr.KFAS, gCKLogLik*nShort
  )
  cat("\n") 
  
  # plot
  oModelNew.KFAS <- SSModel(
    rep(NA, length(x[-(1:nShort)])) ~ SSMtrend(degree=1, Q=list(matrix(0))) + 
      SSMseasonal(12, sea.type="dummy", Q=matrix(0)) + 
      SSMregression(~ -1 + x[-(1:nShort)], Q=matrix(0))
  )
  oModelNew.KFAS$H <- oFitted.KFAS$model$H
  oModelNew.KFAS$Q <- oFitted.KFAS$model$Q
  tsPredict.KFAS <- predict(
    oFitted.KFAS$model, 
    oModelNew.KFAS, 
    interval = "confidence", 
    level = 0.90
  )  
  sub.plotFigure_interval(
    NULL, 
    ts(tsPredict.KFAS[,1], frequency = 12, start=c(1983, 1)),
    ts(tsPredict.KFAS[,1]-tsPredict.KFAS[,2], frequency = 12, start=c(1983, 1)),
    "Figure 8.15 (by predict() of KFAS)", 
    c("forecasts +/- 1.64SE")
  )

}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter8.6c <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME    <- "UKdriversKSI.txt"
  sXFILENAME   <- "logUKpetrolprice.txt"
  # - - - - - - - - - - - 
  
  # reading data
  dfData <- read.table(sFILENAME, skip=1)
  # complete data
  ## tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
  ## n      <- length(tsData)
  # data with missing
  tsWithMiss <- ts(
    c( log(dfData[1:169,1]), rep(NA,nrow(dfData)-169) ), 
    frequency=12, start=c(1969, 1)
  )
  nWithMiss  <- length(tsWithMiss)
  # explanatory data
  dfX1Data <- read.table(sXFILENAME, skip=1)
  x <- ts(dfX1Data[,1], frequency=12, start=c(1969, 1))
  
  # model specs
  nStateVar   <- 13
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsWithMiss, na.rm=TRUE), 0.0001)
  
  cat("\n")
  
  # dlm - - - - - 
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    m3 <- dlmModReg(x, dV=0, addInt=FALSE)
    return( m1 + m2 + m3 )
  }
  # fitting
  oMLE.DLM <- dlmMLE(
    tsWithMiss, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM  <- funModel(oMLE.DLM$par)
  cat("LL by dlm (to compare with exercise.Chapter8.6b()):", oMLE.DLM$value, "\n")
  # filtering
  oFiltered.DLM <- dlmFilter(tsWithMiss, oFitted.DLM)
  # get variances
  lFltVar       <- dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R)
  agLevelVar    <- sapply(lFltVar, function(x) x[1,1])
  agSeasonVar   <- sapply(lFltVar, function(x) x[2,2])
  agRegVar      <- sapply(lFltVar, function(x) x[13,13])
  agSignalVar.DLM <- agLevelVar + agSeasonVar + x * agRegVar
  
  # KFAS - - - - - 
  oModel.KFAS <- SSModel(
    tsWithMiss ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
      SSMseasonal(12, sea.type="dummy", Q=matrix(0), n=n) + 
      SSMregression(~ -1 + x, Q=matrix(0)), 
    H=matrix(NA)
  )
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit)
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering 
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering=c("mean", "state"), smoothing="none")
  cat("LL by KFAS (to compare with exercise.Chapter8.6b()):", oEstimated.KFAS$logLik, "\n")
  # get variances
  agSignalVar.KFAS <- sapply(oEstimated.KFAS$P_mu, function(x) drop(x))
  
  tsSignalVar <- ts(
    cbind(agSignalVar.DLM, agSignalVar.KFAS), 
    frequency = 12, start=c(1969, 1)
  )
  plot(
    window(tsSignalVar, start=c(1975, 1)), 
    plot.type="single", 
    main = "variance of filtered signal", 
    ylab = "variance",
    col = c("black", "red")
  )
  legend(
    "top", 
    legend = c("dlm (maybe misspecified)", "KFAS"),
    lty = c(1, 1),
    col = c("black", "red"),
    cex = .8
  )
  
  tsTotal.Signal <- oEstimated.KFAS$m
  tsTotal.Width <- ts(
    qnorm(0.05, lower = FALSE) * sqrt(agSignalVar.KFAS),
    frequency = 12, start=c(1969, 1)
  )
  sub.plotFigure_interval(
    window(tsWithMiss, start=c(1981, 1)),
    window(tsTotal.Signal, start=c(1981,1)), 
    window(tsTotal.Width, start=c(1981,1)), 
    "Figure 8.15 (filtering by KFAS)", 
    c("log UK drivers KSI", "forecasts +/- 1.64SE")
  )
  
}
# - - - - - - - - - - - - - - - - - 
exercise.Chapter8.7 <- function(){
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKdriversKSI.txt"
  gCKLogLik <- 0.9363361
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  agData <- log(dfData[,1])
  agData[c(48:62, 120:140)] <- NA
  tsData <- ts(agData, frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  # model specs
  nStateVar   <- 12
  nHyperParam <- 2
  
  # init value for variances (epsilon, xi)
  agInit     <- c(var(tsData, na.rm=TRUE), 0.001)  
  
  cat("\n")
  
  cat("estimated by dlm package:\n")
  funModel <- function(parm){
    m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
    m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
    return( m1 + m2 )
  }
  # Fitting
  oMLE.DLM <- dlmMLE(
    tsData, 
    parm  = log(agInit), 
    build = funModel
  )
  stopifnot(oMLE.DLM$convergence == 0)
  oFitted.DLM <- funModel(oMLE.DLM$par)
  # Smoothing
  oSmoothed.DLM    <- dlmSmooth(tsData, oFitted.DLM)
  tsSmoState.DLM   <- dropFirst(oSmoothed.DLM$s)
  # report
  agW           <- diag(oFitted.DLM$W)
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", agW[1]))
  cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", oSmoothed.DLM$s[2,2]))
  cat("\n") 
  
  cat("estimated by KFAS package:\n")
  oModel.KFAS <- SSModel(
    tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + 
      SSMseasonal(12, sea.type="dummy"), 
    H=matrix(NA)
  )
  # Fitting  
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = log(agInit) 
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model)
  # report
  cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))
  cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.KFAS$model$Q[,,1])))
  cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
  cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))
  cat("\n") 
  
  # variance of smoothed states
  lEstVar       <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)[-1]
  agLevelVar    <- sapply(lEstVar, function(x) x[1,1])
  agSeasonVar   <- sapply(lEstVar, function(x) x[2,2])
  
  tsState.KFAS <- oEstimated.KFAS$alphahat
  agLevelVar.KFAS  <- oEstimated.KFAS$V[1,1,]
  agSeasonVar.KFAS <- oEstimated.KFAS$V[2,2,]
  agSignalVar.KFAS <- sapply(oEstimated.KFAS$V_mu, function(x) drop(x))
  
  # plot
  # level variance
  sub.plotFigure_twoseries(
    NULL, 
    ts(agLevelVar, frequency=12, start=c(1969, 1)), 
    "Figure 8.17 (by dlm)", 
    "level estimation error variance"
  )
  # level interval
  sub.plotFigure_interval(
    tsData, 
    tsSmoState.DLM[,1], 
    ts(
      qnorm(0.05, lower = FALSE) * sqrt(agLevelVar), 
      frequency=12, start=c(1969, 1)
    ), 
    "Figure 8.18 (by dlm)", 
    c("log UK drivers KSI", "stochatic level +/- 1.64SE")
  )
  # season
  sub.plotFigure_twoseries(
    NULL, 
    ts(
      agSeasonVar, 
      frequency=12, start=c(1969, 1)
    ), 
    "Figure 8.19 (var(seasonal) by dlm)", 
    "seasonal estimation error variance"
  )
  sub.plotFigure_twoseries(
    NULL, 
    ts(
      agSeasonVar.KFAS, 
      frequency=12, start=c(1969, 1)
    ),
    "Figure 8.19 (var(seasonal) by KFAS)", 
    "seasonal estimation error variance"
  )
  sub.plotFigure_twoseries(
    NULL, 
    ts(
      agSignalVar.KFAS - agLevelVar.KFAS, 
      frequency=12, start=c(1969, 1)
    ),
    "Figure 8.19 (var(signal)-var(level) by KFAS)", 
    "seasonal estimation error variance"
  )
  
  # season interval
  sub.plotFigure_interval(
    NULL, 
    window(tsSmoState.DLM[,2], start=c(1971,1), end=c(1975,1)),
    window(
      ts(
        qnorm(0.05, lower = FALSE) * sqrt(agSeasonVar), 
        frequency=12, start=c(1969, 1)
      ),
      end=c(1975,1)
    ),
    "Figure 8.20 (by dlm)", 
    "deterministic seasonal +/- 1.64SE"
  )
  sub.plotFigure_residual(
    tsData - (tsSmoState.DLM[,1]+tsSmoState.DLM[,2]), 
    "Figure 8.21 (by dlm)",
    "irregular"
  )
}
exercise.Chapter9.4a <- function() {
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKfrontrearseatKSI.txt"
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  colnames(dfData) <- c("agDriver", "agFront", "agRear", "agKilo", "agPetrol")
  dfData$abSeatbelt <- 0
  dfData$abSeatbelt[170:nrow(dfData)] <- 1
  tsData <- ts(dfData, frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  cat("\n")
  
  cat("estimated by KFAS package:\n")
  
  oModel.KFAS <- SSModel(
    cbind(agFront, agRear) ~ SSMtrend(degree=1, type="distinct", Q=matrix(NA, nrow=2, ncol=2))
     + SSMseasonal(12, sea.type="dummy")
     -1 + agKilo + agPetrol + abSeatbelt, 
    H=matrix(NA, nrow=2, ncol=2),
    data = tsData
  )
  ## print(oModel.KFAS)   # -> states are (level*2, seasondummy12*2 ...)
  ## print(t(oModel.KFAS$Z[,,1]))
  
  # Fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits  = rep(0, 8), 
    method = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  
  cat("estimated H matrix:\n")
  print(oFitted.KFAS$model$H[,,1])
  cat("\n")
  
  cat("estimated Q matrix:\n")
  print(oFitted.KFAS$model$Q[,,1])
  cat("\n")
  
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing=c("state", "mean", "disturbance"))
  
  cat("correlation between level disturbance:\n")
  print(cor(oEstimated.KFAS$etahat[,1], oEstimated.KFAS$etahat[,2]))
  cat("\n")
  
  cat("regresssion coefficients:\n")
  print(as.matrix(oEstimated.KFAS$alphahat[1,1:6]))
  cat("\n")
  
  # plot 
  cat("making Figure 9.1 ... \n")
  plot(tsData, main = "Figure 9.1")
  
  cat("making Figure 9.2 ... \n")
  plot(
    as.numeric(oEstimated.KFAS$etahat[,2]), 
    as.numeric(oEstimated.KFAS$etahat[,1]), 
    xlab = "eta_Rear", ylab = "eta_Front", 
    main = "Figure 9.2"
  )
  abline(h=0)
  abline(lm(oEstimated.KFAS$etahat[,1] ~ oEstimated.KFAS$etahat[,2]))
  
  cat("making Figure 9.3 ... \n")
  plot(
    oEstimated.KFAS$alphahat[,7:8], 
    main = "Figure 9.3"
  )
  
  cat("making Figure 9.4 ... \n")
  plot(
    as.numeric(oEstimated.KFAS$alphahat[,8]), 
    as.numeric(oEstimated.KFAS$alphahat[,7]), 
    xlab = "level_Rear", ylab = "level_Front", 
    main = "Figure 9.4"
  )
  abline(lm(oEstimated.KFAS$alphahat[,7] ~ oEstimated.KFAS$alphahat[,8]))
}
# - - - - - - - - - -
exercise.Chapter9.4b <- function() {
  
  library(dlm)
  library(KFAS)
  
  # constants - - - - - -
  sFILENAME <- "UKfrontrearseatKSI.txt"
  # - - - - - - - - - - - 
  
  # read data
  dfData <- read.table(sFILENAME, skip=1)
  colnames(dfData) <- c("agDriver", "agFront", "agRear", "agKilo", "agPetrol")
  dfData$abSeatbelt <- 0
  dfData$abSeatbelt[170:nrow(dfData)] <- 1
  tsData <- ts(dfData, frequency=12, start=c(1969, 1))
  n      <- length(tsData)
  
  cat("\n")
  
  cat("estimated by KFAS package:\n")
  
  oModel.KFAS <- SSModel(
    cbind(agFront,agRear) ~ -1 
       + agPetrol 
       + agKilo
       + SSMregression(~-1+abSeatbelt, data=tsData, index=1)
       + SSMseasonal(period=12, sea.type='dummy')
       + SSMcustom(
           Z     = diag(2),
           T     = diag(2),
           R     = matrix(1,nrow=2,ncol=1),
           Q     = matrix(1),
           P1inf = diag(2)
         )
       ,
    data = tsData,
    H = matrix(NA, ncol=2, nrow=2)
  )
  # see manual of KFS()
  funUpdate <- function(pars, model, ...){
    model$H[,,1]   <- exp(0.5*pars[1:2])
    model$H[1,2,1] <- model$H[2,1,1] <- tanh(pars[3])*prod(sqrt(exp(0.5*pars[1:2])))
    model$R[28:29] <- exp(pars[4:5])
    ## print(model$H[,,1])
    ## print(model$R[28:29])
    model
  }
  # fitting
  oFitted.KFAS   <- fitSSM(
    oModel.KFAS, 
    inits      = c(-7, -7, 1, -1, -3),  # i'm not sure how we can get it ...
    updatefn  = funUpdate, 
    method    = "BFGS"
  )
  stopifnot(oFitted.KFAS$optim.out$convergence == 0)
  
  cat("estimated H matrix:\n")
  print(oFitted.KFAS$model$H[,,1])
  cat("\n")
  
  cat("estimated R matrix for custom disturbance:\n")
  print(oFitted.KFAS$model$R[28:29,,1])
  agQMatrix <- oFitted.KFAS$model$R[28:29,,1]%*%t(oFitted.KFAS$model$R[28:29,,1])
  cat("\n")
  
  cat("shown as Q matrix:\n")
  print(agQMatrix)
  cat("\n")
  
  # Filtering & Smoothing
  oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing=c("state", "mean", "disturbance"))
  
  cat("regression coefficients:\n")
  print(as.matrix(oEstimated.KFAS$alphahat[1,1:5]))
  cat("\n")
  
  # plot
  cat("making Figure 9.6 ... \n")
  plot(
    as.numeric(oEstimated.KFAS$alphahat[,"custom2"]), 
    as.numeric(oEstimated.KFAS$alphahat[,"custom1"]), 
    main = "Figure 9.6", 
    xlab = "custom2", ylab="custom1"
  )
  cat("making Figure 9.7 ... \n")
  plot(
    oEstimated.KFAS$alphahat[,c("custom1", "custom2")], 
    main = "Figure 9.7"
  )
  cat("making Figure 9.8 ... \n")
  plot(
    oEstimated.KFAS$alphahat[,"custom1"] 
    + oEstimated.KFAS$alphahat[,"abSeatbelt.agFront"] * tsData[,"abSeatbelt"], 
    main = "Figure 9.8a", 
    ylab = NULL, xlab=NULL
  )
  legend("top", legend="level+intervention front", lty=1, cex=0.8)
  plot(
    oEstimated.KFAS$alphahat[,"custom2"], 
    main = "Figure 9.8b", 
    ylab = NULL, xlab=NULL
  )
  legend("top", legend="level rear", lty=1, cex=0.8)
  cat("making Figure 9.9 ... \n")
  plot(
    oEstimated.KFAS$alphahat[,"sea_dummy1.agFront"], 
    main = "Figure 9.9a", 
    ylab = NULL, xlab=NULL
  )
  legend("top", legend="seasonal front", lty=1, cex=0.8)
  
  plot(
    oEstimated.KFAS$alphahat[,"sea_dummy1.agRear"], 
    main = "Figure 9.9b", 
    ylab = NULL, xlab=NULL
  )
  legend("top", legend="seasonal rear", lty=1, cex=0.8)
}
# - - - - - - - - - -
exercise.Chapter10 <- function() {
  
  set.seed(123123)
  
  tsRandom <- ts(rnorm(200))
  sub.plotFigure_twoseries(
    tsRandom, 
    NULL, 
    "Figure10.1", 
    "random process"
  )
  sub.plotFigure_ACF (
    tsRandom, 
    12, 
    "Figure 10.2", 
    "ACF - random process"
  )
  
  tsRW <- ts(cumsum(rnorm(200)))
  sub.plotFigure_twoseries(
    tsRW, 
    NULL, 
    "Figure10.3", 
    "random walk"
  )
  sub.plotFigure_ACF (
    tsRW, 
    12, 
    "Figure 10.4", 
    "ACF - random walk"
  )
  
  tsMA <- ts(arima.sim(model=list(ma=0.5), n=200))
  sub.plotFigure_twoseries(
    tsMA, 
    NULL, 
    "Figure10.5", 
    "MA(1) process"
  )
  sub.plotFigure_ACF (
    tsMA, 
    12, 
    "Figure 10.6", 
    "ACF - MA(1) process"
  )
  
  tsAR <- ts(arima.sim(model=list(ar=0.5), n=200))
  sub.plotFigure_twoseries(
    tsAR, 
    NULL, 
    "Figure10.7", 
    "AR(1) process"
  )
  sub.plotFigure_ACF (
    tsAR, 
    12, 
    "Figure 10.8", 
    "ACF - AR(1) process"
  )
  
  tsARMA <- ts(arima.sim(model=list(ar=0.5, ma=0.5), n=200))
  sub.plotFigure_twoseries(
    tsARMA, 
    NULL, 
    "Figure10.9", 
    "ARMA(1,1) process"
  )
  sub.plotFigure_ACF (
    tsARMA, 
    12, 
    "Figure 10.10", 
    "ACF - ARMA(1,1) process"
  )
}
# = = = = = = = = = - - = = = 

# png(width = 480, height = 300)

# exercise.Chapter1 ()
#  
# exercise.Chapter2.1 ()
# exercise.Chapter2.2 ()
# exercise.Chapter2.3 () 
# 
# exercise.Chapter3.1 ()
# exercise.Chapter3.2 ()
# exercise.Chapter3.3 ()
# exercise.Chapter3.4a ()
# exercise.Chapter3.4b ()  
# 
# exercise.Chapter4.1 ()
exercise.Chapter4.2 ()
# exercise.Chapter4.3 () 
# exercise.Chapter4.4 ()
#  
# exercise.Chapter5.1a ()
# exercise.Chapter5.1b ()
# exercise.Chapter5.2 ()
#  
# exercise.Chapter6.1 ()
# exercise.Chapter6.2 ()
# 
# exercise.Chapter7.1 ()
# exercise.Chapter7.2 ()
# exercise.Chapter7.3 ()
# exercise.Chapter7.4 ()
# 
# exercise.Chapter8.6a ()
# exercise.Chapter8.6b () 
# exercise.Chapter8.6c () 
# exercise.Chapter8.7 () 
# 
# exercise.Chapter9.4a () 
# exercise.Chapter9.4b () 
# 
# exercise.Chapter10 () 

# dev.off()
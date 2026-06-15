# val-mi code from paper

###########################################################
# R function get.performance to do modeling and obtain estimates of performance measures
#
# Note that parts of this are experimental
###########################################################


#require(PredictABEL) 
require(pROC)
require(fmsb)        
require(psych)        
require(epicalc)      
require(Hmisc) 
require(cvAUC)
require(survivalROC)
require(survival)


##################### Auxiliary functions

### 1. error and warning handler
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) res = NULL),
                                   warning = w.handler),
       warning = W)
}


### 2. improveProb_mod: from package PredictABEL 
improveProb_mod <- function (x1, x2, y) {
  s <- is.na(x1 + x2 + y)
  if (any(s)) {
    s <- !s
    x1 <- x1[s]
    x2 <- x2[s]
    y <- y[s] }
  n <- length(y)
  y <- as.numeric(y)
  u <- sort(unique(y))
  if (length(u) != 2 || u[1] != 0 || u[2] != 1) stop("y must have two values: 0 and 1")
  r <- range(x1, x2)
  if (r[1] < 0 || r[2] > 1) stop("x1 and x2 must be in [0,1]")
  a <- y == 1
  b <- y == 0
  na <- sum(a)
  nb <- sum(b)
  d <- x2 - x1
  nup.ev <- sum(d[a] > 0)
  pup.ev <- nup.ev/na
  nup.ne <- sum(d[b] > 0)
  pup.ne <- nup.ne/nb
  ndown.ev <- sum(d[a] < 0)
  pdown.ev <- ndown.ev/na
  ndown.ne <- sum(d[b] < 0)
  pdown.ne <- ndown.ne/nb
  nri.ev <- pup.ev - pdown.ev
  v.nri.ev <- (nup.ev + ndown.ev)/(na^2) - ((nup.ev - ndown.ev)^2)/(na^3)
  se.nri.ev <- sqrt(v.nri.ev)
  z.nri.ev <- nri.ev/se.nri.ev
  nri.ne <- pdown.ne - pup.ne
  v.nri.ne <- (ndown.ne + nup.ne)/(nb^2) - ((ndown.ne - nup.ne)^2)/(nb^3)
  se.nri.ne <- sqrt(v.nri.ne)
  z.nri.ne <- nri.ne/se.nri.ne
  nri <- pup.ev - pdown.ev - (pup.ne - pdown.ne)
  se.nri <- sqrt(v.nri.ev + v.nri.ne)
  z.nri <- nri/se.nri
  improveSens <- sum(d[a])/na
  improveSpec <- -sum(d[b])/nb
  idi <- mean(d[a]) - mean(d[b])
  var.ev <- var(d[a])/na
  var.ne <- var(d[b])/nb
  se.idi <- sqrt(var.ev + var.ne)
  z.idi <- idi/se.idi
  structure(llist(n, na, nb, pup.ev, pup.ne, pdown.ev, pdown.ne, 
                  nri, se.nri, z.nri, nri.ev, se.nri.ev, z.nri.ev, nri.ne, 
                  se.nri.ne, z.nri.ne, improveSens, improveSpec, idi, se.idi, 
                  z.idi, labels = FALSE), class = "improveProb") }



### 3. ci.auc from package pROC (with variances output)
ci.auc.mod <- function(roc, conf.level=0.95) {   
  YR <- roc$controls
  XR <- roc$cases
  n <- length(YR)
  m <- length(XR)
  V <- pROC:::delongPlacementsCpp(roc)
  SX <- sum((V$X - V$theta) * (V$X - V$theta))/(m - 1)
  SY <- sum((V$Y - V$theta) * (V$Y - V$theta))/(n - 1)
  S <- SX/m + SY/n
  ci <- qnorm(c(0 + (1 - conf.level)/2, 1 - (1 - conf.level)/2),  mean = V$theta, sd = sqrt(S))   
  pval <- 2 * pnorm(-abs(V$theta-0.5)/sqrt(S))
  if (roc$direction == ">") ci <- rev(1 - ci)
  return(c(auc=V$theta, auc.lcl=ci[1], auc.ucl=ci[2], auc.p = pval,stat=V$theta, stat.var=S)) }  


### 4. delong.paired.test from package pROC
delong.paired.test.mod <- function (roc1, roc2) {
  n <- length(roc1$controls)
  m <- length(roc1$cases)
  VR <- pROC:::delongPlacementsCpp(roc1)
  VS <- pROC:::delongPlacementsCpp(roc2)
  SX <- matrix(NA, ncol = 2, nrow = 2)
  SX[1, 1] <- sum((VR$X - VR$theta) * (VR$X - VR$theta))/(m -1)
  SX[1, 2] <- sum((VR$X - VR$theta) * (VS$X - VS$theta))/(m -1)
  SX[2, 1] <- sum((VS$X - VS$theta) * (VR$X - VR$theta))/(m -1)
  SX[2, 2] <- sum((VS$X - VS$theta) * (VS$X - VS$theta))/(m -1)
  SY <- matrix(NA, ncol = 2, nrow = 2)
  SY[1, 1] <- sum((VR$Y - VR$theta) * (VR$Y - VR$theta))/(n -1)
  SY[1, 2] <- sum((VR$Y - VR$theta) * (VS$Y - VS$theta))/(n -1)
  SY[2, 1] <- sum((VS$Y - VS$theta) * (VR$Y - VR$theta))/(n -1)
  SY[2, 2] <- sum((VS$Y - VS$theta) * (VS$Y - VS$theta))/(n -1)
  S <- SX/m + SY/n
  L <- c(1, -1)
  sig <- sqrt(L %*% S %*% L)
  dauc <- VR$theta - VS$theta
  zscore <- dauc/sig[1]
  pval <- pnorm(-abs(zscore))
  if (is.nan(zscore) && VR$theta == VR$theta && sig[1] == 0) {
    zscore <- 0
    pval <- 1}
  return(c(dauc=dauc, dauc.z=zscore,dauc.p=pval, stat=dauc,stat.var=sig[1]^2))
}


### 5. nricat.one: partly from R package PredictABEL; function reclassification                               
nricat.one <- function(cu, y=y, y_hat1=y_hat1, y_hat0=y_hat0,znorm=znorm, simple=T) {
  
  c1 <- cut(y_hat1, breaks = cu, include.lowest = TRUE, right = FALSE)
  c0 <- cut(y_hat0, breaks = cu, include.lowest = TRUE, right = FALSE)
  c00 <- factor(c0, levels = levels(c0), labels = c(1:length(levels(c0))))
  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  out <- tryCatch(improveProb_mod(x1 = as.numeric(c00) * (1/(length(levels(c00)))), x2 = as.numeric(c11) * (1/(length(levels(c11)))), y = y),error=function(e) NULL)
  if(simple) NRIcat <- c(nricat=out$nri) else {  
    NRIcat <- c(nricat=out$nri, nricat.lcl= out$nri-znorm*out$se.nri, nricat.ucl=out$nri+znorm*out$se.nri, nricat.z = out$z.nri, nricat.p=ifelse(out$se.nri==0, 1, pnorm(-abs(out$z.nri))), stat=out$nri, stat.var=out$se.nri^2 )}
  return(NRIcat) }                                                             


### 6. get.perf.one    
get.perf.one <- function(y, y_hat, y_hat0=NULL,t=NULL, conf,measures, simple=T, noinf=F)   {
  
  is.cv <- is.list(y_hat)
  if(is.cv) {
    if("auclaan" %in% measures) folds <- lapply(1:length(y), function(k) ifelse(k==1,1, sum(unlist(sapply(y,length)[1:(k-1)]))+1):sum(unlist(sapply(y,length)[1:k])))
    y <- unlist(y)
    y_hat <- unlist(y_hat)
    if(!is.null(y_hat0)) y_hat0 <- unlist(y_hat0) 
    if(!is.null(t)) t <- unlist(t) }  
  
  znorm <- qnorm(1-(1-conf)/2)
  ret <- list()
  if(any(is.na(y_hat))) {
    y <- y[complete.cases(y_hat)]
    if(!is.null(y_hat0)) y_hat0 <- y_hat0[complete.cases(y_hat)]
    if(!is.null(t)) t <- t[complete.cases(y_hat)]
    y_hat <- na.omit(y_hat)}
  n <- length(y)
  
  ### 1. AUC
  if ("auc" %in% measures) {      
    roc1 <- tryCatch(pROC:::roc(response=y,predictor=y_hat,direction="<"), error=function(e) NULL)   
    if(simple) ret <- c(ret,auc=list(c(auc=roc1$auc))) 
    if(!simple) {
      res <- tryCatch(ci.auc.mod(roc1),error=function(e) NULL)
      if(is.null(res) & !is.null(roc1$auc)) ret <- c(ret,auc=list(c(auc=roc1$auc))) else ret <- c(ret,auc=list(res)) }        
    if(noinf) ret <- c(ret,auc.noinf=list(c(auc.noinf=0.5)))
  }  
  
  ### 1a. AUC(t)
  if("auc.t" %in% measures) {
    auc <- tryCatch(survivalROC(Stime=t,status=y,marker=y_hat, method="KM",predict.time=10*365.25)$AUC) 
    ret <- c(ret,auc.t=list(c(auc.t=auc)))
    if(noinf) ret <- c(ret,auc.t.noinf=list(c(auc.t.noinf=0.5)))
  }
  
  ### 1b. AUC CI cv van de Laan
  if ("auclaan" %in% measures) {      
    out <- tryCatch(ci.cvAUC(predictions=y_hat, labels=y,folds=folds,confidence=conf), error=function(e) NULL)       
    ret <- c(ret,auclaan=list(c(auclaan=out$cvAUC,auclaan.lcl=out$ci[1],auclaan.ucl=out$ci[2], auclaan.p= pnorm(-abs((out$cvAUC-0.5)/out$se)), stat=out$cvAUC, stat.var=out$se^2)))
  }
  
  ### 2. Brier score 
  if ("brier" %in% measures) {                 
    brier <- c(brier=mean((y-y_hat)^2))
    if(!simple) {
      x <- (y-y_hat)^2 - (y-y_hat0)^2 
      r <- rank(abs(x))
      stat <- sum(r[x>0]) 
      nties <- table(r)
      z <- stat - n * (n + 1)/4    
      sigma <- sqrt(n * (n + 1) * (2 * n + 1)/24 - sum(nties^3 -nties)/48)
      Corr <- sign(z)*0.5
      z <- (z-Corr)/sigma 
      pval <- pnorm(-abs(z))
      brier <- c(brier, brier.p=pval,stat=z, stat.var=1)} 
    ret <- c(ret,brier=list(brier))
    if(noinf) ret <- c(ret,brier.noinf=list(c(brier.noinf=mean(sapply(1:1000,function(i) mean((y-sample(y_hat))^2))))))   } 
  
  ### 3. Pseudo R2 via Brier score 
  if("r2brier" %in% measures) {
    R2 <- tryCatch(1-mean((y-y_hat)^2)/mean((y-y_hat0)^2),error=function(e) NA)
    if(simple) r2brier=c(r2brier=R2)
    if(!simple) {
      rcon <- tryCatch(r.con(sqrt(max(0,R2)),n=n),error=function(e) NULL)
      fisherzR <- tryCatch(fisherz(sqrt(max(0,R2))),error=function(e) NULL)
      r2brier <- c(r2brier = R2, r2brier.lcl=rcon[1]^2,r2brier.ucl=rcon[2]^2, r2brier.p = pnorm(-abs(fisherzR*sqrt(n-3))),stat=fisherzR, stat.var=1/(n-3)) } 
    ret <- c(ret,r2brier=list(r2brier))
    if(noinf) ret <- c(ret,r2brier.noinf=list(c(r2brier.noinf=mean(sapply(1:1000,function(i) 1-mean((y-sample(y_hat))^2)/mean((y-y_hat0)^2))))))   } 
  
  ### 4. Pseudo R2 via cor(y,y_hat)
  if("r2cor" %in% measures) {
    R <- cor(as.numeric(as.character(y)),y_hat,use="p")
    if(simple) r2cor <- c(r2cor=R^2)
    if(!simple) {
      rcon <- tryCatch(r.con(R,n=n),error=function(e) NULL)
      fisherzR <- tryCatch(fisherz(R),error=function(e) NULL)      
      r2cor <- c(r2cor = R^2, r2cor.lcl=rcon[1]^2,r2cor.ucl=rcon[2]^2, r2cor.p = pnorm(-abs(fisherzR*sqrt(n-3))), stat=fisherzR,  stat.var=1/(n-3)) } 
    ret <- c(ret,r2cor=list(r2cor)) 
    if(noinf) ret <- c(ret,r2cor.noinf=list(c(r2cor.noinf=mean(sapply(1:1000,function(i) cor(as.numeric(as.character(y)),sample(y_hat),use="p")^2 )))))   } 
  
  ### 5. Missclass error rate
  if("error" %in% measures) {
    error <- length(which(y != as.numeric(y_hat>=0.5)))/n
    if(simple) error <- c(error=error)
    if(!simple) {
      stat <- error*n
      stat.p <- tryCatch(pbinom(stat,n,0.5),error=function(e) NA) 
      stat.var <- n*0.5*0.5
      stat.z <- (stat-n*0.5)/sqrt(stat.var)
      error = c(error=error,error.z=stat.z, error.p=stat.p, stat=stat, stat.var=stat.var) }   
    ret <- c(ret,error=list(error)) 
    if(noinf) {
      y_hatdec <- as.numeric(y_hat>=0.5)
      p1 <- length(which(y==1))/n
      q1 <- length(which(y_hatdec==1))/n
      ret <- c(ret,error.noinf=list(c(error.noinf=p1*(1-q1) + (1-p1)*q1))) } }
  
  ### 6. Yates discrimination slope
  if("yates" %in% measures) {
    yates <- abs(mean(y_hat[y==1])-mean(y_hat[y==0]))      
    ret <- c(ret,yates=list(c(yates=yates))) 
    if(noinf) ret <- c(ret,yates.noinf=list(c(yates.noinf=mean(sapply(1:1000,function(i) {
      yhatsample <- sample(y_hat)
      abs(mean(yhatsample[y==1])-mean(yhatsample[y==0]))}  )))))   } 
  
  ### 7. Calibration intercept and slope
  if("calint" %in% measures | "calslope" %in% measures) {
    mcal <- tryCatch(summary(glm(y ~ y_hat, family=binomial))$coef,error=function(e) NULL)
    if("calint" %in% measures) {
      if(simple) ret <- c(ret,calint=list(c(calint=mcal[1,1])))
      if(!simple) ret <- c(ret,calint=list(c(calint=mcal[1,1], calint.lcl=mcal[1,1]-znorm*mcal[1,2], calint.ucl=mcal[1,1]+znorm*mcal[1,2], calint.z=mcal[1,3],calint.p=mcal[1,4], stat=mcal[1,1], stat.var=mcal[1,2]^2)))
      if(noinf) ret <- c(ret,calint.noinf=list(c(calint.noinf=0))) }                    
    if("calslope" %in% measures) {
      if(simple) ret <- c(ret,calslope=list(c(calslope=mcal[2,1])))
      if(!simple) ret <- c(ret,calslope=list(c(calslope=mcal[2,1],calslope.lcl=mcal[2,1]-znorm*mcal[2,2], calslope.ucl=mcal[2,1]+znorm*mcal[2,2],  calslope.z=mcal[2,3],calslope.p=mcal[2,4], stat=mcal[2,1], stat.var=mcal[2,2]^2)))
      if(noinf) ret <- c(ret,calslope.noinf=list(c(calslope.noinf=0))) }  }   
  ret}  


### 6.a get.perf.one.stab  
get.perf.one.stab <- function(...)   {
  tryCatch(get.perf.one(...), error=function(e) NULL)
}


### 7. get.add.perf.one 
get.add.perf.one <- function(y, y_hat1,y_hat0,y_hat00=NULL,t=NULL, cutoff=NULL,conf, measures, simple=T, noinf=F)   {        
  
  is.cv <- is.list(y_hat1)
  if(is.cv) {
    y <- unlist(y)
    y_hat1 <- unlist(y_hat1)
    y_hat0 <- unlist(y_hat0)
    if(!is.null(y_hat00)) y_hat00 <- unlist(y_hat00) 
    if(!is.null(t)) t <- unlist(t)}
  
  znorm <- qnorm(1-(1-conf)/2)
  ret <- list()
  if(any(is.na(y_hat1)|is.na(y_hat0))) {
    full <- which(complete.cases(y_hat1,y_hat0))
    y <- y[full]
    y_hat00 <- y_hat00[full]
    y_hat0 <- y_hat0[full]
    y_hat1 <- y_hat1[full] 
    if(!is.null(t)) t <- t[full]}
  n <- length(y)
  
  ### 1. NRIcat
  if("nricat" %in% measures & !is.null(cutoff)) {
    if(!is.list(cutoff))  {
      nricat <- list(nricat=tryCatch(nricat.one(cu=cutoff, y=y,y_hat1=y_hat1,y_hat0=y_hat0,znorm=znorm, simple=simple),error=function(e) NULL)) } else {
        nricat <- lapply(cutoff,function(cu) tryCatch(nricat.one(cu=cu,y=y,y_hat1=y_hat1,y_hat0=y_hat0,znorm=znorm, simple=simple),error=function(e) NULL) )
        names(nricat) <- paste("nricat",1:length(nricat),sep="")     }         
    ret <- c(ret,nricat) }
  
  
  ### 2. NRIcont, IDI
  if("nricont" %in% measures | "idi" %in% measures) {
    out <- tryCatch(improveProb_mod(x1 = y_hat0, x2 = y_hat1, y = y),error=function(e) NULL)
    if("nricont" %in% measures) {
      if(simple) nricont <- list(nricont=c(nricont=out$nri))
      if(!simple) nricont <- list(nricont=c(nricont=out$nri, nricont.lcl= out$nri-znorm*out$se.nri, nricont.ucl=out$nri+znorm*out$se.nri, nricont.z = out$z.nri, nricont.p=pnorm(-abs(out$z.nri)), stat=out$nri, stat.var=out$se.nri^2))
      ret <- c(ret,nricont) } 
    
    if("idi" %in% measures) {
      if(simple) idi <- list(idi=c(idi=out$idi))
      if(!simple) idi <- list(idi=c(idi=out$idi, idi.lcl=out$idi-znorm*out$se.idi, idi.ucl=out$idi+znorm*out$se.idi, idi.z=out$z.idi, idi.p=pnorm(-abs(out$z.idi)), stat=out$idi, stat.var=out$se.idi^2))
      ret <- c(ret,idi) }}                      
  
  ### 3. deltaAUC
  if ("dauc" %in% measures) {      
    roc1 <- tryCatch(pROC:::roc(response=y,predictor=y_hat1,direction="<"),error=function(e) NULL)
    roc0 <- tryCatch(pROC:::roc(response=y,predictor=y_hat0,direction="<"),error=function(e) NULL)
    if(simple) ret <- c(ret,dauc=list(c(dauc=roc1$auc-roc0$auc))) 
    if(!simple) {
      res <- tryCatch(delong.paired.test.mod(roc1,roc0),error=function(e) NULL)
      if(is.null(res) & !is.null(roc1) & !is.null(roc0)) ret <- c(ret,dauc=list(c(dauc=roc1$auc-roc0$auc))) else ret <- c(ret,dauc=list(res)) }}
  
  ### 3a. deltaAUC(t)
  if("dauc.t" %in% measures) {
    auc1 <- tryCatch(survivalROC(Stime=t,status=y,marker=y_hat1, method="KM",predict.time=10*365.25)$AUC) 
    auc0 <- tryCatch(survivalROC(Stime=t,status=y,marker=y_hat0, method="KM",predict.time=10*365.25)$AUC)
    dauc <- list(dauc.t=c(dauc.t=auc1-auc0, auc.t0=auc0,auc.t1=auc1))
    ret <- c(ret,dauc)    }
  
  ### 4. delta Brier score 
  if ("dbrier" %in% measures) {                 
    dbrier <- c(dbrier=mean((y-y_hat1)^2 - (y-y_hat0)^2))
    if(!simple) {
      x <- (y-y_hat1)^2 - (y-y_hat0)^2 # Brier residuals
      r <- rank(abs(x))
      stat <- sum(r[x>0]) 
      nties <- table(r)
      z <- stat - n * (n + 1)/4    
      sigma <- sqrt(n * (n + 1) * (2 * n + 1)/24 - sum(nties^3 -nties)/48)
      Corr <- sign(z)*0.5
      z <- (z-Corr)/sigma # SNV statistic
      pval <- pnorm(-abs(z))
      dbrier <- c(dbrier, dbrier.p=pval,stat=z, stat.var=1)} 
    ret <- c(ret,dbrier=list(dbrier))}
  
  ### 5. delta Pseudo R2 via Brier score 
  if("dr2brier" %in% measures) { 
    ret <- c(ret,dr2brier=list(c(dr2brier=tryCatch(mean((y-y_hat0)^2)/mean((y-y_hat00)^2) - mean((y-y_hat1)^2)/mean((y-y_hat00)^2),error=function(e) NA) ))) }   
  
  ### 6. delta Pseudo R2 via cor(y,y_hat)
  if("dr2cor" %in% measures) {
    R1 <- cor(as.numeric(as.character(y)),y_hat1,use="p")
    R0 <- cor(as.numeric(as.character(y)),y_hat0,use="p")
    ret <- c(ret, dr2cor =list(c(dr2cor=R1^2-R0^2))) } 
  
  ### 7. delta Missclass error rate
  if("derror" %in% measures) {
    error1 <- length(which(y != as.numeric(y_hat1>=0.5)))/n
    error0 <- length(which(y != as.numeric(y_hat0>=0.5)))/n
    ret <- c(ret, derror =list(c(derror=error1-error0)))} 
  
  ### 8. Calibration intercept and slope
  if("calint" %in% measures | "calslope" %in% measures) {
    mcal <- tryCatch(summary(glm(y ~ y_hat1, family=binomial))$coef,error=function(e) NULL)
    if("calint" %in% measures) {
      if(simple) ret <- c(ret,calint=list(c(calint=mcal[1,1])))
      if(!simple) ret <- c(ret,calint=list(c(calint=mcal[1,1],calint.lcl=mcal[1,1]-znorm*mcal[1,2], calint.ucl=mcal[1,1]+znorm*mcal[1,2],   calint.z=mcal[1,3],calint.p=mcal[1,4], stat=mcal[1,1], stat.var=mcal[1,2]^2)))}
    if("calslope" %in% measures) {
      if(simple) ret <- c(ret,calslope=list(c(calslope=mcal[2,1])))
      if(!simple) ret <- c(ret,calslope=list(c(calslope=mcal[2,1], calslope.lcl=mcal[2,1]-znorm*mcal[2,2], calslope.ucl=mcal[2,1]+znorm*mcal[2,2],  calslope.z=mcal[2,3],calslope.p=mcal[2,4], stat=mcal[2,1], stat.var=mcal[2,2]^2))) } }
  
  ret}  

### 7.a get.add.perf.one.stab    
get.add.perf.one.stab <- function(...)   {
  tryCatch(get.add.perf.one(...), error=function(e) NULL)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function get.performance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

get.performance <- function(dat, datBase=NULL, conf=0.95, na.strict=0.9,
                            measures=c("auc", "brier","r2brier","error","r2cor","yates","calint","calslope",
                                       "dauc","dbrier","dr2brier","derror","dr2cor","nricat","nricont","idi"), 
                            cutoff=list(c(0,0.25,1),c(0,0.75,1),c(0, 0.25,0.75,1)), diff.mis = F, is.test=F, 
                            all.simple=T, nondelta.too=F,quadr=NULL,
                            model="logreg") { # nondelta.too: if delta, return res for M0 and M1
  
  if("type" %in% names(dat)) {
    typ=unlist(dat$type) 
    dat <- dat[-which(names(dat)=="type")]}  else typ=NULL
    di <- dims(dat)
    if(di[1]==1) dat <- dat[[1]]
    di <- dims(dat)
    dep <- depth(dat)
    
    if(!is.null(datBase)) {
      if("type" %in% names(datBase)) {
        typBase=unlist(datBase$type) 
        datBase <- datBase[-which(names(datBase)=="type")]}  else typBase=NULL
        diBase <- dims(datBase)
        if(diBase[1]==1) datBase <- datBase[[1]]
        diBase <- dims(datBase)
        depBase <- depth(datBase) }  else depBase=0
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    get.performance.one <- function(datFit,datEval,simple=T,noinf=F) {
      
      is.cv <- length(dims(datFit))>1
      # all but CV:                              
      if(!is.cv) {
        
        # quadr
        if(!is.null(quadr)) {
          if(!is.null(datFit$X_p)) {
            if(any(quadr %in% colnames(datFit$X_p))) datFit$X_p <- data.frame(datFit$X_p, datFit$X_p[,na.omit(match(quadr, colnames(datFit$X_p)))]^2)
            if(any(quadr %in% colnames(datEval$X_p))) datEval$X_p <- data.frame(datEval$X_p, datEval$X_p[,na.omit(match(quadr, colnames(datEval$X_p)))]^2)  }
          if(any(quadr %in% colnames(datFit$X_q))) datFit$X_q <- data.frame(datFit$X_q, datFit$X_q[,na.omit(match(quadr, colnames(datFit$X_q)))]^2)
          if(any(quadr %in% colnames(datEval$X_q))) datEval$X_q <- data.frame(datEval$X_q, datEval$X_q[,na.omit(match(quadr, colnames(datEval$X_q)))]^2) }
        
        y <- datEval$y
        # non-delta  
        if(is.null(datFit$X_p)) {
          if(model=="logreg") m <- tryCatch.W.E(glm(y~X_q,family=binomial,data=list(y=datFit$y, X_q=as.matrix(datFit$X_q))))     
          if(model=="cox") {
            datFit$X_q = data.frame(datFit$X_q)
            datEval$X_q = data.frame(datEval$X_q)
            datFit$X_q$ysurv <- Surv(datFit$t,datFit$y)
            datEval$X_q$ysurv <- Surv(datEval$t,datEval$y)
            m <-  tryCatch.W.E(coxph(ysurv~.,data=datFit$X_q))  }
          war <- m$warning
          m <- m$value
          if(!is.null(m) & length(resid(m))>=na.strict*length(datFit$y)) {
            if(model=="logreg") {
              y_hat <- predict(m,newdata=list(y=datEval$y, X_q=as.matrix(datEval$X_q)),type="response")  
              y_hat0 <- rep(length(which(datFit$y==1))/length(datFit$y),length(datEval$y))
              perf <- get.perf.one.stab(y=y, y_hat=y_hat,y_hat0=y_hat0, conf=conf,measures=measures, simple=simple,noinf=noinf) }
            if(model=="cox") {
              y_hat <- predict(m, newdata=datEval$X_q,type="risk")
              perf <- get.perf.one.stab(y=y,y_hat=y_hat,t=datEval$t, conf=conf,measures=measures,simple=simple,noinf=noinf) }
            perf$warn <- c(warn=as.numeric(!is.null(war)))
            if(noinf) perf$warn.noinf <- c(warn.noinf=0)} else perf <- NULL} else {
              # delta      
              datFit$X <- cbind(datFit$X_p,datFit$X_q)
              datEval$X <- cbind(datEval$X_p,datEval$X_q)
              if(model=="logreg") {
                m0 <- tryCatch.W.E(glm(y~X_p,family=binomial,data=list(y=datFit$y, X_p=as.matrix(datFit$X_p))))
                m1 <- tryCatch.W.E(glm(y~X,family=binomial,data=list(y=datFit$y, X=as.matrix(datFit$X)))) } 
              if(model=="cox") {
                datFit$X_p = data.frame(datFit$X_p)
                datEval$X_p = data.frame(datEval$X_p)
                datFit$X = data.frame(datFit$X)
                datEval$X = data.frame(datEval$X)
                datFit$X_p$ysurv <- datFit$X$ysurv <- Surv(datFit$t,datFit$y)
                datEval$X_p$ysurv <- datEval$X$ysurv <-Surv(datEval$t,datEval$y)
                m0 <- tryCatch.W.E(coxph(ysurv~.,data=datFit$X_p))
                m1 <- tryCatch.W.E(coxph(ysurv~.,data=datFit$X)) }
              war <- c(m0$warning,m1$warning)
              m0 <- m0$value
              m1 <- m1$value                                                                                                                         if(!is.null(m0) & !is.null(m1) & length(resid(m0))>=na.strict*length(datFit$y) & length(resid(m1))>=na.strict*length(datFit$y)) {
                if(model=="logreg") {
                  y_hat0 <- predict(m0,newdata=list(y=datEval$y, X_p=as.matrix(datEval$X_p)) ,type="response")
                  y_hat1 <- predict(m1,newdata=list(y=datEval$y, X=as.matrix(datEval$X)) ,type="response") 
                  y_hat00 <- rep(length(which(datFit$y==1))/length(datFit$y),length(datEval$y))
                  perf <- get.add.perf.one.stab(y=y, y_hat1=y_hat1,y_hat0=y_hat0,y_hat00=y_hat00, cutoff=cutoff,conf=conf, measures=measures,simple=simple,noinf=noinf)  }
                if(model=="cox") {
                  y_hat0 <- predict(m0, newdata=datEval$X_p,type="risk")
                  y_hat1 <- predict(m1, newdata=datEval$X,type="risk")
                  perf <- get.add.perf.one.stab(y=y,y_hat1=y_hat1,y_hat0=y_hat0,t=datEval$t, conf=conf,measures=measures,simple=simple,noinf=noinf) }
                perf$warn <- c(warn=as.numeric(!is.null(war[[1]]) | !is.null(war[[2]]))) 
                if(noinf) perf$warn.noinf <- c(warn.noinf=0)
                if(nondelta.too) {
                  if(model=="logreg") {
                    perf0 <- get.perf.one.stab(y=y, y_hat=y_hat0,y_hat0=y_hat00, conf=conf, measures=measures,simple=simple,noinf=noinf)
                    perf1 <- get.perf.one.stab(y=y, y_hat=y_hat1,y_hat0=y_hat00,conf=conf, measures=measures,simple=simple,noinf=noinf)  }
                  if(model=="cox") {
                    perf0 <- get.perf.one.stab(y=y,y_hat=y_hat0, t=datEval$t, conf=conf, measures=measures,simple=simple,noinf=noinf)
                    perf1 <- get.perf.one.stab(y=y,y_hat=y_hat1, t=datEval$t, conf=conf, measures=measures,simple=simple,noinf=noinf) }
                  if(!is.null(perf0)) names(perf0) <- paste(names(perf0),"0",sep="")
                  if(!is.null(perf1)) names(perf1) <- paste(names(perf1),"1",sep="")
                  perf <- c(perf,perf0,perf1)} } else perf <- NULL}
      }
      # CV
      if(is.cv) {
        
        # quadr
        if(!is.null(quadr)) {
          datFit <- lapply(1:length(datFit), function(k) {          
            if(!is.null(datFit[[k]]$X_p))   if(any(quadr %in% colnames(datFit[[k]]$X_p))) datFit[[k]]$X_p <- data.frame(datFit[[k]]$X_p, datFit[[k]]$X_p[,na.omit(match(quadr, colnames(datFit[[k]]$X_p)))]^2)
            if(any(quadr %in% colnames(datFit[[k]]$X_q))) datFit[[k]]$X_q<- data.frame(datFit[[k]]$X_q, datFit[[k]]$X_q[,na.omit(match(quadr, colnames(datFit[[k]]$X_q)))]^2)
            datFit[[k]]})
          
          datEval <- lapply(1:length(datEval), function(k) {          
            if(!is.null(datEval[[k]]$X_p))   if(any(quadr %in% colnames(datEval[[k]]$X_p))) datEval[[k]]$X_p <- data.frame(datEval[[k]]$X_p, datEval[[k]]$X_p[,na.omit(match(quadr, colnames(datEval[[k]]$X_p)))]^2)
            if(any(quadr %in% colnames(datEval[[k]]$X_q))) datEval[[k]]$X_q <- data.frame(datEval[[k]]$X_q, datEval[[k]]$X_q[,na.omit(match(quadr, colnames(datEval[[k]]$X_q)))]^2)
            datEval[[k]]}) }
        
        # non-delta  
        if(is.null(datFit[[1]]$X_p)) {
          y_hats <- lapply(1:length(datFit), function(k) {
            if(model=="logreg")  m <- tryCatch.W.E(glm(y~X_q,family=binomial,data=list(y=datFit[[k]]$y, X_q=as.matrix(datFit[[k]]$X_q)))) 
            if(model=="cox") {
              datFit[[k]]$X_q = data.frame(datFit[[k]]$X_q)
              datEval[[k]]$X_q = data.frame(datEval[[k]]$X_q)
              datFit[[k]]$X_q$ysurv <- Surv(datFit[[k]]$t,datFit[[k]]$y)
              datEval[[k]]$X_q$ysurv <- Surv(datEval[[k]]$t,datEval[[k]]$y)
              m <-  tryCatch.W.E(coxph(ysurv~.,data=datFit[[k]]$X_q))  }          
            war <- m$warning
            m <- m$value        
            if(!is.null(m) & length(resid(m))>=na.strict*length(datFit[[k]]$y)) {
              if(model=="logreg") {
                y_hat0 <- rep(length(which(datFit[[k]]$y==1))/length(datFit[[k]]$y),length(datEval[[k]]$y)) 
                y_hat <- predict(m,newdata=list(y=datEval[[k]]$y, X_q=as.matrix(datEval[[k]]$X_q)),type="response") 
                ret <- list(y_hat0,y_hat,war)}
              if(model=="cox") {
                y_hat <- predict(m, newdata=datEval[[k]]$X_q,type="risk")
                ret <- list(y_hat0=NULL,y_hat, war) }} else ret <- list(rep(NA,length(datEval[[k]]$y)),rep(NA,length(datEval[[k]]$y)),war)
            ret})          
          y_hat0 <- lapply(y_hats,function(yh) yh[[1]])
          y_hat <- lapply(y_hats,function(yh) yh[[2]])
          war <- lapply(y_hats,function(yh) yh[[3]])
          y <- lapply(datEval, function(da) da$y)
          t <- lapply(datEval, function(da) da$t)
          if(length(which(is.na(unlist(y_hat))))==0) {
            if(model=="logreg") perf <- get.perf.one.stab(y=y, y_hat=y_hat,y_hat0=y_hat0, conf=conf,measures=measures, simple=simple,noinf=noinf)
            if(model=="cox")  perf <- get.perf.one.stab(y=y,y_hat=y_hat, t=t, conf=conf,measures=measures,simple=simple,noinf=noinf)  
            perf$warn <- c(warn=length(which((sapply(war,function(wa) !is.null(wa)))))/length(war))
            if(noinf) perf$warn.noinf <- c(warn.noinf=0)} else perf <- NULL} else {
              # delta      
              y_hats <- lapply(1:length(datFit), function(k) {
                datFit[[k]]$X <- cbind(datFit[[k]]$X_p,datFit[[k]]$X_q)
                datEval[[k]]$X <- cbind(datEval[[k]]$X_p,datEval[[k]]$X_q)
                if(model=="logreg")  {
                  m0 <- tryCatch.W.E(glm(y~X_p,family=binomial,data=list(y=datFit[[k]]$y, X_p=as.matrix(datFit[[k]]$X_p)))) 
                  m1 <- tryCatch.W.E(glm(y~X,family=binomial,data=list(y=datFit[[k]]$y, X=as.matrix(datFit[[k]]$X)))) }
                if(model=="cox") {
                  datFit[[k]]$X_p <- data.frame(datFit[[k]]$X_p)
                  datFit[[k]]$X <- data.frame(datFit[[k]]$X)
                  datEval[[k]]$X_p <- data.frame(datEval[[k]]$X_p)
                  datEval[[k]]$X <- data.frame(datEval[[k]]$X)          
                  datFit[[k]]$X_p$ysurv <- datFit[[k]]$X$ysurv <- Surv(datFit[[k]]$t,datFit[[k]]$y)
                  datEval[[k]]$X_p$ysurv <- datEval[[k]]$X$ysurv <-Surv(datEval[[k]]$t,datEval[[k]]$y)
                  m0 <- tryCatch.W.E(coxph(ysurv~.,data=datFit[[k]]$X_p))
                  m1 <- tryCatch.W.E(coxph(ysurv~.,data=datFit[[k]]$X)) }
                war <- c(m0$warning,m1$warning)
                m0 <- m0$value
                m1 <- m1$value                                                                                                                         if(!is.null(m0) & !is.null(m1) & length(resid(m0))>=na.strict*length(datFit[[k]]$y) & length(resid(m1))>=na.strict*length(datFit[[k]]$y)) {
                  if(model=="logreg"){
                    y_hat0 <- predict(m0,newdata=list(y=datEval[[k]]$y, X_p=as.matrix(datEval[[k]]$X_p)) ,type="response")
                    y_hat1 <- predict(m1,newdata=list(y=datEval[[k]]$y, X=as.matrix(datEval[[k]]$X)) ,type="response") 
                    y_hat00 <- rep(length(which(datFit[[k]]$y==1))/length(datFit[[k]]$y),length(datEval[[k]]$y))
                    ret <- list(y_hat0,y_hat1,y_hat00,war) }
                  if(model=="cox") {
                    y_hat0 <- predict(m0, newdata=datEval[[k]]$X_p,type="risk")
                    y_hat1 <- predict(m1, newdata=datEval[[k]]$X,type="risk")
                    ret <- list(y_hat0,y_hat1,y_hat00=NULL, war)}          } else ret <- list(rep(NA,length(datEval[[k]]$y)),rep(NA,length(datEval[[k]]$y)),rep(NA,length(datEval[[k]]$y)),war)
                ret})
              y_hat0 <- lapply(y_hats,function(yh) yh[[1]])
              y_hat1 <- lapply(y_hats,function(yh) yh[[2]])
              y_hat00 <- lapply(y_hats,function(yh) yh[[3]]) 
              war <- lapply(y_hats,function(yh) yh[[3]])
              y <- lapply(datEval, function(da) da$y)
              t <- lapply(datEval, function(da) da$t)                                                                             
              if(length(which(is.na(unlist(y_hat0))))==0 & length(which(is.na(unlist(y_hat1))))==0) {
                if(model=="logreg") perf <- get.add.perf.one.stab(y=y, y_hat1=y_hat1,y_hat0=y_hat0,y_hat00=y_hat00, cutoff=cutoff,conf=conf, measures=measures,simple=simple,noinf=noinf)
                if(model=="cox")  perf <- get.add.perf.one.stab(y=y,y_hat1=y_hat1,y_hat0=y_hat0, t=t, conf=conf,measures=measures,simple=simple,noinf=noinf)  
                perf$warn <- c(warn=length(which((sapply(war,function(wa) !is.null(wa[[1]]) | !is.null(wa[[2]])))))/length(war))
                if(noinf) perf$warn.noinf <- c(warn.noinf=0)
                if(nondelta.too) {
                  if(model=="logreg") {
                    perf0 <- get.perf.one.stab(y=y, y_hat=y_hat0,y_hat0=y_hat00,conf=conf, measures=measures,simple=simple,noinf=noinf)
                    perf1 <- get.perf.one.stab(y=y, y_hat=y_hat1,y_hat0=y_hat00,conf=conf, measures=measures,simple=simple,noinf=noinf) }
                  if(model=="cox") {
                    perf0 <- get.perf.one.stab(y=y,y_hat=y_hat0, t=t, conf=conf, measures=measures,simple=simple,noinf=noinf)
                    perf1 <- get.perf.one.stab(y=y,y_hat=y_hat1, t=t, conf=conf, measures=measures,simple=simple,noinf=noinf) }
                  if(!is.null(perf0)) names(perf0) <- paste(names(perf0),"0",sep="")
                  if(!is.null(perf1)) names(perf1) <- paste(names(perf1),"1",sep="")
                  perf <- c(perf,perf0,perf1)} } else perf <- NULL}}
      
      return(perf)  }
    
    get.performance.one.stab <- function(...)   {
      tryCatch(get.performance.one(...), error=function(e) NULL)
    }
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    
    if(is.null(datBase)&dep==1) {     # datCom--> perf.orig.orig
      perf <- get.performance.one.stab(datFit=dat, datEval=dat,simple=F,noinf=T) }   # simple=F: get also variance/p 
    if(is.null(datBase)&dep==2) {     # datMI--> perf.orig.orig
      perf <- lapply(1:length(dat), function(m) get.performance.one.stab(datFit=dat[[m]],datEval=dat[[m]],simple=F,noinf=T)) } 
    if(is.test & dep==2) {
      perf <- lapply(1:length(dat), function(b) {
        get.performance.one.stab(datFit=dat[[b]], datEval=datBase[[b]], simple=F, noinf=F)}) }       
    if(is.test & dep==1 & depBase==1) {
      perf <- get.performance.one.stab(datFit=dat, datEval=datBase, simple=F, noinf=F) }       
    if(is.test & dep==1 & depBase==2) {
      perf <- lapply(1:length(datBase), function(m) {
        get.performance.one.stab(datFit=dat, datEval=datBase[[m]], simple=F, noinf=F)}) } 
    
    if(("bs" %in% typ | "ss" %in% typ) & !("mice" %in% typ)) {          # BS/SS in complete data
      perf <- lapply(1:length(dat), function(b) {
        list(perf.bs.bs = get.performance.one.stab(datFit=dat[[b]]$bs.dat, datEval=dat[[b]]$bs.dat),
             perf.bs.oob = get.performance.one.stab(datFit=dat[[b]]$bs.dat, datEval=dat[[b]]$oob.dat, simple=all.simple),
             perf.bs.orig = get.performance.one.stab(datFit=dat[[b]]$bs.dat, datEval=datBase))}) }
    
    if("cv" %in% typ & !("mice" %in% typ)) {          # CV in complete data
      perf <- lapply(1:length(dat), function(b) {
        perf.bs.bs <- lapply(1:length(dat[[b]]), function(k) get.performance.one.stab(datFit=dat[[b]][[k]]$bs.dat, datEval= dat[[b]][[k]]$bs.dat))
        na <- unique(c(sapply(1:length(dat[[b]]), function(k) names(perf.bs.bs[[k]])))) 
        if(length(which(sapply(perf.bs.bs,length)==max(sapply(perf.bs.bs,length))))/length(perf.bs.bs)>=0.5) {
          perf.bs.bs <- lapply(1:length(perf.bs.bs[[1]]), function(me) Reduce("+",lapply(perf.bs.bs, function(pe) pe[[me]]))/length(perf.bs.bs))
          names(perf.bs.bs) <- na } else perf.bs.bs=NULL                                                      
        list(perf.bs.bs = perf.bs.bs,
             perf.bs.oob = get.performance.one.stab(datFit=lapply(1:length(dat[[b]]),function(k) dat[[b]][[k]]$bs.dat), datEval=lapply(1:length(dat[[b]]),function(k) dat[[b]][[k]]$oob.dat), simple=all.simple))}) }
    
    if(("bs" %in% typ | "ss" %in% typ) & "mice" %in% typ) {          #   MI-BS/SS 
      if(which(typ=="mice") > which(typ%in% c("bs","ss"))) {  # arrange also for BS/SS-MI  
        dat <- lapply(1:length(dat[[1]][[1]]), function(m) lapply(1:length(dat), function(b) list(bs.dat=dat[[b]][[1]][[m]],oob.dat=dat[[b]][[2]][[m]])))  }
      perf <- lapply(1:length(dat), function(m) lapply(1:length(dat[[1]]), function(b) {
        list(perf.bs.bs=get.performance.one.stab(datFit=dat[[m]][[b]]$bs.dat, datEval=dat[[m]][[b]]$bs.dat),
             perf.bs.oob=get.performance.one.stab(datFit=dat[[m]][[b]]$bs.dat, datEval=dat[[m]][[b]]$oob.dat, simple=all.simple),
             perf.bs.orig=get.performance.one.stab(datFit=dat[[m]][[b]]$bs.dat, datEval=datBase[[ifelse(diff.mis,sample(1:length(dat)),m)]]))})) }
    
    if("cv" %in% typ & "mice" %in% typ) {          #   MI-CV
      if(which(typ=="mice") > which(typ=="cv")) {  # arrange also for CV-MI  
        dat <- lapply(1:length(dat[[1]][[1]][[1]]), function(m) lapply(1:length(dat), function(b) lapply(1:length(dat[[1]]), function(k) list(bs.dat=dat[[b]][[k]][[1]][[m]],oob.dat=dat[[b]][[k]][[2]][[m]]))))  }
      perf <- lapply(1:length(dat), function(m) lapply(1:length(dat[[m]]), function(b) {
        perf.bs.bs = lapply(1:length(dat[[m]][[b]]), function(k) get.performance.one.stab(datFit=dat[[m]][[b]][[k]]$bs.dat, datEval=dat[[m]][[b]][[k]]$bs.dat))
        na <- unique(c(unlist(sapply(1:length(dat[[m]][[b]]), function(k) names(perf.bs.bs[[k]]))))) 
        if(length(which(sapply(perf.bs.bs,length)==max(sapply(perf.bs.bs,length))))/length(perf.bs.bs)>=0.5) {
          perf.bs.bs=lapply(1:max(sapply(perf.bs.bs,length)), function(me) {
            Reduce("+",lapply(perf.bs.bs[sapply(perf.bs.bs,length)==max(sapply(perf.bs.bs,length))], function(pe) pe[[me]]))}/length(perf.bs.bs))
          names(perf.bs.bs) <- na   } else perf.bs.bs=NULL                                                     
        list(perf.bs.bs=perf.bs.bs,
             perf.bs.oob = get.performance.one.stab(datFit=lapply(1:length(dat[[m]][[b]]),function(k) dat[[m]][[b]][[k]]$bs.dat), 
                                                    datEval=lapply(1:length(dat[[m]][[b]]),function(k) dat[[m]][[b]][[k]]$oob.dat), simple=all.simple))})) }
    
    return(list(perf,type=typ))}

###########################################################
# R functions do.bs and do.mice to perform resampling and multiple imputation
###########################################################

require(parallel)
require(mice)

##################### Auxiliary functions

### 1. depth of a list
depth <- function(this,thisdepth=0){
  if(!is.list(this)| is.data.frame(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
  }
}

### 2. dimensions of a list (assuming that on every level every element has same number of elements)
dims <- function(this){
  res <- NULL
  while(is.list(this) & !is.data.frame(this)) {
    res <- c(res, length(this))
    this <- this[[1]] }
  return(res)}


### 3. names of different levels of a list (assuming that on every level every element has same number of elements
namess <- function(this, thisdepth=0) {
  res <- list(names(this))
  this <- this[[1]]
  while(is.list(this)) {
    if(is.null(res[[1]])) res <- list(res,names(this)) else res <- c(res, names(this)) 
    this <- this[[1]]}
  return(res)}

### 4. Function to do random sampling for a variable which cannot be imputed using MICE (e.g. because the only variable, or to few entries)
mice.impute.sample.mod <- function(y){
  if(any(is.na(y))) {
    ry <- !is.na(y)
    yry <- y[ry]
    if(length(yry)<=2) y[!ry] <- rnorm(sum(!ry)) else y[!ry] <- sample(yry, size=sum(!ry), replace=T) }
  y}


### 5. stable mice
complete.stab <- function(X, M, meth) {
  Xsave <- X
  ret <- tryCatch(mice(X,m=M, meth=meth,maxit=5,printFlag=F),error=function(e) NULL) 
  com <- tryCatch(lapply(1:M, function(m) complete(ret,m)),error=function(e) NULL)
  if(is.null(com)) {
    tab <- apply(X,2,function(x) length(which(is.na(x)))/length(x))
    arr <- which(is.na(X),arr.ind=T)
    probs <- tab[match(arr[,2], 1:length(tab))]
    X[arr[sample(1:nrow(arr),nrow(arr)/4,prob=probs),,drop=F]] <- rnorm(nrow(arr)/4)    
    ret <- tryCatch(mice(X,m=M, meth=meth,maxit=5,printFlag=F),error=function(e) NULL)  
    com <- tryCatch(lapply(1:M, function(m) complete(ret,m)),error=function(e) NULL)}
  if(is.null(com)) {
    tab <- apply(X,2,function(x) length(which(is.na(x)))/length(x))
    arr <- which(is.na(X),arr.ind=T)
    probs <- tab[match(arr[,2], 1:length(tab))] 
    X[arr[sample(1:nrow(arr),nrow(arr)/3,prob=probs),,drop=F]] <- rnorm(nrow(arr)/3)   
    ret <- tryCatch(mice(X,m=M, meth=meth,maxit=5,printFlag=F),error=function(e) NULL)
    com <- tryCatch(lapply(1:M, function(m) complete(ret,m)),error=function(e) NULL)}
  if(is.null(com)) {
    tab <- apply(X,2,function(x) length(which(is.na(x)))/length(x))
    arr <- which(is.na(X),arr.ind=T)
    probs <- tab[match(arr[,2], 1:length(tab))] 
    X[arr[sample(1:nrow(arr),nrow(arr)/2,prob=probs),,drop=F]] <- rnorm(nrow(arr)/2)   
    ret <- tryCatch(mice(X,m=M, meth=meth,maxit=5,printFlag=F),error=function(e) NULL)
    com <- tryCatch(lapply(1:M, function(m) complete(ret,m)),error=function(e) NULL)}
  if(is.null(com)) com <- lapply(1:M, function(m) X)  
  if(any(sapply(com,is.na))) com <- lapply(1:M, function(m)   if(any(is.na(com[[m]]))) com[[m]] <- apply(com[[m]],2,function(co) mice.impute.sample.mod(co))  )
  if(any(sapply(com,is.na))) com <- NULL 
  com }       


### 6. SS fraction
ssfrac <- (1-1/exp(1))


### 7. random up/down rounding
rounddownup <- function(x) {
  ganz <- floor(x)
  rest <- x-ganz
  if(rest > 0)  x = ganz + sample(c(0,1), 1, prob=c(1-rest,rest))
  return(x) }



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function do.mice: perform multiple imputation using mice for M times
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

do.mice <- function(dat,              # either a list with elements y, X_p, X_q (when MICE comes before Resampl/Val)
                    # or a list with 2 elements bs.dat [and oob.dat] each with [K-fold elements each with] elements y, X_p, X_q (when MICE comes after Resamp/Val)
                    # or 1 data set (if MICE on original data)
                    # or list with D elements (from Resampl) each etc...
                    M=10,             # number of imputations
                    quadr=NULL,       # vector of variables for which quadratic terms should be included (names in X_p,X_q)
                    transf=NULL,      # vector of variable which should be checked for normality and then potentially  transformed  
                    y.include=T) {    # logical, indicating whether y variable should be included in the imputation process 
  
  
  if("type" %in% names(dat)) {
    typ=c(unlist(dat$type),"mice") 
    dat <- dat[-which(names(dat)=="type")]}  else typ="mice"
    di <- dims(dat)
    if(di[1]==1) dat <- dat[[1]]
    di <- dims(dat)
    dep <- depth(dat)  
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    do.mice.one <- function(da) {
      if(is.list(da) & "X_q" %in% names(da)) {
        if(!is.null(da$X_q)) {
          p <- ifelse(is.null(da$X_p),0,ifelse(is.vector(da$X_p),1,ncol(da$X_p)))
          q <- ifelse(is.vector(da$X_q),1,ncol(da$X_q))
          if(y.include)  {if(p==0) da.joint <- data.frame(da$y,da$X_q) else da.joint <- data.frame(da$y,da$X_p,da$X_q)  }
          if(!y.include) {if(p==0) da.joint <- data.frame(da$X_q) else da.joint <- data.frame(da$X_p,da$X_q)}
          if("X_aux" %in% names(da)) if(!is.null(da$X_aux)) da.joint <- data.frame(da.joint, da$X_aux)
          if("t" %in% names(da)) if(!is.null(da$t)) da.joint <- data.frame(da.joint, da$t)
          if(is.null(quadr) & ncol(da.joint)>1) ini <- mice(da.joint,maxit=0)
          if(any(is.na(da.joint))) {
            if(!is.null(transf)) {
              Res <- NULL
              for (tr in transf) {
                x <- da.joint[,tr]
                if(any(is.na(x)) & !any(na.omit(x)<0)) {   
                  if(any(na.omit(x)==0)) off=(max(x,na.rm=T)-min(x,na.rm=T))/100 else off=0                  
                  res = c(Raw = shapiro.test(x)$statistic[[1]],                          
                          Log = shapiro.test(log(x+off))$statistic[[1]],
                          Sqrt = shapiro.test(sqrt(x))$statistic[[1]],
                          Crt = shapiro.test(x^(1/3))$statistic[[1]])
                  Max <- names(which.max(res))
                  Res <- rbind(Res, c(Variable =tr, Selected=Max,Off=off))
                  da.joint[,tr] <- switch(Max, Raw = x, Log = log(x+off), Sqrt = sqrt(x), Crt = x^(1/3))}}}
            if(!is.null(quadr)) {
              da.joint <- data.frame(da.joint, da.joint[,quadr]^2)
              ini <- mice(da.joint,maxit=0)
              ini$meth[(length(ini$meth)-length(quadr)+1):length(ini$meth)] <- paste("~I(",quadr,"^2)",sep="") }    
            if(ncol(da.joint)>1)  com <- complete.stab(da.joint,M=M,meth=ini$meth)  else  com <- lapply(1:M, function(m) mice.impute.sample.mod(da.joint))   }   else com <- lapply(1:M,function(m) da.joint)
          
          # backtransformation
          if(!is.null(transf)) {
            if(nrow(Res)>0) {
              com <- lapply(com, function(co) {
                for (i in 1:nrow(Res)) {
                  x <- co[,as.character(Res[i,1])] 
                  co[,as.character(Res[i,1])] <- switch(as.character(Res[i,2]),Raw=x, Log=exp(x)-as.numeric(as.character(Res[i,3])), Sqrt=x^2, Crt=x^3) }
                co}) }}
          
          if(!is.null(com)) com <- lapply(com, function(co) {
            da_ <- da
            if(p==0) da_$X_p <- NULL else da_$X_p=co[,as.numeric(y.include)+(1:p)]
            da_$X_q <- co[,as.numeric(y.include)+p+(1:q)]                           
            if("X_aux" %in% names(da_)) da_ <- da_[-which(names(da_)=="X_aux")] 
            da_ }) else com <- da} else com <- da} else com <- da  
            com     }
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    do.mice.one.stab <- function(...) tryCatch(do.mice.one(...), error=function(e) NULL)
    
    # a little awkward...
    if(dep==1) results <- do.mice.one.stab(dat)
    if(dep==2) results <- lapply(1:di[1],             function(d) do.mice.one.stab(dat[[d]]))
    if(dep==3) results <- lapply(1:di[1],             function(d1) if(!is.list(dat[[d1]])) dat[[d1]] else 
      lapply(1:length(dat[[d1]]), function(d2) if(!is.list(dat[[d1]][[d2]])) dat[[d1]][[d2]] else do.mice.one.stab(dat[[d1]][[d2]])) )
    if(dep==4) results <- lapply(1:di[1],             function(d1) if(!is.list(dat[[d1]])) dat[[d1]] else 
      lapply(1:length(dat[[d1]]), function(d2) if(!is.list(dat[[d1]][[d2]])) dat[[d1]][[d2]] else 
        lapply(1:length(dat[[d1]][[d2]]), function(d3) if(!is.list(dat[[d1]][[d2]][[d3]])) dat[[d1]][[d2]][[d3]] else do.mice.one.stab(dat[[d1]][[d2]][[d3]]))))
    if(dep==5) results <- lapply(1:di[1],             function(d1) if(!is.list(dat[[d1]])) dat[[d1]] else 
      lapply(1:length(dat[[d1]]), function(d2) if(!is.list(dat[[d1]][[d2]])) dat[[d1]][[d2]] else 
        lapply(1:length(dat[[d1]][[d2]]), function(d3) if(!is.list(dat[[d1]][[d2]][[d3]])) dat[[d1]][[d2]][[d3]] else 
          lapply(1:length(dat[[d1]][[d2]][[d3]]), function(d4) if(!is.list(dat[[d1]][[d2]][[d3]][[d4]])) dat[[d1]][[d2]][[d3]][[d4]] else do.mice.one.stab(dat[[d1]][[d2]][[d3]][[d4]])) )))
    if(dep==6) results <- lapply(1:di[1],             function(d1) if(!is.list(dat[[d1]])) dat[[d1]] else 
      lapply(1:length(dat[[d1]]), function(d2) if(!is.list(dat[[d1]][[d2]])) dat[[d1]][[d2]] else 
        lapply(1:length(dat[[d1]][[d2]]), function(d3) if(!is.list(dat[[d1]][[d2]][[d3]])) dat[[d1]][[d2]][[d3]] else 
          lapply(1:length(dat[[d1]][[d2]][[d3]]), function(d4) if(!is.list(dat[[d1]][[d2]][[d3]][[d4]])) dat[[d1]][[d2]][[d3]][[d4]] else 
            lapply(1:length(dat[[d1]][[d2]][[d3]][[d4]]), function(d5) if(!is.list(dat[[d1]][[d2]][[d3]][[d4]][[d5]])) dat[[d1]][[d2]][[d3]][[d4]][[d5]] else do.mice.one.stab(dat[[d1]][[d2]][[d3]][[d4]][[d5]])) ))))
    if(dep>6) stop("depths larger than 6 are currently not supported")
    list(results,type=typ) }



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function do.bs: draw B bootstrap (or subsampling or cross-validation) samples and save BS as well as OOB draws
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

do.bs <- function(dat,               # either a list with elements y, X_p, X_q, Xmiss_p and Xmiss_q (when BS comes before MICE)
                  # or a list with M elements each with elements y, X_p, X_q (when BS comes after MICE)
                  B=1000,            # number of bootstrap replicates
                  K=10,              # number of folds in K-fold CV
                  method="bs",       # "bs","ss","cv"
                  keep.oob=T,
                  seed=NULL,         # seed number to conciliate BS ids e.g. between imputed data sets
                  strat=T){          # logical, indicating whether BS samples should be drawn in a stratified way (to contain same number of y=0/1 as original sample)
  
  if("type" %in% names(dat)) {
    typ=c(unlist(dat$type),method) 
    dat <- dat[-which(names(dat)=="type")]}  else typ=method
    di <- dims(dat)
    if(di[1]==1) dat <- dat[[1]]
    di <- dims(dat)
    dep <- depth(dat)
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    do.bs.one <- function(da) {
      if(is.list(da) & "X_q" %in% names(da)) {
        n <- length(da$y)
        inner <- "bs.ids" %in% names(da)
        if("type" %in% names(da)) typ=c(da$type,method)  else typ=method
        
        if(!is.null(seed)) set.seed(seed)  # will take same ids (rows) for the respective BS/OOB/CV
        
        if(method %in% c("bs","ss")) {
          
          repl <- method=="bs" 
          
          ret <- lapply(1:B, function(b) {
            
            if(!strat) bs.ids <- sample(1:n,ifelse(repl, n, rounddownup(n*ssfrac)),replace=repl)
            if(strat) {
              n0 <- ifelse(repl,length(which(da$y==0)), rounddownup(length(which(da$y==0))*ssfrac))
              n1 <- ifelse(repl, n-n0, rounddownup(n*ssfrac)-n0)
              bs.ids <- c(sample(which(da$y==0), n0, replace=repl),
                          sample(which(da$y==1), n1, replace=repl)) }
            
            if(!inner) {
              if(keep.oob) oob.ids <- setdiff(1:n,bs.ids)     
              bs.orig.ids <- NULL }
            
            if(inner) {
              if(keep.oob) {
                bs.orig.ids <- da$bs.ids[bs.ids]  
                oob.orig.ids <- setdiff(da$bs.ids, bs.orig.ids)
                oob.ids <- match(oob.orig.ids,da$bs.ids)
                n.oob <- n-length(unique(bs.ids))
                if(!strat) oob.ids <- sample(oob.ids, ifelse(repl, n.oob, roundupdown(n.oob*ssfrac)), replace=repl)
                if(strat) {
                  n0  <- length(which(da$y[oob.ids]==0))
                  n1 <- length(which(da$y[oob.ids]==1))
                  n.oob0 <- ifelse(repl, rounddownup(n.oob*n0/(n0+n1)), rounddownup((n.oob*n0/(n0+n1))*ssfrac))
                  n.oob1 <- ifelse(repl, n.oob-n.oob0, rounddownup(n.oob*ssfrac)-n.oob0)          
                  oob.ids <- c(sample(oob.ids[da$y[oob.ids]==0], n.oob0, replace=repl),
                               sample(oob.ids[da$y[oob.ids]==1], n.oob1, replace=repl)) } }
              bs.orig.ids<-da$bs.ids} 
            
            if(is.null(da$X_p)) {   bs.X_p <- oob.X_p <- NULL } else {
              bs.X_p <- as.matrix(da$X_p)[bs.ids,,drop=F]
              if(keep.oob) oob.X_p <- as.matrix(da$X_p)[oob.ids,,drop=F] }
            
            bs.dat <- list(y=da$y[bs.ids], t=da$t[bs.ids], X_p = bs.X_p, X_q=as.matrix(da$X_q)[bs.ids,,drop=F],  X_aux=da$X_aux[bs.ids,], bs.orig.ids=bs.orig.ids, bs.ids=bs.ids)
            if(keep.oob) oob.dat <- list(y=da$y[oob.ids], t=da$t[oob.ids], X_p = oob.X_p, X_q=as.matrix(da$X_q)[oob.ids,,drop=F],  X_aux=da$X_aux[oob.ids,], oob.ids=oob.ids) else {oob.dat <- oob.ids <- NULL}
            
            list(bs.dat=bs.dat, oob.dat=oob.dat)  } )}
        
        
        if(method == "cv") {
          
          if(!inner) {
            if(!strat) cv.ids <- sapply(1:B,function(b) sample(rep(1:K,length.out=n)))   # n x B matrix with k different numbers 1:k assigned to obs        
            if(strat) {
              cv.ids <- matrix(NA, nrow=n, ncol=B)
              cv.ids[which(da$y==0),] <- sapply(1:B,function(b) sample(rep(1:K,length.out=length(which(da$y==0)))))
              cv.ids[which(da$y==1),] <- sapply(1:B,function(b) sample(rep(1:K,length.out=length(which(da$y==1))))) } 
            bs.orig.ids <- NULL}
          
          if(inner) {
            new.unit.ids <- which(!duplicated(da$bs.ids))
            if(!strat)  cv.ids <- sapply(1:B,function(b) sample(rep(1:K,length.out=length(new.unit.ids)))) 
            if(strat) {
              cv.ids <- matrix(NA, nrow=length(new.unit.ids), ncol=B)
              cv.ids[which(da$y[new.unit.ids]==0),] <- sapply(1:B,function(b) sample(rep(1:K,length.out=length(which(da$y[new.unit.ids]==0)))))
              cv.ids[which(da$y[new.unit.ids]==1),] <- sapply(1:B,function(b) sample(rep(1:K,length.out=length(which(da$y[new.unit.ids]==1))))) } 
            new.unit.all <- match(da$bs.ids, da$bs.ids[new.unit.ids])
            cv.ids <- cv.ids[new.unit.all,,drop=F] 
            bs.orig.ids <- da$bs.ids}     # rows correspond to distinct obs (partly instances of the same unit) in oder of da$y e.g.
          
          
          ret <- lapply(1:B, function(b) { lapply(1:K, function(k) {
            train.ids <- which(cv.ids[,b]!=k)
            test.ids <- which(cv.ids[,b]==k)
            if(is.null(da$X_p)) { bs.X_p <- oob.X_p <- NULL } else {
              bs.X_p = as.matrix(da$X_p)[train.ids,]
              oob.X_p = as.matrix(da$X_p)[test.ids,]   }
            bs.dat <- list(y=da$y[train.ids], t=da$t[train.ids], X_p=bs.X_p, X_q=as.matrix(da$X_q)[train.ids,,drop=F], X_aux=da$X_aux[train.ids,], bs.orig.ids=bs.orig.ids, cv.ids=cv.ids)
            oob.dat <- list(y=da$y[test.ids], t=da$t[test.ids], X_p=oob.X_p, X_q=as.matrix(da$X_q)[test.ids,,drop=F], X_aux=da$X_aux[test.ids,]) 
            return(list(bs.dat=bs.dat,oob.dat=oob.dat))})})     }  } else ret <- da
            
            ret }
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    
    if(dep==1) results <- do.bs.one(da=dat)
    if(dep==2) results <- lapply(1:di[1], function(d) do.bs.one(da=dat[[d]]))
    if(dep==3) results <- lapply(1:di[1], function(d1) lapply(1:di[2], function(d2) do.bs.one(da=dat[[d1]][[d2]])))
    if(dep==4) results <- lapply(1:di[1], function(d1) lapply(1:di[2], function(d2) lapply(1:di[3], function(d3) do.bs.one(da=dat[[d1]][[d2]][[d3]])))) 
    if(dep==5) results <- lapply(1:di[1], function(d1) lapply(1:di[2], function(d2) lapply(1:di[3], function(d3) lapply(1:di[4], function(d4) do.bs.one(da=dat[[d1]][[d2]][[d3]][[d4]]))))) 
    if(dep==6) results <- lapply(1:di[1], function(d1) lapply(1:di[2], function(d2) lapply(1:di[3], function(d3) lapply(1:di[4], function(d4) lapply(1:di[5], function(d5) do.bs.one(da=dat[[d1]][[d2]][[d3]][[d4]][[d5]]))))))
    if(dep>6) stop("depths larger than 6 are currently not supported")
    
    list(results,type=typ) }


##################### Auxiliary functions

### 1. fisher transformation (from psyh package)
fisherz <- function(rho) 0.5 * log((1 + rho)/(1 - rho))
fisherz2r <- function(z) (exp(2 * z) - 1)/(1 + exp(2 * z))



### 2. perc.ci (from boot package)
perc.ci.mod <- function(t, conf=0.95) {
  alpha <- (1 + c(-conf, conf))/2
  if(length(which(is.na(t)))/length(t) < 0.5) {
    qq <- suppressWarnings(tryCatch(boot:::norm.inter(na.omit(c(t)), alpha)[,2],error=function(e) NA)) 
    ret <- c(lcl=min(qq),ucl=max(qq))} else ret <- c(lcl=NA,ucl=NA)
    ret}


### 3. rubin
rubin <- function(Means,Vars,conf=0.95, df.model=NULL,meas,theta.noinf) {
  which.complete <- which(complete.cases(Means) & complete.cases(Vars) & !(Means %in% c(-Inf,+Inf)) & !(Vars %in% c(-Inf,+Inf)))
  M <- length(which.complete)
  Means <- Means[which.complete]
  Vars <- Vars[which.complete]
  th <- mean(Means)          
  W = mean(Vars)            
  B = var(Means)              
  To = W+ (1+1/M)*B          
  SE <- sqrt(To)             
  lambda = (1+1/M)*B/To     
  dfold = (M-1)/(lambda^2)   
  dfcom = df.model                     
  dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)   
  DF <- dfold * dfobs/(dfold + dfobs)
  if(lambda==0) DF <- dfobs            
  CI <- th +  qt(1-(1-conf)/2, df=DF)*c(-1,1)*SE
  p <- ifelse(meas %in% c("calint","calslope"), 2, 1)*pt(abs((th-theta.noinf[[sub("laan","",meas)]])/SE), df=DF,lower=F)
  return(c(p=p,lcl=min(CI),ucl=max(CI))) } 

### 3.a rubin.stab
rubin.stab <- function(...) {
  tryCatch(rubin(...),error= function(e)  c(p=NA,lcl=NA,ucl=NA) )  }


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function do.cis.one
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

do.cis.one <- function(perf=NULL,perfBase=NULL, conf=0.95, df.model, inference=F, K=10,pos=NA) {    
  znorm <- qnorm(1-(1-conf)/2)                                       # pos = position of statistic to be considered
  
  if(!is.null(perf)) {
    if("type" %in% names(perf)) {
      typ=unlist(perf$type) 
      perf <- perf[-which(names(perf)=="type")]}  else typ=NULL
      di <- dims(perf)                
      if(di[1]==1) perf <- perf[[1]]
      di <- dims(perf)                 
      if(di[1]==1 & depth(perf)>3) perf <- perf[[1]]    
      di <- dims(perf)
      dep <- depth(perf) }       else dep=0     
      
      if(!is.null(perfBase)) {
        if("type" %in% names(perfBase)) {
          typBase=unlist(perfBase$type) 
          perfBase <- perfBase[-which(names(perfBase)=="type")]}  else typBase=NULL
          diBase <- dims(perfBase)
          if(diBase[1]==1) perfBase <- perfBase[[1]]
          diBase <- dims(perfBase)
          depBase <- depth(perfBase) }  else depBase=0
          
          if(depBase==1) {    
            if(!is.null(perfBase)) {
              measures <- names(perfBase)[!grepl("noinf",names(perfBase))]
              theta.orig <- sapply(measures, function(meas) perfBase[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perfBase[[meas]])))]])   
              if(any(grepl("noinf",setdiff(names(perfBase),"warn.noinf")))) {
                theta.noinf <- sapply(measures, function(meas)  {
                  inside <- inside1 <- paste(gsub("\\d*$","",meas),".noinf",sep="")     
                  num.l <-  grepl("[0-9]",substr(meas,nchar(meas),nchar(meas)))
                  num.ll <-  grepl("[0-9]",substr(meas,nchar(meas)-1,nchar(meas)-1))
                  if(num.l & (!grepl("nricat",meas) | num.ll)) {
                    inside <- paste(substr(meas,1,nchar(meas)-1),".noinf",substr(meas,nchar(meas),nchar(meas)),sep="")
                    inside1 <- paste(substr(meas,1,nchar(meas)-1),".noinf",sep="")}
                  ret <- perfBase[[inside]][[inside1]] 
                  if(is.null(ret)) ret <- 0   
                  ret}) } else {
                    theta.noinf <- theta.orig
                    theta.noinf[1:length(theta.noinf)] <- 0 } 
              if(inference & is.null(perf)) {
                ci.orig <- sapply(measures, function(meas) {
                  if(paste(gsub("\\d*$","",meas),".lcl",sep="") %in% names(perfBase[[meas]])) {
                    stat.lcl <- perfBase[[meas]][[paste(gsub("\\d*$","",meas),".lcl",sep="")]]
                    stat.ucl <- perfBase[[meas]][[paste(gsub("\\d*$","",meas),".ucl",sep="")]] } else {stat.lcl <- stat.ucl <- NA}
                  if(paste(gsub("\\d*$","",meas),".p",sep="") %in% names(perfBase[[meas]])) {
                    stat.p <- perfBase[[meas]][[paste(gsub("\\d*$","",meas),".p",sep="")]] } else stat.p <- NA
                    c(p=stat.p,lcl=stat.lcl,ucl=stat.ucl)}) } } else {
                      theta.orig <- theta.noinf <- NA
                      if(inference & is.null(perf)) ci.orig <- c(p=NA,lcl=NA,ucl=NA)}}
          
          if(depBase==2) {  
            measures <- unique(c(unlist(sapply(1:length(perfBase), function(m) names(perfBase[[m]])[!grepl("noinf",names(perfBase[[m]]))]))))
            theta.orig <- sapply(measures, function(meas) {
              ret <- sapply(1:length(perfBase), function(m) if (!is.null(perfBase[[m]])) perfBase[[m]][[meas]][[ifelse(is.na(pos),ifelse(meas %in% c("r2brier","r2cor"), "stat",gsub("\\d*$","",meas)),min(pos,length(perfBase[[m]][[meas]])))]] else  NA)
              ret[ret==-Inf | ret==Inf] <- NA
              if(length(which(is.na(ret)))/length(ret)<0.5) ret <- mean(ret,na.rm=T) else ret <- NA
              if(!is.na(ret)) if(meas %in% c("r2brier","r2cor")) ret <-  fisherz2r(ret)^2*sign(fisherz2r(ret))
              ret})
            if(any(grepl("noinf",setdiff(names(perfBase[[1]]),"warn.noinf")))) {
              theta.noinf <- sapply(measures, function(meas)  {
                inside <- inside1 <- paste(gsub("\\d*$","",meas),".noinf",sep="")     
                num.l <-  grepl("[0-9]",substr(meas,nchar(meas),nchar(meas)))          
                num.ll <-  grepl("[0-9]",substr(meas,nchar(meas)-1,nchar(meas)-1))
                if(num.l & (!grepl("nricat",meas) | num.ll)) {
                  inside <- paste(substr(meas,1,nchar(meas)-1),".noinf",substr(meas,nchar(meas),nchar(meas)),sep="")
                  inside1 <- paste(substr(meas,1,nchar(meas)-1),".noinf",sep="")}
                ret <- sapply(1:length(perfBase), function(m) {
                  if(!is.null(perfBase[[m]])) re <- perfBase[[m]][[inside]][[inside1]] else re <-NA
                  if(is.null(re)) re <- 0
                  re}) 
                if(is.null(ret)) ret <- rep(0,length(perfBase)) 
                if(!any(!is.na(ret))) ret <- rep(0,length(perfBase))
                ret <- mean(ret,na.rm=T) 
                ret}) } else {
                  theta.noinf <- theta.orig
                  theta.noinf[1:length(theta.noinf)] <- 0 }   
            if(inference & is.null(perf)) {
              ci.mice <- sapply(measures, function(meas) {
                if(any(sapply(1:length(perfBase), function(m) "stat" %in% names(perfBase[[m]][[meas]])))) {     
                  stat <- sapply(1:length(perfBase), function(m) perfBase[[m]][[meas]][["stat"]])
                  stat.var <- sapply(1:length(perfBase), function(m) perfBase[[m]][[meas]][["stat.var"]])
                  if(length(which(stat==Inf | stat==-Inf | stat.var==Inf | stat.var==-Inf | is.na(stat) | is.na(stat.var)))/length(stat)<0.5) {            res <- rubin.stab(stat,stat.var,df.model=df.model, meas=meas, theta.noinf=theta.noinf) } else res <-  c(p=NA,lcl=NA,ucl=NA)   } else res <-  c(p=NA,lcl=NA,ucl=NA)
                  if(!is.na(res["ucl"]) & !is.na(res["lcl"])) {
                    if(meas %in% c("r2brier","r2cor")) {
                      res["lcl"] <- fisherz2r(res["lcl"])^2*sign(fisherz2r(res["lcl"]))
                      res["ucl"] <- fisherz2r(res["ucl"])^2*sign(fisherz2r(res["lcl"])) }}
                  res})       } }
          
          
          if(dep==3) {    
            measures <- unique(c(unlist(sapply(1:length(perf), function(b) c(names(perf[[b]][[1]])[!grepl("noinf",names(perf[[b]][[1]]))],names(perf[[b]][[2]])[!grepl("noinf",names(perf[[b]][[2]]))])))))
            is.cv <- !("perf.bs.orig" %in% names(perf[[1]]))
            thetas.bs.bs <- sapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(b) ifelse(is.null(perf[[b]]$perf.bs.bs), NA, perf[[b]]$perf.bs.bs[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[b]]$perf.bs.bs[[meas]])))]])))
            thetas.bs.oob <- sapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(b) ifelse(is.null(perf[[b]]$perf.bs.oob), NA, perf[[b]]$perf.bs.oob[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[b]]$perf.bs.oob[[meas]])))]])))
            thetas.bs.bs[thetas.bs.bs ==Inf | thetas.bs.bs==-Inf] <- NA
            thetas.bs.oob[thetas.bs.oob ==Inf | thetas.bs.oob==-Inf] <- NA
            if(is.matrix(thetas.bs.bs)) {   
              theta.bs.mean <- apply(thetas.bs.bs,2, function(th) ifelse(length(which(!is.na(th)))/length(th)>=0.5, mean(th,na.rm=T), NA)) } else theta.bs.mean <- thetas.bs.bs
            if(is.matrix(thetas.bs.oob)) {
              theta.oob.mean <- apply(thetas.bs.oob,2, function(th) ifelse(length(which(!is.na(th)))/length(th)>=0.5, mean(th,na.rm=T), NA)) } else theta.oob.mean <- thetas.bs.oob
            if(!is.cv) {
              thetas.bs.orig <- sapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(b) ifelse(is.null(perf[[b]]$perf.bs.orig), NA, perf[[b]]$perf.bs.orig[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[b]]$perf.bs.orig[[meas]])))]])))
              thetas.bs.orig[thetas.bs.orig ==Inf | thetas.bs.orig==-Inf] <- NA
              if(is.matrix(thetas.bs.orig)) {
                theta.orig.mean <- apply(thetas.bs.orig,2,function(th) ifelse(length(which(!is.na(th)))/length(th)>=0.5, mean(th,na.rm=T), NA))} else theta.orig.mean <- thetas.bs.orig
              theta.optcorr <- sapply(names(theta.oob.mean), function(meas) theta.orig[meas]-theta.bs.mean[meas]+theta.orig.mean[meas])
              names(theta.optcorr) <- names(theta.oob.mean)
              fac <-  1-1/exp(1) } else {
                fac <- (K-1)/K    
                theta.cvstar <- sapply(names(theta.oob.mean), function(meas) theta.orig[meas] + fac * (theta.oob.mean[meas]-theta.bs.mean[meas])) 
                names(theta.cvstar) <- names(theta.oob.mean) }
            
            theta.0.632 <- sapply(names(theta.oob.mean), function(meas) (1-fac)*theta.orig[meas] + fac*theta.oob.mean[meas])              
            R <- sapply(names(theta.oob.mean), function(meas) (theta.oob.mean[meas] - theta.orig[meas])/(theta.noinf[meas] - theta.orig[meas]))
            names(R) <- names(theta.oob.mean)
            sel1 <- which(names(R) %in% c("auc","auc.t","r2brier","r2cor","yates","dauc","dauc.t","dr2brier","dr2cor","nricat","nricont","idi","auc1","auc0","auc.t1","auc.t0","r2brier1","r2brier0","r2cor1","r2cor0","yates1","yates0") | grepl("nricat",names(R)))
            sel2 <- which(names(R) %in% c("brier","error","dbrier","brier1","brier0","error1","error0"))
            if(length(sel1)>=1) R[sel1] <- ifelse(theta.oob.mean[sel1] <= theta.noinf[sel1], 1, ifelse(theta.noinf[sel1] < theta.oob.mean[sel1] & theta.oob.mean[sel1] < theta.orig[sel1],R[sel1],0))
            if(length(sel2)>=1) R[sel2] <- ifelse(theta.oob.mean[sel2] >= theta.noinf[sel2], 1, ifelse(theta.noinf[sel2] > theta.oob.mean[sel2] & theta.oob.mean[sel2] > theta.orig[sel2],R[sel2],0))
            w = fac/(1-(1-fac)*R)
            names(w) <- names(theta.oob.mean)
            theta.0.632p <- sapply(names(theta.oob.mean), function(meas) (1-w[meas])*theta.orig[meas] + w[meas]*theta.oob.mean[meas])  
            names(theta.0.632) <- names(theta.0.632p) <- names(theta.oob.mean) 
            
            if(inference) {
              if(!is.cv) {
                ci.bs.perc <- apply(thetas.bs.bs, 2, function(th)  perc.ci.mod(th))   
                ci.bs.cai <- apply(sapply(setdiff(measures,"auclaan"), function(meas) theta.0.632p[meas] - perc.ci.mod(thetas.bs.bs[,meas]-theta.orig[[meas]])),2,sort,na.last=T)
                ci.bs.cai.mod <- apply(sapply(setdiff(measures,"auclaan"), function(meas) theta.0.632p[meas] + perc.ci.mod(thetas.bs.bs[,meas]-theta.orig[[meas]])),2,sort,na.last=T)
                row.names(ci.bs.cai) <- row.names(ci.bs.cai.mod) <- c("lcl","ucl") }
              if(any(sapply(1:length(perf), function(b) sapply(measures, function(me) ifelse(is.null(perf[[b]]$perf.bs.oob), FALSE, "stat.var" %in% names(perf[[b]]$perf.bs.oob[[me]])))))) {  
                ci.ss.median <- sapply(measures, function(meas) {
                  sapl <- sapply(1:length(perf), function(b) {
                    if(!is.null(perf[[b]]$perf.bs.oob)) { 
                      if(paste(gsub("\\d*$","",meas),".lcl",sep="") %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                        stat.lcl <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".lcl",sep="")]]
                        stat.ucl <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".ucl",sep="")]]  }   else {
                          if("stat" %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                            stat <- perf[[b]]$perf.bs.oob[[meas]][["stat"]]
                            stat.var <- perf[[b]]$perf.bs.oob[[meas]][["stat.var"]]
                            stat.lcl <- stat - znorm*sqrt(stat.var)
                            stat.ucl <- stat + znorm*sqrt(stat.var)   
                            if(meas %in% c("r2cor","r2brier")) {
                              stat.lcl <- fisherz2r(stat.lcl)^2*sign(fisherz2r(stat.lcl))
                              stat.ucl <- fisherz2r(stat.ucl)^2*sign(fisherz2r(stat.ucl))  } } else { stat.lcl <- stat.ucl <- NA }}
                      if(paste(gsub("\\d*$","",meas),".p",sep="")  %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                        stat.p <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".p",sep="")]]  } else {
                          if("stat" %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                            stat <- perf[[b]]$perf.bs.oob[[meas]][["stat"]]
                            stat.var <- perf[[b]]$perf.bs.oob[[meas]][["stat.var"]]
                            stat.p <- ifelse(meas %in% c("calint","calslope"),2,1)*pnorm(-abs((stat-theta.noinf[[sub("laan","",meas)]])/sqrt(stat.var))) } else {stat.p <- NA }  }
                      ret <- c(p=stat.p,lcl=stat.lcl,ucl=stat.ucl)} else ret <- c(p=NA,lcl=NA,ucl=NA) 
                      ret})
                  sapl["p",][sapl["lcl",]==0 & sapl["ucl",]==0]  <- 1 
                  sapl <- apply(sapl,1,function(sa) if(length(which(is.na(sa)))/length(sa)<0.5) sa else rep(NA,length(sa)))
                  apply(sapl,2,median,na.rm=T)}) } } }
          
          
          if(dep==4) {   
            measures <- unique(c(unlist(sapply(1:length(perf), function(m) sapply(1:length(perf[[m]]), function(b) c(names(perf[[m]][[b]][[1]])[!grepl("noinf",names(perf[[m]][[b]][[1]]))],names(perf[[m]][[b]][[2]])[!grepl("noinf",names(perf[[m]][[b]][[2]]))]))))))
            thetas.bs.bs <- lapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(m) sapply(1:length(perf[[1]]), function(b) ifelse(is.null(perf[[m]][[b]]$perf.bs.bs), NA, {
              pe <- perf[[m]][[b]]$perf.bs.bs[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[m]][[b]]$perf.bs.bs[[meas]])))]]
              pe[pe==Inf | pe==-Inf] <- NA
              pe}))))
            thetas.bs.oob <- lapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(m) sapply(1:length(perf[[1]]), function(b) ifelse(is.null(perf[[m]][[b]]$perf.bs.oob), NA, {
              pe <- perf[[m]][[b]]$perf.bs.oob[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[m]][[b]]$perf.bs.oob[[meas]])))]]
              pe[pe==Inf | pe==-Inf] <- NA
              pe}))))
            names(thetas.bs.bs) <- names(thetas.bs.oob) <- setdiff(measures,"auclaan") 
            theta.bs.mean <- sapply(thetas.bs.bs,function(th) ifelse(length(which(!is.na(th)))/length(th)>=0.5, mean(th,na.rm=T), NA))
            theta.oob.mean <- sapply(thetas.bs.oob,function(th) ifelse(length(which(!is.na(th)))/length(th)>=0.5, mean(th,na.rm=T), NA))
            is.cv <- !("perf.bs.orig" %in% names(perf[[1]][[1]]))
            if(!is.cv)  {
              thetas.bs.orig <- lapply(setdiff(measures,"auclaan"), function(meas) sapply(1:length(perf), function(m) sapply(1:length(perf[[1]]), function(b) ifelse(is.null(perf[[m]][[b]]$perf.bs.orig), NA, {
                pe <- perf[[m]][[b]]$perf.bs.orig[[meas]][[ifelse(is.na(pos),gsub("\\d*$","",meas),min(pos,length(perf[[m]][[b]]$perf.bs.orig[[meas]])))]]
                pe[pe==Inf | pe==-Inf] <- NA
                pe}))))
              names(thetas.bs.orig) <- setdiff(measures,"auclaan") 
              theta.optcorr <- sapply(names(theta.oob.mean), function(meas) theta.orig[meas]-theta.bs.mean[meas]+mean(thetas.bs.orig[[meas]],na.rm=T))
              names(theta.optcorr) <- names(theta.oob.mean) 
              fac <-  1-1/exp(1) } else {
                fac <- (K-1)/K     
                theta.cvstar <- sapply(names(theta.oob.mean), function(meas) theta.orig[meas] + fac * (theta.oob.mean[meas]-theta.bs.mean[meas])) 
                names(theta.cvstar) <- names(theta.oob.mean) }
            
            theta.0.632 <- sapply(names(theta.oob.mean), function(meas) (1-fac)*theta.orig[meas] + fac*theta.oob.mean[meas])    
            R <- sapply(names(theta.oob.mean), function(meas) (theta.oob.mean[meas] - theta.orig[meas])/(theta.noinf[meas] - theta.orig[meas]))
            names(R) <- names(theta.oob.mean)
            sel1 <- which(names(R) %in% c("auc","auc.t","r2brier","r2cor","yates","dauc","dauc.t","dr2brier","dr2cor","nricat","nricont","idi","auc1","auc0","auc.t1","auc.t0","r2brier1","r2brier0","r2cor1","r2cor0","yates1","yates0") | grepl("nricat",names(R)))
            sel2 <- which(names(R) %in% c("brier","error","dbrier","brier1","brier0","error1","error0"))
            if(length(sel1)>=1) R[sel1] <- ifelse(theta.oob.mean[sel1] <= theta.noinf[sel1], 1, ifelse(theta.noinf[sel1] < theta.oob.mean[sel1] & theta.oob.mean[sel1] < theta.orig[sel1],R[sel1],0))
            if(length(sel2)>=1) R[sel2] <- ifelse(theta.oob.mean[sel2] >= theta.noinf[sel2], 1, ifelse(theta.noinf[sel2] > theta.oob.mean[sel2] & theta.oob.mean[sel2] > theta.orig[sel2],R[sel2],0))
            w = fac/(1-(1-fac)*R)
            names(w) <- names(theta.oob.mean)
            theta.0.632p <- sapply(names(theta.oob.mean), function(meas) (1-w[meas])*theta.orig[meas] + w[meas]*theta.oob.mean[meas]) 
            names(theta.0.632) <- names(theta.0.632p) <- names(theta.oob.mean) 
            
            if(inference) {
              if(!is.cv) {
                ci.bs.perc <- sapply(thetas.bs.bs, function(th) perc.ci.mod(th))
                ci.bs.cai <- apply(sapply(setdiff(measures,"auclaan"), function(meas) theta.0.632p[meas] - perc.ci.mod(thetas.bs.bs[[meas]]-theta.orig[[meas]])),2,sort,na.last=T)
                ci.bs.cai.mod <- apply(sapply(setdiff(measures,"auclaan"), function(meas) theta.0.632p[meas] + perc.ci.mod(thetas.bs.bs[[meas]]-theta.orig[[meas]])),2,sort,na.last=T)
                row.names(ci.bs.cai) <- row.names(ci.bs.cai.mod) <- c("lcl","ucl") }
              if(any(sapply(1:length(perf), function(m) sapply(1:length(perf[[m]]), function(b) sapply(measures, function(me) ifelse(is.null(perf[[m]][[b]]$perf.bs.oob), FALSE, "stat.var" %in% names(perf[[m]][[b]]$perf.bs.oob[[me]]))))))) {   
                ci.ss.median <- sapply(measures, function(meas) { 
                  sapl <- sapply(1:length(perf[[1]]), function(b) {
                    stat_and_var <- sapply(1:length(perf), function(m) {          
                      if(!is.null(perf[[m]][[b]]$perf.bs.oob)) { 
                        if("stat" %in% names(perf[[m]][[b]]$perf.bs.oob[[meas]])) { 
                          c(stat=perf[[m]][[b]]$perf.bs.oob[[meas]][["stat"]], stat.var=perf[[m]][[b]]$perf.bs.oob[[meas]][["stat.var"]]) } else c(stat=NA,stat.var=NA) } else
                            c(stat=NA,stat.var=NA)   })
                    stat <- stat_and_var[1,]
                    stat.var <- stat_and_var[2,]
                    if(length(which(stat==Inf | stat==-Inf | stat.var==Inf | stat.var==-Inf | is.na(stat) | is.na(stat.var)))/length(stat)<0.5) { 
                      res <- rubin.stab(stat,stat.var,df.model=df.model,meas=meas,theta.noinf=theta.noinf) } else res <-  c(p=NA,lcl=NA,ucl=NA) 
                    if(!is.na(res["lcl"]) & !is.na(res["ucl"])) { 
                      if(meas %in% c("r2cor","r2brier")) {
                        res["lcl"] <- fisherz2r(res["lcl"])^2*sign(fisherz2r(res["lcl"]))
                        res["ucl"] <- fisherz2r(res["ucl"])^2*sign(fisherz2r(res["ucl"]))  } }
                    res })
                  sapl["p",][sapl["lcl",]==0 & sapl["ucl",]==0]  <- 1
                  if(ncol(sapl)>1) sapl <- t(apply(sapl,1,function(sa) if(length(which(is.na(sa)))/length(sa)<0.5) sa else rep(NA,length(sa)))) 
                  apply(sapl,1,median,na.rm=T)} )
                perf <- sapply(1:length(perf), function(m) perf[[m]])  
                ci.ss.median.acrossBM <- sapply(measures, function(meas) {   
                  sapl <- sapply(1:length(perf), function(b) {
                    if(!is.null(perf[[b]]$perf.bs.oob)) {
                      if(paste(gsub("\\d*$","",meas),".lcl",sep="") %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                        stat.lcl <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".lcl",sep="")]]
                        stat.ucl <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".ucl",sep="")]]  }   else {
                          if("stat" %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                            stat <- perf[[b]]$perf.bs.oob[[meas]][["stat"]]
                            stat.var <- perf[[b]]$perf.bs.oob[[meas]][["stat.var"]]
                            stat.lcl <- stat - znorm*sqrt(stat.var)
                            stat.ucl <- stat + znorm*sqrt(stat.var)   
                            if(meas %in% c("r2cor","r2brier")) {
                              stat.lcl <- fisherz2r(stat.lcl)^2*sign(fisherz2r(stat.lcl))
                              stat.ucl <- fisherz2r(stat.ucl)^2*sign(fisherz2r(stat.ucl))  } } else { stat.lcl <- stat.ucl <- NA }}
                      if(paste(gsub("\\d*$","",meas),".p",sep="")  %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                        stat.p <- perf[[b]]$perf.bs.oob[[meas]][[paste(gsub("\\d*$","",meas),".p",sep="")]]  } else {
                          if("stat" %in% names(perf[[b]]$perf.bs.oob[[meas]])) {
                            stat <- perf[[b]]$perf.bs.oob[[meas]][["stat"]]
                            stat.var <- perf[[b]]$perf.bs.oob[[meas]][["stat.var"]]
                            stat.p <- ifelse(meas %in% c("calint","calslope"),2,1)*pnorm(-abs((stat-theta.noinf[[sub("laan","",meas)]])/sqrt(stat.var))) } else {stat.p <- NA }  }
                      ret <- c(p=stat.p,lcl=stat.lcl,ucl=stat.ucl)} else ret <- c(p=NA,lcl=NA,ucl=NA) 
                      ret})
                  sapl["p",][sapl["lcl",]==0 & sapl["ucl",]==0]  <- 1 
                  sapl <- apply(sapl,1,function(sa) if(length(which(is.na(sa)))/length(sa)<0.5) sa else rep(NA,length(sa)))
                  apply(sapl,2,median,na.rm=T)}) } } }
          
          file.names <- ls()[substr(ls(),1,6)=="theta." | substr(ls(),1,3)=="ci." ]
          
          if(length(file.names)>0) {
            ret <- lapply(file.names,function(f) get(f))
            names(ret) <- file.names   } else ret <- NULL
          
          ret  }


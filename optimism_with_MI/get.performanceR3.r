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

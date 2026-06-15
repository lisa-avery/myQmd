###########################################################
# R function do.cis.one to do obtain point and CI estimates
#
# Note that parts of this are experimental
###########################################################

require(parallel)
require(mice) 

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

  
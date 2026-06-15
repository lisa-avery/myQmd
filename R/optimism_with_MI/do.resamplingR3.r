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

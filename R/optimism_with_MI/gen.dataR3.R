###########################################################
# R functions gen.data to generate matrices of p and q variables with a certain 
# correlation among variables and impose missingness to a part of the samples; 
# also generate a response y that is affected by X_p and X_q
###########################################################

require(mvtnorm)
require(ROCR)
require(parallel)

##################### Auxiliary functions

### 1. logit function
logit <- function(x) log(x/(1-x))

### 2. logistic function
logistic <- function(x) exp(x)/(1+exp(x))

### 3. random up/down rounding
rounddownup <- function(x) {
  ganz <- floor(x)
  rest <- x-ganz
  if(rest > 0)  x = ganz + sample(c(0,1), 1, prob=c(1-rest,rest))
  return(x) }

### 4. Generate data
gen.data.one <- function(n, p, q, cor_p,cor_q,cor_pq,auc_p,auc_q,frac_cases) {      # cor_pq != 0 not investigated
  
  # plausibility
  if(is.na(auc_p)) p <- 0
  
  # generate y
  ncases <- rounddownup(frac_cases*n)
  y <- sample(c(rep(1,ncases),rep(0,n-ncases)))
    
  # generate X
  if(p==0) {
    X_q <- rmvnorm(n=n, mean=rep(0,q), sigma=matrix(cor_q, ncol=q, nrow=q)+diag(rep(1-cor_q),q))
    X_p <- NULL}
  if(p>0) {
    X <- rmvnorm(n=n, mean=rep(0,p+q), sigma=rbind(cbind(matrix(cor_p,ncol=p,nrow=p),matrix(cor_pq,ncol=q,nrow=p)),
                                                                  cbind(matrix(cor_pq,ncol=p,nrow=q),matrix(cor_q,ncol=q,nrow=q)))+
                                                            diag(1-c(rep(cor_p,p), rep(cor_q,q))))  
    X_p <- X[,1:p,drop=F]
    X_q <- X[,(p+1):ncol(X),drop=F] } 

  # generate y
  
  # translate AUCs to beta coefficients (beta's here are really differences between mean(x/y=1)-mean(x/y=0), see Su and Liu 1993, cited by Pencina 2012; to get betas: Sigma^{-1}*(mu1-mu0)
  if(p==0) {
    beta_p <- numeric(0) 
    if(cor_q==0) beta_q = qnorm(auc_q)*sqrt(2/q)
    if(cor_q!=0) {
      Sigma_1 <- solve(matrix(cor_q,nrow=q,ncol=q)+diag(1-cor_q,q))
      f <- function(beta_q) pnorm(sqrt(t(rep(beta_q,q)) %*% Sigma_1 %*% rep(beta_q,q))/sqrt(2)) - auc_q
      beta_q <- uniroot(f,c(0,10),tol=1E-7)$root } } 

  if(p>0) {
    if(cor_p ==0) beta_p = qnorm(auc_p)*sqrt(2/p) else {
      f <- function(beta_p) pnorm(sqrt(t(rep(beta_p,p)) %*% solve(matrix(cor_p,nrow=p,ncol=p)+diag(1-cor_p,p)) %*% rep(beta_p,p))/sqrt(2)) - auc_p
      beta_p <- uniroot(f,c(0,10),tol=1E-7)$root }
    f <- function(beta_q) pnorm(sqrt(t(c(rep(beta_p,p),rep(beta_q,q))) %*% 
                                solve(cbind(rbind(matrix(cor_p,nrow=p,ncol=p)+diag(1-cor_p,p),
                                                  matrix(cor_pq,nrow=q,ncol=p)),
                                            rbind(matrix(cor_pq,nrow=p,ncol=q),
                                                  matrix(cor_q,nrow=q,ncol=q)+diag(1-cor_q,q)))) %*% c(rep(beta_p,p),rep(beta_q,q)))/sqrt(2)) - auc_p - auc_q    
   beta_q <- uniroot(f,c(0,10),tol=1E-7)$root} 
    
   X_q[y==0,] <- X_q[y==0,] - beta_q/2
   X_q[y==1,] <- X_q[y==1,] + beta_q/2
   if(p>0) {
    X_p[y==0,] <- X_p[y==0,] - beta_p/2
    X_p[y==1,] <- X_p[y==1,] + beta_p/2 }
   
  return(list(X_p=X_p,X_q=X_q,y=y,type="com")) }

### 5. Introduce missingness into data
gen.miss.one <- function(data,n,pq,mech, incom,nblock,block.strength, strength.vary.miss=0) {   # data: list of X_p,X_q,y  # default: no variation of missingness, all vars same %miss 

  X <- Xmiss <- switch(pq, p=data$X_p,q=data$X_q)
  y <- data$y
  if(incom>0 & !is.null(X)) {
    if(is.vector(X)) X <- Xmiss <- data.frame(X)
    P <- ncol(X)
   
    if(mech=="MARblock" & P==1) mech <- "MAR"                                                    
    
    if(mech != "MARblock") {
      if(P==1) miss.vec <- incom 
      if(P>1) miss.vec <- sample(seq(incom-strength.vary.miss*min(1-incom,incom),incom+strength.vary.miss*min(1-incom,incom),length=P))  }

    # dependence of missingness on values of other variables     
    if(mech %in% c("MAR","MARnoy")) {
      logOR <- 1  # relation of variables in their missingness
      if(mech == "MARnoy") mary.beta <- 0 else mary.beta <- 2
      if(P==1) {
        mar.beta <- 0
        related.var <- 1} # just pro forma
      if(P>1) {
        mar.beta <- 2
        related.var <- sapply(1:P,function(j) sample(setdiff(1:P,j),1))}} 
           
    # blockwise dependence of missingness (via mar.beta and not logOR, since we want to get block dependent on covariates) 
    if(mech=="MARblock") {
      blocks <- sort(rep(1:nblock,ceiling(P/nblock))[1:P])   # does not really matter whether random   
      logOR <- 0  # since dependence in missingness comes already through blocks 
      mary.beta <- 2  
      mar.beta <- block.strength
      related.var <- sapply(1:nblock,function(j) sample(setdiff(1:P,which(blocks==j)),1))
      related.var <- related.var[blocks]
      miss.vec <- seq(incom-strength.vary.miss*min(1-incom,incom),incom+strength.vary.miss*min(1-incom,incom),length=nblock)
      miss.vec <- miss.vec[match(blocks,1:nblock)] 
      if(mean(miss.vec) != incom) miss.vec <- miss.vec*incom/mean(miss.vec)} 
    # --> usually blocks defined by some variables, here we only cover random blocks     
  
  
    j=1
    if(miss.vec[j]>0 & miss.vec[j]<1) {
      nmiss <- rounddownup(miss.vec[j]*n)
      if(mech=="MCAR" | (P==1 & mech=="MARnoy")) ids.to.miss <- sample(n,nmiss) 
      if(mech %in% c("MAR","MARblock") | (P>1 & mech=="MARnoy")) {
        f <- function(beta0) mean(logistic(beta0 + mar.beta*X[,related.var[j]] + mary.beta*y))-miss.vec[j]
        beta0 <- tryCatch(uniroot(f,c(-50,50),tol=1E-7)$root,error=function(e) NULL)
        if(is.null(beta0)) beta0 <- mean(logit(miss.vec[j]) - mar.beta*X[,related.var[j]] - mary.beta*y)
        Prob=logistic(beta0 + mar.beta*X[,related.var[j]] + mary.beta*y)
        ids.to.miss <- which(rmultinom(1,1,prob=Prob)==1)  
        if(nmiss>1) {
          for (i in 2:nmiss) {
            which.in <- setdiff(1:n,ids.to.miss)
            ids.to.miss <- c(ids.to.miss, which.in[which(rmultinom(1,1,prob=Prob[which.in])[,1]==1)])} }
        }
      Xmiss[ids.to.miss,j] <- NA } else if(miss.vec[j]==1) {Xmiss[,j] <- NA}
  
    if(P>1) {
      for (j in 2:P) {
        if(miss.vec[j]>0 & miss.vec[j]<1) {
          nmiss <- rounddownup(miss.vec[j]*n)
          if(mech=="MCAR") ids.to.miss <- sample(n, nmiss)              
          if(mech %in% c("MAR","MARblock","MARnoy")) {
            direction <- sample(c(-1,1),1)
            f <- function(beta0) mean(logistic(beta0 + direction*logOR*is.na(Xmiss[,j-1]) + mar.beta*X[,related.var[j]] + mary.beta*y))-miss.vec[j]
            beta0 <- tryCatch(uniroot(f,c(-50,50),tol=1E-7)$root,error=function(e) NULL)
            if(is.null(beta0)) beta0 <- mean(logit(miss.vec[j]) - direction*logOR*is.na(Xmiss[,j-1]) - mar.beta*X[,related.var[j]] - mary.beta*y)
            Prob= logistic(beta0 + direction*logOR*is.na(Xmiss[,j-1]) + mar.beta*X[,related.var[j]] + mary.beta*y)
            ids.to.miss <- which(rmultinom(1,1,prob=Prob)==1)  
            if(nmiss>1) {
              for (i in 2:nmiss) {
                which.in <- setdiff(1:n,ids.to.miss)
                ids.to.miss <- c(ids.to.miss, which.in[which(rmultinom(1,1,prob=Prob[which.in])[,1]==1)])} }
        }
      Xmiss[ids.to.miss,j] <- NA  } else if (miss.vec[j]==1) {Xmiss[,j] <- NA}
    } }
  }  else Xmiss <- X

   # Return data
   ret <- list(Xother=switch(pq,p=data$X_q,q=data$X_p), X = Xmiss, y=y,type="incom")
   names(ret) <- c(paste("X_",ifelse(pq=="p","q","p"),sep=""),paste("X_",pq,sep=""),"y","type")
   ret
  }
    
  
 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function gen.data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

gen.data <- function(D=500,                  # number of simulations
                     seeds=1:D,
                     n=200,                  # sample size
                     p = 1,                  # number of former covariates
                     q = 1,                  # number of new covariates
                     cor_p = 0,              # compound symmetry correlation for former covariates
                     cor_q = 0,              # compound symmetry correlation for new covariates
                     cor_pq = 0,             # correlation between former and new covariates
                     auc_p = NA,             # effect of former covariates on binary response
                     auc_q = 0.5,            # effect of new covariates on binary response
                     frac_cases = 0.5,       # fraction of cases (y=1) in outcome
                     incom_p = 0,            # % incomplete samples in former covariates (y at the moment never missing) 
                     incom_q = 0.25,         # % incomplete samples in new covariates (y at the moment never missing) 
                     mech = "MAR",           # missing data mechanism
                     nblock=3,               # number of blocks for mech="MARblock"
                     block.strength = 10,    # strength of relatedness of missingness between variables for mech="MARblock"
                     strength.vary.miss = 0, # strength of variation of missingess between variables, default: all same %missings
                     ncores = 1) {           # number of cores



### Simulation d=1...D
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
gen.data.miss.one <- function(d) {
  if (!is.null(seeds)) set.seed(seeds[d])
  
### a. Generate complete data (X, y) for each d
  dataXY <- gen.data.one(n=n,p=p,q=q,cor_p=cor_p,cor_q=cor_q,cor_pq=cor_pq,auc_p=auc_p,auc_q=auc_q,frac_cases=frac_cases)  # elements: X_p,X_q,y

### b. Generate missingness in X for each d
 if(incom_q>0) dataXYmiss <- gen.miss.one(data=dataXY, n=n, pq="q", mech=mech,incom=incom_q, nblock=nblock,block.strength=block.strength, strength.vary.miss = strength.vary.miss)
 if(incom_p>0) dataXYmiss <- gen.miss.one(data=dataXYmiss, n=n, pq="p", mech=mech,incom=incom_p,nblock=nblock,block.strength=block.strength, strength.vary.miss = strength.vary.miss)
 if(incom_q==0 & incom_p==0) dataXYmiss <- dataXY
  
  return(list(dataXY,dataXYmiss)) }
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if(ncores==1) results <- lapply(1:D, gen.data.miss.one)  else results <- mclapply(1:D, gen.data.miss.one, mc.cores=ncores)      
return(results) }

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
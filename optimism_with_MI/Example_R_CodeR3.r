###########################################################
###########################################################
# Assessment of predictive performance in incomplete data by combining internal validation and multiple imputation
# Simone Wahl et al.
#
# Example R code
###########################################################
###########################################################


### 1. Generation of incomplete data
source("gen.data.r")  
n=200; p=0; q=10
frac_cases=0.5
dataX <- gen.data(D=250, n=n, p=p, q=q, cor_p=0, cor_q=0, auc_p=NA, auc_q=0.66, 
                     mech="MAR", incom_p=0, incom_q=0.5, frac_cases=frac_cases) 
# is a list of 250 (number of simulations) lists of 2 lists each: 1. complete, 2. incomplete data set
# according to the provided data characteristics
# with objects y=response vector, X_p=baseline covariates (nxp0 format),X_q=additional covariates (nxp format)


### 2. Internal validation and imputation
source("do.resampling.r")
dat <- dataX[[1]][[2]]                              # incomplete data
datCom <- dataX[[1]][[1]]                           # complete data

# Example of data generated for simulation study on point estimation
datComBS <- do.bs(datCom,B=50,method="bs")          # BS
datComSS <- do.bs(datCom,B=50,method="ss")          # SS
datComCV <- do.bs(datCom,B=25,method="cv")          # CV10(rep)
datComCV3 <- do.bs(datCom,B=25,K=3,method="cv")     # CV3(rep)
datMI <- do.mice(dat,M=5,y.include=T)               # MI         
datMInoy <- do.mice(dat,M=5,y.include=F)            # MI(-y)                 
datBS <- do.bs(dat,B=10,method="bs")                
datSS <- do.bs(dat,B=10,method="ss")    
datCV <- do.bs(dat,B=5,method="cv")     
datCV3 <- do.bs(dat,B=5,K=3,method="cv")     
datMIBS <- do.bs(datMI,B=10,method="bs")            # MI-BS
datMISS <- do.bs(datMI,B=10,method="ss")            # MI-SS
datMICV <- do.bs(datMI,B=5,method="cv")             # MI-CV10(rep)
datMICV3 <- do.bs(datMI,B=5,K=3,method="cv")        # MI-CV3(rep)   
datMInoyBS <- do.bs(datMInoy,B=10,method="bs")      # MI(-y)-BS     
datMInoySS <- do.bs(datMInoy,B=10,method="ss")      # MI(-y)-SS  
datMInoyCV <- do.bs(datMInoy,B=5,method="cv")       # MI(-y)-CV10(rep)     
datMInoyCV3 <- do.bs(datMInoy,B=5,K=3,method="cv")  # MI(-y)-CV3(rep)          
datBSMI <- do.mice(datBS,M=5)                       # BS-MI
datSSMI <- do.mice(datSS,M=5)                       # SS-MI
datCVMI <- do.mice(datCV,M=5)                       # CV10(rep)-MI
datCV3MI <- do.mice(datCV3,M=5)                     # CV3(rep)-MI
# each a nested list of data sets, training and test parts separated


### 3. Modeling and determining performance estimates
# Example
source("get.performance.r")
cutoff = c(0,frac_cases/2,frac_cases*3/2,1)
perfMI <- get.performance(datMI,cutoff=cutoff, nondelta.too=T)
perfBSMI <- get.performance(datBSMI,datMI, cutoff=cutoff, nondelta.too=T)
perfCVMI <- get.performance(datCVMI,datMI, cutoff=cutoff)
# each a nested list of performance estimates; in the case where data come from resampling, 
# results from model fitted to training folds and evaluated to training fold, test fold and full data set (BS,SS) separated 

# for big external test data (to get an estimate of 'true' performance)
perfBig <- get.performance(dat=datCom,datBase=datBig, is.test=T, cutoff=cutoff)


### 4. Get estimates and compute confidence intervals
source("do.CIs.r")
df.model <- n-p-q-1
cisMI <- do.cis.one(perfBase=perfMI,df.model=df.model)
cisBSMI <- do.cis.one(perf=perfBSMI,perfBase=perfMI,df.model=df.model) 
cisCVrepMI <- do.cis.one(perf=perfCVMI,perfBase=perfMI,df.model=df.model) 
cisCVMI <- do.cis.one(perf=list(lapply(1:length(perfCVMI[[1]]),function(m) perfCVMI[[1]][[m]][1:1]),type=perfCVMI$type),perfBase=perfMI,df.model=df.model)    # not repeated
# perfMI provides apparent performance, as required e.g. for the formation of the 0.632+ estimate


### 5. Bias and MSE
# data are restructured and all results belonging to one simulation setting are 
# collected in one object "eva", accordingly the performances in the big external test data set in "evaBig"
# Stucture of eva: nested list, performance measure in the upper level, followed by validation/imputation strategies, 
# followed by point/CI estimates

# example for calculation of bias
meas <- "auc"
strat <- "cisBSMI"
est <- "theta.0.632p"
true <- evaBig[meas]
bias <- mean(eva[[meas]][[strat]][[est]]-true,na.rm=T)

# mse
mse <- mean((eva[[meas]][[strat]][[est]]-true)^2,na.rm=T)
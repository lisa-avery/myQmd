



# Helper Functions ------------

# tidy rounding keeping trailing zeros for output tables
rnd <- function(x,digits=2){
  out <- sapply(x, function(x){
    format(round(x,digits),nsmall=digits)
  })
  out <- unlist(out,use.names = FALSE)
  return(out)
}

#' @param probs a vector of probabilities or scores, usually from predict (or a
#'   glm model)
#' @param class a vector denoting class membership, must be binary
fastAUC <- function(probs, class) {
  if (inherits(probs,"glm")){
    class <- probs$y
    probs <- predict(probs,type='response')
  }
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1);
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}

get_roc_data <- function(fitted_obj,digits=2){
  if (inherits(fitted_obj,'glm')){
    class_p <- predict(fitted_obj,type='response')
    obs = fitted_obj$y
    roc_obj <- fastROC(class_p,obs)
    auc <- fastAUC(class_p,obs)
    #  if (missing(title)) title = as.character(fitted_obj$call)[2]
  } else {
    roc_obj <- fastROC(fitted_obj[,1],fitted_obj[,2])
    auc <- fastAUC(fitted_obj[,1],fitted_obj[,2])
    # if (missing(title)) title = NULL
  }
  auc <- niceNum(auc,digits)
  # TODO: Add different techniques for calculating cut points - see paper
  df =  data.frame(x=roc_obj$fpr,
                   y=roc_obj$tpr,
                   Youden = roc_obj$tpr-roc_obj$fpr,
                   cutoff = roc_obj$threshold)
  return(list(data=df,auc=auc))
}

#' Numbers nicely formatted to string
#'
#' @param x numbers to format
#' @param digits number of digits to retain after decimal place
#'   checked for and retained.
#' @keywords helper
niceNum <- function(x,digits=2){
  rndx = sapply(x, function(x) {
    if(is.na(x)) return(x)
    if(is.null(x)) return(x)
    format(round(as.numeric(x),digits),nsmall=digits)})
  return(gsub(" ","",rndx))
}


#' Convert Hypothesis Test Objects to Data Frames
#'
#' @description
#' Converts an object of class "htest" (returned by R's statistical test functions)
#' into a data frame for easier manipulation and integration with other analyses.
#'
#' @param htest_object An object of class "htest", typically returned by statistical test
#' functions like t.test(), wilcox.test(), cor.test(), etc.
#'
#' @return A data frame with one row containing all the components of the hypothesis test.
#' The columns include:
#' \itemize{
#'   \item method: The name of the statistical test
#'   \item statistic: The test statistic
#'   \item p.value: The p-value of the test
#'   \item parameter: Degrees of freedom or other parameters (if present)
#'   \item conf.int: Confidence intervals (if present, may be split into conf.int1, conf.int2)
#'   \item estimate: Point estimates (if present, may be split into multiple columns)
#'   \item alternative: The alternative hypothesis specification (if present)
#'   \item data.name: The name of the data used in the test (if present)
#'   \item null.value: The null hypothesis value (if present)
#' }
#'
#' @details
#' This function extracts all components from an "htest" object and arranges them into
#' a single-row data frame. For components that contain multiple values (like confidence
#' intervals or multiple estimates), the values are separated into different columns with
#' sequential numbering.
#'
#' The function is particularly useful when collecting results from multiple tests,
#' as the resulting data frames can be easily combined using dplyr::bind_rows() (but not rbind because the number of columns may differ)
#'
#' @examples
#' # Convert a t-test result to a data frame
#' t_result <- t.test(1:10, y = c(7:20))
#' t_df <- htest_dataframe(t_result)
#' print(t_df)
#'
#' # Convert a correlation test result
#' cor_result <- cor.test(1:10, 2:11, method = "spearman")
#' cor_df <- htest_dataframe(cor_result)
#' print(cor_df)
#'
#' # Combine multiple test results
#' tests_list <- list(
#'   t_test = t.test(1:10, y = c(7:20)),
#'   wilcox = wilcox.test(1:10, c(7:20))
#' )
#' results_df <- do.call(dplyr::bind_rows, lapply(tests_list, htest_dataframe))'
#' @export
htest_dataframe <- function(htest_object){
  if (!inherits(htest_object,"htest")) stop("htest_object must be an object returned from a statistical test of class htest.")
  if (is.null(htest_object$method)) stop("htest_object must be an object returned from a statistical test of class htest with a methods component.")
  htest <- data.frame(method = htest_object$method)
  for ( j in setdiff(names(htest_object),"method")) {
    if (!is.null(htest_object[[j]])){
      rtn <- data.frame(matrix(unlist(htest_object[[j]]),nrow=1))
      if (ncol(rtn)>1){
        names(rtn) <- paste0(j,1:(ncol(rtn)))
        htest <- merge(htest,rtn,all=T)
      } else if (ncol(rtn) == 1){
        names(rtn) <- j
        htest <- merge(htest,rtn,all=T)
      }
    }}
  return(htest)
}


# Model Evaluation Functions ------------
#' Compute the optimism of the observed AUC through bootstrapping
#'
#' Examine the model AUC and use Efron's method to calculate an optimism
#' estimate by which to correct AUC when internal validation is necessary
#'
#' @param model output from a call to glm with family=binomial
#' @param bs number of bootstrap samples to use
#' @export
efron_auc <- function(model,bs=1000){
  #Using Enfron's enhanced bootstrap (See RMS pg94 Harrell)
  if (!inherits(model,"glm")) stop("This function has been written for glm models only")
  N <- bs
  auc_dev = fastAUC(predict(model),model$y)
  mcall <- model$call
  model_data <- model$model
  bs_auc <- optimism <- bs_on_orig <- numeric(N)
  for (i in 1:N){
    
    # Accuracy Index -auc across bootstraps
    bs_data <- model_data[sample(1:nrow(model_data),replace = T),]
    bs_fit <- update(model,.~.,data=bs_data)
    bs_on_orig[i] <- fastAUC(predict(bs_fit,newdata = model_data),model$y)
    bs_auc[i] <- fastAUC(predict(bs_fit),bs_fit$y)
    # auc of predicting observed from bootstrap models
    #Optimism
    optimism[i] <- bs_auc[i] - bs_on_orig[i]
  }
  
  corrected_auc <- tribble(~Description,~Value,
                           "Unadjusted AUC on sample", rnd(auc_dev),
                           "Bootstrapped Optimism Estimate",rnd(max(0,mean(optimism)),2),
                           "Bootstrapped Optimism-Adjusted AUC",rnd(auc_dev-mean(optimism),2))
  ref <- "@Book{Harrell2001,
  Author = {Harrell, Frank and Slaughter, James},
  Pages = {571},
  Title = {Regression Modeling Strategies : With Applications to Linear Models, Logistic Regression, and Survival Analysis},
  Year = {2001},
  Publisher = {Springer}
}"
  if (interactive()) cat(ref)
  return(corrected_auc)
  
}
#' Check Model Assumptions using DHARMa
#' 
check_model_assumptions <- function(model, nsim = 1000) {
  # Check if DHARMa is installed and load it
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop("The DHARMa package is required. Please install it with install.packages('DHARMa')")
  }
  # Create DHARMa residuals
  sim_residuals <- DHARMa::simulateResiduals(model, n = nsim)
  tests_output <- list()
  tests_output$uniformity <- DHARMa::testUniformity(sim_residuals, plot = F)
  tests_output$dispersion <- DHARMa::testDispersion(sim_residuals, plot = F)
  tests_output$outliers <- DHARMa::testOutliers(sim_residuals, plot = F)
  out <- bind_rows(lapply(tests_output, function(ht) htest_dataframe(ht)),.id="Test")
  return(out)
}


#'Receiving Operating Curve Plot
#'
#'This function will plot the roc curve from a regression model or a data frame
#'containing score and class data
#'
#'TODO: Not robust - this needs to be updated to allow for missing data.
#'
#' @param fitted_obj either an object output from the glm function, must be from
#'   a logistic regression, or a two-column dataframe in the format
#'   (RiskScore,Label), can also be a list of glm objects for multiple curves
#' @param showAUC logical, should the AUC be plotted on the graph
#' @param showCut logical, should the cut point be shown on the graph
#' @param plotOnly logical, should both the plot and the AUC be returned
#' @param title character string with graph title, defaults to the calling
#'   function, use title=NULL to leave empty
#'@param digits the number of digits to use for the AUC, default is 2
#'@param model_names character vector of names to label ROC curves with
#'@param fsize default font size for all text elements
#'@param lsize default line size for roc curves
#' @import ggplot2
#' @keywords plot
#' @export
#'
gg_roc <- function(fitted_obj,showAUC=T,showCut=F,plotOnly=T,title=NULL,digits=2,model_names=NULL,fsize=10,lsize=.8){
  if (inherits(fitted_obj,'list')){
    if (!is.null(model_names)) {
      if (length(model_names)!=length(fitted_obj)) stop('model_names and fitted_objects must have the same length')
      names(fitted_obj) <- model_names
    }
    if (is.null(names(fitted_obj))) names(fitted_obj) <- paste("Model",1:length(fitted_obj))
    obj_auc <- unlist(lapply(fitted_obj,function(x) fastAUC(x)))
    obj_ord <- order(obj_auc,decreasing = T)
    fitted_obj <- fitted_obj[obj_ord]
    roc_objects <- lapply(fitted_obj,function(x) get_roc_data(x,digits))
    roc_data <- lapply(seq_along(roc_objects), function(x) {
      z=roc_objects[[x]]$data
      z$model = names(roc_objects)[x]
      return(z)
    })
    df <- bind_rows(roc_data) |>
      arrange(x,y)
    model_lbl <- data.frame(model_nm= nicename(names(obj_auc)),
                            model=names(obj_auc),
                            auc = obj_auc) |>
      mutate(newName = paste0(nicename(model)," (AUC=",rnd(auc,digits),")"))
    df <- full_join(df,model_lbl)
    if (showAUC){
      df$model <- df$newName
    } else df$model <- df$model_nm
    df <- df |>
      mutate(model=forcats::fct_reorder(model,auc,.desc=T))
    p <- ggplot(data =df,
                aes(x=x,y=y,colour=model,linetype=model))
    auc_txt <- NULL
    
  } else {
    roc_objects <- get_roc_data(fitted_obj,digits)
    cutpoint = roc_objects$data$cutoff[which.max(roc_objects$data$Youden)]
    df <- roc_objects$data |>
      arrange(x,y)
    p = ggplot(data =df,
               aes(x=x,y=y))
    auc_txt <- paste("AUC =",roc_objects$auc)
  }
  
  p <- p +
    geom_line(size=lsize) +
    theme_bw()+
    theme(legend.title = element_blank(),
          text = element_text(size=fsize))+
    xlab("1-Specificity (FPR)") +
    ylab("Sensitivity (TPR)")
  if (showAUC){
    if(!is.null(auc_txt)){
      p <-  p + annotate(geom='text',x=1,y=.1,label=auc_txt,hjust='right')
    } else {
      p <- p+theme(
        legend.position = c(.95, 0),
        legend.text = element_text(size=fsize),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(2,2,2,2),
        legend.background = element_rect(fill = "transparent", color = NA))
    }
  }
  if (showCut & !inherits(fitted_obj,'list')){
    p = p+  geom_segment(
      aes(x = .5, y = .5, xend = x[which.max(Youden)], yend = y[which.max(Youden)]),
      arrow = arrow(length = unit(0.03, "npc")))
    p = p + annotate(geom='text',x=.5,y=.5,label=paste('cutpoint =',round(cutpoint,2)),hjust=0,vjust=1)
  }
  if (!is.null(title)){
    p = p + ggtitle(title) + theme(plot.title = element_text(size=11))
  }
  if (plotOnly){
    return(p)
  } else {
    return(list(plot=p,auc=auc))
  }
}

#' Compute ROC statistics
#'
#' Compute the true positive rate and false positive rate for ROC curves
#'
#' The function accepts a vector of probabilities or scores (probs) and a vector
#' of classes, of which there must be two. For each unique score/probability
#' threshold the sensitivity and 1-specificity are calculated. The returned
#' cutpoints are mid-way between the scores to ensure no ambiguity as to whether
#' the true cut-point is < vs <= the value. The output agrees with that provided
#' by the pROC package.
#'
#' Not designed to be called on its own, does not contain error checking or
#' remove missing data
#' @param probs a vector of probabilities or scores, usually from predict
#' @param class a vector denoting class membership, must be binary
fastROC <- function(probs, class) {
  
  p_ord <- order(probs, decreasing=F)
  probs_sorted <- probs[p_ord]
  cp <- c(-Inf,rowMeans(cbind(unique(probs_sorted)[-1],head(unique(probs_sorted),-1))),Inf) # gives same thresholds as pROC
  class_sorted <- class[p_ord]
  TPR=sapply(cp,function(x) sum(class_sorted[probs_sorted>x])/sum(class_sorted))
  FPR=sapply(cp,function(x) sum(1-class_sorted[probs_sorted>x])/sum(class_sorted==0))
  return(data.frame(threshold=cp, tpr=TPR, fpr=FPR))
  
}


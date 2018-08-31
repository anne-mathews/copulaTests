#' Simulate out of sample forecast
#' 
#' @import copula
#' @param GARCH.model List of individually fitted GARCH model of the returns that are being simulated in the function 
#' @param copulaClass The fitted copula class 
#' @param alpha The confidence level
#' @param sims The number of simulations i.e. paths 
#' @param days The number of days that need to be simulated
#' @param u observation vector
#' 
#' @details Part of this code was obtained from Marius Hofert's repo (https://github.com/qrmtutorial/qrm/blob/master/R/07_Copulas_and_Dependence/07_Copula_estimation_and_goodness-of-fit.R)
#'  
#' @export
#' 
simulateVaR <- function(GARCH.model,copulaClass,alpha=0.05,sims=100,days){
  # get df of each GARCH fit
  df <- sapply(GARCH.model,coef)["shape",]
  dim <- length(GARCH.model) # number of variables
  # get simulations for specified number of days
  paths <- lapply(1:sims, function(b) {
    ## 1) Simulate from the fitted copula
    copulaSim <- rCopula(days, copula = copulaClass)
    ## 2) Quantile-transform to standardized t distributions (for ugarchsim())
    Z <- sapply(1:dim, function(j) sqrt((df[j]-2)/df[j]) * qt(copulaSim[,j], df = df[j]))
    ## 3) Use these multivariate dependent t innovations to sample from the
    ##    time series
    sim_garch <- lapply(1:dim, function(j)
      ugarchsim(GARCH.model[[j]], n.sim = days, m.sim = 1,
                custom.dist = list(name = "sample",
                                   distfit = Z[,j, drop = FALSE])))
    ## 4) Extract simulated series
    sapply(sim_garch, function(x) fitted(x)) # simulated multivariate series X_t
  })
  # calculate returns for each day for each path assuming portfolio of equal weights
  ret <- sapply(paths,rowMeans)
  # take quantile of returns to get VaR
  VaR <- apply(ret,1,function(x) quantile(x,probs=alpha))
}

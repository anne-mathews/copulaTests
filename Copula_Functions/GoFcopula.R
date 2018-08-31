#' Goodness of Fit using Cramer-von Misis criterion
#' 
#' @import doParallel
#' @import copula
#' 
#' @param dim Total number of variables
#' @param n Number of observations in data
#' @param U Uniform pseudo-observations
#' @param copula The type of copula used. Accepted values are "normal", "t", "gumbel", "clayton"
#' @param copulaClass The fitted copula class obtained from fitCopula
#' @param ntest The number of tests to be ran (e.g. n = 100) 
#' @return p-value; where \code{H_0}: Estimated copula model is the true model. \code{C_\theta} = \code{\hat{C}}
#' 
#' 
#' 
#' @details This is a goodness of fit test based on the Cramer-von Misis criterion.
#'
#' @export
GoFcopula <- function(dim,n,U,copula="normal",copulaClass,ntest) {
  # dim - number of variables
  # n - number of observation in data
  # Uniform pseudo-observations
  # copula - type of copula used
  # copulaClass - fitted copula class
  # null hypothesis: estimated copula is correct
  # Simulate data from the estimated model
  m = n+1000 # want m > # of observations
  sample <- rCopula(m,copulaClass)
  # Approximate S_n(distance^2) for our model, S_n compared difference between empirical model and estimated model
  S_n <- sum(apply(U,1,function(i) emp_mod(U,i)-emp_mod(sample,i)))^2
  i <- 0 #keep count of progress 
  # now compare error when we know the true copula and fit an estimated copula
  ## we will use parallel computing to reduce computing time ##
  registerDoParallel(cores = detectCores() - 1)
  cl <- makeCluster(spec = 4) # 4 cores
  registerDoParallel(cl) # synch with doParallel package
  clusterSetRNGStream(cl = cl) # RNG for multiple cores
  system.time({
    reps <- foreach(i=1:ntest,.combine=c,.packages="copula",.export="emp_mod") %dopar% {
      # simulate some data
      sim <- rCopula(copula=copulaClass,n=n)
      ranked <- n*sim  #associated rank vector
      U_rep <- ranked/(n+1)
      # fit data to a copula when we know the true copula of the data
      if (copula == "normal") {
        normFit_rep <- fitCopula(normalCopula(dim = dim), data = U_rep, method = "irho")
        repCopula <- normFit_rep@copula # fitted normal copula
      } else if (copula == 'std') {
        tFit_rep <- fitCopula(tCopula(dim = dim, dispstr = "un"), data = U_rep, method = "itau.mpl")
        repCopula <- tFit_rep@copula # The fitted t-copula
      } else if (copula == 'gumbel') {
        gumFit_rep <- fitCopula(gumbelCopula(dim = dim),data = U_rep, method = "irho")
        repCopula <- gumFit_rep@copula # The fitted gumbel copula
      } else if (copula == 'clayton') {
        clayFit_rep <- fitCopula(claytonCopula(dim = dim),data = U_rep, method = "itau")
        repCopula <- clayFit_rep@copula # The fitted clayton copula
      } else {
        stop("Copula type is not supported")
      }
      # simulate data from estimated copula to estimate CDF
      sample2 <- rCopula(m,repCopula)
      # find distance between true model and fitted model
      S_rep <- sum(apply(U_rep,1,function(i) emp_mod(U_rep,i)-emp_mod(sample2,i)))^2
      #check if true distance when data where true distn is known is greater than distance of our copula model
      S_rep > S_n  
    }
  })
  stopCluster(cl)
  pvalue = sum(reps)/ntest
}

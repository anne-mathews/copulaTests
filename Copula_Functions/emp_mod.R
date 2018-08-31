#' Finds the empirical Copula model
#' 
#' @import copula
#' @param U matrix with sample distribution, uniform
#' @param u observation vector 
#' @export
#'
emp_mod <- function(U,u) 
  # U <- a matrix with sample distribution, uniform
  # u <- observation vector
  # emp_mod finds the empirical CDF
{  # empirical copula ("true" copula), we will compare this to the analytical copula (which we estimate by simulation)
  rows = nrow(U)
  u_mat <- matrix(u,ncol=dim,nrow=rows,byrow=TRUE)
  bool <- U <= u  #get bool matrix
  cdf <- apply(bool,1,all)
  sum(cdf)/rows
}

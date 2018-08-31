#' Percentage of times the actual returns are below the simulated VaRs
#' 
#' @import copula
#' @param simVaR Vector of simulated VaRs
#' @param actual_rets Vector of actual returns
#' 
#' @export
#'
violatedVaRs <- function(simVaR,actual_rets){
  days <- length(actual_rets)
  violations <- sapply(1:days,function(i) {actual_rets[i] < simVaR[i]})
  sum(violations)/days
}

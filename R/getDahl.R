#' Dahl's method to summarize the samples from the MCMC
#'
#' @param MFMfit the result from CDMFM_new function
#' @param burn the number of burn-in interations
#'
#' @return zout = estimated clustering configuration, a n by 1 vector
#' @return Qout = estimated probability matrix, a k by k matrix
#' @export
#'
#' @examples
getDahl <- function(MFMfit, burn)
{
  iters <- MFMfit$Iterates[-(1:burn)] #defining the iters value
  n <- length(iters[[1]][[1]]) #defining the number of iterations
  niters <- length(iters)
  membershipMatrices <- lapply(iters, function(x){
    clusterAssign <- x[[1]]
    outer(clusterAssign, clusterAssign, FUN = "==")
  })
  membershipAverage <- Reduce("+", membershipMatrices)/niters #calculating the average here
  SqError <- sapply(membershipMatrices, function(x, av) sum((x - av)^2),
                    av = membershipAverage)
  DahlIndex <- which.min(SqError) #getting the DahlIndex
  DahlAns <- iters[[DahlIndex]]
  attr(DahlAns, "iterIndex") <- burn + DahlIndex
  attr(DahlAns, "burnin") <- burn
  DahlAns #the result(i.e the zout and qout)
}

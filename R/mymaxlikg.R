#' Max likelikhood Function
#'
#' Computes and plots a one-dimensional likelihood function over a grid of parameter values
#' and returns the parameter value that maxes the likelihood.
#'
#' @param lfun A function that computes the log-likelihood for a given parameter value.
#' @param theta A numeric vector of candidate parameter values over which to evaluate the likelihood.
#'
#' @importFrom graphics axis abline plot
#'
#' @return The value of `theta` that maximizes the likelihood function
#' @export
#'
#' @examples
#' # Example using a custom log-likelihood function
#' loglik_fun <- function(theta) sum(dpois(c(2,4), lambda = theta, log = TRUE))
#' theta_grid <- seq(0.01, 10, length = 100)
#' mymaxlikg(lfun = loglik_fun, theta = theta_grid)

mymaxlikg=function(lfun,theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}

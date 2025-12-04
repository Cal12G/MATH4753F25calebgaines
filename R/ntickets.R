#' Function for optimal ticket sales (both continuous and discrete cases)
#'
#' @param N = number of seats available
#' @param gamma = acceptable prob of overbooking
#' @param p = prob a ticket holder shows up
#'
#' @importFrom stats pbinom uniroot
#' @importFrom graphics abline
#'
#' @returns a list containing
#' \itemize{
#'    \item \code{nd} - optimal tickets, discrete)
#'    \item \code{nc} - optimal tickets, continuous)
#'    \item \code{N} - number of seats
#'    \item \code{p} - probability of showing up
#'    \item \code{gamma} - probability of overbooking
#'  }
#' @export
#'
#' @examples
#' result <- ntickets(N = 400, gamma = 0.02, p = 0.95)
#' result
ntickets <- function(N,gamma,p){
  nmax <- N*1.1 # just to give the function somewhere to stop
  nvals <- N:nmax # ticket numbers

  # discrete
  over_prob_disc <- 1 - pbinom(N,nvals,p) # P(Y>N)
  # Find smallest value where prob of overbooking <= gamma
  idx <- which(over_prob_disc <= gamma)
  if (length(idx) == 0) {
    nd <- NA
    warning("Discrete solution not found in range, increase nmax")
  } else {
    nd <- nvals[max(idx)]
  }

  # continuous

  obj_cont <- function(n) {
    mu <- n*p
    sigma <- sqrt(n*p*(1-p))
    if (sigma == 0) return (1)
    (1 - pnorm((N + .5 - mu)/sigma)) - gamma
  }

  lower <- N
  upper <- nmax
  f_low <- obj_cont(lower)
  f_high <- obj_cont(upper)
  if (f_low * f_high >0) {
    nc <- NA
    warning("Continuous root not within range, increase nmax")
  } else {
    nc <- uniroot(obj_cont, lower=lower, upper=upper)$root
  }

  # plot functions
  obj_disc <- over_prob_disc - gamma
  cont_vals <- sapply(nvals, obj_cont)

  par(mfrow = c(2,1))

  # discrete plot
  plot(nvals, obj_disc, type = "b", pch=21, bg="blue", col="black", lty=1, cex=.6,
       xlab = "n (tickets sold)", ylab = "Objective",
       main = paste("Objective vs n (Discrete) -> gamma = ", gamma, "N = ", N))
  abline(h=0, col="red", lwd=2)
  if (!is.na(nd)) abline(v=nd, col="red", lwd=2)

  # continuous plot
  plot(nvals, cont_vals, type = "l", col = "gray20",
       xlab = "n (tickets sold)", ylab = "Objective",
       main = paste("Objective vs n (Continuous) -> gamma = ", gamma, "N = ", N))
  abline(h=0, col="blue", lwd=2)
  if (!is.na(nc)) abline(v=nc, col="blue", lwd=2)

  par(mfrow = c(1,1))

  # return list

  return(list(nd=nd,nc=nc,N=N,p=p,gamma=gamma))
}

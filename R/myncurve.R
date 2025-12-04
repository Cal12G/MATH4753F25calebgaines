utils::globalVariables("x")

#' myncurve function
#'
#' @param mu mean value
#' @param sigma standard deviation
#' @param a upper limit for shaded area
#'
#' @return A list with mu, sigma, a, and the probability P(Y <= a)
#' @export
#'
#' @importFrom grDevices rgb
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm runif
#'
#' @examples
#' myncurve(mu=1, sigma=2, a=1)
myncurve <- function(mu, sigma, a){
  curve(dnorm(x, mean=mu, sd=sigma),
        xlim=c(mu-3*sigma, mu+3*sigma),
        xlab="Y", ylab="Density",
        main=paste("Normal Distribution N(", mu, ",", sigma^2, ")", sep=""))

  # Shade area
  xcurve <- seq(mu-3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(xcurve[1], xcurve, a), c(0, ycurve, 0),
          col=rgb(runif(1), runif(1), runif(1)), border=NA)

  # Calculate probability
  prob <- pnorm(a, mean=mu, sd=sigma)

  list(mu=mu, sigma=sigma, a=a, P_Y_le_a=prob)
}

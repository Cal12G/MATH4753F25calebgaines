#' Central Limit Theorem Uniform Solution
#'
#' @param n the number of values per iteration
#' @param iter the number of iterations
#' @param a the lower limit
#' @param b the upper limit
#'
#'
#' @returns a histogram of mean values
#' @examples
#' # mycltu(10,10000,0,10)
#'
#'
#' @importFrom stats runif density dnorm dunif
#' @importFrom graphics hist lines curve
#' @export
mycltu <- function(n,iter,a=0,b=10){
  y <- runif(n*iter,a,b)
  data <- matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w <- apply(data,2,mean)
  param <- hist(w,plot=FALSE)
  ymax <- max(param$density)
  ymax <- 1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean",
                  "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}

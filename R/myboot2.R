#' Bootstrap Confidence Interval Function
#'
#' @param iter Number of bootstrap iterations
#' @param x Original sample for resamples
#' @param fun Statistic to compute with each bootstrap sample
#' @param alpha Significance level to compute percentile bootstrap confidence interval
#' @param cx Character expansion factor for size of confidence interval
#' @param ... Additional graphical params given to `hist()`
#'
#' @importFrom stats quantile
#' @importFrom graphics segments text
#'
#' @returns ci, fun, x, xstat
#' @export
#'
#' @examples
#' \dontrun{
#'   # Bootstrap mean CI for a normal sample
#'   set.seed(1)
#'   sam <- rnorm(30, mean = 5, sd = 2)
#'   myboot2(x = sam, fun = "mean", iter = 5000, alpha = 0.05)
#'
#'   # Bootstrap median with a custom statistic
#'   myboot2(x = sam, fun = median, iter = 5000)
#'
#'   # Bootstrap IQR
#'   myboot2(x = sam, fun = IQR, iter = 5000)
#' }
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)
  y=sample(x,n*iter,replace=TRUE) #A
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #B
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha,"iter=",iter,sep=""),
            xlab=paste("Bootstrap",deparse(substitute(fun)),"values"),...)

  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  return(list(ci=ci, fun=fun, x=x, xstat=xstat))
}

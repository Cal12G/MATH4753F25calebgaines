#' mynbin function
#'
#' @param y total trials until rth success
#' @param r number of successes to be achieved
#' @param p probability of success per trial
#'
#' @returns the probability P(Y=y)
#' @export
#'
#' @examples
#' # single value:
#' mynbin(y=10,r=3,p=.4)
#' # or vector:
#' mynbin(y=3:12,r=3,p=.4)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

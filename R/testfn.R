#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
#' testfn(1:10)
testfn <- function(x) {
  y <- 2*x^4
  list(x=x,y=y)
}

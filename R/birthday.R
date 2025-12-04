#' The Birthday Function
#'
#' @param x a vector of values
#'
#' @returns probability for students to share the same birthday
#' @export
#'
#' @examples birthday(20:24)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}

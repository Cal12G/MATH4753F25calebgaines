#' Calculate Mean and SD
#'
#' This function returns the mean and standard deviation of a numeric vector.
#'
#' @param x A numeric vector
#' @importFrom stats sd
#' @return A list with mean and sd
#' @export
mean_sd <- function(x) {
  list(mean = mean(x), sd = sd(x))
}

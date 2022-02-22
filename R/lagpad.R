#' Function to get a vector lagged.
#'
#' @param x numeric vector to be lagged.
#' @param k number of lags to be applied.
#' @return A lagged vector.
#' @export
lagpad <- function(x, k) {
  res <- c(rep(NA, k), x)[1:length(x)]
  return(res)
}

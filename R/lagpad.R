#' Function to get a vector lagged.
#'
#' @param x numeric vector to be lagged.
#' @param k number of lags to be applied.
#' @return A lagged vector.
#' @export
#' @examples
#'  s <- 1:5
#'  s1 <- lagpad(s, k = 1)
#'  s2 <- lagpad(s, k = 2)
#'  data.frame(s, s1, s2)
lagpad <- function(x, k) {
  res <- c(rep(NA, k), x)[1:length(x)]
  return(res)
}

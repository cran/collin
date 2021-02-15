#' Internal function to generate random numbers following over-dispersed
#'   Poisson distribution based on simple cheat of using a standard negative
#'   binomial, but choosing the scale parameter to give the desired mean vs
#'   variance ratio at the given value of the mean.
#'   Taken from: https://stat.ethz.ch/pipermail/r-help/2002-June/022425.html
#'
#' @param n integer. Number of observations.
#' @param lambda numeric. Mean.
#' @param d numeric. Dispersion parameter.
#' @return a vector with the simulated sample.
#' @keywords internal
#' @importFrom stats rpois rnbinom
rpoisod <- function(n, lambda, d = 1) {
  if (d == 1) {
    res <- rpois(n = n, lambda = lambda)
  } else {
    res <- suppressWarnings(rnbinom(n = n,
                                    size = (lambda / (d - 1)),
                                    mu = lambda))
  }
  return(res)
}


#' Internal function to generate random numbers following over-dispersed
#'   Bernoulli distribution, based in the BetaBinomial distribution. It
#'   currently does not work and standard Bernoulli distribution is actually
#'   used instead.
#'
#' @param n integer. Number of observations.
#' @param prob numeric. Probability of success on each trial.
#' @param d numeric. Dispersion parameter.
#' @return a vector with the simulated sample.
#' @keywords internal
#' @importFrom VGAM rbetabinom
#' @importFrom stats rbinom
rbinomod <- function(n, prob, d = 1) {
  if (d == 1) {
    res <- rbinom(n = n, size = 1, prob = prob)
  } else {
    # following way of simulate doesn't work. Meanwhile, I force "binomial" in
    # the main function
    res <- VGAM::rbetabinom(n = n, size = 1, prob = prob, rho = d - 1)
  }
  return(res)
}


#' Internal function to generate random numbers following over-dispersed
#'   Bernoulli or Poisson distribution. It wraps \code{rpoisod} and
#'   \code{rbinomod} functions.
#'
#' @param n integer. Number of observations.
#' @param mu numeric. Probability of success on each trial or mean, for
#'   (quasi)binomial and (quasi)Poisson, respectively.
#' @param d numeric. Dispersion parameter.
#' @param type character indicating the distribution.
#' @return a vector with the simulated sample.
#' @keywords internal
#' @importFrom stats rpois
rod <- function(n, mu, d,
                type = c("binomial", "quasibinomial", "poisson", "quasipoisson")) {
  type <- match.arg(type)

  if (type %in% c("binomial", "quasibinomial")) {
    res <- rbinomod(n = n, prob = mu, d = d)
    return(res)
  }

  if (type %in% c("poisson", "quasipoisson")) {
    res <- rpoisod(n = n, lambda = mu, d = d)
    return(res)
  }
}

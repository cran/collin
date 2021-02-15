#' Simulates effects from a distributed lag model
#' pattern.
#'
#' Simulates results from a distributed lag model under an hypothetical effect
#' pattern provided by the user, which can be linear or non-linear. The output
#' is the passed to the \code{plot} method to visualize consequences of
#' collinearity.
#'
#' @param model a model that includes a crossbasis. Currently, models allowed are those
#'   of class \code{"glm"} or \code{"lme"}, if \code{shape = "linear"}; or
#'   \code{"glm"}, if \code{shape = "nonlinear"}.
#' @param x if \code{shape = "linear"}, a matrix that includes the values of the predictor
#'   under study, in the first column, and the lagged values, up to the maximum lag
#'   considered, as the subsequent columns. If \code{shape = "nonlinear"}, a numeric
#'   vector or 1-column matrix including the original (i.e. lag 0) values of the
#'   predictor.
#' @param cb an object of class \code{"crossbasis"}. The crossbasis included in \code{model}.
#' @param at the increase(s) in the predictor under study to be considered to report the
#'   effects of the variable. If \code{shape = "linear"}, \code{at} must be a single
#'   number. If \code{shape = "nonlinear"}, \code{at} must be a numeric vector with
#'   at least two different values.
#' @param cen a number. Reference value of the predictor under study, used to calculate
#'   effects. If \code{shape = "linear"}, the value of \code{cen} is irrelevant (and
#'   it is internally set to 0).
#' @param effect a vector or a matrix, depending on \code{shape}, including the
#'   hypothetical effect of the predictor under analysis. If \code{shape = "linear"},
#'   a vector including the linear effect at each lag (including lag 0). If
#'   \code{shape = "nonlinear"}, a matrix including the effect at each lag (including
#'   lag 0) (columns) and for each value in \code{at} (rows).
#' @param type a character. If \code{type = "coef"} (default), \code{effect} is supposed
#'   to be in the linear predictor scale (i.e. it is considered as regression
#'   coefficients in \code{model}). If \code{type = "risk"}, \code{effect} is supposed
#'   to be in terms of relative risks (i.e. exp(\code{coef}), as ORs or RRs in logistic
#'   or Poisson families, respectively). If \code{model} is of class \code{"lme"},
#'   then it must be \code{type = "coef"} (default).
#' @param shape the shape of the relationship between the linear predictor of the model
#'   and the outcome. Default is \code{"linear"}.
#' @param nsim number of simulations. Default is 100.
#' @param verbose a logical value indicating output status messages. Default is \code{TRUE}.
#' @param seed a number. Seed for reproducibility of results. Default is \code{NULL} (no
#'   seed).
#' @return A list including the results of the simulations to be passed to the \code{plot}
#'   method.
#' @seealso \code{\link[dlnm]{crossbasis}}, \code{\link[stats]{glm}},
#'   \code{\link[nlme]{lme}}.
#' @examples
#' # For detailed examples:
#' browseVignettes("collin")
#' @export
#' @importFrom dlnm crosspred
collindlnm <- function(model,
                       x,
                       cb,
                       at = 1,
                       cen = 0,
                       effect,
                       type = c("coef", "risk"),
                       shape = c("linear", "nonlinear"),
                       nsim = 100,
                       verbose = TRUE,
                       seed = NULL) {
  ### control "shape"
  shape <- match.arg(shape)

  ### control "model"
  if (missing(model)) {
    stop("'model' must be provided.")
  }

  implementedmodels <- list(linear = c("lme", "glm"), nonlinear = "glm")
  classmod <- class(model)[1]
  if (!classmod %in% implementedmodels[[shape]]) {
    stop(cat("\n For 'shape' being '",
             shape,
             "', 'model' is not allowed to be of class '",
             classmod,
             "' yet.\n Model classes currently implemented for such a shape are:\n",
             paste(" - ", implementedmodels[[shape]], "\n"), "\n", sep = ""))
  }

  ### control "x"
  if ((shape == "linear") & (missing(x) || (!inherits(x, "matrix")))) {
    stop("'x' must be a matrix.")
  }

  if (shape == "nonlinear") {
    cond1 <- !inherits(x, "numeric")
    cond2 <- !inherits(x, "matrix") && (dim(x)[2] == 1)
    if (cond1 & cond2) {
      stop("'x' must be a numeric vector or a 1-column matrix.")
    }
  }

  ### control "cb"
  if (missing(cb) || (!inherits(cb, "crossbasis"))) {
    stop("'cb' must be an object of class 'crossbasis'.")
  }

  ### control "at"
  if ((shape == "linear") & ((length(at) != 1L) || (!inherits(at, "numeric")))) {
    stop("'at' must be a single numeric value.")
  }

  if ((shape == "nonlinear") & ((length(at) < 2) || (!inherits(at, "numeric")))) {
    stop("'at' must be a numeric vector.")
  }

  ### control "cen"
  if (shape == "linear") {
    cen <- 0
  }

  if ((shape == "nonlinear") & ((length(cen) != 1L) || (!inherits(cen, "numeric")))) {
    stop("A single numeric value for 'cen' must be provided.")
  }

  ### control for "effect"
  if ((shape == "linear") & (missing(effect) || (!inherits(effect, "numeric")) || length(effect) != dim(x)[2])) {
    stop("If 'shape' is 'linear', 'effect' must be a numeric vector with length equal to\n
         the number of columns of 'x'.")
  }

  if ((shape == "nonlinear") & (missing(effect) || (!inherits(effect, "matrix")) || dim(effect)[1] != length(at))) {
    stop("If 'shape' is 'nonlinear', 'effect' be a numeric matrix with as many rows as the\n
         length of 'at' and as many columns as 'cb'.")
  }

  ### control "type"
  type <- match.arg(type)

  if ((type != "coef") & (classmod == "lme")) {
    stop(cat("'type' must be equal to 'coef' for the provided 'model' of class '", classmod, "'.", sep = ""))
  }

  ### control "nsim"
  if ((length(nsim) != 1L) || (nsim <= 0) || (!inherits(nsim, "numeric"))) {
    stop("'nsim' must be a single integer value.")
  }

  ### control "verbose"
  if ((!inherits(verbose, "logical")) || (length(verbose) != 1L)) {
    stop("'verbose' must be TRUE or FALSE.")
  }

  ### control "seed"
  if (!is.null(seed)) {
    if ((length(seed) != 1L) || (!inherits(seed, "numeric"))) {
      stop("If 'seed' is not NULL, it must be a single numeric value.")
    }
    set.seed(seed)
  }

  ### coefficients names
  coefnames <- paste0(deparse(substitute(cb)), colnames(cb))

  ### effect scale
  if (type == "risk") {
    effect <- log(effect)
  }

  ### results
  sims <- NULL
  pred <- NULL

  ### get simulations using specific method "simulatedllm"
  simargs <- list(model = model,
                  x = x,
                  cb = cb,
                  at = at,
                  cen = cen,
                  effect = effect,
                  nsim = nsim,
                  verbose = verbose,
                  coefnames = coefnames)
  newclass <- paste0(shape, classmod)
  class(simargs) <- c(newclass, class(simargs))
  sims <- simulatedlnm(simargs)

  ### get estimated effect using original data
  predorig <- crosspred(basis = cb, model = model, cen = cen, at = at)

  ### return simulations, orignal effect and auxiliar objects needed for "plot"
  ### method
  res <- list(sim = sims, pred = predorig, classmod = classmod, at = at,
              effect = effect)
  ### new class for plot method
  newclass <- paste0("collindlnm", shape)
  class(res) <- c(class(res), newclass)
  if (verbose) {
    cat("\n Simulations done.\n")
  }
  return(res)
}

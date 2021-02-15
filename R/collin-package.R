#' collin: Visualizing the effects of collinearity in distributed lag models
#'   and other linear models
#'
#' @description Visual tool to assessing whether the results of a study could be
#'   driven by collinearity.
#' @section Procedure: The tool consists of a two-step procedure.
#'
#'   In the first step, the user provides the fitted model to be analyzed and a
#'   hypothetical effect pattern of the predictor of interest on the outcome.
#'   Then, simulations are performed under such a hypothetical effects and the
#'   given model.
#'
#'   In the second step, a specific \code{plot} method is used to visualize
#'   the results of the simulations to assessing whether these results are
#'   consistent with the results using the real data, so that unexpected results
#'   could be driven by collinearity.
#' @section Authors:
#'   Jose Barrera-Gomez and Xavier Basagana
#'   Maintainer: Jose Barrera-Gomez <jose.barrera@isglobal.org>
#' @references The methodology used in the package is described in
#'
#' Basagana X, Barrera-Gomez J. Visualizing the effects of collinearity in
#'   distributed lag models. \emph{International Journal of Epidemiology}. (under review)
#' @docType package
#' @name collin
NULL

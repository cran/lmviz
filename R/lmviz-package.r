#' lmviz: A package to visualize linear models features and play with them.
#'
#' The lmviz package contains a suite of shyny apps
#' \describe{
#'   \item{\code{\link{SimpleLM}}}{allows to input the parameters of a simple linear model and simulate from it, exploring the various diagnostics and sampling distributions}
#'   \item{\code{\link{SampleDist}}}{allows to input the parameters of a linear model and simulate from it, exploring the sampling distributions of the estimators of the coefficients}
#'   \item{\code{\link{ConfInt}}}{allows to input the parameters of a simple linear model and simulate from it, illustrating confidence intervals and regions for the coefficients}
#'   \item{\code{\link{SlopeTest}}}{allows to input the parameters of a simple linear model, simulate from it and repeatedly perform hypotheses testing on the slope, exploring repeated sampling properties}
#'   \item{\code{\link{Prediction}}}{allows to input the parameters of a simple linear model and simulate from it  illustrating confidence and prediction intervals for E(Y|X=x)}
#'   \item{\code{\link{MultipleLM}}}{allows to input the parameters of a multiple linear model with two covariates and simulate from it, exploring the sampling distributions of the estimators of the coefficients}
#'   \item{\code{\link{Collin}}}{allows to input the parameters of a multiple linear model with two covariates and simulate from it, exploring the consequences of collinearity on sampling distributions and inferential procedures}
#'   \item{\code{\link{LMBoard}}}{the user can draw (the points of) a scatter diagram, the corresponding linear regression model is estimated}
#'   \item{\code{\link{BadLM}}}{allows to set a data generating mechanism violating the homoscedasticity and/or normality assumptions of the linear model and explore by simulation its consequences on inference}
#'   \item{\code{\link{QuizResidual}}}{a game in which the user is prompted to guess whether the diagnostic plots of a linear model suggest some hypotheses is violated}
#'   \item{\code{\link{GuessTheLine}}}{a game in which the user is shown a (random) scatter diagram and must guess the least squares line}
#'   \item{\code{\link{GuessThePoints}}}{a game in which the user must draw a scatter diagram compatible with a series of statistics}
#' }
#'
#'
#' @docType package
#' @name lmviz
NULL

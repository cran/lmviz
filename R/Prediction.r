#' Prediction shiny app
#'
#' Launches the Prediction shiny app, a tool to explore confidence and prediction intervals for the conditional mean in a simple linear model
#'
#'
#'
#'
#' @details
#'
#'
#' Full help is available from within the shiny app.
#'
#' @return None
#'
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#'
#' @examples
#' ## Not run:
#' if (interactive()){
#'  Prediction()
#'  }
#' ## End(Not run)
#' @import shiny
#'
#' @export


Prediction=function(){
  appDir <- system.file("PredictionLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



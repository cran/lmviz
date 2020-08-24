#' SampleDist shiny app
#'
#' Launches the SampleDist shiny app, a tool to explore sampling variability of the linear model and the sampling distributions of the least squares estimators
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
#'  SampleDist()
#'  }
#' ## End(Not run)
#' @import shiny
#' @importFrom car ellipse
#'
#' @export


SampleDist=function(){
  appDir <- system.file("SampleDistLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



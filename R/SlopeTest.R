#' SlopeTest shiny app
#'
#' Launches the SlopeTest shiny app, a tool to illustrate hypotheses testing on the slope in a simple regression model
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
#'  SlopeTest()
#'  }
#' ## End(Not run)
#' @import shiny
#'
#' @export


SlopeTest=function(){
  appDir <- system.file("SlopeTestLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



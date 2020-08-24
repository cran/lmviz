#' ConfInt shiny app
#'
#' Launches the ConfInt shiny app, a tool to explore confidence intervals and regions for the coefficient of simple linear model
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
#'  ConfInt()
#'  }
#' ## End(Not run)
#' @import shiny
#' @importFrom car ellipse
#'
#' @export


ConfInt=function(){
  appDir <- system.file("ConfIntLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



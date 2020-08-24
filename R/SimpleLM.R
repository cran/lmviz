#' SimpleLM shiny app
#'
#' Launches the SimpleLM shiny app, which allows to input the parameters of a linear model and simulate from it, exploring the various diagnostics and sampling distributions
#'
#'
#' @details
#'
#'
#' Full help is available from within the shiny app.
#'
#'
#' @import shiny
#'
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#'
#' @examples
#' ## Not run:
#' if (interactive()){
#'  SimpleLM()
#'  }
#' ## End(Not run)
#'
#'
#'
#' @export
SimpleLM=function(){
  appDir <- system.file("SimpleLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}

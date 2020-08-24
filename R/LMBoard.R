#' LMBoard shiny app
#'
#' Launches the LMBoard shiny app, user can add remove points and visualize changes in the estimates and residual plots
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
#'  LMBoard()
#'  }
#' ## End(Not run)
#' @import shiny
#'
#' @export


LMBoard=function(){
  appDir <- system.file("LMBoardApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



#' GuessThePoints shiny app
#'
#' Launches the GuessThePoints shiny app, the user is prompted to guess a sample of points compatible with a set of parameters.
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
#'  GuessThePoints()
#'  }
#' ## End(Not run)
#' @import shiny
#'
#' @export


GuessThePoints=function(){
  appDir <- system.file("GuessThePointsApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



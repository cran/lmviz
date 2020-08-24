#' GuessTheLine shiny app
#'
#' Launches the GuessTheLine shiny app, the user is prompted to guess the least squares fit of a sample of random points
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
#'  GuessTheLine()
#'  }
#' ## End(Not run)
#' @import shiny
#'
#' @export


GuessTheLine=function(){
  appDir <- system.file("GuessTheLineApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



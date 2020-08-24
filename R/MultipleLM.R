#' MultipleLM shiny app
#'
#' Launches the MultipleLM shiny app, a tool where the user can specify a multiple linearmodel, simulate from it and explore the sampling distribution of estimators under various conditions
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
#'  MultipleLM()
#'  }
#' ## End(Not run)
#' @import shiny
#' @importFrom MASS mvrnorm
#' @importFrom car ellipse scatter3d
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom rgl rgl.open rglwidget
#'
#' @export


MultipleLM=function(){
  appDir <- system.file("MultipleLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



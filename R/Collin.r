#' Collin shiny app
#'
#' Launches the Collinearity shiny app, a tool to explore the consequences of collinearity on sampling distributions and inferential procedures in linear models
#'
#'
#' @details
#'
#'
#' Full help is available from within the shiny app.
#'
#'
#'
#' @return None
#'
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#'
#' @examples
#' ## Not run:
#' if (interactive()){
#'  Collin()
#'  }
#' ## End(Not run)
#' @import shiny
#' @importFrom MASS mvrnorm
#' @importFrom car ellipse scatter3d
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom rgl rgl.open rglwidget
#'
#' @export


Collin=function(){
  appDir <- system.file("CollinLMApp.r", package = "lmviz")
  .GlobalEnv$".wd"=getwd()
  shiny::runApp(appDir)
}



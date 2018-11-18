#' QuizResidual shiny app
#'
#' Launches the QuizResidual shiny app
#'
#' @param Simulation the function to be used to simulate data (see \code{\link{Simulation.default}})
#' @param ComputerDecision  the function to be used to state the answer of the computer player (see \code{\link{ComputerDecision.default}})
#' @param dir.images the directory where images to be used by the shiny app are to be found, default to NULL, images from the package will be used
#' @param dir.sounds the directory where sounds to be used by the shiny app are to be found, default to NULL, sounds from the package will be used
#'
#' @details
#' QuizResidual shiny app is a game in which the player is asked to guess, based on four standard diagnostic plots of a linear model, whether there is a violation of one of the basic assumptions: linearity, homoscedasticity, normality of errors.
#'
#' The program will simulate a sample (x,Y) from a randomly chosen data generating mechanism possibly violating one of the assumptions (function \code{\link{Simulation.default}}), fit a linear model and plot the diagnostic.
#'
#'  The computer player makes a guess on whether there is a violation of assumptions (function \code{\link{ComputerDecision.default}}).
#'
#'  After the answer is given, the true data generating mechanism will be shown in the plot, in particular, the true regression function, the true standard deviation of errors and the true density of errors.
#'
#'  The game can be customized by coding your own Simulation and ComputerDecision functions passing them as arguments.
#'
#'  Sounds will be played depending on whether the correct or wrong answer is given and a final sound is played depending on the outcome, also an appropriate image is shown. (Sounds are taken from the site https://freesound.org/, images from https://wpclipart.com and are public domain, other sounds and images can be used by calling the app with the directories where the images are stored as argument, sounds must be named as follows: suonorr: sound to be played when both the player and the computer give the correct answer; suonowr: sound to be played when the player is correct and the computer is wrong; suonorw and suonoww are analogous; suonofinaleP/V/S: final sound to play in case of tie/win/loss; immagineP/V/S: image to be shown in case of tie/win/loss.)
#'
#' @return None
#'
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#' @seealso \code{\link{ComputerDecision.default}}, \code{\link{Simulation.default}}
#' @examples
#' ## Not run:
#' if (interactive()){
#'   QuizResidual()
#'   # if custom sounds and images are in the directory www in the working directory
#'   QuizResidual(dir.images=paste0(getwd(),"/www"),dir.sounds=paste0(getwd(),"/www"))
#' }
#' ## End(Not run)
#'
#' @import shiny
#'
#' @export

QuizResidual=function(ComputerDecision=ComputerDecision.default,Simulation=Simulation.default,dir.images=NULL,dir.sounds=NULL){

##http://shiny.rstudio.com/articles/deployment-local.html#package
##https://stackoverflow.com/questions/44999615/passing-parameters-into-shiny-server

 # ui <- server <- NULL # avoid NOTE about undefined globals
#  server_env <- environment(server)

  # Here you add any variables that your server can find
 # ComputerDecision <<- ComputerDecision
 # Simulation <<- Simulation

  ###############################################################
  ## Solution 1 to pass arguments to the shiny app
  ##  first two lines in QuizResidualApp.r superfluous (no harm done if left there)
  ##  Source: https://github.com/rstudio/shiny/issues/440
    .GlobalEnv$".ComputerDecision" <- ComputerDecision
    .GlobalEnv$".Simulation"  <- Simulation
    .GlobalEnv$".dir.sounds" <- dir.sounds
    .GlobalEnv$".dir.images"  <- dir.images

  ###############################################################
  ## Solution 2 to pass arguments to the shiny app
  ##  goes WITH the first two lines in QuizResidualApp.r (needed)
  ##  Source: https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  #shinyOptions(ComputerDecision = ComputerDecision)
  #shinyOptions(Simulation = Simulation)
    #  shinyOptions(dir.sounds = dir.sounds)
    #  shinyOptions(dir.images = dir.images)
    appDir <- system.file("QuizResidualApp.r", package = "lmviz")
    shiny::runApp(appDir)
}

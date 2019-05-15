#' BadLM shiny app
#'
#' Launches the BadLM shiny app, a tool to explore the consequences of the violation of homoscedasticity and/or normality assumptions in a linear model
#'
#'
#'
#'
#' @param dist.custom custom generator for Y, see examples below
#' @param dist.custom.veravar variance function for dist.custom, see examples below
#' @param dist.custom.param parameters for dist.custom, see examples below
#'
#' @details
#' Allows to set a data generating mechanism for a response variable \eqn{Y} and an explanatory variable \eqn{x} such that \eqn{E(Y|X=x)=\beta_1+\beta_2 x}, various possible distributions for \eqn{Y} are available, depending on the selected distributional assumptions the variance may also be set as a function of \eqn{x}. The program performs a number of simulations from the fit and visualizes the simulated sampling distributions of the estimators.
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
#' BadLM()
#'
#' # function to generate Y
#' dist=function(n,my,parvet,par,x) {
#'   my+parvet*rt(n,df=par[1])
#' }
#' # function to give the true value of the variance
#' varfun=function(my,parvet,par,x){
#'   if (par[1]>2) {
#'     veravar=parvet^2*par[1]/(par[1]-2)
#'   } else {
#'     veravar=-1
#'   }
#'   return(veravar)
#' }
#' # dist and varfun must have those argument where
#' #  my is the vector mean of Y
#' #  parvet is g() computed at x values
#' #  par is a vector of two parameters
#' param=list(nome="Student-t (bis)", #name of dist for drop down menu (optional)
#'            nomepar1="Gradi di libertà", #name of parameter 1 (optional)
#'            minpar1=1,maxpar1=30, #min/max of param 1 (needed)
#'            valuepar1=10, #initial value of param1 (optional)
#'            steppar1=0.1, #increment of param1 (optional)
#'            enableVarFunPanel=TRUE #whether the panel to input g should appear
#' )
#'
#' BadLM(dist.custom=dist,dist.custom.veravar = varfun,dist.custom.param=param)
#'
#'
#' dist=function(n,my,parvet,par,x) {
#'  my+rnorm(n,0,sqrt(par[1]+par[2]*x^2))
#' }
#' # function to give the true value of the variance
#' varfun=function(my,parvet,par,x){
#'  return(par[1]+par[2]*x^2)
#' }
#' # dist and varfun must have those argument where
#' #  my is the vector mean of Y
#' #  parvet is g() computed at x values
#' #  par is a vector of two parameters
#' param=list(nome="N(.,b1+b2*x^2)", #name of dist for drop down menu (optional)
#'            nomepar1="b1", #name of parameter 1 (optional)
#'            minpar1=1,maxpar1=3, #min/max of param 1 (needed)
#'            valuepar1=1, #initial value of param1 (optional)
#'            steppar1=0.1, #increment of param1 (optional)
#'            nomepar2="b2", #name of parameter 1 (optional)
#'            minpar2=0,maxpar2=3, #min/max of param 1 (needed)
#'            valuepar2=1, #initial value of param1 (optional)
#'            steppar2=0.1, #increment of param1 (optional)
#'            enableVarFunPanel=FALSE, #whether the panel to input g should appear
#'            showVarFun=TRUE
#' )
#'
#'   BadLM(dist.custom=dist,dist.custom.veravar = varfun,dist.custom.param=param)
#' }
#' ## End(Not run)
#' @import shiny
#' @importFrom lmtest resettest bptest
#' @importFrom stats shapiro.test dbeta dchisq dnorm dt lm quantile rbeta rchisq rnorm rt runif sd
#'
#' @export


#dist.custom    =function(n,my,parvet,par) { my+parvet*rt(n,par[1])}
#dist.custom.veravar=function(my,parvet,par){
#  if (par[1]>2) {
#    veravar=parvet^2*par[1]/(par[1]-2)
#  } else {
#    veravar=-1
#  }
#  return(veravar)
#}
#dist.custom.param=list(nome="Prova",nomepar1="df",minpar1=1,maxpar1=9,enableVarFunPAnel=TRUE)


BadLM=function(dist.custom=NULL,dist.custom.veravar=NULL,dist.custom.param=NULL){

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
   # .GlobalEnv$.dist.custom <- dist.custom
  #  .GlobalEnv$.dist.custom.veravar <- dist.custom.veravar
  #  .GlobalEnv$.dist.custom.param <- dist.custom.param
  #  .GlobalEnv$.wd=getwd()

  ###############################################################
  ## Solution 2 to pass arguments to the shiny app
  ##  goes WITH the first two lines in QuizResidualApp.r (needed)
  ##  Source: https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  shinyOptions(dist.custom = dist.custom)
  shinyOptions(dist.custom.veravar = dist.custom.veravar)
  shinyOptions(dist.custom.param = dist.custom.param)
  shinyOptions(wd=getwd())
    appDir <- system.file("BadLMApp.r", package = "lmviz")
    shiny::runApp(appDir)
}

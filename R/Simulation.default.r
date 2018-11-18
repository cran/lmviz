#' Simulates a sample
#'
#' Simulates an (x,y) sample (suitable for estimating a lm) which can be either non linear/heteroscedastic/non normal or in line with standard lm assumptions
#'
#' @param model.to.sim an integer between 1 and 4 where 1=non linearity; 2=heteroscedasticity; 3=non normality; 4=no violation
#'
#' @return A list of objects (of which the first two are essential, the following are needed to display the correct solution in the shiny app)
#' \item{x,y}{the sample}
#' \item{my}{the true mean of Y}
#' \item{sderr}{the true standard deviation of errors}
#' \item{errore}{the errors}
#' \item{xperdens,ferrore}{coordinates of points of the true density of errors}
#'
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#' @seealso \code{\link{ComputerDecision.default}}, \code{\link{checksim}}
#'
#' @examples
#' Simulation.default(1)
#'
#' Simulation.default(sample(1:4,1))
#'
#'
#' @export
Simulation.default=function(model.to.sim){
  ##
  ## The function produces simulated data
  ## The input MUST be an integer between 1 and 4 where
  ##   1=non linearity; 2=eteroschedasticity; 3=non normality; 4=no violation
  ## The output MUST be a list which
  ##   MUST include elements x and y with the simulated data
  ## ONLY in order to get plots of the true model after the answer the output should include
  ##   my = true regression function at x values
  ##   sderr = standard deviation of errors at x values
  ##   errore = (true) errors
  ##   xperdens = value of abscissa for plotting the density of errors
  ##   ferrore = true density of errors at xperdens
  ## inclusion of part of the elements leads to part of the plots
  n=round(runif(1,10,1000),0)

  x=sort(runif(n,0,1))

  simula.x=function(n){
    scelta=sample(1:5,1,prob=c(0.30,0.30,0.20,0.10,0.10))
    if (scelta==1) {x=runif(n,0,1)}
    if (scelta==2) {x=rnorm(n,0,1)}
    if (scelta==3) {x=rchisq(n,0,sample(c(1,4,10,15),1))}
    if (scelta==4) {x=rnorm(n,sample(0:1,n,replace=TRUE),1)}
    if (scelta==5) {x=rt(n,df=sample(c(1,4,10,15),1))}
    x=sort(x)
    x=(x-mean(x))/sd(x)
    return(x)
  }

  x=simula.x(n)
  beta1=rnorm(1,0,2)
  beta2=rnorm(1,0,2)

  if (model.to.sim==1){ ## non linearità
    if (runif(1)<0.5){
      x1=pmin(x,runif(1,quantile(x,0.25),quantile(x,0.75)))
    } else {
      x1=pmax(x,runif(1,quantile(x,0.25),quantile(x,0.75)))
    }
    quale=sample(1:2,1)
    if (quale==1) my=sample(c(-1,1),1)*sin(2*pi*x1/runif(1,1,4))
    if (quale==2) my=beta1+beta2*x1
  } else {
    my=beta1+beta2*x
  }
  if (model.to.sim==2){ ## eteroschedasticità
    if (runif(1)<0.5){
      x1=pmin(x,runif(1,quantile(x,0.25),quantile(x,0.75)))
    } else {
      x1=pmax(x,runif(1,quantile(x,0.25),quantile(x,0.75)))
    }
    sderr=1+runif(1,0,1)+sin(2*pi*x1/runif(1,1,2)-runif(1,0,pi))
  } else {
    sderr=rep(1,n)
  }
  #   xperdens=seq(0.05,0.95,length=30)
  if (model.to.sim==3){ ## non normalità
    quale=sample(1:4,1)
    if (quale==1) {
      a1=sample(c(-1,1),1)
      if (n>500){
        a2=sample(c(2,4,8,12,13,20,30),1)
      } else {
        a2=sample(c(2,4,8,12,13,20),1)
      }
      errore=a1*rchisq(n,df=a2)
      xperdens=seq(min(errore),max(errore),length=30)
      ferrore=dchisq(a1*xperdens,a2)
    }
    if (quale==2) {
      if (n>500){
        a1=sample(c(2,7,12,16,20,21,30),1)
      } else {
        a1=sample(c(2,4,7,10),1)
      }
      errore=rt(n,df=a1)
      xperdens=seq(min(errore),max(errore),length=30)
      ferrore=dt(xperdens,df=a1)
    }
    if (quale==3) {
      a1=sample(c(0.5,1:5),1)
      a2=a1+sample(c(3:6),1)
      errore=rbeta(n,a1,a2)
      xperdens=seq(min(errore),max(errore),length=30)
      ferrore=dbeta(xperdens,a1,a2)
    }
    if (quale==4) {
      a=sample(c(0.5,1:3),1)
      errore=rbeta(n,a,a)
      xperdens=seq(min(errore),max(errore),length=30)
      ferrore=dbeta(xperdens,a,a)
    }} else {
      errore=rnorm(n,0,sderr)
      xperdens=seq(min(errore),max(errore),length=30)
      ferrore=dnorm(xperdens,0,sderr[1])
    }
  my=(my-mean(my))/sd(my)
  randomizzaerrore=runif(1,1,3)  # randomizes signal to noise ratio
  sderrore=sd(errore)
  sderr2=randomizzaerrore*sderr/sd(errore)
  mediaerrore=mean(errore)
  errore=randomizzaerrore*(errore-mediaerrore)/sderrore
  xperdens=randomizzaerrore*(xperdens-mediaerrore)/sderrore
  ferrore=ferrore*sderrore/randomizzaerrore
  y=my+errore
  #    ferrore=ferrore[sort.list(errore)]
  #    errore=errore[sort.list(errore)]
  return(list(x=x,y=y,my=my,sderr=sderr2,errore=errore,ferrore=ferrore,xperdens=xperdens))
}

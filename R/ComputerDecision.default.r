#' Computer player decision
#'
#'
#' Decides whether a fitted lm objects residuals are such that a violation of the assumptions of non linearity, heteroscedasticity, non normality occurs
#'
#' @param fit an object returned by \code{\link[stats]{lm}}
#'
#' @return An integer between 1 and 4 where 1=non linearity; 2=heteroscedasticity; 3=non normality; 4=no violation
#'
#' @author Francesco Pauli, \email{francesco.pauli@@deams.units.it}
#' @seealso \code{\link{Simulation.default}}, \code{\link{checksim}}
#'
#' @examples
#' x=rnorm(10)
#' y=x+rnorm(10,0,0.4)
#' fit=lm(y~x)
#' ComputerDecision.default(fit)
#'
#' x=rnorm(30)
#' y=x+rt(30,2)
#' fit=lm(y~x)
#' ComputerDecision.default(fit)
#'
#' @importFrom methods is
#' @importFrom lmtest resettest bptest
#' @import mgcv
#' @importFrom stats shapiro.test dbeta dchisq dnorm dt lm quantile rbeta rchisq rnorm rt runif sd
#'
#' @export
ComputerDecision.default=function(fit){
  ##
  ## The function regulates the behaviour of the computer
  ## input MUST be an lm object
  ## output MUST be an integer between 1 and 4, where
  ## 1=non linearity; 2=heteroscedasticity; 3=non normality; 4=no violation
  x=fit$residuals
  normal.check=(shapiro.test(x)$p.value<0.05)
  #  etero.check=bptest(fit)$p.value<0.05
  etero.check=bptest(fit)$p.value<0.05

  # etero.check=min(c(bptest(fit)$p.value,gqtest(fit)$p.value))<0.05
  # etero.check=(gqtest(fit())$p.value<0.05)
  # if (is(etero.check)[1]=="try-error") etero.check=FALSE
  # harvtest(fit)$p.value
  #lin.check=(resettest(fit)$p.value<0.05)
  # browser()
  if (length(x)>30){
    fitg=try(gam(y~s(x),data=data.frame(x=fit$model[,2],y=fit$residuals)))
    if (is(fitg)[1]!="try-error") fits=try(summary(fitg))
    if (is(fits)[1]!="try-error") {
      lin.check=(fits$s.table[4]<0.05)
    } else {
      lin.check=(resettest(fit)$p.value<0.05)
    }
  } else {
    lin.check=(resettest(fit)$p.value<0.05)
  }

  nonrvcomputer=4
  if (lin.check) {
    nonrvcomputer=1
  } else {
    if (etero.check){
      nonrvcomputer=2
    } else {
      if (normal.check){
        nonrvcomputer=3
      } else {
        nonrvcomputer=4
      }
    }
  }
  return(nonrvcomputer)
}

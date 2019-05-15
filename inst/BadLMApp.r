#library(shiny)
#library(shinyjs)
##library(mgcv)
#library(lmtest)

#dist.custom <- .dist.custom
#dist.custom.veravar <- .dist.custom.veravar
#dist.custom.param <- .dist.custom.param
#wd=.wd

dist.custom <- getShinyOption("dist.custom",NULL)
dist.custom.veravar <- getShinyOption("dist.custom.veravar",NULL)
dist.custom.param <- getShinyOption("dist.custom.param",NULL)
wd <- getShinyOption("wd",NULL)


#dist.custom.param=param
#dist.custom=dist
#dist.custom.veravar=varfun

ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  tags$h1(id="titolo","Consequences of hypotheses violation"),
  tabsetPanel(id="main",
        tabPanel("?Model specification",
                 verticalLayout(
                   splitLayout(
                     wellPanel(
                       HTML("<p style='white-space: pre-wrap'>The panel allows to specify a distribution for the explanatory variable x and the regression parameters beta1 and beta2.</p>
                            <p style='white-space: pre-wrap'>The explanatory variable x is simulated once (is renewed anytime the parameters of its distribution are changes or by pressing the appropriate button (see below).</p>
                            <p style='white-space: pre-wrap'>The explanatory variable x is standardized between 0 and the maximum value set here, this allows to assess the properties of extrapolation since the the prediction of Y|x=1 is simulated.</p>
                            "),style = "height:400px;"
                     ),
                     wellPanel(
                       HTML("<p style='white-space: pre-wrap'>The panel allows to set thesample size and the distribution of Y.</p>
                            <p style='white-space: pre-wrap'>Select the distribution from the drop-down menu, the parameters will update accordingly and the right panel will change as weel, possibly allowing input of the variance function.</p>
                            <p style='white-space: pre-wrap'>Note that Y is always simulated so that E(Y)=beta1+beta2*x (this is to have a true value of beta1 and beta2 to compare the sample distributions to.</p>
                            "),style = "height:400px;"
                     ),
                     wellPanel(
                       HTML("<p style='white-space: pre-wrap'>Depending on the distribution of Y which is selected the panel will show</p>
                             <p style='white-space: pre-wrap'> - a plot to draw the variance function g(x)=V(Y|X=x) or its square root: g(x) will be the linear interpolation of the points clicked on the plot (double click ona point to cancel it, click on the gray areas on top and bottom to change the range of ordinate axis, click on the gray areas on left and right to move the first and last points;</p>
<p style='white-space: pre-wrap'> - a plot showing the true variance function if this is set in analytic form;</p>
<p style='white-space: pre-wrap'> - the distribution of Y|X=0 and that of Y|X=1;</p>
                            "),style = "height:400px;"
                     )
                   ),
                   wellPanel(HTML("<p style='white-space: pre-wrap'>Simulated values based on the specified data generating mechanism, model fit and residuals. (Buttons to renew values of Y and possibly x are available.)</p>"
                                  ))
                 )
                 ),
        tabPanel("Model specification",
                 verticalLayout(
                   splitLayout(cellArgs=list(style = "overflow:hidden;"),
                               wellPanel(
                                 verticalLayout(
                                   selectInput("distribuzioneX","Distribution of x",
                                               choices = c("Regular grid"="regulargrid","Uniform"="uniform","Gaussian"="conc","Asymmetric"="asymm")),
                                   sliderInput("parx","-",min=0.1,max=1,value=1,step=0.1),
                                   sliderInput("taglia","upper limit for x",min=0.1,max=1,value=1,step=0.1),
                                   h4("Regression parameters (E(Y)=beta1+beta2*x)",style='white-space: pre-wrap'),
                                   splitLayout(
                                     uiOutput("beta1inser"),
                                     uiOutput("beta2inser")
                                 )
                               ),style = "height:500px;"),
                               wellPanel(
                               verticalLayout(
                                 sliderInput("n","Number of observations",min=8,max=1000,value=25,step=1),
                                 selectInput("distribuzione","Distribution of Y",
                                             choices = c("Gaussian V(Y)=g(x)"="dist.normale","AR1"="dist.normaleAR1",
                                                         "Poisson"="dist.poisson","Binomial"="dist.binomiale","Gamma"="dist.gamma",
                                                         "Gamma transl."="dist.gammatrasl","Uniform [-c,c]"="dist.uniforme",
                                                         "Student-t (error=g(x)*t_df)"="dist.student")),
                                  sliderInput("par","parametro",min=1,max=30,value=10,step=1),
                                 sliderInput("par2","parametro2",min=1,max=30,value=10,step=1)),
                               p("Note that E(Y)=beta1+beta2*x for all models",style='white-space: pre-wrap'),
                               style = "height:500px;"),
                               wellPanel(
                                 conditionalPanel(condition="output.servePannelloVarianza",
                                                  checkboxInput("inputvarosd","set g(x) (set square root of g(x))"),
                                                  plotOutput("plotinputvarianza",
                                                             click = "plot_click",dblclick = "plot_dblclick")),
                                 conditionalPanel(condition="!output.servePannelloVarianza",
                                                plotOutput("plotdist")),style = "height:500px;")
                 ),
                 wellPanel(
                 fluidRow(cellArgs=list(style = "overflow:hidden;"),
                          column(width=1,
                                 verticalLayout(
                                   actionButton("rinnova",label="New Y"),
                                   actionButton("rinnovaX",label="New x,Y")
                                 )),
                          column(width=11,
                                 splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                 plotOutput("plotdiprova",height=300),
                             plotOutput("plotdiprova2",height=300),
                             plotOutput("plotdiprova3",height=300)
                               )))
                   ))
                 ),
        tabPanel("Simulation",
                 fluidRow(
                   column(width=2,
                          wellPanel(
                          verticalLayout(
                          checkboxInput("showqqplot","Normal quantile plots"),
                          selectInput("m","N. sim.",choices=c(500,1000,5000,10000)),
                          actionButton("vai","Vai"),
                          selectInput("parametro",label="Parameter",choices=c("beta1","beta2","prev","sigma2")),
                          plotOutput("shapiroplot",height=200),
                          plotOutput("bptestplot",height=200)
                          )
                   )),
                   column(width=10,
                          verticalLayout(
                    conditionalPanel(condition="input.parametro=='beta2'",
                     splitLayout(cellArgs=list(style = "overflow:hidden;"),
                       plotOutput("beta2plot"),
                       plotOutput("beta2tstat"),
                       plotOutput("coperturaintervallibeta2")
                   )),
                   conditionalPanel(condition="input.parametro=='beta1'",
                                    splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                      plotOutput("beta1plot"),
                                      plotOutput("beta1tstat"),
                                      plotOutput("coperturaintervallibeta1")
                                    )),
                   conditionalPanel(condition="input.parametro=='prev'",
                                    splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                      plotOutput("prevplot"),
                                      plotOutput("prevtstat"),
                                      plotOutput("coperturaintervalliprev")
                                    )),
                   conditionalPanel(condition="input.parametro=='sigma2'",
                                    splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                      plotOutput("sigmaplot"),
                                      plotOutput("sigma2plot"),
                                      plotOutput("coperturaintervallisigma2")
                                    )),
                   wellPanel(tableOutput("tabellasim")),
                   actionButton("save","Save results (to object named results.badLM in working space)")
          )
      ))
    ),
    tabPanel("?Simulations",
             fluidRow(
               column(width=3,
                      wellPanel(verticalLayout(
                        HTML("<p style='white-space: pre-wrap'>Set the number of simulations.</p>
                             <p style='white-space: pre-wrap'>Pres the Go button to perform the desired number of simulations (may take some time).</p>
                             <p style='white-space: pre-wrap'>When simulations have been performed the drop-down menu allows to choose the parameter to show on the right.</p>
                             <p style='white-space: pre-wrap'>Plot of p-values of Breusch-Pagan and Shapiro wilks tests are also shown.</p>
                             ")
                      ))
               ),
               column(width=9,
                      verticalLayout(
                        splitLayout(cellArgs=list(style = "overflow:hidden;"),
                          verticalLayout(
                            HTML("<br><p style='white-space: pre-wrap'>For beta1, beta2, prev: Sample distribution of the estimator, the true value, if available, is shown in green, the empirical mean of the estimates is shown as a black triangle.</p>
                                 <p style='white-space: pre-wrap'>If the option quantile plots is chosen the empirical quantiles are compared to those of the appropriate distribution (gaussian for beta1, beta2 and prev, chi.square for the variance estimator.</p>
                                <p style='white-space: pre-wrap'>For the variance: sample distribution of the estimator s or empirical quantiles of s versus quantiles of the square root of a chi-square distribution.</p>
                                 ")
                          ),
                          verticalLayout(
                            HTML("<br><p style='white-space: pre-wrap'>For beta1, beta2, prev: sample distribution of a pivotal quantity associated to the parameter.</p>
                                 <p style='white-space: pre-wrap'>If the option quantile plots is chosen the empirical quantiles are compared to those of the appropriate distribution (t for beta1, beta2 and prev).</p>
                                <p style='white-space: pre-wrap'>For the variance: sample distribution of the estimator s^2 or empirical quantiles of s^2 versus quantiles of the chi-square distribution.</p>
                                 ")
                          ),
                          verticalLayout(
                            HTML("<br><p style='white-space: pre-wrap'>Comparison of mpirical coverage and nominal coverage of confidence intervals for the parameter.</p>
                                 <p style='white-space: pre-wrap'>For the variance, if a heteroschedastic model is considered, there is no true value of the parameter to assess coverage, hence the true variance function is shown together with its estimated and its standard error.</p>
                                 ")
                          )
                        ),
                        wellPanel(
                          HTML("<p style='white-space: pre-wrap'>Table comparing the true value of parameters (when available) and the simulation results.</p>")
                          )
                      )
               )
             )
    )))




server <- function(input, output, session) {
  updateTabsetPanel(session, inputId="main", selected = "Model specification")
#  observe({
  if (!is.null(dist.custom.param)){
    sceltedistY=c("Gaussian V(Y)=g(x)"="dist.normale","AR1"="dist.normaleAR1",
                  "Poisson"="dist.poisson","Binomial"="dist.binomiale","Gamma"="dist.gamma",
                  "Gamma transl."="dist.gammatrasl","Uniform [-c,c]"="dist.uniforme",
                  "Student-t (error=g(x)*t_df)"="dist.student","Custom"="dist.custom")
    if (!is.null(dist.custom.param$nome)) names(sceltedistY)[length(sceltedistY)]=dist.custom.param$nome
    updateSelectInput(session,"distribuzione",choices=sceltedistY)
  }
#  })
  dist.normale   =function(n,my,parvet,par=NULL,x) rnorm(n,my,parvet)
  dist.poisson   =function(n,my,parvet,par=NULL,x) rpois(n,my)
  dist.binomiale =function(n,my,parvet,par,x) rbinom(n,prob=my/par[1],size=par[1])
  dist.normaleAR1=function(n,my,parvet,par,x) {
    if ((par[1]!=0) & (-1<par[1])&(par[1]<1)) {
      ret=my+arima.sim(list(ar=ifelse((-1<par[1])&(par[1]<1),par[1],0)),n,sd=par[2])
    } else {
      ret=my+rnorm(n,0,sd=par[2])
    }
    return(ret)
    }
  dist.gamma     =function(n,my,parvet,par,x) { rgamma(n,par[1],scale=my/par[1]) }
  dist.gammatrasl=function(n,my,parvet,par,x) { my+rgamma(n,par[1],scale=par[2])-par[1]*par[2] }
  dist.uniforme  =function(n,my,parvet,par,x) { my+runif(n,-par[1],+par[1])}
  dist.student   =function(n,my,parvet,par,x) { my+parvet*rt(n,df=par[1])}
  #custom.dist    =function(n,my,sderr,altroparam) {rnorm(n,my,sderr)}
  #custom.veravar=function(sderr) {FALSE}

  dist.normale.veravar=function(my,parvet,par,x){
    if (var(parvet)==0){
      veravar=mean(parvet)^2
    } else {
      veravar=parvet^2
    }
    return(veravar)
  }
  dist.poisson.veravar=function(my,parvet,par,x){
    if (var(my)==0){
      veravar=mean(my)
    } else {
      veravar=my
    }
    return(veravar)
  }
  dist.binomiale.veravar=function(my,parvet,par,x) par[1]*(my/par[1])*(1-my/par[1])
  dist.normaleAR1.veravar=function(my,parvet,par,x) par[2]/(1-par[1]^2)
  dist.gamma.veravar=function(my,parvet,par,x) my^2/par[1]
  dist.gammatrasl.veravar=function(my,parvet,par,x) par[1]*par[2]^2
  dist.uniforme.veravar=function(my,parvet,par,x) (2*par[1])^2/12
  dist.student.veravar=function(my,parvet,par,x){
    if (par[1]>2) {
      veravar=parvet^2*par[1]/(par[1]-2)
    } else {
      veravar=NA
    }
    return(veravar)
  }

  dist.normale.param=list(nome="Normale",enableVarFunPanel=TRUE,showVarFun=FALSE)
  dist.poisson.param=list(nome="Poisson",enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.binomiale.param=list(nome="Binomiale",nomepar1="size",minpar1=1,maxpar1=100,valuepar1=10,steppar1=1,enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.normaleAR1.param=list(nome="NormaleAR1",nomepar1="rho",minpar1=-0.95,maxpar1=0.95,valuepar1=0,steppar1=0.05,nomepar2="sd (innov)",minpar2=0.01,maxpar2=15,valuepar2=1,steppar2=0.1,enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.gamma.param=list(nome="Gamma",nomepar1="Forma",minpar1=0.01,maxpar1=15,valuepar1=3,steppar1=0.1,enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.gammatrasl.param=list(nome="Gamma trasl.",nomepar1="Forma",minpar1=0.01,maxpar1=15,steppar1=0.1,valuepar1=3,nomepar2="Scala",minpar2=0.01,maxpar2=15,steppar2=0.1,valuepar2=1,enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.uniforme.param=list(nome="Uniforme",nomepar1="c",minpar1=0.01,maxpar1=15,valuepar1=1,steppar1=0.1,enableVarFunPAnel=FALSE,showVarFun=FALSE)
  dist.student.param=list(nome="t di Student",nomepar1="Gradi di libertà",minpar1=1,maxpar1=30,valuepar1=10,steppar1=0.1,enableVarFunPanel=TRUE,showVarFun=FALSE)

 # sivarianza=reactiveVal()

  #sivarianza(TRUE)
  output$servePannelloVarianza=reactive({
    dist.param=get(paste(input$distribuzione,".param",sep=""))
    return(dist.param$enableVarFunPanel)
  })
  outputOptions(output, "servePannelloVarianza", suspendWhenHidden = FALSE)
  ###########################################################################################
  ## renderUI of parameters
  observeEvent(input$distribuzione,{
    req(input$distribuzione)
     dist.param=get(paste(input$distribuzione,".param",sep=""))

    if (input$distribuzione %in% c("dist.normale","dist.normaleAR1","dist.uniforme","dist.student","dist.custom")){
      output$beta1inser=renderUI({numericInput("beta1","beta1",min=-5,max=5,value=0,step=0.1)})
      output$beta2inser=renderUI({numericInput("beta2","beta2",min=-5,max=5,value=1,step=0.1)})
    }
    if (input$distribuzione %in% c("dist.poisson","dist.gamma","dist.gammatrasl")){
      output$beta1inser=renderUI({numericInput("beta1","beta1",min=0.01,max=5,value=1,step=0.1)})
      output$beta2inser=renderUI({numericInput("beta2","beta2",min=-input$beta1+0.01,max=5,value=1,step=0.1)})
    }
    if (input$distribuzione %in% c("dist.binomiale")){
      output$beta1inser=renderUI({sliderInput("beta1","beta1",min=0,max=input$par,value=input$par/2,step=0.1)})
      output$beta2inser=renderUI({sliderInput("beta2","beta2",min=-input$beta1,max=input$par-input$beta1,value=0,step=0.1)})
    }
   # if (input$distribuzione=="dist.custom"){
    if (!is.null(dist.param$minpar1)){
      updateSliderInput(session,"par",label=ifelse(is.null(dist.param$nomepar1),"par1",dist.param$nomepar1),
                        min=dist.param$minpar1,max=dist.param$maxpar1,
                        step=ifelse (is.null(dist.param$steppar1),(dist.param$maxpar1-dist.param$minpar1)/100,dist.param$steppar1),
                        value=ifelse(is.null(dist.param$valuepar1),dist.param$minpar1,dist.param$valuepar1))
      shinyjs::enable("par")
    } else {
      updateSliderInput(session,"par",label="-",value=1,min=0,max=2,step=1)
      shinyjs::disable("par")
    }
    if (!is.null(dist.param$minpar2)){
      updateSliderInput(session,"par2",label=ifelse(is.null(dist.param$nomepar2),"par2",dist.param$nomepar2),
                        min=dist.param$minpar2,max=dist.param$maxpar2,
                        step=ifelse (is.null(dist.param$steppar2),(dist.param$maxpar2-dist.param$minpar2)/100,dist.param$steppar2),
                        value=ifelse(is.null(dist.param$valuepar2),dist.param$minpar2,dist.param$valuepar2))
      shinyjs::enable("par2")
    } else {
      updateSliderInput(session,"par2",label="-",value=1,min=0,max=2,step=1)
      shinyjs::disable("par2")
    }
  })

  observeEvent(input$distribuzioneX,{
    if (input$distribuzioneX=="regulargrid"){
      updateSliderInput(session,"parx",label="-",value=1,min=0.01,max=15,step=0.1)
      shinyjs::disable("parx")
    }
    if (input$distribuzioneX=="uniform"){
      updateSliderInput(session,"parx",label="-",value=1,min=0.01,max=15,step=0.1)
      shinyjs::disable("parx")
    }
    if (input$distribuzioneX=="conc"){
      updateSliderInput(session,"parx",label="Dev. st.",value=1,min=0.01,max=15,step=0.1)
      shinyjs::enable("parx")
    }
    if (input$distribuzioneX=="asymm"){
      updateSliderInput(session,"parx",label="Grado asimm (1=max)",value=1,min=1,max=10,step=1)
      shinyjs::enable("parx")
    }

  })
  output$plotdist=renderPlot({
    par(mar=c(5,4,1,1))
    if (input$distribuzione=="dist.gamma" & (input$beta1>0) & (input$beta1+input$beta2>0)){
      limsupx=max(c(qgamma(0.99,input$par,scale=input$beta1),qgamma(0.99,input$par,scale=input$beta1+input$beta2)))
      curve(dgamma(x,input$par,scale=input$beta1),from=0,to=limsupx,lwd=2,col="darkblue",
            xlab="errre",ylab="densità",yaxt="n")
      curve(dgamma(x,input$par,scale=input$beta1+input$beta2),add=TRUE,lwd=2,col="lightblue")
    }
    if (input$distribuzione=="dist.gammatrasl" & (input$par>0) & (input$par2>0)){
      #xseq=0:qmamma(0.95,input$par,scale=input$par2)
            curve(dgamma(x,input$par,scale=input$par2),from=0,to=qgamma(0.99,input$par,scale=input$par2),xaxt="n",lwd=2,col="darkblue",
                  xlab="errre",ylab="densità",yaxt="n")
      axis(1,at=c(0,input$par*input$par2),label=signif(c(-input$par*input$par2,0),2))
    }
    if (input$distribuzione=="dist.poisson" & (input$beta1>0) & (input$beta1+input$beta2>0)){
      #xseq=0:qmamma(0.95,input$par,scale=input$par2)
      xseq=(min(c(qpois(0.01,input$beta1),qpois(0.01,input$beta1+input$beta2)))):(max(c(qpois(0.99,input$beta1),qpois(0.99,input$beta1+input$beta2))))
      y1=dpois(xseq,input$beta1)
      y2=dpois(xseq,input$beta1+input$beta2)
      plot(xseq,y1,type="n",ylim=range(c(y1,y2)),xlab="y",ylab="Dist. in x=0 (scura) e x=1 (chiara)")
      segments(xseq,0,xseq,y1,col="darkblue",lwd=2)
      segments(xseq+0.25,0,xseq+0.25,y2,col="lightblue",lwd=2)
    }
    if (input$distribuzione=="dist.binomiale"){
      #xseq=0:qmamma(0.95,input$par,scale=input$par2)
      prob.par=min(max(c(0,((input$beta1+input$beta2)/input$par))),1)
      prob1.par=min(max(c(0,((input$beta1)/input$par))),1)
      xseq=(min(c(qbinom(0.01,size=max(1,input$par),
                         prob=prob1.par),
                  qbinom(0.01,size=max(1,input$par),
                         prob=prob.par)))):
        (max(c(qbinom(0.99,size=max(1,input$par),
                      prob=prob1.par),
               qbinom(0.99,size=max(1,input$par),
                      prob=prob.par))))
      y1=dbinom(xseq,size=max(1,input$par),prob=prob1.par)
      y2=dbinom(xseq,size=max(1,input$par),prob=prob.par)
      plot(xseq,y1,type="n",ylim=range(c(y1,y2)),xlab="y",ylab="Dist. in x=0 (dark) e x=1 (light)")
      segments(xseq,0,xseq,y1,col="darkblue",lwd=2)
      segments(xseq+0.25,0,xseq+0.25,y2,col="lightblue",lwd=2)
    }
    dist.param=get(paste(input$distribuzione,".param",sep=""))
    if (dist.param$showVarFun){
      #xseq=0:qmamma(0.95,input$par,scale=input$par2)
      funzione.veravar=get(paste(input$distribuzione,".veravar",sep=""))
      if (is.null(funzione.veravar)) funzione.veravar=function(my,parvet,par,x) NA
      my=input$beta1+input$beta2*x()
      altroparam=c(input$par,input$par2)
      curve(funzione.veravar(my,sdfunz(),altroparam,x),from=0,to=1,col="darkblue",lwd=2,xlab="x",ylab=expression(V(epsilon[x])),las=1)
    }
  })

  distx.regulargrid=function(n,par) seq(0,1,length=n)
  distx.uniform=function(n,par) runif(n,0,1)
  distx.conc=function(n,par) par*rt(n,df=5)
  distx.asymm=function(n,par) rchisq(n,df=par)

  rv=reactiveValues()
  rv$veravar=c(0)
  x=reactive({
    input$rinnovaX
    distx=get(paste0("distx.",input$distribuzioneX))
    x=distx(input$n,input$parx)
    x=(x-min(x))/(max(x)-min(x))
    x=x*input$taglia
    x=sort(x)
   return(x)
    })
  ###########################################################################################
  ########
  #######   INPUT DELLA FUNZIONE DI VARIANZA

  observe({
    if (is.null(rv$xcoordvar)){
      rv$xcoordvar=c(0,1)
      rv$ycoordvar=c(1,1)
    rv$min=0
    rv$max=2
    }
  })

  observeEvent(input$plot_click$x,{
    #cat(rv$xcoordvar)
    if (is.null(rv$xcoordvar)){
      rv$xcoordvar=c(0,1)
      rv$ycoordvar=c(1,1)}
  temp=data.frame(x=rv$xcoordvar,y=rv$ycoordvar)
   if (input$plot_click$x<=0){
    if (any(temp$x==0)){
      temp$y[temp$x==0]=input$plot_click$y
    } else {
      temp=rbind(temp,c(0,input$plot_click$y))
    }
  }
  if (input$plot_click$x>=1){
    if (any(temp$x==1)){
      temp$y[temp$x==1]=input$plot_click$y
    } else {
      temp=rbind(temp,c(1,input$plot_click$y))
    }
  }
  if ((input$plot_click$x<1)&(input$plot_click$x>0)) {
    if ((input$plot_click$y>rv$min) & (input$plot_click$y<rv$max)){
      temp=rbind(temp,c(input$plot_click$x,input$plot_click$y))
    }
    if ((input$plot_click$y<rv$min)){
      rv$min=max(0,rv$min-0.1*(rv$max-rv$min))
    }
    if ((input$plot_click$y>rv$max)){
      rv$max=rv$max+0.1*(rv$max-rv$min)
    }
  }
      rv$xcoordvar=temp$x
      rv$ycoordvar=temp$y
  })


  observeEvent(input$plot_dblclick,{
    if (input$plot_dblclick$x>0 & input$plot_dblclick$x<1){
      if ((input$plot_dblclick$y>rv$min) & (input$plot_dblclick$y<rv$max)){
        temp=data.frame(x=rv$xcoordvar,y=rv$ycoordvar)
        temp=nearPoints(temp,input$plot_dblclick,maxpoints=1,allRows=TRUE,xvar="x",yvar="y")
        if (nrow(temp[temp$selected_,])>0){
          if ((temp$x[temp$selected_]!=0) & (temp$x[temp$selected_]!=1)){
            temp=temp[!temp$selected_,]
            rv$xcoordvar=temp$x
            rv$ycoordvar=temp$y
          }
        }
      }
      if ((input$plot_dblclick$y<rv$min)){
        rv$min=min(rv$ycoordvar)
      }
      if ((input$plot_dblclick$y>rv$max)){
        rv$max=max(rv$ycoordvar)
      }

    }
    if ((input$plot_dblclick$x>1)){
      rv$xcoordvar=c(0,1)
      rv$ycoordvar=c(1,1)*max(rv$ycoordvar[rv$xcoordvar==1])
    }
  })


  output$plotinputvarianza=renderPlot({
    par(mar=c(5,4.5,1,1))
    limy=range(c(rv$min,rv$max))
    limy1=c(limy[1]-0.04*(limy[2]-limy[1]),limy[2]+0.04*(limy[2]-limy[1]))
    if (input$inputvarosd) {
      espr=expression(g(x))
    } else {
      espr=expression(sqrt(g(x)))
    }
    plot(1,1,type="n",ylim=limy1,xlim=c(-0.01,1.01),yaxs="i",xlab="x",ylab=espr,las=1)

    axis(1,at=c(0,0.2,0.4,0.6,0.8,1))
    rect(-1.5,-1.5,0,limy1[2],col=gray(0.8),border=gray(0.8))
    rect(-1.5,-100,1.5,limy[1],col=gray(0.8),border=gray(0.8))
    rect(1,-1.5,1.5,limy1[2],col=gray(0.8),border=gray(0.8))
    rect(-1.5,limy[2],1.5,limy1[2],col=gray(0.8),border=gray(0.8))
    points(rv$xcoordvar,rv$ycoordvar,pch=20,col="darkblue",cex=2.2)
    if (length(rv$xcoordvar)>1){
      funzvarinterpolata=approxfun(rv$xcoordvar,rv$ycoordvar)
      curve(funzvarinterpolata(x),add=TRUE,lwd=2,col="darkblue")
    }
    rug(x())
    abline(v=rv$xcoordvar,lty=2,col=gray(0.7))
  })

  sdfunz=reactive({
    if (input$inputvarosd){
      sderr=sqrt(approx(rv$xcoordvar,rv$ycoordvar,xout=x())$y)
    } else {
      sderr=approx(rv$xcoordvar,rv$ycoordvar,xout=x())$y
    }
    return(sderr)
  })

  sdfunz1=reactive({
    if (input$inputvarosd){
      sderr=sqrt(approx(rv$xcoordvar,rv$ycoordvar,xout=1)$y)
    } else {
      sderr=approx(rv$xcoordvar,rv$ycoordvar,xout=1)$y
    }
    return(sderr)
  })

  ###########################################################################################
  ########
  #######   SIMULAZIONE
  simula=eventReactive(input$vai,{
    x=x()
    my=input$beta1+input$beta2*x
    altroparam=c(input$par,input$par2)
    beta1.v=rep(NA,input$m)
    beta2.v=rep(NA,input$m)
    t1.v=rep(NA,input$m)
    t2.v=rep(NA,input$m)
    sd1.v=rep(NA,input$m)
    sd2.v=rep(NA,input$m)
    sigma.v=rep(NA,input$m)
    prev.v=rep(NA,input$m)
    prevsd.v=rep(NA,input$m)
    y1.sim=rep(NA,input$m)
    shap.test.v=rep(NA,input$m)
    bptest.v=rep(NA,input$m)
#    withProgress(message = 'Making plot', value = 0, {
    for (i in 1:input$m){
#    incProgress(1/mm, detail = paste("Doing part", i))
      disty=get(input$distribuzione)
      funzione.veravar=get(paste(input$distribuzione,".veravar",sep=""))
      if (is.null(funzione.veravar)) funzione.veravar=function(my,parvet,par,x) NA
      rv$veravar=funzione.veravar(my,sdfunz(),altroparam,x)
      y=disty(input$n,my,sdfunz(),altroparam,x)
      fit=lm(y~x)
      fits=summary(fit)
      beta1.v[i]=fit$coef[1]
      beta2.v[i]=fit$coef[2]
      t1.v[i]=(fits$coefficients[1,1]-input$beta1)/fits$coefficients[1,2]
      t2.v[i]=(fits$coefficients[2,1]-input$beta2)/fits$coefficients[2,2]
      sd1.v[i]=fits$coefficients[1,2]
      sd2.v[i]=fits$coefficients[2,2]
      prtemp=predict(fit,newdata=data.frame(x=1),se.fit=TRUE)
      prev.v[i]=prtemp$fit
      prevsd.v[i]=sqrt(prtemp$se.fit^2+fits$sigma^2)
      sigma.v[i]=fits$sigma
      y1.sim[i]=disty(1,input$beta1+input$beta2,sdfunz1(),altroparam,x)
      bptest.v[i]=lmtest::bptest(fit)$p.value
      shap.test.v[i]=shapiro.test(fit$residuals)$p.value
    }#})
    return(list(beta1.v=beta1.v,beta2.v=beta2.v,sigma.v=sigma.v,t1.v=t1.v,t2.v=t2.v,sd1.v=sd1.v,sd2.v=sd2.v,prev.v=prev.v,prevsd.v=prevsd.v,y1.sim=y1.sim,bptest.v=bptest.v,shap.test.v=shap.test.v))
  })



  campioneperplotdiprova=reactive({
    req(input$beta1*input$beta2)
    input$rinnova
    x=x()
    my=input$beta1+input$beta2*x
    altroparam=c(input$par,input$par2)
    if (input$distribuzione=="dist.poisson") my=pmax(0,my)
    if (input$distribuzione=="dist.binomiale") my=pmin(input$par,pmax(0,my))
    if (input$distribuzione=="dist.gamma") my=pmax(0,my)
    disty=get(input$distribuzione)
    y=disty(input$n,my,sdfunz(),altroparam,x)
    if (all(!is.na(y))) fit=lm(y~x) else fit=NULL
    return(list(x=x,y=y,fit=fit))
  })

  output$plotdiprova=renderPlot({
    req(all(!is.na(campioneperplotdiprova()$y)))
    par(mar=c(5,4,0.5,0.5))
    plot(campioneperplotdiprova()$x,campioneperplotdiprova()$y,xlab="x",ylab="y")
    abline(coef(campioneperplotdiprova()$fit),col="darkgreen",lwd=2)
    abline(input$beta1,input$beta2,col="darkred",lwd=2)
  },height=300)
  output$plotdiprova2=renderPlot({
    req(all(!is.na(campioneperplotdiprova()$y)))
    par(mar=c(5,4,0.5,0.5))
    plot(campioneperplotdiprova()$x,campioneperplotdiprova()$fit$resid,xlab="x",ylab="Residuals")
    abline(h=0)
  },height=300)
  output$plotdiprova3=renderPlot({
    req(all(!is.na(campioneperplotdiprova()$y)))
    par(mar=c(5,4,0.5,0.5))
    qqnorm(campioneperplotdiprova()$fit$resid,main="")
    qqline(campioneperplotdiprova()$fit$resid)
      },height=300)


  plotEstimators=function(whichplot,sampledest,vero,xlab,statT=FALSE){
    if (!whichplot){
      par(mar=c(5,2,4,1))
      a=hist(sampledest,n=30)
      plot(a,yaxt="n",border="white",col="darkred",xlab=xlab,freq=FALSE,ylab="",
           main=expression(bold(paste("Sample distribution"))))
      points(vero,0,pch="|",col="green",cex=3)
      points(mean(sampledest),0,pch=17,col="black",cex=2.2)
      if (statT) curve(dt(x,df=input$n-2),lwd=2,col="darkgreen",add=TRUE)
    } else {
      par(mar=c(5,4,4,1))
      if (statT) {
        plot(qt(((1:length(simula()$t2.v))-0.5)/length(simula()$t2.v),input$n-2),sort(simula()$t2.v),
             xlab=expression(paste("Theoretical quantiles ",t[n-2])),ylab="Sample quantiles")
             abline(0,1)
      } else {
        qqnorm(sampledest,main="")
        qqline(sampledest)
      }
    }
  }

  grafcopertura=function(seqlc,copertura,tit="Coverage of c.i."){
    par(mar=c(5,4,4,1))
    plot(seqlc,copertura,pch=20,xlim=range(c(seqlc,copertura)),ylim=range(c(seqlc,copertura)),cex=1.8,las=1,
         xlab="Nominal coverage",ylab="Empirical coverage",
         main=tit)
    abline(h=c(0.8,0.85,0.9,0.95),lty=2,col=gray(0.8))
    abline(v=c(0.8,0.85,0.9,0.95),lty=2,col=gray(0.8))
    if (length(simula()$beta1.v)<5000){ text(mean(range(c(seqlc,copertura))),max(c(seqlc,copertura))-0.02,pos=1,labels=c("COVERAGE\nPROBABILITIES\nNON RELIABLE\nINCREASE \nNUMBER OF\nSIMULATIONS"),cex=1.8,col=gray(0.3)) }
    abline(0,1)
  }

  output$beta2plot=renderPlot({
    req(input$vai)
    plotEstimators(whichplot=input$showqqplot,sampledest=simula()$beta2.v,vero=input$beta2,xlab=expression(hat(beta)[2]))
  })
  output$beta2tstat=renderPlot({
    req(input$vai)
    plotEstimators(whichplot=input$showqqplot,sampledest=simula()$t2.v,vero=0,xlab=expression(t[2]),statT=TRUE)
  })
  output$coperturaintervallibeta2=renderPlot({
    seqlc=c(seq(0.8,0.99,by=0.01))
    copertura=rep(NA,length(seqlc))
    # browser()
    for (i in 1:length(seqlc)){
      copertura[i]=mean(abs(simula()$beta2.v-input$beta2)< -qt((1-seqlc[i])/2,input$n-2)*simula()$sd2.v)
    }
    grafcopertura(seqlc,copertura)
  })
  ##################################################################################################################
  output$beta1plot=renderPlot({
    req(input$vai)
    plotEstimators(whichplot=input$showqqplot,sampledest=simula()$beta1.v,vero=input$beta1,xlab=expression(hat(beta)[1]))
  })
  output$beta1tstat=renderPlot({
    req(input$vai)
    plotEstimators(whichplot=input$showqqplot,sampledest=simula()$t1.v,vero=0,xlab=expression(t[1]),statT=TRUE)
  })
  output$coperturaintervallibeta1=renderPlot({
    seqlc=c(seq(0.8,0.99,by=0.01))
    copertura=rep(NA,length(seqlc))
    # browser()
    for (i in 1:length(seqlc)){
      copertura[i]=mean(abs(simula()$beta1.v-input$beta1)< -qt((1-seqlc[i])/2,input$n-2)*simula()$sd1.v)
    }
    grafcopertura(seqlc,copertura)
  })
  ##################################################################################################################
  output$prevplot=renderPlot({
    req(input$vai)
    plotEstimators(whichplot=input$showqqplot,sampledest=simula()$prev.v,vero=input$beta1+input$beta2,xlab=expression(hat(y)[x==1]))
  })
  output$prevtstat=renderPlot({
    req(input$vai)
    tt=(simula()$prev.v-simula()$y1.sim)/simula()$prevsd.v
    plotEstimators(whichplot=input$showqqplot,sampledest=tt,vero=0,xlab=expression((hat(y)[1]-y(1))/sqrt(s^2+hat(V)(hat(y(1))))),statT=TRUE)
  })
  output$coperturaintervalliprev=renderPlot({
    seqlc=c(seq(0.8,0.99,by=0.01))
    copertura=rep(NA,length(seqlc))
    # browser()
    tt=(simula()$prev.v-simula()$y1.sim)/simula()$prevsd.v
    for (i in 1:length(seqlc)){
      copertura[i]=mean(abs(tt)< -qt((1-seqlc[i])/2,input$n-2))
    }
    grafcopertura(seqlc,copertura,tit="Pred.int. coverage")
  })
  ##################################################################################################################
  output$sigmaplot=renderPlot({
    req(input$vai)
    if (!input$showqqplot){
      par(mar=c(5,2,4,1))
      a=hist(simula()$sigma.v,n=30)
      plot(a,yaxt="n",border="white",col="darkred",xlab=expression(s),freq=FALSE,ylab="",
           main=expression(bold(paste("Sample distribution of ",s))))
      if (length(rv$veravar)==1) { if (!is.na(rv$veravar[1])){  points(sqrt(rv$veravar[1]),0,pch="|",col="green",cex=3) }}
      points(mean(simula()$sigma.v),0,pch=17,col="black",cex=2.2)
    } else {
      par(mar=c(5,4,4,1))
      plot(sqrt(qchisq(((1:length(simula()$sigma.v))-0.5)/length(simula()$prev.v),input$n-2)),sort(simula()$sigma.v),
           xlab="Theoretical quantiles (t)",ylab="Sample quantiles",
           main=expression(bold(paste("Quantiles of sample dist. of",s^2))))
    #  abline(0,1)
    }
  })
  output$sigma2plot=renderPlot({
    req(input$vai)
    if (!input$showqqplot){
      par(mar=c(5,2,4,1))
      a=hist(simula()$sigma.v^2,n=30)
      plot(a,yaxt="n",border="white",col="darkred",xlab=expression(s^2),freq=FALSE,ylab="",
           main=expression(bold(paste("Sample distribution of ",s^2))))
      if (length(rv$veravar)==1) { if (!is.na(rv$veravar[1])){  points(sqrt(rv$veravar[1]),0,pch="|",col="green",cex=3) }}
      points(mean(simula()$sigma.v^2),0,pch=17,col="black",cex=2.2)
    } else {
      par(mar=c(5,4,4,1))
      plot(qchisq(((1:length(simula()$sigma.v^2))-0.5)/length(simula()$prev.v),input$n-2),sort(simula()$sigma.v^2),
           xlab="Theoretical quantiles (t)",ylab="Sample quantiles",
           main=expression(bold(paste("Quantiles ",s^2,"-quantiles ",chi[n-2]^2))))
    #  abline(0,1)
    }
  })
  output$coperturaintervallisigma2=renderPlot({
   # plot(1,1,type="n",xaxt="n",yaxt="n",bty="n")
    if (length(rv$veravar)==1) { if (!is.na(rv$veravar[1])) {
      seqlc=c(seq(0.8,0.99,by=0.01))
      copertura=rep(NA,length(seqlc))
      qpiv=((input$n-2)*simula()$sigma.v^2/rv$veravar[1])
      for (i in 1:length(seqlc)){
        copertura[i]=mean( (qpiv>=qchisq((1-seqlc[i])/2,input$n-2))  & (qpiv <= qchisq(1-(1-seqlc[i])/2,input$n-2)))
      }
      grafcopertura(seqlc,copertura)

    }}
    if ((length(rv$veravar)>1) & (!is.na(rv$veravar[1]))){
      par(mar=c(5,4,4,1))
      plot(x(),rv$veravar,xlab="x",ylab="",las=1,type="l",
           main=expression(bold(paste(s^2," and ",V(epsilon[x])))),lwd=2,col="darkgreen")
      axis(2,at=c(mean(simula()$sigma.v^2)),line=1,label=expression(bar(s^2)),las=1)
      abline(h=mean(simula()$sigma.v^2),col="darkred",lwd=2)
      abline(h=mean(simula()$sigma.v^2)+c(-1,1)*sd(simula()$sigma.v^2),col="darkred",lwd=2,lty=2)

    }
  })

  output$tabellasim=renderTable({
    riassunti=function(x,v){
      c("true"=v,
        "mean"=mean(x),
        "median"=median(x,0.5),
        "bias"=mean(x)-v,
        "std dev"=sd(x),
        "q025"=as.numeric(quantile(x,0.025)),
        "q975"=as.numeric(quantile(x,0.975))
        )
    }
    if ((length(rv$veravar)==1) &  (!is.null(rv$veravar))) { veravarianza=rv$veravar } else {veravarianza=NA}
    a=data.frame(rbind(riassunti(simula()$beta1.v,input$beta1),riassunti(simula()$beta2.v,input$beta2),
                     riassunti(simula()$sigma.v,sqrt(veravarianza)),riassunti(simula()$sigma.v^2,veravarianza),
                     riassunti(simula()$prev.v,input$beta1+input$beta2)))
    rownames(a)=c("beta1","beta2","sigma","sigma2","y(1)")
    a
  },rownames=TRUE)

  output$shapiroplot=renderPlot({
    par(mar=c(5,1,1,1))
    hist(simula()$shap.test.v,border="white",col="darkblue",n=30,freq=FALSE,main="Shapiro-Wilks test",ylab="",yaxt="n",xlab="p-value")
  })
  output$bptestplot=renderPlot({
    par(mar=c(5,1,1,1))
    hist(simula()$bptest.v,border="white",col="darkblue",n=30,freq=FALSE,main="Breusch-Pagan test",ylab="",yaxt="n",xlab="p-value")
  })

  observeEvent(input$save,{
    cat(getwd())
    results=list(simulations=simula(),
                 truevar=rv$veravar,
                 beta1=input$beta1,
                 beta2=input$beta2,
                 truesd=sdfunz,
                 x=x(),
                 g=approxfun(rv$xcoordvar,rv$ycoordvar))
    .GlobalEnv$results.badLM=results
    #save(results,file=paste0(wd,"/results.Rdata"))
  })
}


shinyApp(ui = ui, server = server)

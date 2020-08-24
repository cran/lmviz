#library(shiny)
#library(shinyjs)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Simple linear regression"),
  #tabsetPanel(
  #   tabPanel("a",
  # Input functions
  sidebarLayout(
    sidebarPanel(
      sliderInput("n","Number of observations",min=8,max=1000,value=25,step=1),
      hr(),
      p(strong("Model: \\(y_i=\\beta_1+\\beta_2x_i+\\varepsilon_i\\), \\(\\varepsilon_i\\thicksim IID(\\mathcal N(0,\\sigma^2))\\)")),
      splitLayout(
        numericInput("beta0","\\(\\beta_1\\)",0,-10,10,0.1,width="100%"),
        numericInput("beta1","\\(\\beta_2\\)",1,-10,10,0.1,width="100%"),
        numericInput("sdeverr","\\(\\sigma\\)",1,0,10,0.1,width="100%")
      ),
      hr(),
      p(strong("Explanatory variable")),
      splitLayout(
        numericInput("sdevx","\\(\\sqrt{V(x)}\\)",0.5,0,1,0.1,width="100%"),
        numericInput("xbar","\\(\\bar{x}\\)",0,-3,3,0.1,width="100%")
      ),
      checkboxInput("mostra","show estimates on plot"),
      hr(),
      actionButton("aggiorna","Draw Y"),
      p("A new sample for Y is drawn based on the input parameters."),
      actionButton("aggiorna2","Draw Y repeatedly"),
      p("A new simulation will be drawn every 0.5/0.1 seconds"),
      hr(),
      p("Sample distributions and c.i. plots are reset if any parameter is changed."),
      hr(),
      actionButton("scarica","Download data"),
      p("Saves data in temp.csv in the working directory")
    ),
    mainPanel(
      fluidRow(
        column(width = 8,{
          verticalLayout(
            plotOutput("scatter")
          )
        }),
        column(width = 4,
               wellPanel(
                 plotOutput("stim")
               ))
      ),
      tabsetPanel(
        tabPanel("Residuals",
                 splitLayout(
                   plotOutput("residui1", width = "100%"),
                   plotOutput("residui2", width = "100%"),
                   plotOutput("residui3", width = "100%")
                 )
        ),
        tabPanel("Sample dist. coefs",
                 splitLayout(
                   plotOutput("beta0plot",  width = "100%"),
                   plotOutput("beta1plot",  width = "100%"),
                   plotOutput("beta0beta1", width = "100%")
                 ),
                 textOutput("numsim")
        ),
        tabPanel("Conf. int.",

                 splitLayout(
                   wellPanel(
                     numericInput("livconf","Confidence level",0.95,0.5,0.999,0.001,width="100%"),
                     p("Do not cover the true value"),
                     p(textOutput("beta0ictesto")),
                     p(textOutput("beta1ictesto"))
                   ),
                   plotOutput("beta0ic", width = "100%"),
                   plotOutput("beta1ic", width = "100%")
                 )
        ),
        tabPanel("Sample dist var",
                 splitLayout(
                   plotOutput("sigmaplot", width = "100%"),
                   plotOutput("sigma2plot", width = "100%")
                 ),
                 textOutput("numsim2")
        ),
        tabPanel("Piv. Q.",
                 splitLayout(
                   plotOutput("qp1plot", width = "100%"),
                   plotOutput("qp2plot", width = "100%")
                 ),
                 textOutput("numsim3")
        ),
        tabPanel("Prediction",
                 splitLayout(
                   wellPanel(
                     verticalLayout(
                      checkboxInput("previsioni","Show predictions on plot"),
                      sliderInput("livello","Level",0.5,0.99,0.90,0.01,width="100%"),
                      sliderInput("extra","extrapolation (%)",0,200,0,1,width="100%")
                     )),
                     plotOutput("previsioniv")
                 )
        )
      )
    )
    #   p("Prova a inventare un insieme di osservazioni che corrisponda al diagrama a scatola disegnato sopra."),
    #    textInput('vec1', 'Inserisci le osservazioni separate da , (usa il . per i decimali)', ""),
    # Ouptut functions
    #    actionButton("invio","Invia"),
    #    plotOutput("boxplot2", width = "66%")
  )
)
#    textOutput("qq1")),

nintf=function(x) {
  if (length(x)<50) 5
  if ((length(x)>=50) & (length(x)<500)) 20
  if ((length(x)>=500)) 30
}



server <- function(input, output, session) {

  # input$<id> available
  # data <- reactive ({}) #respond to every value in input, to be used as data()
  # output$<nomeoutput> <- render<TIPO>({ <codice> })
  rv=reactiveValues()
  rv$beta0.v=c()
  rv$beta1.v=c()
  rv$sigma.v=c()
  rv$sd.v=c()
  fit=reactive({fit=lm(y()~x())})
  summfit=reactive({summary(fit())})


  tempo=reactiveVal(30000000000000000)
  observeEvent(input$aggiorna2,{
    if ((input$aggiorna2 %% 3)==0){
      tempo(30000000000000000)
      updateActionButton(session, "aggiorna2",label="Draw Y repeatedly")
    } else {
      if ((input$aggiorna2 %% 3)==1){
        tempo(500)
        updateActionButton(session, "aggiorna2",label="Accelerate drawings")
      } else {
        tempo(50)
        updateActionButton(session, "aggiorna2",label="Stop automatic drawings")
      }
    }
  })
  observe({
    shinyjs::click("aggiorna")
    invalidateLater(tempo())
  })

  observeEvent(y(),{
    temp=rv$beta1.v
    temp=c(temp,fit()$coefficients[2])
    rv$beta1.v=temp
    temp=rv$beta0.v
    temp=c(temp,fit()$coefficients[1])
    rv$beta0.v=temp
    temp=rv$sigma.v
    temp=c(temp,sum(fit()$resid^2)/(input$n-2))
    rv$sigma.v=temp
  })
  observeEvent(input$n*input$beta0*input$beta1*input$sdeverr*input$sdevx*input$xbar,{
    rv$beta1.v=c()
    rv$beta0.v=c()
    rv$sigma.v=c()
  })
  observeEvent(input$scarica,{
    #dati=data.frame(x=x(),y=y())
    #.GlobalEnv$results.SimpleLM=dati
    write.table(dati,sep=",",file=paste0(.wd,"/temp.csv"))
  })
  output$numsim=renderText(
    paste("Sample distributions of estimators based on ",
          length(rv$beta1.v),
          "simulations; green: true values, gray: theorical distribution, triangle: mean of simulated estimates.")
  )
  output$numsim2=renderText(
    paste("Sample distributions of estimators based on ",
          length(rv$beta1.v),
          "simulations; green: true values, gray: theorical distribution.")
  )
  output$numsim3=renderText(
    paste("Sample distributions of pivotal quantities based on",
          length(rv$beta1.v),
          "simulations; green: theorical distribution.")
  )
  output$stim=renderPlot({
    req(is.numeric(input$beta1))
    par(mar=c(1,1,1,1),cex=1.5)
    plot(c(0,1),c(1.5,9),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
    text(0,9,adj=c(0,1),col="darkred",
         label=substitute(paste(hat(beta)[1],"=",b0),
                          list(b0=signif(fit()$coef[1],3),vb1=signif(summfit()$coefficients[1,2],3))))
    text(0,8,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(hat(v)(hat(beta)[1])),"=",vb1),
                          list(b0=signif(fit()$coef[1],3),vb1=signif(summfit()$coefficients[1,2],3))))
    text(0,7,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(v(hat(beta)[1])),"=",vb1v),
                          list(vb1v=signif(input$sdeverr*sqrt(1/input$n+input$xbar^2/Dx()),3))))
    text(0,6,adj=c(0,1),col="darkred",
         label=substitute(paste(hat(beta)[2],"=",b1),
                          list(b1=signif(fit()$coef[2],3))))
    text(0,5,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(hat(v)(hat(beta)[2])),"=",vb2),
                          list(b1=signif(fit()$coef[1],3),vb2=signif(summfit()$coefficients[2,2],3))))
    text(0,4,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(v(hat(beta)[2])),"=",vb1v),
                          list(vb1v=signif(input$sdeverr*sqrt(1/Dx()),3))))
    text(0,3,adj=c(0,1),col="darkred",
         label=substitute(paste(s^2,"=",s2," (s=",ss,")"),
                          list(s2=signif(summfit()$sigma^2,3),ss=signif(summfit()$sigma,3))))
    text(0,2,adj=c(0,1),col="darkred",
         label=substitute(paste(R^2,"=",r2),
                          list(r2=signif(summfit()$r.squared,3))))

  })

  x=eventReactive(input$n*input$sdevx*input$xbar,{
    req(is.numeric(input$xbar) & is.numeric(input$sdevx) & (input$sdevx>0))
    x=rnorm(input$n,0,1)
    x=(x-mean(x))/sd(x)
    x=input$xbar+input$sdevx*x
    return(x)
  })
  Dx=reactive({sum((x()-input$xbar)^2)})
  y=eventReactive(
    input$aggiorna*input$n*input$beta0*input$beta1*input$sdeverr*input$sdevx*input$xbar, {
      req(is.numeric(input$beta1) & is.numeric(input$beta0)& is.numeric(input$sdeverr) & (input$sdeverr>0))
      #autoInvalidate()
      y=input$beta0+input$beta1*x()+rnorm(input$n,0,input$sdeverr)
      return(y)
    },ignoreNULL = FALSE)

  output$scatter=renderPlot({
    par(mar=c(4,4,1,1))
    if (input$mostra){
      limx=c(min(c(0,1,x())),max(c(0,1.1,x())))
      limy=range(c(input$beta0+input$beta1*(input$xbar+c(-1,1)*4*input$sdevx)+c(-1,1)*3*input$sdeverr,input$beta0+c(-1,1)*qnorm(0.95)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx()),input$beta0+c(-1,1,-1,1)*qnorm(0.95)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())+input$beta1+c(-1,-1,1,1)*qnorm(0.95)*input$sdeverr*sqrt(1/Dx())))
    } else {
      limx=c(min(x()),max(x()))
      limy=range(input$beta0+input$beta1*(input$xbar+c(-1,1)*4*input$sdevx))+c(-1,1)*3*input$sdeverr
    }
    if (input$previsioni){
      updateCheckboxInput(session, "mostra",value=FALSE)
      limx=c(min(x())-(input$extra/200)*(max(x())-min(x())),max(x())+(input$extra/200)*(max(x())-min(x())))
      limy=range(c(input$beta0+input$beta1*limx[1]+c(-1,1)*qt(0.99,5)*input$sdeverr*sqrt(1+1/input$n+(limx[1]-input$xbar)^2/Dx()),
                   input$beta0+input$beta1*limx[2]+c(-1,1)*qt(0.99,5)*input$sdeverr*sqrt(1+1/input$n+(limx[2]-input$xbar)^2/Dx())
      ))
    }
    a=plot(x(),y(),xlim=limx,
           ylim=limy,las=1,xlab="x",ylab="y",las=1,pch=20,yaxt="n",bty="n")
    axis(1,at=c(limx[1]-1,limx[2]+1),labels=c("",""))
    axis(2,las=1,
         at=c(limy[1]-4,limy[2]+4),
         labels=c("",""))
    axis(2,las=1,
         at=c(limy,input$beta0),
         labels=signif(c(limy,input$beta0),3))
    rug(x())
    text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2],adj=c(0+(input$beta1<=0),1),col="darkred",
         label=substitute(paste(hat(y),"=",b0,segno,b1,"x"," (est. reg. line)"),
                          list(b0=signif(fit()$coef[1],3),b1=signif(fit()$coef[2],3),r2=signif(summfit()$r.squared,3),segno=ifelse(fit()$coef[2]>0,"+","")))
    )
    text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2]-0.1*(limy[2]-limy[1]),adj=c(0+(input$beta1<=0),1),col="darkgray",
         label=substitute(paste(E(y),"=",b0,segno,b1,"x"," (true reg. line)"),
                          list(b0=signif(input$beta0,3),b1=signif(input$beta1,3),segno=ifelse(input$beta1>0,"+","")))
    )
    #         expression(paste("Retta stimata:",hat(y),"=",signif(fit()$coef[1],3)," + ",signif(fit()$coef[2],3),"x")))
    curve(input$beta0+input$beta1*x,col=gray(0.5),lwd=2,add=TRUE)
    curve(fit()$coef[1]+fit()$coef[2]*x,col="darkred",lwd=2,add=TRUE)
    if (input$mostra){
      par(xpd=NA)
      text(0,fit()$coef[1],adj=c(fit()$coef[2]>=0,0),
           labels=expression(hat(beta)[1]),col="darkgreen",cex=1.5)
      text(1,mean(c(fit()$coef[1],fit()$coef[1]+fit()$coef[2])),pos=4,labels=expression(hat(beta)[2]),col="darkgreen",cex=1.5)
      par(xpd=FALSE)
      points(0,fit()$coef[1],pch=20,cex=1.5,col="darkgreen")
      abline(v=c(0,1),lty=2)
      segments(1,fit()$coef[1],1,fit()$coef[1]+fit()$coef[2],lwd=2,col="darkgreen")
      segments(0,fit()$coef[1],1,fit()$coef[1],lty=2)
    }
    if (input$previsioni){
      xseq=seq(limx[1],limx[2],length=100)
      polygon(c(xseq,rev(xseq)),c(fit()$coef[1]+fit()$coef[2]*xseq+qt((1-input$livello)/2,input$n-2)*summfit()$sigma*sqrt(1/input$n+(xseq-input$xbar)^2/Dx()),fit()$coef[1]+fit()$coef[2]*rev(xseq)-qt((1-input$livello)/2,input$n-2)*summfit()$sigma*sqrt(1/input$n+(rev(xseq)-input$xbar)^2/Dx())),border=NA,col="#8C000069")
      curve(fit()$coef[1]+fit()$coef[2]*x+qt((1-input$livello)/2,input$n-2)*summfit()$sigma*sqrt(1+1/input$n+(x-input$xbar)^2/Dx()),add=TRUE,lwd=2,col="darkred",lty=3)
      curve(fit()$coef[1]+fit()$coef[2]*x-qt((1-input$livello)/2,input$n-2)*summfit()$sigma*sqrt(1+1/input$n+(x-input$xbar)^2/Dx()),add=TRUE,lwd=2,col="darkred",lty=3)

    }
  })


  output$beta1plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
      a=hist(rv$beta1.v,n=nintf(rv$beta1.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[2]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta1,input$beta1,input$sdeverr*sqrt(1/Dx())))))
      points(input$beta1,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta1.v),0,pch=17,col="black",cex=2.2)
      curve(dnorm(x,input$beta1,input$sdeverr*sqrt(1/Dx())),add=TRUE,col=gray(0.7),lwd=2)
  })
  output$beta0plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
      a=hist(rv$beta0.v,n=nintf(rv$beta0.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[1]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta0,input$beta0,input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())))))
      points(input$beta0,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta0.v),0,pch=17,col="black",cex=2.2)
      curve(dnorm(x,input$beta0,input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())),add=TRUE,col=gray(0.7),lwd=2)
  })
  output$qp1plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
      qp=(rv$beta0.v-input$beta0)/(rv$sigma.v*sqrt(1/input$n+input$xbar^2/Dx()))
      a= hist(qp,n=nintf(qp))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(t[1]),freq=FALSE,
           xlim=range(c(qt(c(0.001,0.999),df=input$n-2),qp)),ylim=c(0,max(1.2*dt(0,df=input$n-2),0*a$density)))
      curve(dt(x,df=input$n-2),add=TRUE,col="darkgreen",lwd=2)
  })
  output$qp2plot=renderPlot({
    req(length(rv$beta0.v)>0)
      par(mar=c(5,1,1,1))
      qp=(rv$beta1.v-input$beta1)/(rv$sigma.v*sqrt(1/Dx()))
      a= hist(qp,n=nintf(qp))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(t[2]),freq=FALSE,
           xlim=range(c(qt(c(0.001,0.999),df=input$n-2),qp)),ylim=c(0,max(1.2*dt(0,df=input$n-2),0*a$density)))
      curve(dt(x,df=input$n-2),add=TRUE,col="darkgreen",lwd=2)
  })
  output$beta0beta1=renderPlot({
    req(length(rv$beta0.v)>0)
      par(mar=c(5,5,1,1))
      plot(rv$beta0.v,rv$beta1.v,pch=20,col="darkred",xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),las=1)
      points(input$beta0,input$beta1,pch=20,col="darkgreen",cex=2)
  })

  output$previsioniv=renderPlot({
    par(mar=c(5,4,0.5,0.5))
    limx=c(min(x()),max(x()))
    if (input$previsioni){
      limx=c(min(x())-(input$extra/200)*(max(x())-min(x())),max(x())+(input$extra/200)*(max(x())-min(x())))
    }
    matplot(matrix(rep(limx,length(rv$beta0.v)),nrow=2),
            rbind(rv$beta0.v+rv$beta1.v*limx[1],rv$beta0.v+rv$beta1.v*limx[2]),type="l",lty=1,col="#8C000069",
            xlab="x",ylab="y")
    abline(c(input$beta0,input$beta1),col=gray(0.25),lwd=2)

  })

  output$sigmaplot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
      a=hist(sqrt(rv$sigma.v),n=nintf(rv$sigma.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-2,input$n-2)*2*sqrt(input$sdeverr^2)*(input$n-2)/input$sdeverr^2)))
      points(input$sdeverr,0,pch=20,col="darkgreen",cex=3)
      curve(dchisq(x^2*(input$n-2)/input$sdeverr^2,input$n-2)*2*x*(input$n-2)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
  })
  output$sigma2plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
    a=hist(rv$sigma.v,n=nintf(rv$sigma.v))
    plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)^2),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-2,input$n-2)*(input$n-2)/input$sdeverr^2)))
    points(input$sdeverr^2,0,pch=20,col="darkgreen",cex=3)
    curve(dchisq(x*(input$n-2)/input$sdeverr^2,input$n-2)*(input$n-2)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
  })
  output$beta0ic=renderPlot({
    req(length(rv$beta0.v)>0)
      par(mar=c(5,1,1,1))
      plot(rv$beta0.v,1:length(rv$beta1.v),
           bty="n",
           yaxt="n",
           xaxt="n",
           xlim=c(input$beta0-2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx()),
                  input$beta0+2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())),
           ylim=c(0,max(100,length(rv$beta1.v))),
           xlab=expression(hat(beta)[1]))
      axis(1,at=signif(c(input$beta0-4*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx()),
                         input$beta0+c(-1,1,-0.5,0.5)*2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx()),
                         input$beta0,
                         input$beta0+4*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())),2))
      abline(v=input$beta0,col=gray(0.8),lwd=2)
      segments(rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()),
               1:length(rv$beta0.v),
               rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()),
               col=c("black","darkred")[1+
                                          ((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0)
                                        ])
  })
  output$beta0ictesto=renderText({
    req(length(rv$beta0.v)>0)
    paste(sum(((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0)),
          "of",
          length(rv$beta1.v),"(",
          round(100*sum(((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0))/length(rv$beta1.v),1),"%)")
  })

  output$beta1ic=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
      plot(rv$beta1.v,1:length(rv$beta1.v),
           bty="n",
           yaxt="n",
           xaxt="n",
           xlim=c(input$beta1-2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/Dx()),
                  input$beta1+2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/Dx())),
           ylim=c(0,max(100,length(rv$beta1.v))),
           xlab=expression(hat(beta)[2]))
      axis(1,at=signif(c(input$beta1-4*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/Dx()),
                         input$beta1+c(-1,1,-0.5,0.5)*2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/Dx()),
                         input$beta1,
                         input$beta1+4*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/Dx())),2))
      abline(v=input$beta1,col=gray(0.8),lwd=2)
      segments(rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()),
               1:length(rv$beta1.v),
               rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()),
               col=c("black","darkred")[1+
                                          ((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1)
                                        ])
  })
  output$beta1ictesto=renderText({
    req(length(rv$beta0.v)>0)
    paste(sum(((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1)),
          "of",
          length(rv$beta1.v),"(",
          round(100*sum(((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1))/length(rv$beta1.v),1),"%)")}
  )

  output$residui1=renderPlot({
    par(mar=c(5,4,1,1))
    plot(x(),fit()$resid,pch=20,las=1,
         xlab="x",ylab="residuals")
    abline(h=0)
  })
  output$residui2=renderPlot({
    par(mar=c(5,3,1,1))
    hist(fit()$resid,border="white",col=gray(0.6),main="",xlab="residuals")
  })
  output$residui3=renderPlot({
    par(mar=c(5,3,1,1))
    qqnorm(fit()$resid,pch=20,main="")
    qqline(fit()$resid,pch=20)
  })

}

shinyApp(ui = ui, server = server)

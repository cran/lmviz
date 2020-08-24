library(MASS)
library("scatterplot3d")
library(car)
library(rgl)
#library(shiny)
#library(shinyjs)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Multiple linear model"),
  #tabsetPanel(
  #   tabPanel("a",
  # Input functions
  sidebarLayout(
    sidebarPanel(
      sliderInput("n","Number of observations",min=8,max=1000,value=25,step=1),
      hr(),
      p(strong("Model: \\(y_i=\\beta_1+\\beta_2x_{i2}+\\beta_3x_{i3}+\\varepsilon_i\\), \\(\\varepsilon_i\\thicksim IID(\\mathcal N(0,\\sigma^2))\\)")),
      splitLayout(
        numericInput("beta1","\\(\\beta_2\\)",1,-10,10,0.1,width="100%"),
        numericInput("beta2","\\(\\beta_3\\)",1,-10,10,0.1,width="100%"),
        numericInput("sdeverr","\\(\\sigma\\)",1,0,10,0.1,width="100%")
      ),
      hr(),
      #splitLayout(
        sliderInput("corr","\\(\\mbox{corr}(x_2,x_3)\\)",min=-1,max=1,value=0,step=0.01,width="100%"),
      hr(),
      sliderInput("angle","Angle for 3d display",min=-90,max=90,value=40,step=1,width="100%"),
      #),
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
          tabsetPanel(
            tabPanel("3d",
              plotOutput("scatter")
            ),
            tabPanel("pairs",
                     plotOutput("pairs")
            ),
            tabPanel("3d2",
                     textOutput("testonoscatter"),
                     rglwidgetOutput("scatter3")
                     #plotOutput("scatter3")
            )
          )
        }),
        column(width = 4,
               wellPanel(
                 plotOutput("stim")
               ))
      ),
      h3("Model \\(y_i=\\beta_1+\\beta_2 x_{i2}+\\beta_3 x_{i3} + e_i,\\;\\;e_i\\thicksim\\mathcal N(0,\\sigma^2)\\)"),
      fluidRow(
        column(width = 4,
               plotOutput("beta1plot", width = "100%", height="300px")
        ),
        column(width = 4,
               plotOutput("beta2plot", width = "100%", height="300px")
        ),
        column(width = 4,
               plotOutput("sigma2plot", width = "100%", height="300px")
        )
      ),
      h3("Model \\(y_i=\\beta_1+\\beta_2 x_{i2} + e_i,\\;\\;e_i\\thicksim\\mathcal N(0,\\sigma^2)\\)"),
      fluidRow(
        column(width = 4,
               plotOutput("beta1plot1", width = "100%", height="300px")
        ),
        column(width = 4, offset=4,
               plotOutput("sigma2plot1", width = "100%", height="300px")
        )
      )#,
#      h3("Model \\(y_i=\\beta_1+\\beta_3 x_{i3} + e_i,\\;\\;e_i\\thicksim\\mathcal N(0,\\sigma^2)\\)"),
#      fluidRow(
#        column(width = 4,offset=4,
#               plotOutput("beta2plot2", width = "100%", height="300px")
#        ),
#        column(width = 4,
#               plotOutput("sigma2plot2", width = "100%", height="300px")
#        )
#      )
    )


    )
    #   p("Prova a inventare un insieme di osservazioni che corrisponda al diagramma a scatola disegnato sopra."),
    #    textInput('vec1', 'Inserisci le osservazioni separate da , (usa il . per i decimali)', ""),
    # Ouptut functions
    #    actionButton("invio","Invia"),
    #    plotOutput("boxplot2", width = "66%")

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
  rv$beta2.v=c()
  rv$sigma.v=c()
  rv$beta1.1.v=c()
  rv$beta2.2.v=c()
  rv$sigma.1.v=c()
  rv$sigma.2.v=c()
  rv$sd.v=c()
  rv$testres=rep(0,8)
  fit=reactive({fit=lm(y()~x()[,1]+x()[,2])})
  summfit=reactive({summary(fit())})
  fit1=reactive({fit1=lm(y()~x()[,1])})
  summfit1=reactive({summary(fit1())})
  fit2=reactive({fit2=lm(y()~x()[,2])})
  summfit2=reactive({summary(fit2())})


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
    temp=rv$beta2.2.v
    temp=c(temp,fit2()$coefficients[2])
    rv$beta2.2.v=temp
    temp=rv$beta1.1.v
    temp=c(temp,fit1()$coefficients[2])
    rv$beta1.1.v=temp

    temp=rv$beta2.v
    temp=c(temp,fit()$coefficients[3])
    rv$beta2.v=temp
    temp=rv$beta1.v
    temp=c(temp,fit()$coefficients[2])
    rv$beta1.v=temp
    temp=rv$beta0.v
    temp=c(temp,fit()$coefficients[1])
    rv$beta0.v=temp
    temp=rv$sigma.v
    temp=c(temp,sum(fit()$resid^2)/(input$n-3))
    rv$sigma.v=temp
    temp=rv$sigma.1.v
    temp=c(temp,sum(fit1()$resid^2)/(input$n-2))
    rv$sigma.1.v=temp
    temp=rv$sigma.2.v
    temp=c(temp,sum(fit2()$resid^2)/(input$n-2))
    rv$sigma.2.v=temp
    temp=rv$testres
    elemento=1+4*(pf(summfit()$fstatistic[1],summfit()$fstatistic[2],summfit()$fstatistic[3])>0.95)+
      2*(summfit()$coefficients[2,4]<0.05) +
      (summfit()$coefficients[3,4]<0.05)
    temp[elemento]=temp[elemento]+1
    rv$testres=temp

  })
  observeEvent(input$n*input$beta1*input$beta2*input$sdeverr*input$corr,{
    rv$beta2.v=c()
    rv$beta1.v=c()
    rv$beta0.v=c()
    rv$sigma.v=c()
    rv$beta1.1.v=c()
    rv$beta2.2.v=c()
    rv$sigma.1.v=c()
    rv$sigma.2.v=c()
    rv$testres=rep(0,8)
  })
  observeEvent(input$scarica,{
    dati=data.frame(x=x(),y=y())
    .GlobalEnv$results.Multicol=dati
    #write.table(data.frame(x=x(),y=y()),sep=",",file=paste0(.wd,"/temp.csv"))
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
    par(mar=c(5,4,0.5,0.5),cex=1)
    ci=confint(fit())
    a=ellipse(c(input$beta1,input$beta2),shape=invxtx()[2:3,2:3],
              radius=sqrt(2*qf(0.9999,2,input$n-3)*input$sdeverr^2))
    lim2=range(input$beta1+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,1])
    lim3=range(input$beta2+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,2])
    plot(fit()$coef[2],fit()$coef[3],xlim=lim2,ylim=lim3,xaxs="i",yaxs="i",
         xlab=expression(hat(beta)[2]),ylab=expression(hat(beta)[3]),las=1)
    confidenceEllipse(fit(),which.coef=c(2,3),add=TRUE,col="darkred")
    segments(ci[2,1],lim3[1],ci[2,2],lim3[1],lwd=5,col="darkred")
    segments(lim2[1],ci[3,1],lim2[1],ci[3,2],lwd=5,col="darkred")
    points(input$beta1,input$beta2,pch=20,cex=2,col="darkgreen")
    segments(input$beta1,input$beta2,input$beta1,lim3[1],col="darkgreen",lty=2)
    segments(input$beta1,input$beta2,lim2[1],input$beta2,col="darkgreen",lty=2)
  })



  x=eventReactive(input$n*input$corr,{
    req(is.numeric(input$corr))
    r=input$corr
    x=mvrnorm(n=input$n, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
    return(x)
  })
  invxtx=reactive({ solve(t(cbind(1,x())) %*% cbind(1,x())) })
  y=eventReactive(
    input$aggiorna*input$n*input$beta1*input$beta2*input$sdeverr*input$corr, {
      req(is.numeric(input$beta2) &is.numeric(input$beta1) &  is.numeric(input$sdeverr) & (input$sdeverr>0))
      #autoInvalidate()
      y=input$beta1*x()[,1]+input$beta2*x()[,2]+rnorm(input$n,0,input$sdeverr)
      return(y)
    },ignoreNULL = FALSE)

  output$pairs=renderPlot({
    #par(mfrow=c(2,2),mar=c(4,4,1,1))
    layout(matrix(c(1,4,5,7,2,6,8,9,3),ncol=3,byrow=TRUE))
    par(mar=c(0,0,0,0),oma=c(2,2,2,2))
    #    limy=input$beta0+c(-1,1)*3*abs(input$beta1)+c(-1,1)*3*input$sdeverr
    #    limx=c(min(0,input$xbar-4*input$sdevx),max(0,input$xbar+4*input$sdevx,ifelse(input$mostra,input$xbar+1.1,0)))
    a=hist(y(),col=gray(0.9),border="white",main="",xlab="",ylab="",yaxt="n")
    text(mean(range(a$breaks)),0,pos=3,offset=1,labels="Y",cex=8)
    a=hist(x()[,1],col=gray(0.9),border="white",main="",xlab="",ylab="",yaxt="n")
    text(mean(range(a$breaks)),0,pos=3,offset=1,labels=expression(x[2]),cex=8)
    a=hist(x()[,2],col=gray(0.9),border="white",main="",xlab="",ylab="",yaxt="n")
    text(mean(range(a$breaks)),0,pos=3,offset=1,labels=expression(x[3]),cex=8)
    plot(y()~x()[,1],main="",xlab="",ylab="",xaxt="n",yaxt="n")
    plot(y()~x()[,2],main="",xlab="",ylab="",xaxt="n",yaxt="n")
    axis(4,at=pretty(y()),las=1)
    plot(x()[,1]~x()[,2],main="",xlab="",ylab="",xaxt="n",yaxt="n")
    axis(4,at=pretty(x()[,1]),las=1)
    plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
    text(0.5,0.3,labels=signif(cor(y(),x()[,1]),3),cex=5)
    plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
    text(0.5,0.3,labels=signif(cor(y(),x()[,2]),3),cex=5)
    plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
    text(0.5,0.3,labels=signif(cor(x()[,1],x()[,2]),3),cex=5)

    # par(mar=c(1,1,1,1),cex=1.5)
  #  plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
  #  legend(1,1,xjust=1,yjust=1,lwd=2,col=c("darkred","darkgreen"),
  #         legend=c("multiple reg. estimate","univ. reg. estimate"))
  })

  output$scatter=renderPlot({
    #par(mar=c(4,4,1,1))
    #    limy=input$beta0+c(-1,1)*3*abs(input$beta1)+c(-1,1)*3*input$sdeverr
    #    limx=c(min(0,input$xbar-4*input$sdevx),max(0,input$xbar+4*input$sdevx,ifelse(input$mostra,input$xbar+1.1,0)))
    a=scatterplot3d(x()[,1],x()[,2],y(),
                   xlab=expression(x[2]),ylab=expression(x[3]),zlab=expression(y),
                   type="h",pch=20,
                   mar=c(2,0,1,0)+1,
                   angle=input$angle)
    a$plane3d(fit())

  })

  output$scatter2=renderPlot({
    #par(mar=c(4,4,1,1))
    #    limy=input$beta0+c(-1,1)*3*abs(input$beta1)+c(-1,1)*3*input$sdeverr
    #    limx=c(min(0,input$xbar-4*input$sdevx),max(0,input$xbar+4*input$sdevx,ifelse(input$mostra,input$xbar+1.1,0)))
    if ((input$aggiorna2 %% 3)==0){
      a=scatterplot3d(x()[,1],x()[,2],y(),
                      xlab=expression(x[2]),ylab=expression(x[3]),zlab=expression(y),
                      pch=20,
                      mar=c(2,0,1,0)+1,
                      angle=input$angle)
      a$plane3d(fit())
      orig     <- a$xyz.convert(x()[,1],x()[,2],y())
      plane    <- a$xyz.convert(x()[,1],x()[,2], fit()$fitted)
      segments(orig$x,orig$y,plane$x,plane$y)
      a$points3d(x()[,1],x()[,2],fit()$fitted,col="darkred",pch=20)
    } else {
      plot(1,1)
    }
  })

  output$scatter3=renderRglwidget({
    if ((input$aggiorna2 %% 3)==0){
      rgl.open(useNULL=T)
      #par(mar=c(4,4,1,1))
      #    limy=input$beta0+c(-1,1)*3*abs(input$beta1)+c(-1,1)*3*input$sdeverr
      #    limx=c(min(0,input$xbar-4*input$sdevx),max(0,input$xbar+4*input$sdevxifelse#(input$mostra,input$xbar+1.1,0)))
      #plot3d(x()[,1],x()[,2],y())
      scatter3d(x()[,1],y(),x()[,2])
      rglwidget()
    }
  })

  output$testonoscatter=renderText({
    if ((input$aggiorna2 %% 3)==0){
      testonoscatter=""
    } else {
      testonoscatter="3d rgl scatter not available during automatic simulations."
    }
  })
  output$beta1plot=renderPlot({
    if (length(rv$beta1.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$beta1.v,n=nintf(rv$beta1.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[2]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta1,input$beta1,input$sdeverr*sqrt(invxtx()[2,2])))),
           xlim=range(rv$beta1.v,rv$beta1.1.v))
      points(input$beta1,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta1.v),0,pch=17,col="black",cex=2.2)
#      curve(dnorm(x,input$beta1,input$sdeverr*sqrt(invxtx()[2,2])),add=TRUE,col=gray(0.7),lwd=2)
      curve(dnorm(x,mean(rv$beta1.v),sd(rv$beta1.v)),add=TRUE,col=gray(0.7),lwd=2)
      }
  })
  output$beta1plot1=renderPlot({
    if (length(rv$beta1.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$beta1.1.v,n=nintf(rv$beta1.1.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[2]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta1,input$beta1,input$sdeverr*sqrt(invxtx()[2,2])))),
           xlim=range(rv$beta1.v,rv$beta1.1.v))
      points(input$beta1,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta1.1.v),0,pch=17,col="black",cex=2.2)
#      curve(dnorm(x,input$beta1,input$sdeverr*sqrt(invxtx()[2,2])),add=TRUE,col=gray(0.7),lwd=2)
      curve(dnorm(x,mean(rv$beta1.1.v),sd(rv$beta1.1.v)),add=TRUE,col=gray(0.7),lwd=2)
    }
  })
  output$beta2plot=renderPlot({
    if (length(rv$beta2.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$beta2.v,n=nintf(rv$beta2.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[3]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta2,input$beta2,input$sdeverr*sqrt(invxtx()[3,3])))),
           xlim=range(rv$beta2.v,rv$beta2.2.v))
      points(input$beta2,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta2.v),0,pch=17,col="black",cex=2.2)
      #curve(dnorm(x,input$beta2,input$sdeverr*sqrt(invxtx()[3,3])),add=TRUE,col=gray(0.7),lwd=2)
      curve(dnorm(x,mean(rv$beta2.v),sd(rv$beta2.v)),add=TRUE,col=gray(0.7),lwd=2)
    }
  })
  output$beta2plot2=renderPlot({
    if (length(rv$beta2.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$beta2.2.v,n=nintf(rv$beta2.2.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[3]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta2,input$beta2,input$sdeverr*sqrt(invxtx()[3,3])))),
           xlim=range(rv$beta2.v,rv$beta2.2.v))
      points(input$beta2,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta2.2.v),0,pch=17,col="black",cex=2.2)
      curve(dnorm(x,input$beta2,input$sdeverr*sqrt(invxtx()[3,3])),add=TRUE,col=gray(0.7),lwd=2)
    }
  })
  output$beta1beta2=renderPlot({
    if (length(rv$beta0.v>1)){
      par(mar=c(5,5,1,1))
      plot(rv$beta1.v,rv$beta2.v,pch=20,col="darkred",xlab=expression(hat(beta)[2]),ylab=expression(hat(beta)[3]),las=1)
      points(input$beta1,input$beta2,pch=20,col="darkgreen",cex=2)
    }
  })
  output$sigmaplot=renderPlot({
    if (length(rv$sigma.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(sqrt(rv$sigma.v),n=nintf(rv$sigma.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-3,input$n-3)*2*sqrt(input$sdeverr^2)*(input$n-3)/input$sdeverr^2)))
      points(input$sdeverr,0,pch=20,col="darkgreen",cex=3)
      #curve(dchisq(x^2*(input$n-3)/input$sdeverr^2,input$n-3)*2*x*(input$n-3)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
      curve(dchisq(x^2*(input$n-3)/mean(rv$sigma.v),input$n-3)*2*x*(input$n-3)/mean(rv$sigma.v),col=gray(0.7),lwd=2,add=TRUE)
    }
  })
  output$sigma2plot=renderPlot({
    if (length(rv$sigma.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$sigma.v,n=nintf(rv$sigma.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)^2),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-2,input$n-2)*(input$n-2)/input$sdeverr^2)),
           xlim=range(rv$sigma.v,rv$sigma.1.v,rv$sigma.2.v))
      points(input$sdeverr^2,0,pch=20,col="darkgreen",cex=3)
      #curve(dchisq(x*(input$n-2)/input$sdeverr^2,input$n-2)*(input$n-2)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
      curve(dchisq(x*(input$n-2)/mean(rv$sigma.v),input$n-2)*(input$n-2)/mean(rv$sigma.v),col=gray(0.7),lwd=2,add=TRUE)
    }
  })
  output$sigma2plot1=renderPlot({
    if (length(rv$sigma.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$sigma.1.v,n=nintf(rv$sigma.1.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)^2),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-2,input$n-2)*(input$n-2)/input$sdeverr^2)),
           xlim=range(rv$sigma.v,rv$sigma.1.v,rv$sigma.2.v))
      points(input$sdeverr^2,0,pch=20,col="darkgreen",cex=3)
      #curve(dchisq(x*(input$n-2)/input$sdeverr^2,input$n-2)*(input$n-2)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
      curve(dchisq(x*(input$n-2)/mean(rv$sigma.1.v),input$n-2)*(input$n-2)/mean(rv$sigma.1.v),col=gray(0.7),lwd=2,add=TRUE)
    }
  })
  output$sigma2plot2=renderPlot({
    if (length(rv$sigma.v>1)){
      par(mar=c(5,1,1,1))
      a=hist(rv$sigma.2.v,n=nintf(rv$sigma.2.v))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(sigma)^2),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dchisq(input$n-2,input$n-2)*(input$n-2)/input$sdeverr^2)),
           xlim=range(rv$sigma.v,rv$sigma.1.v,rv$sigma.2.v))
      points(input$sdeverr^2,0,pch=20,col="darkgreen",cex=3)
      curve(dchisq(x*(input$n-2)/input$sdeverr^2,input$n-2)*(input$n-2)/input$sdeverr^2,col=gray(0.7),lwd=2,add=TRUE)
    }
  })

}

shinyApp(ui = ui, server = server)

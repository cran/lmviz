#library(shiny)
#library(shinyjs)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Prediction with the linear model"),
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
      splitLayout(
        sliderInput("livello","Level",0.5,0.99,0.90,0.01,width="100%"),
        numericInput("x0","value of x",0,-10,10,0.01)
      ),
      fluidRow(
        column(width = 6,{
            plotOutput("scatter")
        }),
        column(width = 6,
             plotOutput("previsioniic")
        )
      ),
      fluidRow(
        column(width = 6,{
            checkboxInput("mostraI","c.i. for \\(E(Y|x=x_0)\\)",value=TRUE)
        }),
        column(width = 6,
               p(textOutput("predEtesto"))
        )
      ),
      fluidRow(
        column(width = 6,{
          checkboxInput("mostraP","prediction interval for \\(Y|x=x_0\\)",value=TRUE)
        }),
        column(width = 6,
               p(textOutput("predYtesto"))
        )
      )
)))
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
  rv$pred=c()
  rv$predse=c()
  rv$newobs=c()

  fit=reactive({
    y=y()
    x=x()
    fit=lm(y~x)
  })
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




  observeEvent(y()*input$x0,{
    temp=rv$beta1.v
    temp=c(temp,fit()$coefficients[2])
    rv$beta1.v=temp
    temp=rv$beta0.v
    temp=c(temp,fit()$coefficients[1])
    rv$beta0.v=temp
    temp=rv$sigma.v
    temp=c(temp,sum(fit()$resid^2)/(input$n-2))
    rv$sigma.v=temp
    t0=predict(fit(),newdata=data.frame(x=input$x0),se.fit=TRUE)

    temp=rv$pred
    temp=c(temp,t0$fit)
    rv$pred=temp
    temp=rv$predse
    temp=c(temp,t0$se.fit)
    rv$predse=temp
    temp=rv$newobs
    temp=c(temp,newobs())
    rv$newobs=temp
  })

  observeEvent(input$n*input$beta0*input$beta1*input$sdeverr*input$sdevx*input$xbar*input$x0,{
    rv$beta1.v=c()
    rv$beta0.v=c()
    rv$sigma.v=c()
    rv$pred=c()
    rv$predse=c()
    rv$newobs=c()

  })
    observeEvent(input$scarica,{
    #dati=data.frame(x=x(),y=y())
    #.GlobalEnv$results.SimpleLM=dati
    write.table(dati,sep=",",file=paste0(.wd,"/temp.csv"))
  })


  output$scatter=renderPlot({
    limx=range(c(x(),input$x0))
    xseq=seq(limx[1],limx[2],length=100)
    prev.i=predict(fit(),newdata=data.frame(x=xseq),interval="confidence",level=input$livello)
    prevY.i=predict(fit(),newdata=data.frame(x=xseq),interval="prediction",level=input$livello)

    par(mar=c(4,4,1,1))
    limy=range(c(prevY.i))

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
    text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2]-0.07*(limy[2]-limy[1]),adj=c(0+(input$beta1<=0),1),col="darkgray",
         label=substitute(paste(E(y),"=",b0,segno,b1,"x"," (true reg. line)"),
                          list(b0=signif(input$beta0,3),b1=signif(input$beta1,3),segno=ifelse(input$beta1>0,"+","")))
    )
    if (input$mostraP){
      text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2]-0.14*(limy[2]-limy[1]),adj=c(0+(input$beta1<=0),1),col="blue",
           label="x new obs (not used in estimation)"
        )
      }
    #         expression(paste("Retta stimata:",hat(y),"=",signif(fit()$coef[1],3)," + ",signif(fit()$coef[2],3),"x")))
    curve(input$beta0+input$beta1*x,col=gray(0.5),lwd=2,add=TRUE)
    curve(fit()$coef[1]+fit()$coef[2]*x,col="darkred",lwd=2,add=TRUE)


    if (input$mostraI) {
      prev.x0=predict(fit(),newdata=data.frame(x=input$x0),interval="confidence",level=input$livello)
      segments(input$x0,prev.x0[2],input$x0,prev.x0[3],lwd=3)
      lines(xseq,prev.i[,2],lwd=2,col="darkred",lty=3)
      lines(xseq,prev.i[,3],lwd=2,col="darkred",lty=3)
    }
    if (input$mostraP){
      polygon(c(xseq,rev(xseq)),c(prevY.i[,2],rev(prevY.i[,3])),border=NA,col="#8C000069")

      prevY.x0=predict(fit(),newdata=data.frame(x=input$x0),interval="prediction",level=input$livello)
      segments(input$x0,prevY.x0[2],input$x0,prevY.x0[3],lwd=1,lty=3)
      points(input$x0,newobs(),pch=4,cex=1.3,col="blue")
    }
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
  newobs=eventReactive(y()*input$x0,{
    newobs=input$beta0+input$beta1*input$x0+rnorm(1,0,input$sdeverr)
  })

  output$previsioniv=renderPlot({
    req(length(rv$pred)>=1)
    par(mar=c(4,4,1,1))
    limx=range(c(x(),input$x0))
    matplot(matrix(rep(limx,length(rv$beta0.v)),nrow=2),
            rbind(rv$beta0.v+rv$beta1.v*limx[1],rv$beta0.v+rv$beta1.v*limx[2]),type="l",lty=1,col="#8C000069",
            xlab="x",ylab="y")
    abline(c(input$beta0,input$beta1),col=gray(0.25),lwd=2)

  })


  output$previsioniic=renderPlot({
    req(length(rv$pred)>=1)
    EYx0true=input$beta0+input$beta1*input$x0
    predYlow=rv$pred+qt((1-input$livello)/2,input$n-2)*sqrt(rv$sigma.v^2+rv$predse^2)
    predYhig=rv$pred-qt((1-input$livello)/2,input$n-2)*sqrt(rv$sigma.v^2+rv$predse^2)
    predElow=rv$pred+qt((1-input$livello)/2,input$n-2)*rv$predse
    predEhig=rv$pred-qt((1-input$livello)/2,input$n-2)*rv$predse
    par(mar=c(4,.5,1,3))
      plot(rev(1:length(rv$pred)),rv$newobs,
           bty="n",
           yaxt="n",
           xaxt="n",
           ylim=range(c(predYlow,predYhig,newobs(),input$beta0+input$beta1*input$x0)),
           xlim=c(0,max(50,length(rv$pred))),
           xlab="", #expression(hat(beta)[2]),
           pch=4,cex=1.3,col="blue",type="n")



      axis(4,at=signif(c(input$x0-4*qnorm(1-(1-input$livello)/5)*input$sdeverr*sqrt(1/Dx()),
                         input$x0+c(-1,1,-0.5,0.5)*2*qnorm(1-(1-input$livello)/5)*input$sdeverr*sqrt(1/Dx()),
                         input$x0,
                         input$x0+4*qnorm(1-(1-input$livello)/5)*input$sdeverr*sqrt(1/Dx())),2),las=1)
      abline(h=EYx0true,col=gray(0.8),lwd=2)
      if (input$mostraP) {
        points(rev(1:length(rv$pred)),rv$newobs,pch=4,cex=1.3,col="blue")
        segments(rev(1:length(rv$pred)),
                 predYlow,
                 rev(1:length(rv$pred)),
                 predYhig,
                 col=c("black","darkred")[1+(predYlow>rv$newobs)+(predYhig<rv$newobs)],
                 lwd=c(rep(1,length(rv$pred)-1),1),
                 lty=3)
      }
      if (input$mostraI) {
        segments(rev(1:length(rv$pred)),
                 predElow,
                 rev(1:length(rv$pred)),
                 predEhig,
                 col=c("black","darkred")[1+(predElow>EYx0true)+(predEhig<EYx0true)],
                 lwd=c(rep(1,length(rv$pred)-1),2))
      }
  })

  output$predYtesto=renderText({
    req(length(rv$pred)>=1)
    if (input$mostraP){
      predYlow=rv$pred+qt((1-input$livello)/2,input$n-2)*sqrt(rv$sigma.v^2+rv$predse^2)
      predYhig=rv$pred-qt((1-input$livello)/2,input$n-2)*sqrt(rv$sigma.v^2+rv$predse^2)
      covered=sum((predYlow<=rv$newobs)&(predYhig>=rv$newobs))
      total=length(rv$pred)
      perc=signif(100*covered/total,3)
      ris=paste(perc,"% of prediction intervals cover the value of the new observation (",covered," of ",total,")")
    } else {
      ris="                                                                                        "
    }
    ris
  })

  output$predEtesto=renderText({
    req(length(rv$pred)>=1)
    if (input$mostraI){
      predElow=rv$pred+qt((1-input$livello)/2,input$n-2)*rv$predse
      predEhig=rv$pred-qt((1-input$livello)/2,input$n-2)*rv$predse
      trueEYx0=input$beta0+input$beta1*input$x0
      covered=sum((predElow<=trueEYx0)&(predEhig>=trueEYx0))
      total=length(rv$pred)
      perc=signif(100*covered/total,3)
      ris=paste(perc,"% of confidence intervals cover the true value E(Y|x0) (",covered," of ",total,")")
    } else {
      ris="                                                                                        "
    }
    ris
  })



}

shinyApp(ui = ui, server = server)

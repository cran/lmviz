#library(shiny)
#library(shinyjs)
library(car) ## per ellipse()
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Sample distributions for the estimators of the coefficients"),
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
        numericInput("sdevx","\\(\\sqrt{V(x)}\\)",1,0,2,0.1,width="100%"),
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
      p("Saves data in temp.csv in the working directory"),
    width=2),
    mainPanel(
      #numericInput("livconf","Confidence level",0.95,0.5,0.999,0.001,width="100%"),
      fluidRow(
        column(width = 8,{
          verticalLayout(
            plotOutput("scatter",height="300px")
          )
        }),
        column(width = 4,
                 plotOutput("stim", width = "100%", height="300px")
               )
      ),
      fluidRow(
        column(width = 4,{
          verticalLayout(
            #plotOutput("beta0ic", width = "100%", height="300px"),
            plotOutput("beta0plot", width = "100%", height="300px"),
            #p("Do not cover the true value")
            tableOutput("beta0testo")
          )
        }),
        column(width = 4,{
          verticalLayout(
            #plotOutput("beta1ic", width = "100%", height="300px"),
            plotOutput("beta1plot", width = "100%", height="300px"),
            #p("Do not cover the true value")
            tableOutput("beta1testo")
          )
        }),
        column(width = 4,{
          verticalLayout(
            #plotOutput("beta0beta1ic", width = "100%", height="300px"),
            plotOutput("beta0beta1", width = "100%", height="300px"),
            #p("Do not cover the true value")
            tableOutput("beta01testo")
          )
        })
      ),
      width=10)
  )
)

nintf=function(x) {
  if (length(x)<50) 5
  if ((length(x)>=50) & (length(x)<500)) 20
  if ((length(x)>=500)) 30
}



server <- function(input, output, session) {
  livconf=0.95
  # input$<id> available
  # data <- reactive ({}) #respond to every value in input, to be used as data()
  # output$<nomeoutput> <- render<TIPO>({ <codice> })
  rv=reactiveValues()
  rv$beta0.v=c()
  rv$beta1.v=c()
  rv$sigma.v=c()
  rv$sd.v=c()
  rv$ctrlellipse=c()
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
    rv$sd.v=c()
    rv$ctrlellipse=c()
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
  xtx=reactive({  t(cbind(1,x())) %*% cbind(1,x()) })
  invxtx=reactive({ solve(xtx()) })
  output$stim=renderPlot({
    par(mar=c(5,4,0.5,0.5),cex=1)
    ci=confint(fit(),level=livconf)
    a=ellipse(c(input$beta0,input$beta1),shape=invxtx()[1:2,1:2],
              radius=sqrt(2*qf(0.9999,2,input$n-3)*input$sdeverr^2))
    lim2=range(input$beta0+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,1])
    lim3=range(input$beta1+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,2])
    plot(fit()$coef[1],fit()$coef[2],xlim=lim2,ylim=lim3,xaxs="i",yaxs="i",
         xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),las=1)
    confidenceEllipse(fit(),which.coef=c(1,2),add=TRUE,col="darkred",level=livconf)
    segments(ci[1,1],lim3[1],ci[1,2],lim3[1],lwd=5,col="darkred")
    segments(lim2[1],ci[2,1],lim2[1],ci[2,2],lwd=5,col="darkred")
    points(input$beta0,input$beta1,pch=20,cex=2,col="darkgreen")
    segments(input$beta0,input$beta1,input$beta0,lim3[1],col="darkgreen",lty=2)
    segments(input$beta0,input$beta1,lim2[1],input$beta1,col="darkgreen",lty=2)
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

  })


  output$beta1plot=renderPlot({
    req(length(rv$beta1.v)>0)
    par(mar=c(5,1,1,1))
      a=hist(rv$beta1.v,n=nintf(rv$beta1.v))
      xlim=c(input$beta1-2*qnorm(1-(1-livconf)/5)*input$sdeverr*sqrt(1/(input$n*input$sdevx^2)),
             input$beta1+2*qnorm(1-(1-livconf)/5)*input$sdeverr*sqrt(1/(input$n*input$sdevx^2)))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[2]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta1,input$beta1,input$sdeverr*sqrt(1/Dx())))),
           xlim=xlim)
      points(input$beta1,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta1.v),0,pch=17,col="black",cex=2.2)
      curve(dnorm(x,input$beta1,input$sdeverr*sqrt(1/Dx())),add=TRUE,col=gray(0.7),lwd=2)
  })
  output$beta0plot=renderPlot({
    req(length(rv$beta1.v)>0)
    par(mar=c(5,1,1,1))
      a=hist(rv$beta0.v,n=nintf(rv$beta0.v))
      xlim=c(input$beta0-2*qnorm(1-(1-livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(input$xbar)^2/(input$n*input$sdevx^2)),
             input$beta0+2*qnorm(1-(1-livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(input$xbar)^2/(input$n*input$sdevx^2)))
      plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(hat(beta)[1]),freq=FALSE,
           ylim=c(0,max(0*a$density,1.2*dnorm(input$beta0,input$beta0,input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())))),xlim=xlim)
      points(input$beta0,0,pch=20,col="darkgreen",cex=3)
      points(mean(rv$beta0.v),0,pch=17,col="black",cex=2.2)
      curve(dnorm(x,input$beta0,input$sdeverr*sqrt(1/input$n+mean(x())^2/Dx())),add=TRUE,col=gray(0.7),lwd=2)
  })
  output$beta0beta1=renderPlot({
    req(length(rv$beta1.v)>0)
    par(mar=c(5,5,1,1))
      plot(rv$beta0.v,rv$beta1.v,pch=20,col="darkred",xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),las=1)
      points(input$beta0,input$beta1,pch=20,col="darkgreen",cex=2)
  })

  output$beta0testo=renderTable({
    req(length(rv$beta0.v)>3)
    dd=data.frame(summaries=c(length(rv$beta0.v),min(rv$beta0.v),quantile(rv$beta0.v,c(0.025,0.5)),mean(rv$beta0.v),quantile(rv$beta0.v,c(0.975)),max(rv$beta0.v),sd(rv$beta0.v)))
    rownames(dd)=c("n.sim","min","2.5% quant.","median","mean","97.5% quant.","max","sd")
    dd
  },rownames=TRUE,colnames=FALSE)

  output$beta1testo=renderTable({
    req(length(rv$beta1.v)>3)
    dd=data.frame(summaries=c(length(rv$beta1.v),min(rv$beta1.v),quantile(rv$beta1.v,c(0.025,0.5)),mean(rv$beta1.v),quantile(rv$beta1.v,c(0.975)),max(rv$beta1.v),sd(rv$beta1.v)))
    rownames(dd)=c("n.sim","min","2.5% quant.","median","mean","97.5% quant.","max","sd")
    dd
  },rownames=TRUE,colnames=FALSE)

  output$beta01testo=renderTable({
    req(length(rv$beta1.v)>3)
    dd=data.frame(summaries=c(length(rv$beta1.v),cor(rv$beta0.v,rv$beta1.v)))
    rownames(dd)=c("n.sim","corr")
    dd
  },rownames=TRUE,colnames=FALSE)


}

shinyApp(ui = ui, server = server)

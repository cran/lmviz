#library(shiny)
#library(shinyjs)
library(car)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Confidence intervals for the coefficients"),
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
      p("Saves data in temp.csv in the working directory"),
    width=2),
    mainPanel(
      numericInput("livconf","Confidence level",0.95,0.5,0.999,0.001,width="100%"),
      fluidRow(
        column(width = 8,{
          verticalLayout(
            p("Below you see a realization of the linear model with the parameters you specified on the left panel.
          The red line is the least squares (maximum likelihood) estimate."),
            plotOutput("scatter",height="300px")
          )
        }),
        column(width = 4,
               p("The plot below represents the confidence intervals for \\(\\beta_1\\) and \\(\\beta_2\\) and the confidence region for the pair \\((\\beta_1,\\beta_2)\\)."),
                 plotOutput("stim", width = "100%", height="300px")
               ),


       # column(width = 4,{
       #   verticalLayout(
       #     plotOutput("beta0ic", width = "100%", height="300px"),
       #     plotOutput("qp1plot", width = "100%", height="300px"),
       #     p(textOutput("beta0ictesto"))
       #   )
       # }),
        column(width = 6,{
          verticalLayout(
            p(textOutput("testolivconf")),
            p(textOutput("testo2")),
            plotOutput("beta1ic", width = "100%", height="300px"),
            p(textOutput("beta1ictesto")),
            p("The above c.i. are based on the fact that \\(T_2=\\frac{\\hat\\beta_2-\\beta_2}{\\hat{V}(\\hat{\\beta_2})}\\thicksim t_{n-2}\\) and so \\(P(t_{n-2,1-\\alpha/2}\\leq T_2 \\leq t_{n-2,1-\\alpha/2})=1-\\alpha\\)."),
            p("in other words, the c.i. includes those values that would be accepted as null hypotheses in a t-test on \\(\\beta_2\\), this means that those c.i. that do not cover the true value correspond to samples where a null hypotheses on \\(\\beta_2\\) would reject the true value. Below we depict the test statistics for that hypothesis."),
            plotOutput("qp2plot", width = "100%", height="300px")
          )
        }),
        column(width = 6,{
          verticalLayout(
            p(textOutput("testolivconf2")),
            p(textOutput("testo22")),
            plotOutput("beta0beta1ic", width = "100%", height="300px"),
            p(textOutput("beta01ictesto")),
            p("The above c.r. are based on the fact that \\(F(\\beta)=\\frac{(\\hat\\beta-\\beta)^TX^TX(\\hat\\beta-\\beta)/(2n)}{\\hat\\sigma^2/(n-2)} \\thicksim F_{2,n-2} \\)  and so \\(P(F(\\beta)\\leq F_{2,n-2,1-\\alpha})=1-\\alpha\\)."),
            p("in other words, the c.r. includes those values that would be accepted as null hypotheses in a F-test on \\(\\beta_1,\\beta_2\\), this means that those c.r. that do not cover the true value correspond to samples where a null hypotheses on \\(\\beta_1,\\beta_2\\) would reject the true value. Below we depict the test statistics for that hypothesis."),            plotOutput("Fplot", width = "100%", height="300px")

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

  # input$<id> available
  # data <- reactive ({}) #respond to every value in input, to be used as data()
  # output$<nomeoutput> <- render<TIPO>({ <codice> })
  rv=reactiveValues()
  rv$beta0.v=c()
  rv$beta1.v=c()
  rv$sigma.v=c()
  rv$sd.v=c()
  rv$ctrlellipse=c()
  rv$qp=c()
  rv$qpx=c()
  rv$ftest=c()
  rv$ftestx=c()
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
    stimasigma=sum(fit()$resid^2)/(input$n-2)
    temp=c(temp,stimasigma)
    rv$sigma.v=temp
    temp=rv$qp
    temp=c(temp,(fit()$coefficients[2]-input$beta1)/(sum(fit()$resid^2)/(input$n-2)*sqrt(1/Dx())))
    rv$qp=temp
    temp=rv$qpx
    temp=c(temp,runif(1))
    rv$qpx=temp
    temp=rv$ftest
    fit1=lm(I(y()-input$beta0-input$beta1*x())~x())
    fit0=lm(I(y()-input$beta0-input$beta1*x())~ -1)
    aa=anova(fit0,fit1)$F[2]

    vec=fit()$coefficients-c(input$beta0,input$beta1)
    controllo=((vec %*% xtx() %*% vec)/(2*input$n))/(stimasigma/(input$n-2))


    temp=c(temp,controllo)
    rv$ftest=temp
    temp=rv$ftestx
    temp=c(temp,runif(1))
    rv$ftestx=temp
  })
  observeEvent(input$n*input$beta0*input$beta1*input$sdeverr*input$sdevx*input$xbar,{
    rv$beta1.v=c()
    rv$beta0.v=c()
    rv$sigma.v=c()
    rv$sd.v=c()
    rv$ctrlellipse=c()
    rv$qp=c()
    rv$qpx=c()
    rv$ftest=c()
    rv$ftestx=c()
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
    ci=confint(fit(),level=input$livconf)
    a=ellipse(c(input$beta0,input$beta1),shape=invxtx()[1:2,1:2],
              radius=sqrt(2*qf(0.9999,2,input$n-3)*input$sdeverr^2))
    lim2=range(input$beta0+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,1])
    lim3=range(input$beta1+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,2])
    plot(fit()$coef[1],fit()$coef[2],xlim=lim2,ylim=lim3,xaxs="i",yaxs="i",
         xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),las=1)
    confidenceEllipse(fit(),which.coef=c(1,2),add=TRUE,col="darkred",level=input$livconf)
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




  output$beta0ic=renderPlot({
    req(length(rv$beta1.v)>0)
    par(mar=c(5,1,1,1))
      plot(rv$beta0.v,1:length(rv$beta1.v),
           bty="n",
           yaxt="n",
           xaxt="n",
           xlim=c(input$beta0-2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(input$xbar)^2/(input$n*input$sdevx^2)),
                  input$beta0+2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/input$n+mean(input$xbar)^2/(input$n*input$sdevx^2))),
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
    req(length(rv$beta1.v)>0)
    paste0(sum(((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0)),
          " out of ",
          length(rv$beta1.v)," intervals do not include the true value (",
          round(100*sum(((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0))/length(rv$beta1.v),1),"%), their average length is approximately",signif(mean(2*qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx())),3))
  })

  output$beta1ic=renderPlot({
    req(length(rv$beta1.v)>0)
    par(mar=c(5,1,1,1))
      plot(rv$beta1.v,1:length(rv$beta1.v),
           bty="n",
           yaxt="n",
           xaxt="n",
           xlim=c(input$beta1-2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/(input$n*input$sdevx^2)),
                  input$beta1+2*qnorm(1-(1-input$livconf)/5)*input$sdeverr*sqrt(1/(input$n*input$sdevx^2))),
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
    req(length(rv$beta1.v)>0)
    paste(sum(((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1)),
          " out of ",
          length(rv$beta1.v)," intervals do not include the true value (",
          round(100*sum(((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1))/length(rv$beta1.v),1),"%), their average length is approximately",signif(mean(2*qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx())),3))
  })
  output$beta01ictesto=renderText({
    req(length(rv$beta1.v)>0)
    paste0(length(rv$beta1.v)-rv$ctrlellipse,
          " out of ",
          length(rv$beta1.v)," ellipses do not include the true value (",
          round(100*(length(rv$beta1.v)-rv$ctrlellipse)/length(rv$beta1.v),1),"%)")
  })
  output$testolivconf=renderText({
    req(input$livconf)
    paste0("The random interval for \\(\\beta_r\\) is built so that the probability that it includes \\(\\beta_r\\) is ",100*input$livconf,"%.")
  })
  output$testolivconf2=renderText({
    req(input$livconf)
    paste0("The random ellipse for \\((\\beta_1,\\beta_2)\\) is built so that the probability that it includes \\((\\beta_1,\\beta_2)\\) is ",100*input$livconf,"%.")
  })
  output$testo2=renderText({
    req(rv$beta1.v)
    paste0("Below we depict ",length(rv$beta1.v)," random confidence intervals, we expect that ",100*(1-input$livconf),"% of them (which is approximately ",round((1-input$livconf)*length(rv$beta1.v),0),") do not cover the true value.")
  })
  output$testo22=renderText({
    req(rv$beta1.v)
    paste0("Below we depict ",length(rv$beta1.v)," random confidence ellipses, we expect that ",100*(1-input$livconf),"% of them (which is approximately ",round((1-input$livconf)*length(rv$beta1.v),0),") do not cover the true value.")
  })
  #  output$testoempirical=renderText({
#    req(length(rv$beta1.v)>0)
#    coverellipse=rv$ctrlellipse
#    coverb1=length(rv$beta1.v)-sum(((rv$beta1.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))>input$beta1)+((rv$beta1.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/Dx()))<input$beta1))
#    coverb0=length(rv$beta1.v)-sum(((rv$beta0.v-qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))>input$beta0)+((rv$beta0.v+qt(1-(1-input$livconf)/2,input$n-2)*sqrt(rv$sigma.v)*sqrt(1/input$n+mean(x())^2/Dx()))<input$beta0))
#  })

  output$beta1ictestoTeo=renderText({
    paste0("The above c.i. are based on the fact that \\(T_2=\\frac{\\hat\\beta_2-\\beta_2}{\\hat{V}(\\hat{\\beta_2})}\\thicksim t_{",input$n-2,"}\\) and so \\(P(t_{",input$n-2,",",(1-input$livconf)/2,"}\\leq T_2 \\leq t_{",input$n-2,",",1-(1-input$livconf)/2,"})=",input$livconf,"\\).")

  })

  output$FtestoTeo=renderText({
    paste0("The above c.r. are based on the fact that \\(F(\\beta)=\\frac{(\\hat\\beta-\\beta)^TX^TX(\\hat\\beta-\\beta)/(2n)}{\\hat\\sigma^2/(n-2)} \\thicksim F_{2,n-2} \\)  and so \\(P(F(\\beta)\\leq F_{2,",input$n-2,"})=",input$livconf,"\\).")

  })




  output$beta0beta1ic=renderPlot({
    req(length(rv$beta1.v)>0)

    par(mar=c(5,4,0.5,0.5),cex=1)
    ci=confint(fit(),level=input$livconf)
    a=ellipse(c(input$beta0,input$beta1),shape=invxtx()[1:2,1:2],
              radius=sqrt(2*qf(0.9999,2,input$n-3)*input$sdeverr^2))
    lim2=range(input$beta0+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,1])
    lim3=range(input$beta1+6*c(-1,1)*input$sdeverr/sqrt(input$n),a[,2])
    plot(input$beta0,input$beta1,xlim=lim2,ylim=lim3,xaxs="i",yaxs="i",
         xlab=expression(hat(beta)[1]),ylab=expression(hat(beta)[2]),las=1)
    #confidenceEllipse(fit(),which.coef=c(1,2),add=TRUE,col="darkred",level=input$livconf)
    #colore=hsv(4/360,.13,.98,alpha=0.3)
    colore=c("darkred","gray")
    contatore=0
    for (i in 1:length(rv$beta1.v)){
      vec=c(rv$beta0.v[i],rv$beta1.v[i])-c(input$beta0,input$beta1)
      controllo=(vec %*% xtx() %*% vec)<=(2*input$n*qf(input$livconf,2,input$n-2)*rv$sigma.v[i]/(input$n-2))
      ellipse(c(rv$beta0.v[i],rv$beta1.v[i]),shape=invxtx()[1:2,1:2],
               radius=sqrt(2*input$n*qf(input$livconf,2,input$n-2)*rv$sigma.v[i]/(input$n-2)),col=colore[1+controllo],fill=FALSE,lwd=0.5,center.pch=FALSE)
      contatore=contatore+controllo
    }
    rv$ctrlellipse=contatore
    segments(ci[1,1],lim3[1],ci[1,2],lim3[1],lwd=5,col="darkred")
    segments(lim2[1],ci[2,1],lim2[1],ci[2,2],lwd=5,col="darkred")
    points(input$beta0,input$beta1,pch=20,cex=2,col="darkgreen")
    segments(input$beta0,input$beta1,input$beta0,lim3[1],col="darkgreen",lty=2)
    segments(input$beta0,input$beta1,lim2[1],input$beta1,col="darkgreen",lty=2)


  })

  output$qp1plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
    qp=(rv$beta0.v-input$beta0)/(sqrt(rv$sigma.v)*sqrt(1/input$n+input$xbar^2/Dx()))
    a= hist(qp,n=nintf(qp))
    plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(t[1]),freq=FALSE,
         xlim=range(c(qt(c(0.001,0.999),df=input$n-2),qp)),ylim=c(0,max(1.2*dt(0,df=input$n-2),0*a$density)))
    curve(dt(x,df=input$n-2),add=TRUE,col="darkgreen",lwd=2)
  })
  output$qp2plotB=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
    #qp=(rv$beta1.v-input$beta1)/(rv$sigma.v*sqrt(1/Dx()))
    #bb=c(-1,1)*qt(input$livconf/2,df=input$n-2)
    #limiti=sort(c(seq(min(qp),bb[1],length=3),seq(bb[1],bb[2],length=7),seq(bb[2],max(qp),length=3)))
    a= hist(rv$qp,n=nintf(rv$qp))
    plot(a,main="",yaxt="n",border="white",col="darkred",xlab=expression(t[2]),freq=FALSE,
         xlim=range(c(qt(c(0.001,0.999),df=input$n-2),rv$qp)),ylim=c(0,max(1.2*dt(0,df=input$n-2),0*a$density)),
         xlab="Sample values of T2")
    curve(dt(x,df=input$n-2),add=TRUE,col="darkgreen",lwd=2)
  })
  output$qp2plot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
    bb=c(-1)*qt((1-input$livconf)/2,df=input$n-2)
    plot(rv$qp,rv$qpx,col=c("black","darkred")[1+(abs(rv$qp)>bb)],yaxt="n",
         xlim=c(-1,1)*pmax(qt(0.999,df=input$n-2),max(abs(rv$qp))),ylim=c(0,1),
         xlab="Simulated values of T2",
         xaxt="n")
    axis(1,at=c(0,c(-1,1)*round(bb,2)))
    abline(v=c(-1,1)*bb)
  },height=200)




  output$Fplot=renderPlot({
    req(length(rv$beta0.v)>0)
    par(mar=c(5,1,1,1))
    bb=qf(input$livconf,df1=2,df2=input$n-2)
    plot(rv$ftest,rv$ftestx,col=c("black","darkred")[1+(rv$ftest>bb)],yaxt="n",
         xlim=c(0,pmax(qf(0.999,df1=2,df2=input$n-2),max(rv$ftest))),ylim=c(0,1),
         xlab="Simulated values of F",
         xaxt="n")
    axis(1,at=c(0,round(bb,2)))
    abline(v=bb)
  },height=200)

}

shinyApp(ui = ui, server = server)

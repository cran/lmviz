library(shiny)
library(shinyjs)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  #  tags$img(width=300,height=50,src="deams.png"),
  #  tags$img(width=300,height=50,src="logoUnits2.jpg"),
  h1("Hypotheses test and p-values for the slope"),
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
      verticalLayout(
        wellPanel(
          splitLayout(
            numericInput("ipbeta","\\(H_0: \\beta_2=\\)",0,-10,10,0.1),
            numericInput("siglev","(significance level) \\(\\alpha=\\)",0.05,0.0001,0.5,0.0001)
          )
          ),
      fluidRow(
        column(width = 6,{
          verticalLayout(
            plotOutput("scatter")
          )
        }),
        column(width = 6,
                 plotOutput("stim")
              )
      ),
      h4(textOutput("rejrate")),
      h4(textOutput("rejratecomm")),
      splitLayout(
         plotOutput("pvalueplot", width = "100%", height="300px")
    )
  )
)))

nintf=function(x) {
  if (length(x)<50) res=5
  if ((length(x)>=50) & (length(x)<500)) res=20
  if ((length(x)>=500)) res=30
  return(res)
}


server <- function(input, output, session) {

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
    temp=rv$tobs.v
    temp=c(temp,(fit()$coef[2]-input$ipbeta)/summfit()$coef[2,2])
    rv$tobs.v=temp
    temp=rv$pval.v
    temp=c(temp,2*(1-pt(abs(fit()$coef[2]-input$ipbeta)/summfit()$coef[2,2],input$n-2)))
    rv$pval.v=temp
  })
  observeEvent(input$n*input$beta0*input$beta1*input$sdeverr*input$sdevx*input$xbar*input$ipbeta,{
    rv$beta1.v=c()
    rv$beta0.v=c()
    rv$sigma.v=c()
    rv$tobs.v=c()
    rv$pval.v=c()
  })
  observeEvent(input$scarica,{
    #dati=data.frame(x=x(),y=y())
    #.GlobalEnv$results.SimpleLM=dati
    write.table(dati,sep=",",file=paste0(.wd,"/temp.csv"))
  })
  output$rejrate=renderText(
    paste0("Rejection rate: ", 100*signif(mean(rv$pval.v<input$siglev),3), "% (",sum(rv$pval.v<input$siglev)," rejections in ",length(rv$beta1.v)," simulations).")
  )
  output$rejratecomm=renderText({
    if (input$ipbeta==input$beta1){
      paste0("Since the simulations are made under the null, the rejection rate is the type I error rate, that is, the significance level.")
    } else {
      paste0("Since the simulations are not made under the null, the rejection rate is the power of the test (i.e. 1-type II error rate) when the value of the parameter is ",input$beta1,".")
    }
  })

  output$stim=renderPlot({
    par(mar=c(4,1,1,1),cex=1.2)
    xxlim=c()
    tobs=(fit()$coef[2]-input$ipbeta)/summfit()$coef[2,2]
    xxlim[1]=min(-4,-1.5*abs(tobs))
    xxlim[2]=max(4,1.5*abs(tobs))
    curve(dt(x,input$n-2),from=xxlim[1],to=xxlim[2],xaxt="n",xlab="",ylab="",bty="n",yaxt="n",
          ylim=c(0,dt(0,input$n-2)),yaxs="i")
    abline(v=0,lty=3,col="darkgrey")
    accreg=c(-1,1)*qt(1-input$siglev/2,df=input$n-2)
    segments(accreg[1],0,accreg[2],0,lwd=4,col="darkgreen")
    segments(xxlim[1],0,accreg[1],0,lwd=4,col="darkred")
    segments(xxlim[2],0,accreg[2],0,lwd=4,col="darkred")

    par(xpd=NA)
    rect(accreg[1],-5,accreg[2],0,lwd=4,col="#A6F0C3",border=NA) #c("#F5D7D7", "#FFFFFF", "#FFFFFF")"#A6F0C3")
    rect(xxlim[1],-1,accreg[1],0,lwd=4,col="#F5D7D7",border=NA) #"#8C000069")
    rect(xxlim[2],-1,accreg[2],0,lwd=4,col="#F5D7D7",border=NA) #"#8C000069")


    par(xpd=FALSE)

    xseq=seq(abs(tobs),xxlim[2],length=30)
    xseq2=c(xseq,rev(xseq))
    polygon(xseq2,c(0*xseq,dt(rev(xseq),df=input$n-2)),col="#8C000069",border="#8C000069")
    polygon(-xseq2,c(0*xseq,dt(rev(xseq),df=input$n-2)),col="#8C000069",border="#8C000069")
    axis(1,at=xxlim,labels=c("",""),col.ticks="white")
    axis(1,at=c(0),labels='0')
    axis(1,at=c(signif(tobs,3)),
         hadj=(tobs<0),
         labels=substitute(paste(t[oss],"=",toss),
                           list(toss=signif(tobs,3))))
    mtext("accept. reg.",side=1,line=2,at=0,adj=0.5,col="darkgreen")
    mtext("rej. reg.",side=1,line=2,at=accreg[2],adj=0,col="darkred")
    mtext("rej. reg.",side=1,line=2,at=accreg[1],adj=1,col="darkred")
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

      limx=c(min(x()),max(x()))
      limy=range(input$beta0+input$beta1*(input$xbar+c(-1,1)*4*input$sdevx))+c(-1,1)*3*input$sdeverr


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
                          list(b0=signif(fit()$coef[1],3),b1=signif(fit()$coef[2],3),r2=signif(summfit()$r.squared,3),segno=ifelse(fit()$coef[2]>=0,"+","")))
    )
    text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2]-0.07*(limy[2]-limy[1]),adj=c(0+(input$beta1<=0),1),col="darkgray",
         label=substitute(paste(E(y),"=",b0,segno,b1,"x"," (true reg. line)"),
                          list(b0=signif(input$beta0,3),b1=signif(input$beta1,3),segno=ifelse(input$beta1>=0,"+","")))
    )
    text(ifelse(input$beta1>0,limx[1],limx[2]),limy[2]-0.14*(limy[2]-limy[1]),adj=c(0+(input$beta1<=0),1),col="darkgreen",
         label=substitute(paste(E(y),"=",b0,segno,b1,"x"," (null hyp line)"),
                          list(b0=signif(fit()$coef[1],3),b1=input$ipbeta,segno=ifelse(input$ipbeta>=0,"+","")))
    )
    #         expression(paste("Retta stimata:",hat(y),"=",signif(fit()$coef[1],3)," + ",signif(fit()$coef[2],3),"x")))
    curve(input$beta0+input$beta1*x,col=gray(0.5),lwd=2,add=TRUE)
    curve(fit()$coef[1]+fit()$coef[2]*x,col="darkred",lwd=2,add=TRUE)
    curve(fit()$coef[1]+input$ipbeta*x,col="darkgreen",lwd=2,add=TRUE)


  })




  output$pvalueplot=renderPlot({
    if (length(rv$beta1.v>1)){
      par(mar=c(5,1,2,1))
      if (input$ipbeta==input$beta1){
        br=seq(0,1,length=nintf(rv$pval.v))
      } else {
        br=seq(0,max(c(0.1,min(1,1.1*max(rv$pval.v)))),length=nintf(rv$pval.v))
      }
      a= hist(rv$pval.v,breaks = br)
      plot(a,main=paste0("Empirical distribution of the p-value (",length(rv$pval.v)," MC simulations)"),yaxt="n",border="white",col="darkblue",xlab="p-value",freq=FALSE)
      if (input$ipbeta==input$beta1) abline(h=1,lwd=2,col="darkgreen")
      #curve(dt(x,df=input$n-2),add=TRUE,col="darkgreen",lwd=2)
    }
  })


}

shinyApp(ui = ui, server = server)

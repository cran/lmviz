library(shiny)
library(shinyjs)
library(mgcv)
#library(lmtest)

#dist.custom <- .dist.custom
#dist.custom.veravar <- .dist.custom.veravar
#dist.custom.param <- .dist.custom.param
#wd=.wd


ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  tags$h1(id="titolo","Guess the line"),
  textOutput("testo1"),
  textOutput("testo2"),
  textOutput("testo3"),
  fluidRow(
    column(width=2,
           verticalLayout(
                numericInput("grado","degree",3,min=1,max=5,step=1),
               actionButton("aggiorna","Draw Y"),
               actionButton("check","check"),
               actionButton("change","change"),
               actionButton("reset","reset"),
               actionButton("show","Solution")
           )
    ),
    column(width=5,
           verticalLayout(
             plotOutput("grafico",
                        click = "plot_click",dblclick = "plot_dblclick"),
             plotOutput("grafico1",
                        click = "plot_click",dblclick = "plot_dblclick"),
             plotOutput("grafico2",
                        click = "plot_click",dblclick = "plot_dblclick")

           )

    ),
    column(width=5,
           plotOutput("graficoError")
    )
)
)



server <- function(input, output, session) {


  rv=reactiveValues()
  rv$show=0
  rv$testo1="Try to guess the line of best fit for the points in the plot: select some points, your guess will be the spline which interpolates them, you can delete points by double-clicking on them."
  rv$testo2="When you are satisfied click on the check button"
  #rv$x=c()
  #rv$y=c()
  rv$sumsq=c()
  rv$minsumsq=0
  rv$numprova=0
  rv$generalizationError=c(NA,NA)
  ###########################################################################################
  ########
  #######   INPUT DEI PUNTI

  observe({
    if (is.null(rv$xcoord)){
      rv$xcoord=c() #c(0)
      rv$ycoord=c() #c(0)
    }
  })
  #observe({
  #  shinyjs::click("aggiorna")
  #  #invalidateLater(tempo())
  #})


  observeEvent(input$aggiorna,{
    session$reload()
  })

  #observeEvent(input$aggiorna,{
  #rv$numprova=rv$numprova+1
  n1=sample(4:10,1)
  x1=sort(runif(n1,0,1))
  y1=rnorm(n1,0,1)

  n=trunc(runif(1,20*n1,50*n1))
  x=sort(runif(n,0,1))
  my=spline(x1,y1,xout=x)$y #sin(2*pi*x)
  stdeverr=runif(1,0.5,1)*sd(my)
  errori=rnorm(n,0,stdeverr)
  y=my+errori
  M=1000
  errori1=matrix(rnorm(n*M,0,stdeverr),ncol=M)
  testset=my+errori1
  fit=gam(y~s(x))
  prev=predict(fit,newdata=data.frame(x=x))
  #rv$fit=fit
  bestest=mean((testset-as.vector(prev))^2)
  err.adatt=mean((y-as.vector(prev))^2)
  #rv$bestest=mean((testset-as.vector(prev))^2)
  #rv$testset=testset
  #rv$x=x
  #rv$y=y
  #})
  rv$check=0


  observeEvent(input$reset,{
    rv$show=0
    rv$xcoord=rv$xcoordlast
    rv$ycoord=rv$ycoordlast
  })

  observeEvent(input$change,{
    rv$show=0
    rv$xcoordlast=rv$xcoord
    rv$ycoordlast=rv$ycoord
    generr=c(rv$generalizationError[2],NA)
    rv$generalizationError=generr
    adapterr=c(rv$adaptError[2],NA)
    rv$adaptError=adapterr
    rv$check=1
    rv$testo2="You have changed your guess, now you can try to improve it again."
  })



  shinyjs::disable("change")
  observeEvent(input$plot_click,{
    shinyjs::disable("change")

  })

  observeEvent(input$show,{
    rv$show=1
  })
  observeEvent(input$check,{
    rv$check=rv$check+1

    if (rv$check==1) {
      rv$testo2="Nice, now the plot on the right shows the generalization error of your line; the blue line is the genralization error of a GCV-GAM estimate. You can change your guess by adding or deleting points, if you press on check again you will see the erro for the new line."
      shinyjs::disable("change")
      }
    if (rv$check>1) {
      rv$testo2="The plot on the right now shows the errors of your previous line and of the present one, you can change the latter and check again. You can also forget the first guess by clicking on the change button."
      shinyjs::enable("change")
      }

  })

  observeEvent(rv$show,{
    if (rv$show==0){
      shinyjs::disable("aggiorna")
    } else {
      shinyjs::enable("aggiorna")
    }
  })

  observeEvent(rv$generalizationError[1],{
    if (is.na(rv$generalizationError[1])){
      shinyjs::disable("show")
      shinyjs::disable("reset")
    } else {
      shinyjs::enable("show")
      shinyjs::enable("reset")
    }
  })

#  y=eventReactive(input$aggiorna*input$n*input$beta0*input$beta1,{
#    y=beta0()+beta1()*x()+rnorm(input$n,0,stdeverr())
#  })

#  observeEvent(input$plot_click$x*input$aggiorna*rv$y,{
#    if (length(rv$xcoord)==0) {
#      rv$testo3="Click anywhere on the plot to choose the first point."
#    }
#    if (length(rv$xcoord)==1) {
#      rv$testo3="Now click on a second point."
#    }
#    if (length(rv$xcoord)==2) {
#      rv$testo3="You have a guess! On the right you see the residual sum of squares for your guess, the green line is the residual sum of #squares associated to the least squares line. Try to improve your line: you can change the guess by clicking anywhere on the plot the #nearest point will be substituted."
#    }
#  })


  observeEvent(input$plot_click$x,{

      temp=rv$xcoord
      temp=c(temp,input$plot_click$x)
      rv$xcoord=temp
      temp=rv$ycoord
      temp=c(temp,input$plot_click$y)
      rv$ycoord=temp
  })

  shinyjs::disable("check")
  observeEvent(rv$xcoord,{
    if (length(rv$xcoord)<1){
      shinyjs::disable("check")
    } else {
      shinyjs::enable("check")
    }
  })

  observeEvent(input$plot_dblclick,{
    selected=which.min(abs(rv$xcoord-input$plot_dblclick$x))

    temp=data.frame(x=rv$xcoord,y=rv$ycoord)
    temp=temp[-selected,]
    rv$xcoord=temp$x
    rv$ycoord=temp$y

    #temp=nearPoints(temp,input$plot_dblclick,maxpoints=1,allRows=TRUE,xvar="x",yvar="y")
    #if (nrow(temp[temp$selected_,])>0){
    #  if ((temp$x[temp$selected_]!=0) & (temp$x[temp$selected_]!=1)){
    #    temp=temp[!temp$selected_,]
    #    rv$xcoord=temp$x
    #    rv$ycoord=temp$y
    #  }
    #}
    })



  output$grafico1=renderPlot({
    req(rv$xcoord>0)
    xseq=seq(min(x),max(x),length=100)
    Xseqmat=cbind(1,xseq,outer(xseq,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
    matplot(xseq,Xseqmat[,-1],type="l")
  })
  output$grafico2=renderPlot({
    req(rv$xcoord>0)
    Xmat=cbind(1,x,outer(x,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
    Bhat= solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% matrix(y,ncol=1)
    xseq=seq(min(x),max(x),length=100)
    Xseqmat=cbind(1,xseq,outer(xseq,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
    for (i in 1:(ncol(Xseqmat)-1)) Xseqmat[,i+1]=Xseqmat[,i+1]*Bhat[i+1]
    matplot(xseq,Xseqmat[,-1],type="l")
  })



  output$grafico=renderPlot({
    req(x*y)
    par(mar=c(5,4.5,1,1))
    plot(x,y,xlab="x",ylab="y",las=1)

    if (length(rv$xcoord)>0){
      #points(rv$xcoord,rv$ycoord,col="darkred",pch=20)
      abline(v=rv$xcoord,lty=2)
    }
    colore=c("darkred","darkgreen")
    if (length(rv$xcoordlast)>0){
      #points(rv$xcoordlast,rv$ycoordlast,col="darkred",pch=20)
      #spguess=splinefun(rv$xcoordlast,rv$ycoordlast) #############################################
      #curve(spguess(x),add=TRUE,col="darkred",lwd=2) #############################################

      abline(v=rv$xcoord,lty=2,col="darkred")
      Xmat=cbind(1,x,outer(x,rv$xcoordlast,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
      Bhat= solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% matrix(y,ncol=1)
      xseq=seq(min(x),max(x),length=100)
      Xseqmat=cbind(1,xseq,outer(xseq,rv$xcoordlast,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
      lines(xseq,Xseqmat %*% Bhat,col="darkred",lwd=2)

    }
    if (length(rv$xcoord)>0){
      #points(rv$xcoord,rv$ycoord,col=colore[1+(rv$check>0)],pch=20)
      #spguess=splinefun(rv$xcoord,rv$ycoord) #############################################
      #curve(spguess(x),add=TRUE,col=colore[1+(rv$check>0)],lwd=2) #############################################

      abline(v=rv$xcoord,lty=2,col=colore[1+(rv$check>0)])
      Xmat=cbind(1,x,outer(x,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
      Bhat= solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% matrix(y,ncol=1)
      xseq=seq(min(x),max(x),length=100)
      Xseqmat=cbind(1,xseq,outer(xseq,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
      lines(xseq,Xseqmat %*% Bhat,col=colore[1+(rv$check>0)],lwd=2)

    }
    if (rv$show!=0){
      curve(predict(fit,newdata=data.frame(x=x)),col="darkblue",add=TRUE,lwd=2)
    }
  })

  observeEvent(input$check,{
    #splinecurr=spline(rv$xcoord,rv$ycoord,xout=x) #############################################

    Xmat=cbind(1,x,outer(x,rv$xcoord,FUN=function(a,b) (((a-b)>0)*(a-b))^input$grado))
    prevspline= as.vector(Xmat %*% solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% matrix(y,ncol=1))
    #prevsplinetestset= Xmat %*% solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% testset

    if (rv$check==1){
      rv$xcoordlast=rv$xcoord
      rv$ycoordlast=rv$ycoord
      cat(rv$ycoordlast)
      rv$generalizationError[1]=mean((testset-prevspline)^2)
      rv$adaptError[1]=mean((y-prevspline)^2)
    }
    if (rv$check>1){
      rv$generalizationError[2]=mean((testset-prevspline)^2)
      rv$adaptError[2]=mean((y-prevspline)^2)
    }
  })



  observeEvent(input$scarica,{
    dati=data.frame(y=rv$ycoord,x=rv$xcoord)
    .GlobalEnv$data.LMboard=dati
    #save(results,file=paste0(wd,"/results.Rdata"))
  })

  output$testo1=renderText(
    paste(rv$testo1)
  )
  output$testo2=renderText(
    paste(rv$testo2)
  )
  output$testo3=renderText(
    paste(rv$testo3)
  )

  output$graficoError=renderPlot({
    req(rv$generalizationError[1]>0)
    par(mar=c(5,4.5,1,1))
    plot(1:2,rv$generalizationError,type="n",
         xlim=c(0,3),
         ylim=c(0,1.2*max(c(rv$adaptError,rv$generalizationError),na.rm=TRUE)), #max(rv$sumsq)),
         xlab="",ylab="Errors")
    rect(0.3,0,0.7,rv$adaptError[1],col="darkred",border=NA)
    rect(0.8,0,1.2,rv$generalizationError[1],col="darkred",border=NA)
    rect(1.8,0,2.2,rv$adaptError[2],col="darkgreen",border=NA)
    rect(2.3,0,2.7,rv$generalizationError[2],col="darkgreen",border=NA)
    abline(h=bestest,col="darkblue",lwd=3)
  })
}


shinyApp(ui = ui, server = server)

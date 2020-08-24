library(shiny)
library(shinyjs)
#library(mgcv)
#library(lmtest)

#dist.custom <- .dist.custom
#dist.custom.veravar <- .dist.custom.veravar
#dist.custom.param <- .dist.custom.param
#wd=.wd


ui <- fixedPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  verticalLayout(
  wellPanel(
    fluidRow(
    column(width=4,
      tags$h1(id="titolo",HTML("Guess the line"))
    ),
    column(width=6,
           verticalLayout(
             textOutput("testo1"),
             textOutput("testo2")
             )
    ),
    column(width=2,
           verticalLayout(
             actionButton("aggiorna","Start",style="position: relative;height: 60px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#A8DEF0;border-radius: 6px;border-color:#35C4F0;border-width:9px;text-decoration:none"),
             br(),
             actionButton("show","Solution",style="position: relative;height: 60px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#A8DEF0;border-radius: 6px;border-color:#35C4F0;border-width:9px;text-decoration:none")
           )
    )
  )),
  fluidRow(
    fillRow(plotOutput("grafico",
                       click = "plot_click",dblclick = "plot_dblclick"),
            plotOutput("graficossq",
                       click = "plot_click",dblclick = "plot_dblclick"),
            flex=c(4,1)
    )

)
)
)


server <- function(input, output, session) {

  disable("show")
  rv=reactiveValues()
  rv$show=0
  rv$testo1="Try to guess the line of best fit for the points in the plot: click on the plot on two points to specify the line which goes through them: Click anywhere on the plot to choose the first point."
  rv$x=c()
  rv$y=c()
  rv$sumsq=c()
  rv$currsumsq=NA
  rv$currbest=NA
  rv$minsumsq=0
  rv$numprova=0
  rv$Error=c(NA,NA)
  ###########################################################################################
  ########
  #######   INPUT DEI PUNTI
  shinyjs::click("aggiorna")
  observe({
    if (is.null(rv$xcoord)){
      rv$xcoord=c() #c(0)
      rv$ycoord=c() #c(0)
    }
  })

  observeEvent(rv$numprova,{
    if (rv$numprova>0) updateActionButton(session,"aggiorna",label="New")
  })

  observeEvent(input$aggiorna,{
    rv$xcoord=c() #c(0)
    rv$ycoord=c() #c(0)
    rv$sumsq=c()
    rv$currsumsq=NA
    rv$currbest=NA
    rv$testo1="Try to guess the line of best fit for the points in the plot: click on the plot on two points to specify the line which goes through them: Click anywhere on the plot to choose the first point."
    rv$testo2=""
    rv$show=0
    rv$numprova=rv$numprova+1

    n=trunc(runif(1,10,30))
    sceltax=sample(1:pmin(3,rv$numprova),1)
    if (sceltax==1) x=rnorm(n,0,1)
    if (sceltax==2) x=runif(n,0,1)
    if (sceltax==3) {
      x=sample(c(-1,1),1)*rchisq(n,1)
    }
    x=(x-mean(x))/sd(x)

    beta0=rnorm(1,0,10)
    beta1=rnorm(1,0,10)
    my=beta0+beta1*x

    sceltaeV=sample(1:ifelse(rv$numprova>2,2,1),1)
    if (sceltaeV==1) stdeverr=runif(1,0.5,1)*sd(my)
    if (sceltaeV==2) stdeverr=runif(1,0.5,1)*abs(my)

    sceltae=sample(1:ifelse(rv$numprova>2,3,1),1)
    if (sceltae==1)  errori=rnorm(n,0,stdeverr)
    if (sceltae==2){
      errori=sample(c(-1,1),1)*rchisq(n,sample(1:3,1))
      errori=stdeverr*errori/sd(errori)
      errori=errori-mean(errori)
    }
    if (sceltae==3){
      errori=runif(n,-1,1)
      errori=stdeverr*errori/sd(errori)
      errori=errori-mean(errori)
    }

    y=beta0+beta1*x+errori

    sceltaP=sample(1:6,1)
    if ((sceltaP<4) & (rv$numprova>2)){
      x0=sample(c(mean(x),mean(x)+c(1.5,0.5,-0.5,-1.5)*(max(x)-mean(x))),sceltaP,replace=TRUE)*rnorm(sceltaP,0,0.1*sd(x))
      y0=beta0+beta1*x0+sample(c(2,3,4),sceltaP,replace=TRUE)*sample(c(-1,1),sceltaP,replace=TRUE)*mean(abs(errori))
      x=c(x,x0)
      y=c(y,y0)
    }
    y=(y-mean(y))/sd(y)
    fit=lm(y~x)
    prev=predict(fit,newdata=data.frame(x=x))
    rv$fit=fit
    rv$minsumsq=sum(resid(fit)^2)
    #rv$testset=testset
    rv$x=x
    rv$y=y
  })
  rv$check=0



  observeEvent(input$show,{
    rv$show=1
  })

  observeEvent(rv$sumsq,{
    if (length(rv$sumsq)>0){
      shinyjs::enable("show")
    } else {
      shinyjs::disable("show")
    }
  })



  observeEvent(input$plot_click$x,{
    if (length(rv$xcoord)<2){
      temp=rv$xcoord
      temp=c(temp,input$plot_click$x)
      rv$xcoord=temp
      temp=rv$ycoord
      temp=c(temp,input$plot_click$y)
      rv$ycoord=temp
      rv$testo1="Ok, now choose a second point."
    } else {
      new=c(input$plot_click$x,input$plot_click$y)
      if (sum((new-c(rv$xcoord[1],rv$ycoord[1]))^2)<sum((new-c(rv$xcoord[2],rv$ycoord[2]))^2)) {
        #sostituisce il primo
        rv$xcoord[1]=input$plot_click$x
        rv$ycoord[1]=input$plot_click$y
      } else {
        #sostituisce il secondo
        rv$xcoord[2]=input$plot_click$x
        rv$ycoord[2]=input$plot_click$y
      }
    }
    if (length(rv$xcoord)>1){
      m=(rv$ycoord[2]-rv$ycoord[1])/(rv$xcoord[2]-rv$xcoord[1])
      q=rv$ycoord[1]-m*rv$xcoord[1]
      currsumsq=sum((rv$y-m*rv$x-q)^2)
      if (length(rv$sumsq)>0){
        rv$testo2=""
        temp=rv$sumsq
        lastsumsq=temp[length(temp)]
        if (currsumsq<=lastsumsq){
          temp=c(temp,currsumsq)
          rv$sumsq=temp
          rv$currsumsq=NA
          rv$currbest=c(q,m)
          rv$testo1=paste0("You have improved your guess: the new line has residual sum of squares ",
                           signif(currsumsq,4),
                           ", your previous guess had residual sum of squares ",
                           signif(lastsumsq,4),
                           ", the line of best fit has sum of squares ",signif(rv$minsumsq,4),". Can you do better?")
          insertUI(selector = "#titolo",
                   where = "afterEnd",
                   ui = tags$audio(src = paste0("sounds/suonorr.wav"),
                                   type = "audio/wav",
                                   autoplay = TRUE, controls = "controls"
                                   , style="display:none;"))
        } else {
          rv$currsumsq=currsumsq
          rv$testo1=paste0("You have worsened your guess: the new line has residual sum of squares ",signif(currsumsq,4),", your previous guess had residual sum of squares ",signif(lastsumsq,4)," and the line of best fit has sum of squares ",signif(rv$minsumsq,4),", try again.")
          insertUI(selector = "#titolo",
                   where = "afterEnd",
                   ui = tags$audio(src = paste0("sounds/suonowr.wav"),
                                   type = "audio/wav",
                                   autoplay = TRUE, controls = "controls"
                                   , style="display:none;"))
        }
      } else {
        rv$sumsq=currsumsq
        rv$currbest=c(q,m)
        rv$testo1=paste0("Great! You have your first guess, your line has residual sum of squares ",signif(currsumsq,4),
                         " the line of best fit has sum of squares ",signif(rv$minsumsq,4)," (see the plot on the right).")
        rv$testo2=paste0("You can try to improve your guess, by clicking on the graph you will move the nearest point and change the line accordingly.")
      }

    }

  })

  observeEvent(rv$show,{
    if (rv$show>0) {
      rv$testo1=paste0("Your best guess  has residual sum of squares ",signif(min(rv$sumsq),4),
                       " the line of best fit, plotted in green, has sum of squares ",signif(rv$minsumsq,4))
      rv$testo2="Press New to try again."
    }
  })

  output$grafico=renderPlot({
    req(rv$x*rv$y)
    par(mar=c(5,4.5,1,1))
    plot(rv$x,rv$y,xlab="x",ylab="y",las=1)

    if (length(rv$xcoord)>0){
      points(rv$xcoord,rv$ycoord,col="darkred",cex=2,pch=20)
    }
    if (!is.na(rv$currbest[1])) abline(rv$currbest,lwd=2)
    if ((length(rv$xcoord)>1)&!is.na(rv$currsumsq)){
      m=(rv$ycoord[2]-rv$ycoord[1])/(rv$xcoord[2]-rv$xcoord[1])
      q=rv$ycoord[1]-m*rv$xcoord[1]
      abline(q,m,col="darkred",lwd=2)
    }
    if (rv$show!=0){
      abline(coef(rv$fit),col="darkgreen",lwd=2)
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

  output$graficossq0=renderPlot({
    req(length(rv$sumsq)>0)
    par(mar=c(5,4.5,1,1))
    plot(1:pmax(5,length(rv$sumsq)+1),rep(rv$minsumsq,pmax(5,length(rv$sumsq)+1)),type="n",
         ylim=c(rv$minsumsq,max(c(max(rv$sumsq),rv$currsumsq),na.rm=TRUE)), #max(rv$sumsq)),
         xlab="",ylab="Residual sum of squares",xaxt="n")
    points(1:length(rv$sumsq),rv$sumsq,pch=20,cex=2)
    lines(1:length(rv$sumsq),rv$sumsq,lwd=2)
    if (!is.na(rv$currsumsq)){
      points((length(rv$sumsq)+1),rv$currsumsq,col="darkred",pch=20,cex=2)
      lines(length(rv$sumsq)+c(0,1),c(rv$sumsq[length(rv$sumsq)],rv$currsumsq),col="darkred",lwd=2)
    }
    abline(h=rv$minsumsq,col="darkgreen",lwd=2)
  })

  output$graficossq=renderPlot({
    req(length(rv$sumsq)>0)
    par(mar=c(5,1,1,4))
    miny=rv$minsumsq-0.1*(max(rv$sumsq)-rv$minsumsq)
    maxy=max(c(max(rv$sumsq)+0.5*(max(rv$sumsq)-rv$minsumsq),rv$currsumsq),na.rm=TRUE)
    assex=signif(c(miny,rv$minsumsq,rv$sumsq,maxy),4)
   plot(0,0,type="n",
         ylim=c(miny,maxy),xlim=c(-0.5,0.5),xaxs="i",
         xlab="",ylab="Residual sum of squares",xaxt="n",yaxt="n",las=1,bty="n")
   rect(-0.5,miny,0.5,maxy)
   rect(-0.5,rv$minsumsq,0.5,rv$sumsq[length(rv$sumsq)],col=gray(0.5))
   rect(-0.5,miny,0.5,rv$minsumsq,col="darkgreen")
   if (!is.na(rv$currsumsq)){
     rect(-0.5,rv$sumsq[length(rv$sumsq)],0.5,rv$currsumsq,col="darkred")
     #abline(h=rv$currsumsq,col="darkred",lwd=2)
   }
    axis(4,at=assex,las=1)
    abline(h=rv$minsumsq,col="darkgreen",lwd=2)
  })




}


shinyApp(ui = ui, server = server)

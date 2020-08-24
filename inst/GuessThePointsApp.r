#library(shiny)
library(shinyjs)
##library(mgcv)
#library(lmtest)

#dist.custom <- .dist.custom
#dist.custom.veravar <- .dist.custom.veravar
#dist.custom.param <- .dist.custom.param
#wd=.wd


ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  tags$h1(id="titolo","Guess the points"),
  p("Click on the white part of the plot to select a point, double click on a point to cancel it, when at least three points are selected a regression line is estimated. Clicking on the gray areas extends the ranges (double click to extend more)."),
  fluidRow(
    column(width = 8,{
        plotOutput("plotinput",
                     click = "plot_click",dblclick = "plot_dblclick")
    }),
  column(width = 4,
         #☻wellPanel(
           plotOutput("stim")
         #))
)),

splitLayout(
  checkboxInput("int","Show confidence bands"),
  actionButton("aggiorna","aggiorna"),
  actionButton("reset2","Reset plot range"),
  actionButton("reset","Reset data"),
  actionButton("scarica","Download data")
)#,

#splitLayout(
#  plotOutput("residui1", width = "100%", height="300px"),
#  plotOutput("residui2", width = "100%", height="300px"),
#  plotOutput("residui3", width = "100%", height="300px")
#)
#

)



server <- function(input, output, session) {


  rv=reactiveValues()
  ##
  ##
#  observeEvent(input$aggiorna,{
    n=sample(20:30,1)
    rv$n=n
    beta1=runif(1,-1,1)
    beta2=runif(1,-1,1)
    x=runif(n,0,1)
    my=beta1+beta2*x
    y=my+runif(1,0.1,3)*sd(my)*rnorm(n,0,1)
    rv$x=x
    rv$y=y
    fit=lm(y~x)
    rv$fit=fit
    sfit=summary(fit)
    rv$sfit=sfit
    rv$n.par=n+c(-5,5,-10,10)
    rv$beta1.par=sfit$coef[1,1]+c(-1,1,-2,2)*sfit$coef[1,2]
    rv$beta2.par=sfit$coef[2,1]+c(-1,1,-2,2)*sfit$coef[2,2]

    rv$vx.par=c(1/15,1/10,1/20,1/8)
    rv$r2.par=c(pmax(0,sfit$r.squared-0.1),pmin(1,sfit$r.squared+0.1),
                pmax(0,sfit$r.squared-0.2),pmin(1,sfit$r.squared+0.2))
    rv$s.par=c(sfit$sigma)*c(0.9,1.1,0.8,1.2)
 # })

  observe({ click("aggiorna")})
  ###########################################################################################
  ########
  #######   INPUT DEI PUNTI

  observe({
    req(rv$x,rv$n)
    if (is.null(rv$xcoord)){
      rv$xcoord=rv$x[1:floor(rv$n/2)] #c(0)
      rv$ycoord=rv$y[1:floor(rv$n/2)] #c(0)
      if (length(rv$xcoord)>0){
        rv$min=min(rv$ycoord)-ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
        rv$max=max(rv$ycoord)+ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
        rv$xmin=min(rv$xcoord)-ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
        rv$xmax=max(rv$xcoord)+ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
      } else {
        rv$min=min(rv$y)
        rv$max=max(rv$y)
        rv$xmin=min(rv$x)
        rv$xmax=max(rv$x)
      }
    }
  })

  observeEvent(input$reset,{
    req(rv$x,rv$n)
    rv$xcoord=rv$x[1:floor(rv$n/2)] #c(0)
    rv$ycoord=rv$y[1:floor(rv$n/2)] #c(0)
    if (length(rv$xcoord)>0){
      rv$min=min(rv$ycoord)-ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$max=max(rv$ycoord)+ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$xmin=min(rv$xcoord)-ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
      rv$xmax=max(rv$xcoord)+ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
    } else {
      rv$min=min(rv$y)
      rv$max=max(rv$y)
      rv$xmin=min(rv$x)
      rv$xmax=max(rv$x)
    }
  })

  observeEvent(input$reset2,{
    if (length(rv$xcoord)>0){
      rv$min=min(rv$ycoord)-ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$max=max(rv$ycoord)+ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$xmin=min(rv$xcoord)-ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
      rv$xmax=max(rv$xcoord)+ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
    } else {
      rv$min=min(rv$y)
      rv$max=max(rv$y)
      rv$xmin=min(rv$x)
      rv$xmax=max(rv$x)
    }
  })


  observeEvent(input$plot_click$x,{
    if (length(rv$xcoord)>0) temp=data.frame(x=rv$xcoord,y=rv$ycoord)
    if ((input$plot_click$x<rv$xmax)&(input$plot_click$x>rv$xmin)) {
      if ((input$plot_click$y>rv$min) & (input$plot_click$y<rv$max)){
        if (exists("temp")){
          temp=rbind(temp,c(input$plot_click$x,input$plot_click$y))
        } else {
          temp=data.frame(x=input$plot_click$x,y=input$plot_click$y)
        }
      }}
      if ((input$plot_click$y<rv$min)){
        rv$min=rv$min-0.1*(rv$max-rv$min)
      }
      if ((input$plot_click$y>rv$max)){
        rv$max=rv$max+0.1*(rv$max-rv$min)
      }
      if ((input$plot_click$x<rv$xmin)){
        rv$xmin=rv$xmin-0.1*(rv$xmax-rv$xmin)
      }
      if ((input$plot_click$x>rv$xmax)){
        rv$xmax=rv$xmax+0.1*(rv$xmax-rv$xmin)
      }
    if (exists("temp")){
       rv$xcoord=temp$x
       rv$ycoord=temp$y
    }
  })


  observeEvent(input$plot_dblclick,{
    #if (length(rv$xcoord)>1){
    if (input$plot_dblclick$x>rv$xmin & input$plot_dblclick$x<rv$xmax){
      if ((input$plot_dblclick$y>rv$min) & (input$plot_dblclick$y<rv$max)){
        temp=data.frame(x=rv$xcoord,y=rv$ycoord)
        temp=nearPoints(temp,input$plot_dblclick,maxpoints=1,allRows=TRUE,xvar="x",yvar="y")
        if (nrow(temp[temp$selected_,])>0){
          #if ((temp$x[temp$selected_]!=0) & (temp$x[temp$selected_]!=1)){
            temp=temp[!temp$selected_,]
            rv$xcoord=temp$x
            rv$ycoord=temp$y
          #}
        }
      }}
      if ((input$plot_dblclick$y<rv$min)){
        rv$min=rv$min-0.7*(rv$max-rv$min)
      }
      if ((input$plot_dblclick$y>rv$max)){
        rv$max=rv$max+0.7*(rv$max-rv$min)
      }
      if ((input$plot_dblclick$x<rv$xmin)){
        rv$xmin=rv$xmin-0.7*(rv$xmax-rv$xmin)
      }
      if ((input$plot_dblclick$x>rv$xmax)){
        rv$xmax=rv$xmax+0.7*(rv$xmax-rv$xmin)
      }

  })


  output$plotinput=renderPlot({
    #req(rv$x,rv$y,rv$xmin,rv$xmax)
    par(mar=c(5,4.5,1,1))
    limy=range(c(rv$min,rv$max))
    limy1=c(limy[1]-0.04*(limy[2]-limy[1]),limy[2]+0.04*(limy[2]-limy[1]))
    limx=range(c(rv$xmin,rv$xmax))
    limx1=c(limx[1]-0.04*(limx[2]-limx[1]),limx[2]+0.04*(limx[2]-limx[1]))

    plot(1,1,type="n",ylim=limy1,xlim=limx1,yaxs="i",xaxs="i",xlab="x",ylab="y",las=1)
    abline(coef(rv$fit),col="darkgreen",lwd=2)

    #axis(1,at=c(0,0.2,0.4,0.6,0.8,1))
    rect(limx1[1],limy1[1],limx[1],limy1[2],col=gray(0.8),border=gray(0.8))
    rect(limx1[1],limy[1],limx1[2],limy1[1],col=gray(0.8),border=gray(0.8))
    rect(limx[2],limy1[1],limx1[2],limy1[2],col=gray(0.8),border=gray(0.8))
    rect(limx[1],limy[2],limx[2],limy1[2],col=gray(0.8),border=gray(0.8))
    # req(rv$xcoord,rv$ycoord)
    points(rv$xcoord,rv$ycoord,pch=20,col="darkblue",cex=2.2)
    if (!is.null(c(rv$beta1.value,rv$beta2.value))){
      bb=c(rv$beta1.value,rv$beta2.value)
      lines(c(rv$xmin,rv$xmax),bb[1]+bb[2]*c(rv$xmin,rv$xmax),lwd=2,col="darkred")
    }
      if (input$int){
        seqx=seq(rv$xmin,rv$xmax,length=30)
        prev1=predict(rv$fit,newdata=data.frame(x=seqx),interval="prediction",level=0.95)
        lines(seqx,prev1[,2],col="darkgreen")
        lines(seqx,prev1[,3],col="darkgreen")
      }

  })

  observe({
    n.value=0
    beta1.value=NULL
    beta2.value=NULL
    r2.value=NULL
    s.value=NULL
    vx.value=NULL
    if ((!is.null(rv$xcoord)) & (!is.null(rv$ycoord))){
      n.value=length(rv$xcoord)
      vx.value=0
    }
    if (n.value>1){
      y=rv$ycoord
      x=rv$xcoord
      vx.value=mean((x-mean(x))^2)
      fit=lm(y~x)
      beta1.value=fit$coef[1]
      beta2.value=fit$coef[2]
      r2.value=1
      s.value=0
    }
    if (n.value>2){
      summfit=summary(fit)
      r2.value=summfit$r.squared
      s.value=summfit$sigma
    }

    rv$n.value=n.value
    rv$beta1.value=beta1.value
    rv$beta2.value=beta2.value
    rv$vx.value=vx.value
    rv$r2.value=r2.value
    rv$s.value=s.value

  })
#  summfit=reactive({summary(fit())})

  output$stim=renderPlot({
    #req(rv$beta1.value)
    par(mfrow=c(6,1),mar=c(1,3,2,2),cex=1)


    indicatore=function(value,truevalue,nome,flag=FALSE,flagr2=FALSE){
      vec=sort(c(truevalue))
      if (!is.null(value)){
        minrange=pmin(vec[1],value-0.1*(vec[4]-vec[1]),na.rm=TRUE)
        maxrange=pmax(vec[4],value+0.1*(vec[4]-vec[1]),na.rm=TRUE)
      } else {
        minrange=vec[1]
        maxrange=vec[4]
      }
      if (flag) { minrange=pmax(0,minrange) }
      if (flagr2) { maxrange=pmin(1,maxrange) }
      col.int1=gray(0.6) #"#CDF7BE"  #c("#F51313", "#13F754", "#CDF7BE", "#F2C6CB")
      col.int2=gray(0.4) #"#F2C6CB"
      col.right="#4CA65E"
      col.wrong="#F51313"

      plot(0,0,type="n",bty="n",ylim=c(-0.6,1.6),xlim=c(minrange,maxrange),xaxt="n",yaxt="n",yaxs="i",xlab="",ylab="")
      axis(3,at=signif(c(minrange,maxrange,vec[2:3]),3),tick=FALSE,line=-1)

      rect(minrange,0,vec[2],1,col=col.int2,border=NA)
      rect(vec[2],0,vec[3],1,col=col.int1,border=NA)
      rect(vec[3],0,maxrange,1,col=col.int2,border=NA)
      rect(minrange,0,maxrange,1,lwd=3,border="black")
      if (!is.null(value)){
        axis(1,at=signif(c(value),3),tick=FALSE,line=-1,font.axis = 2,col.axis=c(col.right,col.wrong)[1+((value<vec[2])|(value>vec[3]))])
        abline(v=value,lwd=4,col=c(col.right,col.wrong)[1+((value<vec[2])|(value>vec[3]))])
        axis(2,at=0.5,label=nome,las=1,tick=FALSE,col.axis=c(col.right,col.wrong)[1+((value<vec[2])|(value>vec[3]))])
      } else {
        axis(2,at=0.5,label=nome,las=1,tick=FALSE,col.axis=gray(0.8))
      }
    }

    indicatore(value=rv$n.value,
               truevalue=rv$n.par,
               nome="n",
               flag=TRUE)
     indicatore(value=rv$vx.value,
               truevalue=rv$vx.par,
               nome="V(X)",
               flag=TRUE)
     indicatore(value=rv$beta1.value,
               truevalue=rv$beta1.par,
               nome=expression(hat(beta)[1]))
    indicatore(value=rv$beta2.value,
               truevalue=rv$beta2.par,
               nome=expression(hat(beta)[2]))
    indicatore(value=rv$r2.value,
               truevalue=rv$r2.par,
               nome=expression(R^2),
               flag=TRUE,flagr2=TRUE)
    indicatore(value=rv$s.value,
               truevalue=rv$s.par,
               nome=expression(s),
               flag=TRUE)
  })


  observeEvent(input$scarica,{
    dati=data.frame(y=rv$ycoord,x=rv$xcoord)
    .GlobalEnv$data.LMboard=dati
    #save(results,file=paste0(wd,"/results.Rdata"))
  })


}


shinyApp(ui = ui, server = server)

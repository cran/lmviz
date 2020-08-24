#library(shiny)
#library(shinyjs)
##library(mgcv)
#library(lmtest)

#dist.custom <- .dist.custom
#dist.custom.veravar <- .dist.custom.veravar
#dist.custom.param <- .dist.custom.param
#wd=.wd


ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  tags$h1(id="titolo","Drawing board"),
  p("Click on the white part of the plot to select a point, double click on a point to cancel it, when at least three points are selected a regression line is estimated. Clicking on the gray areas extends the ranges (double click to extend more)."),
  fluidRow(
    column(width = 8,{
        plotOutput("plotinputvarianza",
                     click = "plot_click",dblclick = "plot_dblclick")
    }),
  column(width = 4,
         #☻wellPanel(
           plotOutput("stim")
         #))
)),

splitLayout(
  checkboxInput("int","Show confidence bands"),
  actionButton("reset2","Reset plot range"),
  actionButton("reset","Reset data"),
  actionButton("scarica","Download data")
),

splitLayout(
  plotOutput("residui1", width = "100%", height="300px"),
  plotOutput("residui2", width = "100%", height="300px"),
  plotOutput("residui3", width = "100%", height="300px")
)


)



server <- function(input, output, session) {


  rv=reactiveValues()
  rv$veravar=c(0)

  ###########################################################################################
  ########
  #######   INPUT DEI PUNTI

  observe({
    if (is.null(rv$xcoord)){
      rv$xcoord=c() #c(0)
      rv$ycoord=c() #c(0)
      rv$min=-1
      rv$max=1
      rv$xmin=-1
      rv$xmax=1
    }
  })

  observeEvent(input$reset,{
    rv$xcoord=c() #c(0)
    rv$ycoord=c() #c(0)
    rv$min=-1
    rv$max=1
    rv$xmin=-1
    rv$xmax=1
  })

  observeEvent(input$reset2,{
    if (length(rv$xcoord)>0){
      rv$min=min(rv$ycoord)-ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$max=max(rv$ycoord)+ifelse((max(rv$ycoord)-min(rv$ycoord))==0,0.1,0.1*(max(rv$ycoord)-min(rv$ycoord)))
      rv$xmin=min(rv$xcoord)-ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
      rv$xmax=max(rv$xcoord)+ifelse((max(rv$xcoord)-min(rv$xcoord))==0,0.1,0.1*(max(rv$xcoord)-min(rv$xcoord)))
    } else {
      rv$min=-1
      rv$max=1
      rv$xmin=-1
      rv$xmax=1
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


  output$plotinputvarianza=renderPlot({
    par(mar=c(5,4.5,1,1))
    limy=range(c(rv$min,rv$max))
    limy1=c(limy[1]-0.04*(limy[2]-limy[1]),limy[2]+0.04*(limy[2]-limy[1]))
    limx=range(c(rv$xmin,rv$xmax))
    limx1=c(limx[1]-0.04*(limx[2]-limx[1]),limx[2]+0.04*(limx[2]-limx[1]))

    plot(1,1,type="n",ylim=limy1,xlim=limx1,yaxs="i",xaxs="i",xlab="x",ylab="y",las=1)

    #axis(1,at=c(0,0.2,0.4,0.6,0.8,1))
    points(rv$xcoord,rv$ycoord,pch=20,col="darkblue",cex=2.2)
    if (length(rv$xcoord)>2){
      bb=fit()$coef
      lines(c(rv$xmin,rv$xmax),bb[1]+bb[2]*c(rv$xmin,rv$xmax),lwd=2,col="darkred")
      if (input$int){
        seqx=seq(rv$xmin,rv$xmax,length=30)
        prev1=predict(fit(),newdata=data.frame(x=seqx),interval="confidence",level=0.95)
        lines(seqx,prev1[,2],col="darkred")
        lines(seqx,prev1[,3],col="darkred")
      }
    }
    rect(limx1[1],limy1[1],limx[1],limy1[2],col=gray(0.8),border=gray(0.8))
    rect(limx1[1],limy[1],limx1[2],limy1[1],col=gray(0.8),border=gray(0.8))
    rect(limx[2],limy1[1],limx1[2],limy1[2],col=gray(0.8),border=gray(0.8))
    rect(limx[1],limy[2],limx[2],limy1[2],col=gray(0.8),border=gray(0.8))
  })

  fit=reactive({
    req(length(rv$xcoord)>2)
    y=rv$ycoord
    x=rv$xcoord
    fit=lm(y~x)
  })
  summfit=reactive({summary(fit())})

  output$stim=renderPlot({
    req(summfit)
    par(mar=c(2,1,0,1),cex=1.5)
    plot(c(0,1),c(1.5,7),type="n",bty="n",xaxt="n",yaxt="n",xlab="n",ylab="n")
    text(0,7,adj=c(0,1),col="darkred",
         label=substitute(paste(hat(beta)[1],"=",b0),
                          list(b0=signif(fit()$coef[1],3),vb1=signif(summfit()$coefficients[1,2],3))))
    text(0,6,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(hat(v)(hat(beta)[1])),"=",vb1),
                          list(b0=signif(fit()$coef[1],3),vb1=signif(summfit()$coefficients[1,2],3))))
    text(0,5,adj=c(0,1),col="darkred",
         label=substitute(paste(hat(beta)[2],"=",b1),
                          list(b1=signif(fit()$coef[2],3))))
    text(0,4,adj=c(0,1),col="darkred",
         label=substitute(paste(sqrt(hat(v)(hat(beta)[2])),"=",vb2),
                          list(b1=signif(fit()$coef[1],3),vb2=signif(summfit()$coefficients[2,2],3))))
    text(0,3,adj=c(0,1),col="darkred",
         label=substitute(paste(s^2,"=",s2," (s=",ss,")"),
                          list(s2=signif(summfit()$sigma^2,3),ss=signif(summfit()$sigma,3))))
    text(0,2,adj=c(0,1),col="darkred",
         label=substitute(paste(R^2,"=",r2),
                          list(r2=signif(summfit()$r.squared,3))))

  })

  output$residui1=renderPlot({
    req(fit())
    par(mar=c(5,4,1,1))
    plot(fit(),1)
  })
  output$residui2=renderPlot({
    req(fit())
    par(mar=c(5,4,1,1))
    plot(fit(),2)
  })
  output$residui3=renderPlot({
    req(fit())
    par(mar=c(5,4,1,1))
    plot(fit(),5)
  })

  observeEvent(input$scarica,{
    dati=data.frame(y=rv$ycoord,x=rv$xcoord)
    .GlobalEnv$data.LMboard=dati
    #save(results,file=paste0(wd,"/results.Rdata"))
  })


}


shinyApp(ui = ui, server = server)

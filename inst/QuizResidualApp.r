#library(shiny)
#library(shinyjs)
#library(mgcv)
#library(lmtest)
#
#
ComputerDecision <- getShinyOption("ComputerDecision",NULL)
Simulation <- getShinyOption("Simulation",NULL)
dir.sounds <- getShinyOption("dir.sounds",NULL)
dir.images <- getShinyOption("dir.images",NULL)
#
#source("lmviz/R/ComputerDecision.default.r")
#source("lmviz/R/Simulation.default.r")
#Simulation=Simulation.default
#ComputerDecision=ComputerDecision.default

#Simulation <- .Simulation
#ComputerDecision <- .ComputerDecision
#dir.sounds <- .dir.sounds
#dir.images <- .dir.images

ui <- fluidPage(id="all",
                shinyjs::useShinyjs(),
                withMathJax(),
                tags$h1(id="titolo","QuizResidual"),
                conditionalPanel(
                  condition="input.parti==0",
                  tags$h2(textOutput("browser"),style="color: red"),
                  HTML("<p style='font-size: 20px'>The program will simulate (x,Y) data and estimate a standard linear model.</p>
                       <p style='font-size: 20px'>You will be shown four diagnostic plots:</p>
                       <ul style='font-size: 20px'>
                       <li>scatter plot of data and estimated regression line</li>
                       <li>scatter plot of residuals versus x</li>
                       <li>histogram of residuals</li>
                       <li>normal quantile plot of residuals</li>
                       </ul>
                       <p style='font-size: 20px'>Based on these you have to decide whether one of the basic assumptions is violated or not.</p>
                       <p style='font-size: 20px'>You play against the computer, who will make a guess based on statistical tests.</p>
                       <p style='font-size: 20px'>After the answer is given, the true data generating mechanism will be shown.</p>
                       "),
                  sliderInput("maxit","How many turns do you want to play?",min=2,max=20,value=3),
                  actionButton("parti","Start the game",
                               style="position: relative;height: 70px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#98E0B5;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none")
                  ),
                conditionalPanel(
                  condition="input.parti!=0",
                  fluidRow(
                    column(width=9,
                           tags$mark(id="segnaposto"),
                           verticalLayout(
                             splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                         verticalLayout(
                                           plotOutput("scatter", width = "100%"),
                                           plotOutput("residui",width="100%")
                                         ),
                                         verticalLayout(
                                           plotOutput("resplot", width = "100%"),
                                           plotOutput("resqqplot", width = "100%")
                                         ))
                           )
                    ),
                    column(width=3,
                           actionButton("prossimo","Next",
                                        style="position: relative;height: 70px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#98E0B5;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none"),
                           h3(""),
                           splitLayout(cellArgs=list(style = "overflow:hidden;"),
                                       verticalLayout(
                                         uiOutput("buttonlin"),
                                         p(""),
                                         uiOutput("buttonetero"),
                                         p(""),
                                         #verticalLayout(
                                         uiOutput("buttonnorm"),
                                         p(""),
                                         uiOutput("buttonok"))
                           ),
                           tags$h2(textOutput("punteggiotesto"),style="text-align:center;font-size: 70px;font-weight: bold;text-shadow: 3px 2px white"),
                           plotOutput("graficopunteggi",height="200px"),
                           checkboxInput("suono","Turn sounds off",value=FALSE)
                    )
                  )
                ))

server <- function(input, output, session) {
  ##https://stackoverflow.com/questions/24397034/how-do-i-refer-to-files-in-the-inst-directory-of-an-r-package-from-a-script-in
  addResourcePath("sounds", paste0(system.file("sounds", package = "lmviz")))
  addResourcePath("images", paste0(system.file("images", package = "lmviz")))
  if (!is.null(dir.sounds)) addResourcePath("sounds", dir.sounds)
  if (!is.null(dir.images)) addResourcePath("images", dir.images)
  # cat(getwd())
  # browser()
  output$browser=renderText({
    if (grepl("rstudio",session$request$HTTP_USER_AGENT)){
      testo="Please, open in browser to get sounds"
    } else {
      testo=""
    }
    testo
  })

  miorosso="#D14747"
  mioverde="#41BA6D"
  miosfondo2="#35C4F0"
  miorosso2="#F52C2C"
  mioverde2="#1EF56D"
  miorosso2=miorosso
  mioverde2=mioverde
  # input$<id> available
  # data <- reactive ({}) #respond to every value in input, to be used as data()
  # output$<nomeoutput> <- render<TIPO>({ <codice> })
  rv=reactiveValues()
  rv$vero=0
  rv$computer=0
  rv$giocatore=0
  iterazione=reactiveVal(0)
  punteggio=reactiveVal(0)
  # maxit=3
  # hide("finaletesto")
  simula=eventReactive(input$prossimo,{
    iterazione(iterazione()+1)
    rv$giocatore=0
    rv$computer=0

    scelta=sample(1:4,1,prob=c(0.25,0.25,0.25,0.25))
    rv$vero=scelta
    Simulation(scelta)
  })
  #suoni=c("suono1","suono2","good1","good2")
  etsuoni=c("w","r")

  fit=reactive({fit=lm(y~x,data=data.frame(x=simula()$x,y=simula()$y))})

  output$risultato=renderText("")

  observeEvent(input$parti,{
    rv$puntigiocatore=rep(-1,length=input$maxit)
    rv$punticomputer=rep(-1,length=input$maxit)
    shinyjs::click("prossimo")
  })

  observe({
    if ((iterazione()==input$maxit) & (rv$giocatore!=0)){
      updateActionButton(session,"prossimo",label="End")
    } else {
      updateActionButton(session,"prossimo",label="Next")
    }
  })

  observeEvent(input$lin,{
    rv$giocatore=1
    punteggio(punteggio()+(rv$giocatore==rv$vero)-(rv$computer==rv$vero))
    rv$puntigiocatore[iterazione()]=(rv$giocatore==rv$vero)
    rv$punticomputer[iterazione()]=(rv$computer==rv$vero)
    if (!input$suono){
      insertUI(selector = "#buttonlin",
               where = "afterEnd",
               ui = tags$audio(src = paste0("sounds/suono",etsuoni[1+(rv$giocatore==rv$vero)],etsuoni[1+(rv$computer==rv$vero)], ".wav"),
                               type = "audio/wav",
                               autoplay = TRUE, controls = "controls"
                               , style="display:none;")

      )}

  })


  observeEvent(input$etero,{
    rv$giocatore=2
    punteggio(punteggio()+(rv$giocatore==rv$vero)-(rv$computer==rv$vero))
    rv$puntigiocatore[iterazione()]=(rv$giocatore==rv$vero)
    rv$punticomputer[iterazione()]=(rv$computer==rv$vero)
    if (!input$suono){
      insertUI(selector = "#buttonlin",
               where = "afterEnd",
               ui = tags$audio(src = paste0("sounds/suono",etsuoni[1+(rv$giocatore==rv$vero)],etsuoni[1+(rv$computer==rv$vero)], ".wav"),
                               type = "audio/wav",
                               autoplay = TRUE, controls = "controls"
                               , style="display:none;")

      )}
  })
  observeEvent(input$norm,{
    rv$giocatore=3
    punteggio(punteggio()+(rv$giocatore==rv$vero)-(rv$computer==rv$vero))
    rv$puntigiocatore[iterazione()]=(rv$giocatore==rv$vero)
    rv$punticomputer[iterazione()]=(rv$computer==rv$vero)
    if (!input$suono){
      insertUI(selector = "#buttonlin",
               where = "afterEnd",
               ui = tags$audio(src = paste0("sounds/suono",etsuoni[1+(rv$giocatore==rv$vero)],etsuoni[1+(rv$computer==rv$vero)], ".wav"),
                               type = "audio/wav",
                               autoplay = TRUE, controls = "controls"
                               , style="display:none;")

      )}
  })
  observeEvent(input$ok,{
    rv$giocatore=4
    punteggio(punteggio()+(rv$giocatore==rv$vero)-(rv$computer==rv$vero))
    rv$puntigiocatore[iterazione()]=(rv$giocatore==rv$vero)
    rv$punticomputer[iterazione()]=(rv$computer==rv$vero)
    if (!input$suono){
      insertUI(selector = "#buttonlin",
               where = "afterEnd",
               ui = tags$audio(src = paste0("sounds/suono",etsuoni[1+(rv$giocatore==rv$vero)],etsuoni[1+(rv$computer==rv$vero)], ".wav"),
                               type = "audio/wav",
                               autoplay = TRUE, controls = "controls"
                               , style="display:none;")

      )}
  })


  observeEvent(input$prossimo,{
    req(iterazione()<=input$maxit)
    rv$computer=ComputerDecision(fit())
  })



  stileb=function(coloresto="black",coloresfondo="#A8DEF0",spessorebordo="9",colorebordo=miosfondo2,tipotesto="none"){
    paste("position: relative;height: 60px;width: 100%;text-align:center;color:",
          coloresto,";font-weight: bold;background-color:",coloresfondo,
          ";border-radius: 6px;border-color:",colorebordo,";border-width:",spessorebordo,"px;","text-decoration:",tipotesto,sep="")
  }

  output$buttonlin=renderUI({
    req(iterazione()<=input$maxit)
    req(input$prossimo)
    if (rv$giocatore==0){
      stile = stileb()
      actionButton("lin",HTML("Non linear"),style=stile)
    } else {
      tipotestoi=ifelse(rv$giocatore==1,"underline","none")
      coloresfondoi=ifelse((rv$vero!=1) & (rv$giocatore==1),miorosso,"#A8DEF0")
      coloresfondoi=ifelse(rv$vero==1,mioverde,coloresfondoi)
      spessorebordoi="9" #ifelse(rv$computer==1,"9","2")
      colorebordoi=miosfondo2
      colorebordoi=ifelse(rv$computer==1 & rv$vero==1,mioverde2,colorebordoi)
      colorebordoi=ifelse(rv$computer==1 & rv$vero!=1,miorosso2,colorebordoi)
      stile = stileb(tipotesto=tipotestoi,coloresfondo=coloresfondoi,
                     colorebordo=colorebordoi,spessorebordo = spessorebordoi)
      shinyjs::disabled(actionButton("lin",HTML("Non linear"),style=stile))
    }
  })
  output$buttonetero=renderUI({
    req(iterazione()<=input$maxit)
    req(input$prossimo)
    if (rv$giocatore==0){
      stile = stileb()
      actionButton("etero",HTML("Heteroschedastic"),style=stile)
    } else {
      tipotestoi=ifelse(rv$giocatore==2,"underline","none")
      coloresfondoi=ifelse((rv$vero!=2) & (rv$giocatore==2),miorosso,"#A8DEF0")
      coloresfondoi=ifelse(rv$vero==2,mioverde,coloresfondoi)
      spessorebordoi="9" #ifelse(rv$computer==1,"9","2")
      colorebordoi=miosfondo2
      colorebordoi=ifelse(rv$computer==2 & rv$vero==2,mioverde2,colorebordoi)
      colorebordoi=ifelse(rv$computer==2 & rv$vero!=2,miorosso2,colorebordoi)
      stile = stileb(tipotesto=tipotestoi,coloresfondo=coloresfondoi,
                     colorebordo=colorebordoi,spessorebordo = spessorebordoi)
      shinyjs::disabled(actionButton("etero",HTML("Heteroschedastic"),style=stile))
    }
  })
  output$buttonnorm=renderUI({
    req(iterazione()<=input$maxit)
    req(input$prossimo)
    if (rv$giocatore==0){
      stile = stileb()
      actionButton("norm",HTML("Non Gaussian"),style=stile)
    } else {
      tipotestoi=ifelse(rv$giocatore==3,"underline","none")
      coloresfondoi=ifelse((rv$vero!=3) & (rv$giocatore==3),miorosso,"#A8DEF0")
      coloresfondoi=ifelse(rv$vero==3,mioverde,coloresfondoi)
      spessorebordoi="9" #ifelse(rv$computer==1,"9","2")
      colorebordoi=miosfondo2
      colorebordoi=ifelse(rv$computer==3 & rv$vero==3,mioverde2,colorebordoi)
      colorebordoi=ifelse(rv$computer==3 & rv$vero!=3,miorosso2,colorebordoi)
      stile = stileb(tipotesto=tipotestoi,coloresfondo=coloresfondoi,
                     colorebordo=colorebordoi,spessorebordo = spessorebordoi)
      shinyjs::disabled(actionButton("norm",HTML("Non Gaussian"),style=stile))
    }
  })
  output$buttonok=renderUI({
    req(iterazione()<=input$maxit)
    req(input$prossimo)
    spessorebordoi="9"
    colorebordoi=miosfondo2
    if (rv$giocatore==0){
      stile = stileb()
      actionButton("ok",HTML("No violation"),style=stile)
    } else {
      tipotestoi=ifelse(rv$giocatore==4,"underline","none")
      coloresfondoi=ifelse((rv$vero!=4) & (rv$giocatore==4),miorosso,"#A8DEF0")
      coloresfondoi=ifelse(rv$vero==4,mioverde,coloresfondoi)
      spessorebordoi="9" #ifelse(rv$computer==1,"9","2")
      colorebordoi=miosfondo2
      colorebordoi=ifelse(rv$computer==4 & rv$vero==4,mioverde2,colorebordoi)
      colorebordoi=ifelse(rv$computer==4 & rv$vero!=4,miorosso2,colorebordoi)
      stile = stileb(tipotesto=tipotestoi,coloresfondo=coloresfondoi,
                     colorebordo=colorebordoi,spessorebordo = spessorebordoi)
      shinyjs::disabled(actionButton("ok",HTML("No violation"),style=stile))
    }

  })
  observe({
    shinyjs::toggleState(id = "prossimo", condition = (rv$giocatore!=0))
  })



  output$punteggiotesto=renderText({
    paste(sum(rv$puntigiocatore==1),"-",sum(rv$punticomputer==1))
  })

  output$finaletesto=renderText({
    if (iterazione()<=input$maxit){
      risp=""
    } else {
      shinyjs::hide("prossimo")
      risp = paste("Game over, ",c("you lost","it's a tie","you won")[1*(punteggio()<0)+2*(punteggio()==0)+3*(punteggio()>0)])
    }
    risp
  })

  observeEvent(iterazione(),{
    if (iterazione()>input$maxit){
      insertUI(selector = "#titolo",
               where = "afterEnd",
               ui=tags$h1(id="testofinale",textOutput("finaletesto")))
      insertUI(selector = "#segnaposto",
               where = "beforeBegin",
               ui=tags$img(id="immaginefinale",width="50%",src=paste0("images/immagine",c("S","P","V")[1+(punteggio()>=0)+(punteggio()>0)],".jpg"))
      )
      insertUI(selector = "#immaginefinale",
               where = "afterEnd",
               ui=tags$h1(
                 HTML("<h1>Important notes:</h1>
                      <p>The game is meant to familiarize with the main features of the diagnostic plots and how they are associated with the violation of assumptions.</p>
                      <p>Even if a violation is present this may not be detectable in the data (because of the small sample size, the low signal to noise ratio, because of chance), so even if the answer is marked incorrect, it may well be a de facto correct answer, <b>for this reason it is more relevant to compare with the computer answer rather than the true answer</b>.</p>
                      <p id='testofinale2'>The presence of a violation (even if detectable) does not always meanthat the model results are unreliable, in fact the linear model is fairly robust to minor violations of its assumptions, this is to say that <b>in analyzing data it may be reasonable to ignore violations of hypotheses</b>. The BadLM() app in the package allows to explore this.</p>
                      <br>")
                 ))
      insertUI(selector = "#testofinale2",
               where = "afterEnd",
               ui=actionButton("startagain",label="Play again",
                               style="position: relative;height: 70px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#98E0B5;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none")
      )
      observeEvent(input$startagain,{
        session$reload()
      })

      if (!input$suono){
        insertUI(selector = "#prossimo",
                 where = "afterEnd",
                 ui = tags$audio(src = paste0("sounds/suonofin",c("S","P","V")[1+(punteggio()>=0)+(punteggio()>0)], ".wav"),
                                 type = "audio/wav",
                                 autoplay = TRUE, controls = "controls"
                                 , style="display:none;")

        )}
    }
  })


  output$graficopunteggi=renderPlot({
    colori=c(gray(0.7),miorosso,mioverde)
    par(mar=c(3,0,0,0))
    plot(1,1,type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="",
         xlim=c(-0.15,2.25),ylim=c(1,input$maxit+1))
    axis(1,at=c(0.5,1.6),label=c("You","comp"),tick=FALSE,lwd=0)
    rect(rep(0,input$maxit),1:input$maxit,rep(1,input$maxit),2:(1+input$maxit),col=colori[2+rv$puntigiocatore],border="white")
    rect(rep(1.10,input$maxit),1:input$maxit,rep(2.10,input$maxit),2:(1+input$maxit),col=colori[2+rv$punticomputer],border="white")
  })

  ########################################################################################################
  output$scatter=renderPlot({
    req(iterazione()<=input$maxit)
    par(mar=c(5,4,1,1))
    a=plot(simula()$x,simula()$y,las=1,xlab="x",ylab="y",col=c("black",gray(0.7))[1+(rv$giocatore!=0)])
    abline(fit(),col="darkred",lwd=2)
    if ((rv$giocatore!=0) & !is.null(simula()$my)){
      lines(simula()$x,simula()$my,col="darkgreen",lwd=2)
      if ((rv$vero!=3) & (!is.null(simula()$sderr))){
        lines(simula()$x,simula()$my+simula()$sderr,col="darkgreen",lwd=2,lty=2)
        lines(simula()$x,simula()$my-simula()$sderr,col="darkgreen",lwd=2,lty=2)
      }
    }
  })
  output$residui=renderPlot({
    req(iterazione()<=input$maxit)
    par(mar=c(5,4,1,1))
    hist(resid(fit()),main="",xlab="Istogramma dei residui",freq=FALSE)
    if ((rv$giocatore!=0) & (rv$vero!=2)  & !is.null(simula()$xperdens) & !is.null(simula()$ferrore)){
      a=hist(resid(fit()),plot=FALSE)
      plot(a,main="",xlab="Istogramma dei residui",freq=FALSE,
           ylim=range(c(a$density,simula()$ferrore,dnorm(a$breaks,mean(simula()$errore),sd(simula()$errore)))))
      lines(simula()$xperdens,simula()$ferrore,col="darkgreen",lwd=2)
      if ((rv$vero==3) & !is.null(simula()$errore)){
        curve(dnorm(x,mean(simula()$errore),sd(simula()$errore)),add=TRUE,lwd=2,col="darkred")
      }
    }
  })
  output$resplot=renderPlot({
    req(iterazione()<=input$maxit)
    par(mar=c(5,4,1,1))
    plot(simula()$x,fit()$resid,main="",xlab="x",ylab="Residui",col=c("black",gray(0.7))[1+(rv$giocatore!=0)])
    if ((rv$giocatore!=0) & (rv$vero!=3) & (!is.null(simula()$sderr))){
      polygon(c(simula()$x,rev(simula()$x)),c(simula()$sderr,rev(-simula()$sderr)),col="lightgreen",border="lightgreen")
      points(simula()$x,fit()$resid,col=c("black",gray(0.7))[1+(rv$giocatore!=0)])
      # lines(simula()$x,simula()$sderr,col="darkgreen",lwd=2)
    }
  })
  output$resqqplot=renderPlot({
    req(iterazione()<=input$maxit)
    par(mar=c(5,4,1,1))
    plot(fit(),2,main="")
  })



}

shinyApp(ui = ui, server = server)

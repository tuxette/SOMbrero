library(SOMbrero)

# SOM training function
trainTheSom <- function(data, type, dimx, dimy, disttype, maxit, varnames) {
  trainSOM(data[, varnames], dimension=c(dimx,dimy), dist.type= disttype, 
           maxit= maxit, type= type)
}

# Server
shinyServer(function(input, output, session) {
  dInput <- reactive({
    in.file <- input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, 
                              row.names=1, dec=input$dec)
    } else {
      the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, dec=input$dec)
    }

    updateCheckboxGroupInput(session, inputId= "varchoice",
                             choices= as.list(colnames(the.table)),
                             selected= as.list(colnames(the.table)))
    the.table
  })

  # update the scaling option when the somtype is changed (not working)
  updateScaling <- reactive({
    input$trainbutton
    input$somtype
    updateSelectInput(session, inputId= "somtype", choices= c("none"))
#                      choices= switch(input$somtype,
#                                      "numeric"= list("unitvar", "none", 
#                                                      "center", "chi2"),
#                                      "Korresp"= list("chi2"),
#                                      "relational"= list("none")),
#                      selected= switch(input$somtype,
#                                       "numeric"= "unitvar",
#                                       "korresp"= "chi2",
#                                       "relational"= "none"))
  })

  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  })

  current.som <- NULL # this variable will contain the current SOM
  server.env <- environment() # used to allocate to current.som in functions

  # This function trains the SOM when the button is hit
  theSom<- function() {
    input$trainbutton
    server.env$current.som <- isolate(trainTheSom(dInput(), input$somtype, 
                                                  input$dimx, input$dimy,
                                                  input$disttype, input$maxit,
                                                  varnames= input$varchoice))
    server.env$current.som
  }

  # This function renders the summary of the SOM
  output$summary <- renderPrint({
    if (input$trainbutton==0) 
      return("Hit the Train button to train the map.")
    summary(theSom())
  })

  # Output the computed som object to be downloaded
  output$som.download <- downloadHandler(
    filename= function() {
      paste("som ", Sys.time(),".rda", sep="")
    },
    content= function(file) {
      som.export <- server.env$current.som
      save(som.export, file= file)
    }
  )

  # Output the sombrero logo :
  output$sombrero.logo <- renderImage(list(src= "sombrero.png"), 
                                      deleteFile= FALSE)

  # Output the SAMM logo :
  output$samm.logo <- renderImage(list(src= "logo SAMM.png"), 
                                  deleteFile= FALSE)

})




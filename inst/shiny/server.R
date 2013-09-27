library(SOMbrero)

# SOM training function
trainTheSom <- function(data, type, dimx, dimy, disttype, maxit, varnames, 
                        rand.seed, scaling, eps0, init.proto, nb.save) {
  set.seed(rand.seed)
  if(type=="numeric")
    data= data[, varnames]
  trainSOM(data, dimension=c(dimx,dimy), dist.type= disttype, 
           maxit= maxit, type= type, scaling= scaling, eps0= eps0, 
           init.proto= init.proto, nb.save= nb.save)
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

    # update the "input variables" checkbox (if somtype is numeric)
    if (input$somtype == "numeric") {
      output$varchoice <- renderUI(
        checkboxGroupInput(inputId= "varchoice", label= "Input variables:",
                           choices= as.list(colnames(the.table)),
                           selected= as.list(colnames(the.table)[sapply(the.table, class)== "numeric"])))
    } else {
      output$varchoice <- renderText("")
    }

    # update the map dimensions
    updateNumericInput(session, inputId= "dimx", value= {
      if (input$somtype == "korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else {
        max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
      }
    })
    updateNumericInput(session, inputId= "dimy", value= {
      if (input$somtype == "korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else {
        max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
      }
    })

    # return the table
    the.table
  })

  # update the scaling option when the somtype is changed (not working)
#  updateScaling <- reactive({
#    input$trainbutton
#    input$somtype
#    updateSelectInput(session, inputId= "somtype",
#                      choices= switch(input$somtype,
#                                      "numeric"= list("unitvar", "none", 
#                                                      "center", "chi2"),
#                                      "Korresp"= list("chi2"),
#                                      "relational"= list("none")),
#                      selected= switch(input$somtype,
#                                       "numeric"= "unitvar",
#                                       "korresp"= "chi2",
#                                       "relational"= "none"))
#  })

  # update the scaling option when the somtype is changed
  output$scaling <- renderUI({
    selectInput(inputId= "scaling", label= "Data scaling:", 
                choices= switch(input$somtype,
                               "numeric"= list("unitvar", "none", 
                                               "center"),
                               "korresp"= list("chi2"),
                               "relational"= list("none")),
                selected= switch(input$somtype,
                                 "numeric"= "unitvar",
                                 "korresp"= "chi2",
                                 "relational"= "none"))
  })

  # data preview table
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

  # Train the SOM when the button is hit
  theSom<- function() {
    input$trainbutton
    server.env$current.som <- isolate(trainTheSom(dInput(), input$somtype, 
                                                  input$dimx, input$dimy, 
                                                  input$disttype, input$maxit, 
                                                  varnames= input$varchoice, 
                                                  rand.seed= input$rand.seed, 
                                                  scaling= input$scaling, 
                                                  eps0= input$eps0, 
                                                  init.proto= input$init.proto, 
                                                  nb.save= input$nb.save))
    server.env$current.som
  }

  # This function renders the summary of the SOM
  output$summary <- renderPrint({
    if (is.null(input$file1))
      return("First import a dataset.")
    if (input$trainbutton==0) 
      return("Hit the Train button to train the map.")
    summary(theSom())
  })

  # Output the computed som object to be downloaded
  # TODO: output an error if map not trained
  output$som.download <- {
    downloadHandler(
      filename= function() {
        paste("som ", Sys.time(),".rda", sep="")
      },
      content= function(file) {
        som.export <- server.env$current.som
        save(som.export, file= file)
      })
  }

  # Output the sombrero logo :
  output$sombrero.logo <- renderImage(list(src= "sombrero.png"), 
                                      deleteFile= FALSE)

  # Output the SAMM logo :
  output$samm.logo <- renderImage(list(src= "samm.png"), 
                                  deleteFile= FALSE)

  # Seed selector:
  output$rand.seed <- renderUI(
    numericInput("rand.seed", "Set a random seed for reproducible results:",
                 sample(1:1e5, size= 1)))
                                            

})




library(SOMbrero)

# Max file input size :
options(shiny.maxRequestSize=30*1024^2)

# SOM training function
trainTheSom <- function(data, type, dimx, dimy, disttype, maxit, varnames, 
                        rand.seed, scaling, eps0, init.proto, nb.save) {
  set.seed(rand.seed)
  if(type=="numeric")
    data= data[, varnames]
  trainSOM(data, dimension=c(dimx,dimy), dist.type= disttype, 
           maxit= maxit, type= type, scaling= scaling, #eps0= eps0, 
           init.proto= init.proto, nb.save= nb.save)
}

# List of plot types options per SOM type and "what" :
all.plot.types= list("numeric"= 
                   list("prototypes"= 
                          list("color", "3d", "lines", 
                               "barplot", "smooth distances"= "smooth.dist",
                               "polygon distances"= "poly.dist",
                               "grid distances"= "grid.dist",
                               "U matrix distances"= "umatrix",
                               "mds", "radar"),
                        "obs"= c("hitmap", "color", "lines", "barplot", 
                                 "names", "boxplot", "radar"),
                        "add"= c("pie", "color", "lines", "boxplot", "barplot", 
                                 "radar", "names", "words", "graph")),
                 "korresp"= 
                   list("prototypes"= 
                          list("color", "3d", "lines", 
                               "barplot", "polygon distances"= "poly.dist",
                               "grid distances"= "grid.dist",
                               "U matrix distances"= "umatrix",
                               "mds", "radar"),
                        "obs"= c("hitmap", "names"),
                        "add"= "NA"),
                 "relational"= 
                   list("prototypes"=
                          list("lines", "barplot",
                               "polygon distances"= "poly.dist",
                               "grid distances"= "grid.dist",
                               "U matrix distances"= "umatrix",
                               "mds", "radar"),
                        "obs"= c("hitmap", "names"),
                        "add"= c("pie", "color", "lines", "boxplot", "barplot", 
                                 "radar", "names", "words", "graph")))
# Server
shinyServer(function(input, output, session) {
  # server environment variables
  server.env <- environment() # used to allocate in functions
  current.som <- NULL # this variable will contain the current SOM
  current.sc <- NULL #  this will contain the current superclass object
  current.table <- NULL
  
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
                           selected= as.list(colnames(the.table)[sapply(
                                             the.table, class) %in% c("integer",
                                                                      "numeric")
                                                                 ])))
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
    server.env$current.table <- the.table
    the.table
  })

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

  # Train the SOM when the button is hit
  theSom<- function() {
    input$trainbutton
    server.env$current.som <- isolate(trainTheSom(dInput(), input$somtype, 
                                                  input$dimx, input$dimy, 
                                                  input$disttype, input$maxit, 
                                                  varnames= input$varchoice, 
                                                  rand.seed= input$randseed, 
                                                  scaling= input$scaling, 
                                                  eps0= input$eps0, 
                                                  init.proto= input$init.proto, 
                                                  nb.save= input$nb.save))

    # return the computed som
    server.env$current.som
  }

  # Render the summary of the SOM
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

  # Input number of superclasses or cutting height
  output$scHorK <- renderUI(
    switch(input$sc.cut.choice, 
           "nclust"= numericInput("sc.k", "Number of superclasses:", 2, 
                                  min=1, max= input$dimx * input$dimy - 1), 
           "tree.height"= numericInput("sc.h", "Height in dendrogram:", 10,
                                       min= 0))
  )

  # Compute superclasses when the button is hit
  computeSuperclasses <- function() {
    input$superclassbutton
    server.env$current.sc <- isolate(
      switch(input$sc.cut.choice, 
             "nclust"= superClass(sommap= current.som, k= input$sc.k),
             "tree.height"= superClass(sommap= current.som, h= input$sc.h)))
    server.env$current.sc
  }

  output$sc.summary <- renderPrint( {
    if (input$superclassbutton==0) 
      return("Hit the Compute superclasses button to show the results.")
    summary(computeSuperclasses())
  })

  # Render the dendrogram
  output$dendrogram <- renderPlot(expr= {
    if (input$superclassbutton == 0) {
      return(NULL)
    } else {
      plot(server.env$current.sc)
    }
  })

  # Download the superclass classification
  # TODO: output an error if map not trained
  output$sc.download <- {
    downloadHandler(
      filename= function() {
        paste("superclasses ", Sys.time(),".csv", sep="")
      },
      content= function(file) {
        classes.export <- 
          data.frame(obs= row.names(server.env$current.sc$som$data),
                     cluster= server.env$current.sc$cluster[
                                       server.env$current.sc$som$clustering])
        write.csv(classes.export, file= file, row.names= FALSE)
      })
  }

  # Adapt plottype to the somtype and the "what" arguments
  observe({
    updateSelectInput(session, "plottype", 
                      choices= all.plot.types[[input$somtype]][[input$plotwhat]])
  })
                                            
  # Adapt the variable choice to the "what" and "type" arguments
#   observe(output$plotoptions2 <- {
#     if (input$plotwhat != "prototypes" | is.null(server.env$current.table) |
#         !(input$plottype %in% c("color", "3d", "radar"))) {
#       return(NULL)
#     }
#     renderUI(selectInput("plot.var", "Variable to plot", 
#                          choices= colnames(server.env$current.table)[
#                                            sapply(server.env$current.table, 
#                                              class) == "numeric"]))
#   })

  observe(updateSelectInput(session, "plotvar",
    choices= if (!(input$plotwhat %in% c("obs","prototypes")) | 
                 is.null(server.env$current.table) |
                 !(input$plottype %in% c("color", "3d"))) {
               "(Not Available)"
             } else colnames(current.som$data)))

  # Render plot
#   observe(output$somplot <- {
#    input$plotwhat
#    input$plottype
#    print("entré plot")
#    if (is.null(server.env$current.som)) {
#      print("pense que null")
#      return(NULL)
#    }
#    if (input$plotwhat == "superclass") {
#      print("superclass")
#      renderPlot(expr= plot(current.sc, type= input$plottype))
#    } else {
#      print("pas superclass")
#      tmp.som <- current.som
#      return(renderPlot(expr= plot(tmp.som, type= input$plottype)))
#    }})
##  observe(output$somplot <- {
##    if (is.null(server.env$current.som)) {
##      return(NULL)
##    }
#    observe(if(!is.null(server.env$current.som)){

  observe(output$somplot <- renderPlot(
        expr= plot(x= switch((!is.null(current.sc) & input$plotsc) + 1, 
                             current.som, current.sc),
                   what= input$plotwhat, type= input$plottype,
                          variable= if(input$plotwhat %in% c("prototypes",
                                                             "obs")
                                       & input$plottype %in% 
                                       c("color", "3d")) {
                                      input$plotvar
                                    } else {
                                      NULL
                                    }))
      )

#    })
  
})

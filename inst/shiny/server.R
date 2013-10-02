library(SOMbrero) # Version 0.4

# Max file input size :
options(shiny.maxRequestSize= 30*1024^2)

# SOM training function
trainTheSom <- function(data, type, dimx, dimy, disttype, maxit, varnames, 
                        rand.seed, scaling, eps0, init.proto, nb.save) {
  set.seed(rand.seed)
  if(type=="numeric")
    data <- data[, varnames]
  trainSOM(data, dimension=c(dimx,dimy), dist.type= disttype, 
           maxit= maxit, type= type, scaling= scaling,
           init.proto= init.proto, nb.save= nb.save)
}

# List of somplot types options per SOM type and "what" :
all.somplot.types <- list("numeric"= 
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

all.scplot.types <- list("numeric"= 
                            list("prototypes"= 
                                   list("color", "lines", 
                                        "barplot",
                                        "polygon distances"= "poly.dist",
                                        "mds", "radar"),
                                 "obs"= c("hitmap", "color", "lines", "barplot", 
                                          "boxplot", "radar"),
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
  
  # File input function
  dInput <- reactive({
    in.file <- input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, 
                              row.names=1, dec=input$dec)
    } else the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, dec=input$dec)
    
    # update the "input variables" checkbox (if somtype is numeric or integer)
    if (input$somtype == "numeric") {
      output$varchoice <- renderUI(
        checkboxGroupInput(inputId= "varchoice", label= "Input variables:",
                           choices= as.list(colnames(the.table)),
                           selected= as.list(colnames(the.table)[sapply(
                                             the.table, class) %in% c("integer",
                                                                      "numeric")
                                                                 ])))
    } else output$varchoice <- renderText("")

    # update the map dimensions
    updateNumericInput(session, inputId= "dimx", value= {
      if (input$somtype == "korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })
    updateNumericInput(session, inputId= "dimy", value= {
      if (input$somtype == "korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })

    # return the table
    server.env$current.table <- the.table
    the.table
  })

  #### Tab Preview Data
  ##############################################################################
  
  # data preview table
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  })

  #### Tab Self-organize
  ##############################################################################

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
        paste("som ", format(Sys.time(), format= "%Y-%m-%d_%H:%M"),
              ".rda", sep="")
      },
      content= function(file) {
        som.export <- server.env$current.som
        save(som.export, file= file)
      })
  }
  
  #### Tab Plot
  ##############################################################################
  
  # Adapt plottype to the somtype and the "what" arguments
  observe({
    updateSelectInput(session, "somplottype", 
                      choices= all.somplot.types[[input$somtype]][[
                        input$somplotwhat]])
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
  
  observe(updateSelectInput(session, "somplotvar",
                            choices= if (!(input$somplotwhat %in% c("obs","prototypes")) | 
                                           is.null(server.env$current.table) |
                                           !(input$somplottype %in% c("color", "3d"))) {
                              "(Not Available)"
                            } else colnames(current.som$data)))
  
  # Render SOM plot
  observe(output$somplot <- {
    #     if (is.null(current.som)) {
    #       renderPlot(expr= plot(NULL, xlim= c(0,1), ylim= c(0,1)))
    #     } else {
    renderPlot(expr= plot(x= current.som, what= input$somplotwhat, 
                          type= input$somplottype,
                          variable= if(input$somplotwhat %in% 
                                         c("prototypes","obs")
                                       & input$somplottype %in% 
                                         c("color", "3d")) {
                            input$somplotvar
                          } else NULL
                          , print.title= input$somplottitle
    )
    ) 
  })

  
  #### Tab Superclass
  ##############################################################################
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
    } else plot(server.env$current.sc)
  })

  # Download the superclass classification
  # TODO: output an error if map not trained
  output$sc.download <- {
    downloadHandler(
      filename= function() {
        paste("superclasses ", format(Sys.time(), format= "%Y-%m-%d_%H:%M"),
              ".csv", sep="")
      },
      content= function(file) {
        classes.export <- 
          data.frame(obs= row.names(server.env$current.sc$som$data),
                     cluster= server.env$current.sc$cluster[
                                       server.env$current.sc$som$clustering])
        write.csv(classes.export, file= file, row.names= FALSE)
      })
  }

  
  # Adapt scplottype to the somtype and the "what" arguments
  observe({
    updateSelectInput(session, "scplottype", 
                      choices= all.scplot.types[[input$somtype]][[
                        input$scplotwhat]])
  })
  
  observe(updateSelectInput(session, "scplotvar",
                            choices= if (!(input$scplotwhat %in% 
                                             c("obs","prototypes")) | 
                                           is.null(server.env$current.table) |
                                           !(input$scplottype %in% 
                                               c("color", "3d"))) {
                              "(Not Available)"
                            } else colnames(current.som$data)))
  # Render SuperClass plot
  observe(output$scplot <- renderPlot(
    expr= plot(x= current.sc,
               what= input$scplotwhat, type= input$scplottype,
               variable= if(input$scplotwhat %in% c("prototypes", "obs")
                            & input$scplottype %in% c("color", "3d")) {
                 input$scplotvar
               } else NULL
    )
  ))

  #### Tab About
  ##############################################################################
  
  # Output the sombrero logo :
  output$sombrero.logo <- renderImage(list(src= "sombrero.png"), 
                                      deleteFile= FALSE)
  
  # Output the SAMM logo :
  output$samm.logo <- renderImage(list(src= "samm.png"), 
                                  deleteFile= FALSE)
  
})

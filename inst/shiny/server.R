library(SOMbrero) # Version 0.4
library(Matrix)

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
                                 "names", "boxplot", "radar")),
                 "korresp"= 
                   list("prototypes"= 
                          list("color", "3d", "lines", 
                               "barplot", "polygon distances"= "poly.dist",
                               "grid distances"= "grid.dist",
                               "U matrix distances"= "umatrix",
                               "mds", "radar"),
                        "obs"= c("hitmap", "names")),
                 "relational"= 
                   list("prototypes"=
                          list("lines", "barplot",
                               "polygon distances"= "poly.dist",
                               "grid distances"= "grid.dist",
                               "U matrix distances"= "umatrix",
                               "mds", "radar"),
                        "obs"= c("hitmap", "names")))
                                         
all.scplot.types <- list("numeric"= 
                            list("prototypes"= 
                                   list("dendrogram", "color", "lines", "grid",
                                        "barplot", "dendro3d",
                                        "polygon distances"= "poly.dist",
                                        "mds", "radar"),
                                 "obs"= c("hitmap", "color", "lines", "barplot", 
                                          "boxplot", "radar", "grid")),
                          "korresp"= 
                            list("prototypes"= 
                                   list("dendrogram", "color", "lines", "grid", 
                                        "barplot", 
                                        "polygon distances"= "poly.dist",
                                        "mds", "radar", "dendro3d"),
                                 "obs"= "hitmap"),
                          "relational"= 
                            list("prototypes"=
                                   list("dendrogram", "lines", "barplot", 
                                        "grid", "polygon distances"="poly.dist",
                                        "mds", "radar", "dendro3d"),
                                 "obs"= "hitmap"))


# Server
shinyServer(function(input, output, session) {

  # server environment variables
  server.env <- environment() # used to allocate in functions
  current.som <- NULL # this variable will contain the current SOM
  current.sc <- NULL #  this will contain the current superclass object
  current.table <- NULL
  current.addtable <- NULL # this will contain the table of additional variables
  
  # File input
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
    
    updatePlotSomVar() # update variable choice for som plots
    updatePlotScVar() # update variable choice for sc plots
    plotTheSom() # render plot

    # return the computed som
    server.env$current.som
  }

  # Render the summary of the SOM
  output$summary <- renderPrint({
    dInput()
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
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    updateSelectInput(session, "somplotvar", 
                      choices= colnames(current.som$data))
    updateSelectInput(session, "somplotvar2", 
                      choices= colnames(current.som$data), 
                      selected= colnames(current.som$data)[
                        1:min(5,ncol(current.som$data))])
  })
  
  # Render SOM plot
  plotTheSom <- function() observe({
    output$somplot <- renderPlot({
      switch(input$somplottype, 
             "boxplot"= plot(x= current.som, what= input$somplotwhat, 
                             type= input$somplottype,
                             variable= (1:ncol(current.som$data))[ 
                                       colnames(current.som$data) %in% 
                                       input$somplotvar2],
                             print.title= input$somplottitle,
                             view= input$somplotrowcol),
             "radar"= plot(x= current.som, what= input$somplotwhat, 
                           type= input$somplottype,
                           variable= input$somplotvar,
                           print.title= input$somplottitle,
                           view= input$somplotrowcol, 
                           key.loc=c(-1,2), mar=c(0,10,2,0)),
             plot(x= current.som, what= input$somplotwhat, 
                  type= input$somplottype,
                  variable= input$somplotvar,
                  print.title= input$somplottitle,
                  view= input$somplotrowcol))
    })
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
    
#    plotTheDendro() # plot the dendrogram
    plotTheSc() # plot the SuperClass plot
    server.env$current.sc
  }

  output$sc.summary <- renderPrint( {
    if (input$superclassbutton==0)
      return("Hit the Compute superclasses button to show the results.")
    summary(computeSuperclasses())
  })

  # Render the dendrogram
  plotTheDendro <- function() observe({
    output$dendrogram <- renderPlot(plot(server.env$current.sc))
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
  
  # update variables available for plotting
  updatePlotScVar <- function() observe({
    updateSelectInput(session, "scplotvar", 
                      choices= colnames(current.som$data))
    updateSelectInput(session, "scplotvar2", 
                      choices= colnames(current.som$data),
                      selected= colnames(current.som$data)[
                        1:min(5,ncol(current.som$data))])
  })
  
  # function to render SuperClass plot
  plotTheSc <- function() observe({
    output$scplot <- renderPlot({
      switch(input$scplottype, 
             "dendrogram"= plot(x= current.sc, type= "dendrogram"),
             "boxplot"= plot(x= current.sc, what= input$scplotwhat, 
                             type= input$scplottype,
                             variable= (1:ncol(current.som$data))[ 
                                       colnames(current.som$data) %in% 
                                       input$scplotvar2],
                             view= input$scplotrowcol),
             "radar"= plot(x= current.sc, what= input$scplotwhat, 
                           type= input$scplottype,
                           variable= input$scplotvar,
                           print.title= input$scplottitle,
                           view= input$scplotrowcol, 
                           key.loc=c(-1,2), mar=c(0,10,2,0)),
             plot(x= current.sc, what= input$scplotwhat, 
                  type= input$scplottype,
                  variable= input$scplotvar,
                  print.title= input$scplottitle,
                  view= input$scplotrowcol))
    })
  })

  #### Tab Combine with additional data
  ##############################################################################
  
  # File input for additional variables
  dInputAdd <- reactive({
    in.file <- input$file2
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames2) {
      the.table <- read.table(in.file$datapath, header=input$header2, 
                              sep=input$sep2, quote=input$quote2, 
                              row.names=1, dec=input$dec2)
    } else the.table <- read.table(in.file$datapath, header=input$header2, 
                                   sep=input$sep2, quote=input$quote2, 
                                   dec=input$dec2)
    server.env$current.addtable <- the.table
    
    updateAddPlotVar() # update variable selector    
    plotTheAdd() # launch the addplot
    
    the.table
  })
  
  # additional data preview table
  output$addview <- renderTable({
    d.input <- dInputAdd()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview.add) 
      d.input <- d.input[,1:input$ncol.preview.add]
    head(d.input, n=input$nrow.preview.add) 
  })
  
  # Adapt available variables from second file
  updateAddPlotVar <- function() observe({
    if(is.null(current.addtable))
      return(NULL)
    updateSelectInput(session, "addplotvar",
                      choices= colnames(current.addtable), 
                      selected= colnames(current.addtable)[1])
  })
  
  # function to render Additional data Plot
  plotTheAdd <- function() observe({
    if(input$addplottype != "graph") {
      output$addplot <- renderPlot({
        plot(x= current.som, what= "add", type= input$addplottype, 
             variable= current.addtable[,input$addplotvar], 
             key.loc=c(-1,2), mar=c(0,10,2,0))
      })
    } else {
#      tmpGraph <- graph.adjacency(Matrix(as.matrix(server.env$current.addtable)), 
#                                  mode= "undirected")
      tmpGraph <- graph.adjacency(server.env$current.addtable!=0, 
                                  mode= "undirected")
      renderPlot(plot(lesmis.som, what= "add", type= "graph", 
                      variable= tmpGraph))
    }
    
  })
  
  #### Tab About
  ##############################################################################
  
  # Output the sombrero logo :
  output$sombrero.logo <- renderImage(list(src= "sombrero.png"), 
                                      deleteFile= FALSE)
  
  # Output the SAMM logo :
  output$samm.logo <- renderImage(list(src= "samm.png"), 
                                  deleteFile= FALSE)
  
})

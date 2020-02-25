###############################################################################
## Global variables

# Server
shinyServer(function(input, output, session) {

  #############################################################################
  ## Server variables
  #server.env <- environment() # used to allocate in functions
  #current.som <- NULL # this variable will contain the current SOM
  #current.table <- NULL
  
  RVserver.env <- reactiveValues(current.som = NULL) # used to allocate in functions
  
  #### Panel 'Self Organize' 
  ############################################################################## 

  ####### bsPanel 'Type of algorithm' 
  ############################################################################## 
  output$typealgo <- renderUI({
    if(input$somtype==""){
      text <- "1. Type of algorithm"
    } else {
      text <- paste("1. Type of algorithm, selected : ", input$somtype)
    }
    text
  })
  
  observeEvent(input$somtype, {
    if(input$somtype!=""){
      updateCollapse(session, "collapsestep1", open = "bscoll2")
    }
  })
  
  ####### bsPanel 'Data preparation'
  ##############################################################################

  output$texttypedata <- renderUI({
    switch(input$somtype,
           "numeric" = HTML("You have chosen the Numeric algorithm. Your data is expected to be a dataset with numeric variables. You can take <b>'iris'</b> as an example."),
           "korresp" = HTML("You have chosen the Korresp algorithm. Your data is expected to be a contengency table between two factors. You can take <b>'presidentielles2002'</b> as an example."),
           "relational" = HTML("You have chosen the numeric algorithm. Your data is expected to be a dissimilarity matrix, square, symetric and with a null diagonal. You can take <b>'dissim.lesmis'</b> as an example."),
           ""
    )
  })
  
  #### Panel 'Import data'
  ##############################################################################

  observe({
    if(length(dataframes)>0){
      updateSelectInput(session, inputId="file1envir", choices=dataframes)
    }
  })

  val <- reactiveValues(data=NULL)  
  
  observeEvent(input$loaddatabutton, {
    val$data <- get(input$file1envir, envir = .GlobalEnv)
  }, ignoreInit=T)
  
  observeEvent(c(input$file1, input$sep, input$quote, input$dec, input$header, input$rownames), {
    the.sep <- switch(input$sep, "Comma"=",", 
                      "Semicolon"=";", 
                      "Tab"="\t",
                      "Space"="")
    the.quote <- switch(input$quote, "None"="",
                        "Double Quote"='"',
                        "Single Quote"="'")
    the.dec <- switch(input$dec, 
                      "Period"=".", 
                      "Comma"=",")
    if (input$rownames) {
      the.table <- read.table(input$file1$datapath, header=input$header, 
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
    } else {
      the.table <- read.table(input$file1$datapath, header=input$header, 
                              sep=the.sep, quote=the.quote, dec=the.dec)
    }
    val$data <- the.table
  }, ignoreInit=T)
  
  output$dataready <- renderText({
    text <- "No data loaded"
    if(is.null(val$data)==F){
      text <- "Preview of the data"
    } 
    text
  })
  
  dInput <- reactive({
    if (is.null(val$data))
      return(NULL)
    
    the.table <- val$data
    
    # update the "input variables" checkbox (if somtype is numeric or integer)
    if (input$somtype =="numeric") {
      output$varchoice <- renderUI(
        selectInput(inputId="varchoice", label="Input variables:", multiple=T,
                           choices=as.list(colnames(the.table)),
                           selected=as.list(colnames(the.table)[
                             sapply(the.table, class) %in%
                               c("integer", "numeric")])))
      
        # checkboxGroupInput(inputId="varchoice", label="Input variables:",
        #                    choices=as.list(colnames(the.table)),
        #                    selected=as.list(colnames(the.table)[
        #                      sapply(the.table, class) %in%
        #                        c("integer", "numeric")])))
    } else output$varchoice <- renderText("")

    # update the map dimensions
    updateNumericInput(session, inputId="dimx", value={
      if (input$somtype =="korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })
    updateNumericInput(session, inputId="dimy", value={
      if (input$somtype =="korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })
    
    # update the max. iterations option
    updateNumericInput(session, "maxit", value=5 * nrow(the.table))

    # return the table
    the.table
  })
  
  # data preview table
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  }, rownames = TRUE, spacing='xs')

  output$missingrows <- renderText({
    shiny::validate(need(is.null(dInput())==F, 'Choose data'))
    nrowmissing <- nrow(dInput())-input$nrow.preview
    ncolmissing <- ncol(dInput())-input$ncol.preview
    if(nrowmissing>0 & ncolmissing<=0){
      text <- paste(nrowmissing, "rows not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing<=0 & ncolmissing>0){
      text <- paste(ncolmissing, "cols not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing>0 & ncolmissing>0){
      text <- paste(nrowmissing, "rows  and", ncolmissing, "columns not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else {
      text <- NULL
    }
    text
  })
  
  #### Panel 'Self-organize'
  #############################################################################

  observeEvent(input$showadvlink, {
    toggleElement(id='divadvancedoptions')
  })
  
  
  observe({
    # update the scaling option when input$somtype is changed
    updateSelectInput(session, inputId="scaling",  
                  choices=switch(input$somtype,
                                 "numeric"=c("unitvar", "none", "center"),
                                 "korresp"=c("chi2"),
                                 "relational"=c("none","cosine")),
                  selected=switch(input$somtype, "numeric"="unitvar",
                                  "korresp"="chi2", "relational"="none")
                  )
    
    # update the initialization method when input$somtype is changed
    updateSelectInput(session, "initproto", 
                      selected=switch(input$somtype, 
                                      "numeric"="random",
                                      "korresp"="random",
                                      "relational"="obs"))
  })

  # update the distance option when input$radiustype is changed
  observe({
    updateSelectInput(session, inputId="disttype", 
                choices=switch(input$radiustype,
                                "letremy"=c("letremy", "maximum", "euclidean",
                                             "manhattan", "canberra", "binary",
                                             "minkowski"),
                                "gaussian"=c("maximum", "euclidean", 
                                             "manhattan", "canberra", "binary",
                                             "minkowski")),
                selected=switch(input$radiustype, "letremy"="letremy",
                                 "gaussian"="euclidean"))
  })
  

  # Train the SOM when the button is hit
  observeEvent(input$trainbutton, {
    RVserver.env$current.som <- trainTheSom(dInput(), input$somtype, 
                                                    input$topo,
                                                    input$dimx, input$dimy, 
                                                    input$affectation,
                                                    input$disttype, input$maxit, 
                                                    varnames=input$varchoice, 
                                                    rand.seed=input$randseed, 
                                                    scaling=input$scaling, 
                                                    eps0=input$eps0, 
                                                    init.proto=input$initproto, 
                                                    nb.save=input$nb.save,
                                                    radiustype=input$radiustype)
      
      updatePlotSomVar() # update variable choice for som plots
      updatePlotScVar() # update variable choice for sc plots
  })


  # Render the summary of the SOM
  output$summary <- renderPrint({
    if (input$somtype=="") {
      return("Choose a type of algorithm.")
    }
    if (is.null(val$data))
      return("First import a dataset.")
    if (input$trainbutton==0) {
      return("Hit the Train button to train the map.")
    }
    if (is.null(RVserver.env$current.som)) {
      return("Hit the Train button to train the map.")
    }
    summary(RVserver.env$current.som)
  })

  observeEvent(RVserver.env$current.som, {
    if(is.null(RVserver.env$current.som)){
      shinyjs::disable("som.download")
      shinyjs::hide("nextplot")
    } else {
      shinyjs::enable("som.download")
      shinyjs::show("nextplot")
    }
  })
  
  # Output the computed som object to be downloaded
  # TODO: output an error if map not trained
  output$som.download <- {
    downloadHandler(filename=function() {
        paste0("som ",format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".rda",sep="")
      },
      content=function(file) {
        som.export <- RVserver.env$current.som
        save(som.export, file=file)
      })
  }
  
  observeEvent(RVserver.env$current.som$clustering, {
    if(is.null(RVserver.env$current.som$clustering)){
      shinyjs::disable("clustering.download")
    } else {
      shinyjs::enable("clustering.download")
    }
  })
  
  output$clustering.download <- {
    downloadHandler(filename = function() {
      paste0("clustering", format(Sys.time(), format="-%Y-%m-%d_%H:%M"),
             ".txt")
    },
    content = function(file) {
      som.export <- data.frame("name" = names(RVserver.env$current.som$clustering),
                               "cluster" = RVserver.env$current.som$clustering)
      write.table(som.export, file = file, row.names = FALSE, sep = "\t")
    })
  }
  
  observeEvent(input$nextplot, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "PlotMap")
    js$refocus()
  }) 
  #### Panel 'Plot'
  ##############################################################################
  
  # Adapt plottype to the somtype and the "what" arguments
  observe({
    updateSelectInput(session, "somplottype", 
                      choices=all.somplot.types[[input$somtype]][[
                        input$somplotwhat]])
  })
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    tmp.names <- colnames(RVserver.env$current.som$data)
    if (input$somtype =="korresp")
      tmp.names <- c(tmp.names, rownames(RVserver.env$current.som$data))
    updateSelectInput(session, "somplotvar", choices=tmp.names)
    updateSelectInput(session, "somplotvar2", choices=tmp.names, selected=tmp.names)
  })
  
  # Plot the SOM
  output$somplot <- renderPlot({
    if(is.null(dInput()))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    tmp.view <- NULL
    if (input$somtype =="korresp")
      tmp.view <- input$somplotrowcol

    if (input$somplotwhat =="energy") {
      plot(RVserver.env$current.som, what=input$somplotwhat)
    } else {
      tmp.var <- 1
      if(input$somplottype == 'color' || input$somplottype == '3d'){
        tmp.var <- input$somplotvar
      }
      if(input$somplottype == 'boxplot' || input$somplottype == 'barplot' || 
         input$somplottype == 'lines'  || input$somplottype == 'radar'){
        tmp.var <- input$somplotvar2
      }
      plot(x=RVserver.env$current.som, what=input$somplotwhat, type=input$somplottype,
                      variable=tmp.var, print.title=input$somplottitle,
                      view=tmp.view)
    }
  })
  
  #### Panel 'Superclass'
  ##############################################################################
  # Input number of superclasses or cutting height
  output$scHorK <- renderUI(
    switch(input$sc.cut.choice, 
           "Number of superclasses"=
             numericInput("sc.k", "Number of superclasses:", 2, min=1,
                          max=input$dimx*input$dimy-1), 
           "Height in dendrogram"=
             numericInput("sc.h", "Height in dendrogram:", 10, min=0))
  )

  # Compute superclasses when the button is hit
  computeSuperclasses <- reactive({
    if (is.null(dInput()))
      return(NULL)
    if (input$superclassbutton==0)
      return(NULL)
    
    isolate(switch(input$sc.cut.choice, 
                   "Number of superclasses"=
                     superClass(sommap=RVserver.env$current.som, k=input$sc.k),
                   "Height in dendrogram"=
                     superClass(sommap=RVserver.env$current.som, h=input$sc.h)))
  })

  output$sc.summary <- renderPrint({
    if (input$superclassbutton==0)
      return("Hit the 'Compute superclasses' button to see the results.")
    tmp.sc <- computeSuperclasses()
    summary(tmp.sc)
  })

  # Download the superclass classification
  # TODO: output an error if map not trained
  output$sc.download <- {
    downloadHandler(
      filename=function() {
        paste0("superclasses ", format(Sys.time(), format="%Y-%m-%d_%H:%M"),
              ".csv", sep="")
      },
      content=function(file) {
        the.sc <- computeSuperclasses()
        classes.export <- data.frame(obs=row.names(the.sc$som$data),
                                     cluster=the.sc$cluster[
                                       the.sc$som$clustering])
        write.csv(classes.export, file=file, row.names=FALSE)
      })
  }

  # Adapt scplottype to the somtype and the "what" arguments
  observe({
    updateSelectInput(session, "scplottype", 
                      choices=all.scplot.types[[input$somtype]][[
                        input$scplotwhat]])
  })
  
  # update variables available for plotting
  updatePlotScVar <- function() observe({
    tmp.names <- colnames(RVserver.env$current.som$data)
    if (input$somtype =="korresp")
      tmp.names <- c(tmp.names, rownames(RVserver.env$current.som$data))
    updateSelectInput(session, "scplotvar", choices=tmp.names)
    updateSelectInput(session, "scplotvar2", choices=tmp.names, 
                      selected=tmp.names[1:min(5,length(tmp.names))])
  })
  
  # Update SuperClass plot
  output$scplot <- renderPlot({
    if(is.null(dInput()))
      return(NULL)
    
    the.sc <- computeSuperclasses()
    if(input$superclassbutton ==0)
      return(NULL)

    if (input$scplottype == "dendrogram")
      return(plot(the.sc, type=input$scplottype))
    if (input$scplottype == "grid")
      return(plot(the.sc, type = input$scplottype, plot.legend = TRUE,
                  print.titles = TRUE))

    tmp.view <- NULL
    if (input$somtype =="korresp")
      tmp.view <- input$scplotrowcol
    
    if (input$scplottype =="radar") {
      plot(x=the.sc, what=input$scplotwhat, type=input$scplottype,
                  variable=input$scplotvar, view=tmp.view, key.loc=c(-1,2),
                  mar=c(0,10,2,0))
    } else { 
      if (input$scplottype =="boxplot") {
        tmp.var <- (1:ncol(RVserver.env$current.som$data))[colnames(RVserver.env$current.som$data) %in% 
                                                input$scplotvar2]
      } else tmp.var <- input$scplotvar
      
      plot(x = the.sc, what = input$scplotwhat, type = input$scplottype,
           variable = tmp.var, view = tmp.view, plot.legend = TRUE)
    }
  })

  #### Panel 'Combine with additional data'
  ##############################################################################
  
  # File input for additional variables
  dInputAdd <- reactive({
    in.file <- input$file2
    
    if (is.null(in.file))
      return(NULL)
    
    the.sep <- switch(input$sep2, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
    the.quote <- switch(input$quote2, "None"="","Double Quote"='"',
                        "Single Quote"="'")
    the.dec <- switch(input$dec2, "Period"=".", "Comma"=",")
    
    if (input$rownames2) {
      the.table <- read.table(in.file$datapath, header=input$header2, 
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
      som.export <- data.frame("name" = names(RVserver.env$current.som$clustering),
                               "cluster" = RVserver.env$current.som$clustering)
    } else the.table <- read.table(in.file$datapath, header=input$header2, 
                                   sep=the.sep, quote=the.quote, dec=the.dec)
    
    updateAddPlotVar() # update variable selector    
    
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
    d.input <- dInputAdd()
    if(is.null(d.input))
      return(NULL)
    updateSelectInput(session, "addplotvar", choices=colnames(d.input), 
                      selected=colnames(d.input)[1])
    updateSelectInput(session, "addplotvar2", choices=colnames(d.input), 
                      selected=colnames(d.input)[1:min(5,ncol(d.input))])
  })
  
  # function to render Additional data Plot
  output$addplot <- renderPlot({
    d.input <- dInputAdd()
    if (is.null(d.input)) return(NULL)
    
    if (input$addplottype %in% c("pie","color","names")) {
      tmp.var <- input$addplotvar
    } else tmp.var <- input$addplotvar2
    
    if(input$addplottype =="radar") {
      plot(x=RVserver.env$current.som, what="add", type=input$addplottype, 
           variable=d.input[,tmp.var], key.loc=c(-1,2), mar=c(0,10,2,0))
    } else if (input$addplottype !="graph") {
      plot(x=RVserver.env$current.som, what="add", type=input$addplottype, 
           variable=d.input[,tmp.var])
    } else {
      adjBin <- as.matrix(d.input!=0)
      tmpGraph <- graph.adjacency(adjBin, mode="undirected")
      plot(RVserver.env$current.som, what="add", type="graph", variable=tmpGraph)
    }
  })
})

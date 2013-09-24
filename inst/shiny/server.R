library(SOMbrero)

# SOM training function
train <- function(data, type, dimx, dimy, disttype, maxit) {
	trainSOM(data, dimension=c(dimx,dimy), dist.type= disttype, maxit= maxit, type= type)
}

# Server
shinyServer(function(input, output) {
  dInput <- reactive({
    in.file <- input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      read.table(in.file$datapath, header=input$header, sep=input$sep,
               quote=input$quote, row.names=1, dec=input$dec)
    } else {
      read.table(in.file$datapath, header=input$header, sep=input$sep,
                 quote=input$quote, dec=input$dec)
    }
  })

  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input)>input$ncol.preview) d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  })

	# This function trains the SOM when the button is hit
	the_som<- function() {
		input$trainbutton
		res<- isolate(train(dInput(), input$somtype, input$dimx, input$dimy, input$disttype, input$maxit))
		res
	}
  
	# This function renders the summary of the SOM
	output$summary <- renderPrint({
		if (input$trainbutton==0) return("Hit the Train button to train the map.")
		summary(the_som())
	})
})




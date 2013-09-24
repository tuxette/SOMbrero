# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("SOMbrero"),

  sidebarPanel(
		h2("Data importation"),
    
		selectInput('somtype', 'Type of SOM:', c(Numeric="numeric", Korresp="korresp",Relational="relational")), br(), br(), br(),
		fileInput('file1', 'Choose CSV/TXT File',
		          accept=c('text/csv', 'text/comma-separated-values,text/plain', "*.csv")),
		checkboxInput('header', ' Header?', TRUE),
    checkboxInput('rownames', ' Row names?', FALSE),
		selectInput('sep', 'Separator:',
                c(Comma=',',Semicolon=';',Tab='\t', Space=' '), 'Comma'),
		selectInput('quote', 'Quote:',
                c(None='','Double Quote'='"','Single Quote'="'"),
                 'Double Quote'),
		selectInput('dec', 'Decimal mark', c(Dot='.', Comma=','), 'Dot')
  ),

	mainPanel(
	  tabsetPanel(
	    tabPanel("Data preview",
	             h3("First step: import data"),
	             p(HTML("To run the application, import your data set using the 
                       import button on the left panel. Your data must
                       be supplied on the form of a text/csv file (if you want to run the numeric SOM or Korresp algorithms), [or ...]. If the importation is
                       done properly, a preview of the data is displayed below as a table or a graph.")),
			p("When this is done, you can proceed to the next step : Train SOM"),
			numericInput('nrow.preview', 'Number of rows in the preview:', 20),
			numericInput('ncol.preview', 'Number of columns in the preview:', 10),
	             tableOutput("view")
	    ),
      
	    tabPanel("Train SOM",
		h3("Second step: train the self-organizing map"),
		actionButton("trainbutton","Train SOM"),
		br(),
		br(),
		numericInput("dimx", "Map dimension X", 5, min= 1),
		numericInput("dimy", "Map dimension Y", 5, min= 1),
		numericInput("maxit", "Max. iterations:", 500),
		selectInput("disttype", "Distance type:", c("letremy"="letremy", "maximum"="maximum", "euclidean"="euclidean", "manhattan"="manhattan", "canberra"="canberra", "binary"= "binary", "minkowski"="minkowski")),

		verbatimTextOutput("summary")),
      
	    tabPanel("Plot"),
	    tabPanel("Superclass")
	))
))

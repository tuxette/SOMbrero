# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("SOMbrero"),

  sidebarPanel(
    h2("Data importation"),

    selectInput('somtype', 'Type of SOM:', c(Numeric="numeric", 
                                             Korresp="korresp",
                                             Relational="relational")), 
    br(), br(), br(),

    fileInput('file1', 'Choose CSV/TXT File'),#,
#              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    checkboxInput('header', ' Header?', TRUE),
    checkboxInput('rownames', ' Row names?', FALSE),
    selectInput('sep', 'Separator:', 
                c(Comma=',', Semicolon=';', Tab='\t', Space=' '),
                'Comma'),
    selectInput('quote', 'Quote:',  
                c(None='', 'Double Quote'='"', 'Single Quote'="'"), 
                'Double Quote'),
    selectInput('dec', 'Decimal mark', c(Period='.', Comma=','), 'Period')
  ),

  mainPanel(
    tabsetPanel(

      tabPanel("Data preview",
        h3("First step: import data"),
        p(HTML("To run the application, import your data set using the 
                import button on the left panel. Your data must be supplied 
                in the form of a text/csv file. If the importation is done 
                properly, a preview of the data is displayed below as a 
                table. When this is done, you can proceed to the next step: 
                Train SOM")),
        p(HTML('The interface can be tested using example data files for the
               <a href= "http://"> numeric</a>, <a href= "http://"> korresp</a> 
               and <a href= "http://"> relational </a> algorithms.')),
        numericInput('nrow.preview', 'Number of rows in the preview:', 20),
        numericInput('ncol.preview', 'Number of columns in the preview:', 10),
        helpText("Note: while the preview will show only the specified number 
                  of observations, the map will be based on the full dataset."),
        tableOutput("view")
      ),

      tabPanel("Train SOM",
        h3("Second step: train the self-organizing map"),
        p(HTML('<a href=help.html> Help page</a>: how to choose the right parameter
               values')),
        actionButton("trainbutton","Train SOM"),
        br(), br(),
        verbatimTextOutput("summary"),
        br(),
	downloadButton("som.download", "Download the SOM file"),
        br(), br(),
        checkboxGroupInput("varchoice", "Training variables:", choices= list("NoVariables")),
        numericInput("dimx", "Map dimension X", 5, min= 1),
        numericInput("dimy", "Map dimension Y", 5, min= 1),
        numericInput("maxit", "Max. iterations:", 500),
        selectInput("disttype", "Distance type:", 
                    c("letremy"="letremy", "maximum"="maximum",
                      "euclidean"="euclidean", "manhattan"="manhattan",
                      "canberra"="canberra", "binary"="binary",
                      "minkowski"="minkowski")),
        selectInput("scaling", "Data scaling:", 
                    c("unitvar", "none", "center", "chi2"), "unitvar")),

      tabPanel("Plot"),
      tabPanel("Superclass"),

      tabPanel("About",
        imageOutput("sombrero.logo", width= "200px", height= "200px"),
        p(HTML('This is the web interface of the 
               <a href= "http://sombrero.r-forge.r-project.org/"> 
               SOMbrero </a> R package, implementing self-organizing maps (aka
               Kohonen maps) for various types of data. It is developed by J.
               Boelaert, <a href= http://samm.univ-paris1.fr/-Madalina-Olteanu->
               M. Olteanu</a> and <a href= http://www.nathalievilla.org/> N. 
               Villa-Vialaneix</a>, using shiny, and its source code is freely 
               available on github:
               <br> <span style="font-size:12px;font-family:courrier;
               background-color:#FADDF2;border:1px solid black;">
               <font color="#870500"><b>git clone 
               https://github.com/tuxette/sombrero.git</b>
               </font></code></span>')),
        imageOutput("samm.logo", width= "100%", height= "100%"),
        p(HTML('It is kindly provided by the 
               <a href= "http://samm.univ-paris1.fr/"> SAMM </a> team under the
               <a href= "https://www.gnu.org/licenses/gpl-2.0.html"> GPL-2.0 
               </a> license.')),
	br(),
        h3('References:'),
	p(HTML('<li> <span style="font-variant: small-caps;">Kohonen T.</span> 
               (2001) <I>Self-Organizing Maps</I>. Berlin/Heidelberg: 
               Springer-Verlag, 3rd edition.</li>
               <li> <span style="font-variant: small-caps;">Cottrell M., Letremy
               P., Roy E.</span> (1993) Analyzing a 
               contingency table with Kohonen maps: a Factorial Correspondence 
               Analysis. In: <I>Proceedings of IWANN’93</I>, 
               <span style="font-variant: small-caps;">J. Cabestany,  
               J. Mary, A. Prieto</span> (Eds.), <I>Lecture Notes in Computer 
               Science, </I>Springer-Verlag, 305–311.</li>
               <li> <span style="font-variant: small-caps;">Olteanu M., 
               Villa-Vialaneix N., Cottrell M.</span> (2012) On-line 
               relational SOM for dissimilarity data. <I>Advances in 
               Self-Organizing Maps (Proceedings of WSOM 2012, Santiago, Chili, 
               12-14 decembre 2012)</I>, 
               <span style="font-variant: small-caps;">Estevez P., Principe J., 
               Zegers P., Barreto G.</span> (Eds.), <I>Advances in Intelligent 
               Systems and Computing series</I>, Berlin/Heidelberg: Springer 
               Verlag, 198, 13-22</li>'))
      )
    )
)))

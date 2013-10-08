# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("SOMbrero"),

  sidebarPanel(
    imageOutput("sombrero.logo", width= "200px", height= "200px"),
    p(HTML("<h5>Welcome to SOMbrero, the free online interface for 
           self-organizing (aka Kohonen) maps.</h5> Train a map on your data 
           and visualize their topology in three simple steps using 
           the tabs on the right.")),
    p(HTML('This page is the web interface of the 
           <a href= "http://sombrero.r-forge.r-project.org/"> 
           SOMbrero </a> R package, implementing self-organizing maps for 
           numeric, contingency, and relational data.')),
    imageOutput("samm.logo", width= "100%", height= "100%"),
    p(HTML('It is kindly provided by the
           <a href= "http://samm.univ-paris1.fr/"> SAMM </a> team under the
           <a href= "https://www.gnu.org/licenses/gpl-2.0.html"> GPL-2.0 
           </a> license, and was developed by Julien Boelaert, 
           <a href= http://samm.univ-paris1.fr/-Madalina-Olteanu->
           Madalina Olteanu</a> and <a href= http://www.nathalievilla.org/> 
           Nathalie Villa-Vialaneix</a>, using Shiny. Its source code is
           freely available on github:
           <br> <span style="font-size:12px;font-family:courrier;
           background-color:#FADDF2;border:1px solid black;">
           <font color="#870500"><b>git clone 
           https://github.com/tuxette/sombrero.git</b>
           </font></code></span>')),
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
  ),

  mainPanel(
    tabsetPanel(

      tabPanel("Import Data",
        h3("First step: import data"),
        selectInput('somtype', "Select the type of data you'd like to
                    analize:", c(Numeric="numeric",
                                 Korresp="korresp",
                                 Relational="relational")), 
        br(), br(),
        p(HTML("To run the application, import your data set using the 
                import button on the left panel. Your data must be supplied 
                in the form of a text/csv file. If the importation is done 
                properly, a preview of the data is displayed below as a 
                table. When this is done, you can proceed to the next step: 
                self-organize a map.")),
        p(HTML('The interface can be tested using example data files for the
               <a href= 
"http://owncloud.nathalievilla.org/apps/files_sharing/get.php?token=9d319bedf64098340ca9b094a527f70608e2d67c"> 
               numeric</a>, <a href= 
"http://owncloud.nathalievilla.org/apps/files_sharing/get.php?token=d824f59d7d934468de7f08b873f79d291aa129fd"> 
               korresp</a> and <a href= 
               "http://"> relational </a> algorithms.')),
               
        br(), 
        fileInput('file1', 'Choose CSV/TXT File'),
        checkboxInput('header', ' Header?', TRUE),
        checkboxInput('rownames', ' Row names?', FALSE),
        selectInput('sep', 'Separator:',
                    c(Comma=',', Semicolon=';', Tab='\t', Space=' '), 'Comma'),
        selectInput('quote', 'Quote:',  
                    c(None='', 'Double Quote'='"', 'Single Quote'="'"), 
                    'Double Quote'),
        selectInput('dec', 'Decimal mark', c(Period='.', Comma=','), 'Period'),
        numericInput('nrow.preview', 'Number of rows in the preview:', 20),
        numericInput('ncol.preview', 'Number of columns in the preview:', 10),
        helpText("Note: while the preview will show only the specified number 
                 of observations, the map will be based on the full dataset."),
        tableOutput("view")
      ),

      tabPanel("Self-Organize",
        h3("Second step: train the self-organizing map"),
        p("Once you have imported a dataset you can train a self-organizing ",
          "map, and tweak it using the options below. You can then download ",
          "the resulting map in .rda format (you will need R and the SOMbrero ",
          "package to open this file), or proceed to the next tabs to ",
          "visualize the results."),
        p(HTML('<a href=help.html> Help page</a>: how to choose the right 
               parameter values')),
        actionButton("trainbutton","Train SOM"),
        br(), br(),
        verbatimTextOutput("summary"),
        br(),
	downloadButton("som.download", "Download the SOM file"),
        br(), br(),
        h4("Options"),
        uiOutput("varchoice"),
        numericInput("dimx", "Map dimension X:", 5, min= 1),
        numericInput("dimy", "Map dimension Y:", 5, min= 1),
        h4("Advanced options"),
        numericInput("maxit", "Max. iterations:", 500),
        selectInput("disttype", "Distance type:", 
                    c("letremy"="letremy", "maximum"="maximum",
                      "euclidean"="euclidean", "manhattan"="manhattan",
                      "canberra"="canberra", "binary"="binary",
                      "minkowski"="minkowski")),
        uiOutput("scaling"),
        numericInput("randseed", "Set a random seed for reproducible results:",
                     sample(1:1e5, size= 1)),
        numericInput("eps0", "Epsilon0: (scaling value for gradient descent)", 
                     1, min= 0, step= .1),
        numericInput("nb.save", "Number of intermediate back-ups:", 0, min= 0), 
        selectInput("init.proto", "Prototypes initialization method:", 
                    choices= c("random","obs"), "random")),

      tabPanel("Plot Map",
        h3("Third step: plot the self-organizing map"),
        p("In this tab and the next ones you can visualize the computed ",
          "self-organizing map. This tab contains the standard plots used to ",
          "analyze the map and the variables used to train it."),
        h4("Options"),
        selectInput("somplotwhat", "Plot what?", 
                    choices= list("Observations"= "obs",
                                  "Prototypes"= "prototypes")),
        selectInput("somplottype", "Type of plot:", 
                    choices= c("hitmap", "color", "lines", "barplot", 
                               "names", "boxplot", "radar")),
        checkboxInput("somplottitle", "Show cluster names:"),
        conditionalPanel("input.somplottype != 'boxplot'",
                         selectInput("somplotvar", 
                                     "Variable: (only used for '3d', 'color' 
                                     and 'boxplot' plots if available)", 
                                     choices= "(Not Available)")),
        conditionalPanel("input.somplottype == 'boxplot'",
                         selectInput("somplotvar2", 
                                     "Variable: (hold Ctrl to select multiple
                                     variables)", 
                                     choices= "(Not Available)", 
                                     multiple= TRUE)),
        conditionalPanel("input.somtype == 'korresp'", 
                         selectInput("somplotrowcol", 
                                     "Plot rows or columns:",
                                     choices= list("columns"= "c", 
                                                   "rows"= "r"))),
        plotOutput("somplot")),
               
      tabPanel("Superclasses",
        h3("Group prototypes into superclasses"),
        p("In this tab you can group the prototypes found by the ",
          "self-organizing map into superclasses (using hierarchical ",
          "clustering), download the resulting clustering in csv format,",
          "and visualize the results on plots. The 'dendrogram'",
          "plot can help you determine the optimal number of superclasses."),
        selectInput("sc.cut.choice", "Choose clustering criterion:",
                    choices= c("Number of superclasses"= "nclust", 
                               "Height in dendrogram"=  "tree.height"),
                    "Number of superclasses"),
        uiOutput("scHorK"), #  Superclass Height or K (nb of clusters)
        actionButton("superclassbutton","Compute superclasses"),
        downloadButton("sc.download", "Download superclass classification"),
        br(), br(),
        verbatimTextOutput("sc.summary"),
#        plotOutput("dendrogram"),
        br(), br(),
         h4("Plot the superclasses:"),
         selectInput("scplotwhat", "Plot what?", 
                     choices= list("Prototypes"= "prototypes",
                                   "Observations"= "obs")),
         selectInput("scplottype", "Type of plot:", 
                     choices= c("hitmap", "color", "lines", "barplot", 
                                "names", "boxplot", "radar")),
        conditionalPanel("input.scplottype != 'boxplot'",
                         selectInput("scplotvar", 
                                     "Variable: (only used for '3d', 'color' 
                                     and 'boxplot' plots if available)",
                                     choices= "(Not Available)")),
        conditionalPanel("input.scplottype == 'boxplot'",
                         selectInput("scplotvar2", 
                                     "Variables: (hold Ctrl to select multiple  
                                     variables)", 
                                     choices= "(Not Available)", 
                                     multiple= TRUE)),
        conditionalPanel("input.somtype == 'korresp'", 
                         selectInput("scplotrowcol", 
                                     "Plot rows or columns:",
                                     choices= list("columns"= "c", 
                                                   "rows"= "r"))),
         plotOutput("scplot")),
      
      tabPanel("Combine with external information",
               h3("Combine the map with external information"),
               p("In this tab you can combine the self-organizing map with ",
                 "variables not used for the training. To do so you must first",
                 "import an additional file using the form below.",
                 "The file must either contain the same number of rows as the ",
                 "file used for training (in the same order), or a (square)",
                 "adjacency matrix for 'graph' plots."),

               h4("Import file for additional variables"),
               fileInput('file2', 'Choose csv/text file'),
               checkboxInput('header2', ' Header?', TRUE),
               checkboxInput('rownames2', ' Row names?', FALSE),
               selectInput('sep2', 'Separator:', 
                           c(Comma=',', Semicolon=';', Tab='\t', Space=' '),
                           'Comma'),
               selectInput('quote2', 'Quote:',  
                           c(None='', 'Double Quote'='"', 'Single Quote'="'"), 
                           'Double Quote'),
               selectInput('dec2', 'Decimal mark', c(Period='.', Comma=','),
                           'Period'),
               numericInput('nrow.preview.add', 
                            'Number of rows in the preview:', 20),
               numericInput('ncol.preview.add', 
                            'Number of columns in the preview:', 10),
               tableOutput("addview"),

               h4("Plot additional variables:"),
               conditionalPanel("input.somtype == 'korresp'",
                                p("Option not available for 'Korresp' type of",
                                  "SOM")),
               conditionalPanel("input.somtype != 'korresp'", 
                 selectInput("addplottype", "Type of plot:",
                             choices= c("pie", "color", "lines", "boxplot", 
                                        "barplot", "radar", "names", "words", 
                                        "graph")),
                 conditionalPanel("input.addplottype == 'pie' ||
                                   input.addplottype == 'color' ||
                                   input.addplottype == 'names'",
                                  selectInput("addplotvar", "Select variable:",
                                              choices= "(first import file)")),
                 conditionalPanel("input.addplottype != 'pie' &&
                                   input.addplottype != 'color' &&
                                   input.addplottype != 'names' && 
                                   input.addplottype != 'graph'",
                                  selectInput("addplotvar2", "Select variables:
                                               (hold Ctrl to select multiple
                                               variables)",
                                              choices= "(first import file)",
                                              multiple= TRUE)),
                 plotOutput("addplot")
                 )
      )
    )
)))

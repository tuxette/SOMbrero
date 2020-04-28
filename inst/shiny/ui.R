# Define UI for dataset viewer application
PAGE_TITLE <- "SOMbrero"
PAGE_TITLE_NAV <- div(
  img(
    src = "sombrero_cut.png",
    height = 50,
    style = "margin:0px 10px; padding: 0px 0px;"
  ),
  PAGE_TITLE
)
shinyUI(
  fluidPage(
    tags$style(HTML("
  		.navbar-brand {
      padding: 5px 5px;
  		}
      .navbar-default .navbar-nav > .active > a {
      background-color: #d4ceff;
      }
      ")),
  navbarPage(PAGE_TITLE_NAV,
    #"SOMbrero Web User Interface (v2.0)",
             windowTitle="SOMbrero",
             useShinyjs(),  # Set up shinyjs
             extendShinyjs(text = jscode, functions = "refocus"),
             id="tabs",
             selected="Intro",

  #### Panel 'Intro'
  ##############################################################################
  tabPanel("Intro",  value="Intro",
  #sidebarPanel(
  tags$table(
    tags$tr(
      tags$td(img(src = "sombrero.png",
                    height = 250)),
      tags$td(
        h2("SOMbrero Web User Interface (v1.2)"),
        br(),
        p(
        HTML(
          "<h5>Welcome to SOMbrero, the open-source on-line interface for
  self-organizing maps (SOM).</h5> This interface trains SOM for numerical data,
  contingency tables and dissimilarity data using the <strong>R</strong> package
  <a href='http://sombrero.r-forge.r-project.org/'>SOMbrero</a> (v1.2-3). Train a
  map on your data and visualize their topology in three simple steps using the
  panels on the right."
        )
      )
      )
    )
  ),
      br(),
      div(img(src = "samm.png",
              height = 100), 
          img(src = "miat.png",
              height = 100)),
      br(),
      br(),
      p(
        HTML(
          'It is kindly provided by the
  <a href= "http://samm.univ-paris1.fr/">SAMM</a> team and the
  <a href= "carlit.toulouse.inra.fr">MIA-T</a> team under the
  <a href= "https://www.gnu.org/licenses/gpl-2.0.html">GPL-2.0</a>
  license, and was developed by Julien Boelaert,
  <a href= http://samm.univ-paris1.fr/-Madalina-Olteanu->Madalina Olteanu</a> and
  <a href= http://www.nathalievialaneix.eu/> Nathalie Vialaneix</a>, using
  <a href="http://www.rstudio.com/shiny/">Shiny</a>. It is also included in the
  <strong>R</strong> package
  <a href="http://sombrero.r-forge.r-project.org/">SOMbrero</a>. Its source code
  is freely available on github: <br>
  <span style="font-size:10px;font-family:courrier; background-color:#FADDF2;
  border:1px solid black;"><font color="#870500"><b>
  git clone https://github.com/tuxette/sombrero.git</b></font></code></span>'
        )
      ),
      br(),
      
      h3('References:'),
      p(
        HTML(
          '<li> <span style="font-variant: small-caps;">Kohonen T.</span>(2001)
  <I>Self-Organizing Maps</I>. Berlin/Heidelberg: Springer-Verlag, 3rd edition.
  </li><li> <span style="font-variant: small-caps;">Cottrell M., Ibbou S., Letremy
  P.</span> (2004) SOM-based algorithms for qualitative variables. <em>Neural
  Networks</em>, <strong>17</strong>, 1149-1167.</li>
  <li> <span style="font-variant: small-caps;">Olteanu M., Villa-Vialaneix N.,
  Cottrell M.</span> (2015) On-line relational and multiple relational SOM.
  <em>Neurocomputing</em>, <strong>147</strong>, 15-30.</li>'
        )
      )
  ),
  
      #### Panel 'SOM'
      #########################################################################
      tabPanel("Self Organize", value="SelfOrganize",
        bsCollapse(id = "collapsestep1", multiple = F, open = "bscoll1",
        bsCollapsePanel(title = uiOutput("typealgo"), value="bscoll1", style="success",
          selectizeInput(
            'somtype',
            label = NULL,
            c(Numeric = "numeric",
              Korresp = "korresp",
              Relational = "relational"
            ),
            options = list(
              placeholder = 'Please select an option',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        bsCollapsePanel(title = HTML("2. Data preparation"), value="bscoll2", style="success",
          uiOutput("texttypedata"),
          br(),
          fluidRow(
            column(3,
              bsCollapse(open='envir',
                bsCollapsePanel(title=HTML('Choose from the environment/examples'), value="envir",
                                selectInput('file1envir', label=NULL, choices=dataframes),
                                actionButton("loaddatabutton", "Load", class="btn-primary")
                                )
                ,
                bsCollapsePanel(title=HTML('OR Choose CSV/TXT File'),
                                p(
                                  HTML(
                                    'The interface can be tested using example data files for
    the <a href=
    "http://nextcloud.nathalievilla.org/index.php/s/BWnWADSPxayGSGa"
    target="_blank">numeric</a>, <a href=
    "http://nextcloud.nathalievilla.org/index.php/s/Tw2H2ZBKwBAPo0v"
    target="_blank">korresp</a> and <a href=
    "http://nextcloud.nathalievilla.org/index.php/s/R2Vyt5Vkg3xlYPD"
    target="_blank">relational </a> algorithms (download these files on your computer and
    proceed).'
                                  )
                                ),
                                fileInput('file1', label=NULL),
                                checkboxInput('header', ' Header?', TRUE),
                                checkboxInput('rownames', ' Row names?', FALSE),
                                selectInput(
                                  'sep',
                                  'Separator:',
                                  c("Comma", "Semicolon", "Tab", "Space"),
                                  'Comma'
                                ),
                                selectInput(
                                  'quote',
                                  'Quote:',
                                  c("None","Double Quote","Single Quote"),
                                  'Double Quote'),
                                selectInput(
                                  'dec',
                                  'Decimal mark',
                                  c("Period", "Comma"),
                                  'Period')
                )
               )
          ),
          column(9,
             h4(textOutput("dataready")),
             conditionalPanel("output.dataready == 'Preview of the data'",
              #h4("Preview of the data"),
              fluidRow(
                column(6, numericInput('nrow.preview', 'Number of rows:', 10)),
                column(6, numericInput('ncol.preview', 'Number of columns:', 10))
              ),
              tableOutput("view"),
              textOutput("missingrows")
             )
          )
        )
      ),
      
      #### Panel 'Self-organize'
      #########################################################################2
      bsCollapsePanel(title = HTML("3. Train the self-organizing map"), value="bscoll3", style="success",
        p(
          HTML(
            "Once your dataset is loaded, you can train a
self-organizing map (SOM) and explore it. You can then
download the resulting SOM in .rda format (you will need <strong>R</strong> and
the package <a href= 'http://sombrero.r-forge.r-project.org/'>SOMbrero</a> to
open this file and use the SOM; its class is the 'somRes' class, handled by
<a href='http://sombrero.r-forge.r-project.org/'>SOMbrero</a>). You can also
explore it using the next panels to visualize the results, compute super-classes
or combine it with additional variables."
          )
        ),
        p(
          HTML(
            'Consult the "Help" panel for information on how to choose
adequate parameter values.'
          )
        ),

        hr(),

        fluidRow(
          column(3,
                radioButtons("topo", label="Topology", choices = c("square", "hexagonal"), selected="hexagonal"),
                HTML("<b>Map dimensions :</b>"),
                fluidRow(
                  column(2, HTML("<div style='padding-top:5px;'><b>X: </b></div>")
                         ),
                  column(10, numericInput(inputId = "dimx", label = NULL, 5, min = 1)
                        )
                ),
                fluidRow(
                  column(2, HTML("<div style='padding-top:1px;'><b>Y: </b></div>")
                  ),
                  column(10, numericInput(inputId = "dimy", label = NULL, 5, min = 1)
                  )
                ),
                uiOutput("varchoice"),
                #   )
                 #),
                 br(),
                 actionLink("showadvlink", "Show advanced options", icon=icon("gear")),
                 shinyjs::hidden(div(id="divadvancedoptions",
                            h4("Advanced options"),
                            fluidRow(
                              column(6, selectInput("affectation", "Affectation type:",
                                                    c("standard", "heskes")),
                                     selectInput("initproto", label="Prototypes initialization method:", choices=c("random","obs","pca")),
                                     numericInput("maxit", "Max. iterations:", 500),
                                     selectInput(inputId="disttype", label="Distance scaling:", choices=NULL),
                                     numericInput("randseed",
                                                  HTML(
                                                    "Random seed for reproducible results
                       <a href='#pseudor'><sup>(1)</sup></a>:"
                                                  ),sample(1:1e5, size = 1))
                              ),
                              column(6, selectInput("radiustype", "Radius type:",
                                                    c("letremy", "gaussian")),
                                     selectInput(inputId="scaling", label="Data scaling:", choices=NULL),
                                     numericInput("eps0",
                                                  "Scaling value for gradient descent",
                                                  1,
                                                  min = 0.01,
                                                  step = .01),
                                     numericInput("nb.save", "Number of intermediate back-ups:", 0,
                                                  min = 0)
                                     )
                              
                            ),
                            
                            br(),
                            p(
                              HTML(
                                "<span style='font-size:10px'><a name='pseudor'><sup>(1)
</sup></a> SOMbrero is based on a stochastic (on-line) version of the SOM
algorithm and thus uses randomness. Setting a seed results in fixing the random
procedure in order to obtain reproducible results (runing several times the
process with the same random seed will give the same map). More information on
pseudo-random generators at
<a href='http://en.wikipedia.org/wiki/Pseudorandom_number_generator'>this link
</a></span>."
                              )
                            )))
                 ),
          column(9,
                 disabled(actionButton("trainbutton", "Train SOM", class="btn-primary")),
                 hidden(actionButton("nextplot", "Next: plot the SOM map", class="btn-primary")),
                 br(),
                 br(),
                 verbatimTextOutput("runcodesom"),
                 verbatimTextOutput("summary"),
                 br(),
                 disabled(downloadButton("som.download", "Download the SOM file (rda)")),
                 disabled(downloadButton("clustering.download", "Download the clustering (txt)"))
                 )
        )
        
      )
      )
      ),
      
      #### Panel 'Plot'
      #########################################################################
      tabPanel("Plot Map", value="PlotMap",
        h3("Plot the self-organizing map"),
        p(
          "In this panel and the next ones you can visualize the computed
self-organizing map. This panel contains the standard plots used to analyze the
map."
        ),
        fluidRow(
          column(3, 
                 h4("Options"),
                 selectInput(
                   "somplotwhat",
                   "Plot what?",
                   choices = list(
                     "Observations" = "obs",
                     "Prototypes" = "prototypes",
                     "Energy" = "energy"
                   )
                 ),
                 selectInput(
                   "somplottype",
                   "Type of plot:",
                   choices = c(
                     "hitmap",
                     "color",
                     "lines",
                     "barplot",
                     "names",
                     "boxplot"
                   )
                 ),
                 conditionalPanel(
                   "input.somtype == 'korresp' && input.somplotwhat == 'prototypes'",
                   radioButtons(
                     "somplotrowcol",
                     "Plot rows or columns (when relevant for the chart):",
                     choices = list("columns" = "c", "rows" = "r"),
                     inline=F
                   )
                 ),
                 conditionalPanel(
                   "input.somplottype == 'color' || input.somplottype == '3d'",
                   selectInput(
                     "somplotvar",
                     "Variable:",
                     choices = "(Not Available)"
                   )
                 ),
                 conditionalPanel(
                   "input.somplottype == 'boxplot' || input.somplottype == 'barplot' || 
                   input.somplottype == 'lines' || input.somplottype == 'meanline'",
                   selectInput(
                     "somplotvar2",
                     "Variables:",
                     choices = "(Not Available)",
                     multiple = TRUE
                   )
                 ),
                 conditionalPanel(
                   "input.somplottype == '3d'",
                   sliderInput("theta", "theta (azimuthal direction)", min=0, max=180, value=0),
                   sliderInput("phi", "phi (colatitude)", min=0, max=180, value=15)
                 ),
                 checkboxInput("somplottitle", "Show cluster names", value = F),
                 checkboxInput("somplotlegend", "Hide the legend", value = F)
                 ),
          column(9,
                 verbatimTextOutput("runcodeplot"),
                 jqui_resizable(plotOutput("somplot", width = 750, height = 600))
                 )
        ),
        br(),
        br()
      ),
      
      ################################ Panel 'Superclasses'
      #########################################################################
      tabPanel("Superclasses", value="Superclasses",
               p(
                 "In this panel you can group the clusters into 'superclasses'
(using a hierarchical clustering on the neurons' prototypes), download the
resulting clustering in csv format and visualize it on charts. The 'dendrogram'
plot can help you determine the adequate number of superclasses."
               ),
               bsCollapse(id = "collapsesuperclass", multiple = F, open = "computesc",
                          bsCollapsePanel(title = "1. Group prototypes into superclasses", value="computesc", style="success",
                                          fluidRow(
                                            column(3,
                                                   selectInput("scmethod", "Method for hclust", choices=c("ward.D", "single", "complete", "average", "mcquitty","median","centroid" ), selected="complete"),
                                                   radioButtons(
                                                     "sc.cut.choice",
                                                     "Choose clustering criterion:",
                                                     choices = c("Number of superclasses",
                                                                 "Height in dendrogram"),
                                                     selected="Number of superclasses",
                                                     inline=F
                                                   ),
                                                   #  Superclass Height or K (nb of clusters)
                                                   uiOutput("scHorK"),
                                                   actionButton("superclassbutton", "Compute superclasses", class="btn-primary"),
                                                   br(),
                                                   br(),
                                                   shinyjs::hidden(actionButton("nextplotsc", "Next: plot the superclasses", class="btn-primary")),
                                                   br(),
                                                   br(),
                                                   disabled(downloadButton("sc.download", "Download superclass classification"))
                                            ),
                                            column(9,
                                                   jqui_resizable(plotOutput("somplotscdendro")),
                                                   verbatimTextOutput("runcodesc"),
                                                   verbatimTextOutput("sc.summary")
                                            )
                                          )
                                          ),
                          bsCollapsePanel(title = "2. Plot the superclasses", value="plotsc", style="success",
                                          fluidRow(
                                            column(3,
                                                   selectInput(
                                                     "scplotwhat",
                                                     "Plot what?",
                                                     choices = list("Prototypes" = "prototypes",
                                                                    "Observations" = "obs")
                                                   ),
                                                   selectInput(
                                                     "scplottype",
                                                     "Type of plot:",
                                                     choices = c(
                                                       "hitmap",
                                                       "color",
                                                       "lines",
                                                       "barplot",
                                                       "names",
                                                       "boxplot"
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     "input.scplottype == 'color'",
                                                     selectInput(
                                                       "scplotvar",
                                                       "Variable:",
                                                       choices = "(Not Available)"
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     "input.scplottype == 'boxplot' || input.scplottype == 'barplot' || 
                   input.scplottype == 'lines' || input.scplottype == 'meanline'",
                                                     selectInput(
                                                       "scplotvar2",
                                                       "Variables:",
                                                       choices = "(Not Available)",
                                                       multiple = TRUE
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     "input.somtype == 'korresp'",
                                                     radioButtons(
                                                       "scplotrowcol",
                                                       "Plot rows or columns (when relevant for the chart):",
                                                       choices = list("columns" = "c", "rows" = "r"),
                                                       inline=F
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     "input.scplottype == 'dendro3d'",
                                                     sliderInput(
                                                       "scangle3d",
                                                       "Angle between x and y axes", min = 10, max=170, value = 40
                                                     )
                                                   )
                                            ),
                                            column(9,
                                                   verbatimTextOutput("runcodescplot"),
                                                   jqui_resizable(plotOutput("scplot"))
                                            )
                                          ),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br()
                                          )
                          )
                          
      
        
      ),
      
      #### Panel 'external information'
      #########################################################################
      tabPanel("Combine with external information", value="Combine",
               p(
                 "In this panel you can combine the self-organizing map with
variables not used for the training. To do so, you must first import an
additional file using the form below. The file must either contains the same
number of rows as the file used for training (in the same order), or a (square)
adjacency matrix  for 'graph' plots (the adjacency matrix has a dimension equal
to the number of rows ."
               ),
               bsCollapse(id = "collapseadd", multiple = F, open = "loadadd",
                          bsCollapsePanel(title = "1. Load additional information", value="loadadd", style="success",
                                          fluidRow(
                                            column(3,
                                                   bsCollapse(open='unused',
                                                              bsCollapsePanel(title = HTML("From unused variales"), value="unused",
                                                                              selectInput('unusedvar', label=NULL, choices=c("No unused variable"=""), multiple = T),
                                                                              actionButton("loadunusedvarbutton", "Select", class="btn-primary")
                                                              ),
                                                              bsCollapsePanel(title = HTML("Choose from the environment/examples"), value="envir",
                                                                              selectInput('file2envir', label=NULL, choices=dataframes),
                                                                              actionButton("loaddatabuttonadd", "Load", class="btn-primary")
                                                              )
                                                              ,
                                                              bsCollapsePanel(title=HTML("OR Choose CSV/TXT File"),
                                                                              fileInput('file2', label=NULL),
                                                                              checkboxInput('header2', ' Header?', TRUE),
                                                                              checkboxInput('rownames2', ' Row names?', FALSE),
                                                                              selectInput(
                                                                                'sep2',
                                                                                'Separator:',
                                                                                c("Comma", "Semicolon", "Tab", "Space"),
                                                                                'Comma'
                                                                              ),
                                                                              selectInput(
                                                                                'quote2',
                                                                                'Quote:',
                                                                                c("None", "Double Quote", "Single Quote"),
                                                                                'Double Quote'
                                                                              ),
                                                                              selectInput('dec2', 'Decimal mark', c("Period", "Comma"),
                                                                                          'Period')
                                                              )
                                                   )
                                            ),
                                            column(9,
                                                   h4(textOutput("data2ready")),
                                                   conditionalPanel("output.data2ready == 'Preview of the data'",
                                                                    fluidRow(
                                                                      column(6, numericInput('nrow.preview.add', 'Number of rows:', 10)),
                                                                      column(6, numericInput('ncol.preview.add', 'Number of columns:', 10))
                                                                    ),
                                                                    tableOutput("addview"),
                                                                    textOutput("missingrowsadd")
                                                   )
                                            )
                                          )),
                          bsCollapsePanel(title = "2. Plot additional variables", value="plotadd", style="success",
                                          fluidRow(
                                            column(3,conditionalPanel(
                                              "input.somtype == 'korresp'",
                                              p("Option not available for 'Korresp' type of
                                  SOM")
                                            ),
                                            conditionalPanel(
                                              "input.somtype != 'korresp'",
                                              selectInput(
                                                "addplottype",
                                                "Type of plot:",
                                                choices = c(
                                                  "pie",
                                                  "color",
                                                  "lines",
                                                  "meanline",
                                                  "boxplot",
                                                  "barplot",
                                                  "names",
                                                  "words",
                                                  "graph"
                                                )
                                              ),
                                              conditionalPanel(
                                                "input.addplottype == 'pie' ||
                                   input.addplottype == 'color' ||
                                   input.addplottype == 'names'",
                                                selectInput("addplotvar", "Select variable:",
                                                            choices = "(first import file)")
                                              ),
                                              conditionalPanel(
                                                "input.addplottype != 'pie' &&
                                   input.addplottype != 'color' &&
                                   input.addplottype != 'names' &&
                                   input.addplottype != 'graph'",
                                                selectInput(
                                                  "addplotvar2",
                                                  "Select variables:",
                                                  choices = "(first import file)",
                                                  multiple = TRUE
                                                )
                                              )
                                            )),
                                            column(9,
                                                   verbatimTextOutput("runcodeaddplot"),
                                                   jqui_resizable(plotOutput("addplot", width = 750, height = 600))
                                            ),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                            )
               )
                          
  
       
        )
      ),
      tabPanel("Help",
               source(file="help_panel.R", local=T)$value
      )
    )
  )
)
#list(
#   p(
#     HTML(
#       "<h4>Topics:</h4>
# <a href= #somtype> Types of maps and data</a> <br />
# <a href= #importdata> Data importation</a> <br />
# <a href= #train> Training options</a> <br />
# <a href= #plots> Types of plots</a> <br />
# <a href= #superclasses> Grouping prototypes into superclasses</a> <br />
# <a href= #externalinfo> Combine with external information</a> <br />"
#     )
#   ),
bsCollapse(id = "collapsehelp", open="somtype",
           bsCollapsePanel(title = "Types of self-organizing maps", value="somtype", style="success",
                           p(
                             HTML(
                               "Different types of data require different types of maps
to analyze them. <a href='http://sombrero.r-forge.r-project.org/'>SOMbrero</a>
offers three types of algorithms, all based on the on-line (as opposed to batch)
SOM:
<ul>
<li><b>Numeric</b> is the standard self-organizing map, which uses numeric
variables only. The data is expected to contains variables in columns and observations 
in rows. <br /> It can be applied, for instance,
to the four first variables of the <a href=
'http://nextcloud.nathalievilla.org/index.php/s/BWnWADSPxayGSGa'
>iris dataset</a>.</li>

<li><b>Korresp</b> applies the self-organizing algorithm to contingency tables
between two factors.<br /> For instance, in the supplied <a href=
'http://nextcloud.nathalievilla.org/index.php/s/Tw2H2ZBKwBAPo0v'
>dataset 'presidentielles 2002'</a>, which contains the results for the first
round of the 2002 French prensidential elections, columns represent presidential
candidates and rows represent the French districts called 'departements', so
that each cell contains the number of votes for a specific candidate in a
specific 'departement'.</li>

<li><b>Relational</b> is used for dissimilarity matrices, in which each cell
contains a measure of distance between two objects. For this method the data
must be a square numeric matrix, in which rows  and columns represent the same
observations. The matrix must be symetric, contains only positive entries with a
null diagonal.<br /> 
For instance, the supplied <a href='http://nextcloud.nathalievilla.org/index.php/s/R2Vyt5Vkg3xlYPD'
>dataset 'Les Miserables'</a> contains the shortest path lengths between
characters of Victor Hugo's novel <I>Les Misérables</I> in the co-appearance
network provided <a href=
'http://www-personal.umich.edu/~mejn/netdata/lesmis.zip'>here</a>.</li></ul>"
                             )
                           )),
           bsCollapsePanel(title = "Data loading", value="importdata", style="success",
                           
                           p(HTML("You can choose the data among your current environment 
                           datasets (in the class data.frame or matrix). The three examples datasets
                           are automatically loaded so you can try the methods.")),
                           p(
                             HTML(
                               "Data also can be imported as a table, in text or csv format
(columns are separated by specific symbols such as spaces or semicolons (option
'Separator'). Row names can be included in the data set for better post-analyses
of the data (some of the plots will use these names). Check at the bottom of the
'Import Data' panel to see if the data have been properly imported. If not,
change the file importation options."
                             )
                           )),
           bsCollapsePanel(title = "Training options", value="train", style="success",
                           p(
                             HTML(
                               "The default options in the 'Self-Organize' panel are set
according to the type of map and the size of the dataset, but you can modify the
options at will:

<ul>
<li><b>Topology:</b> choose the topology of the map (squared or hexagonal).</li>

<li><b>Input variables:</b> choose on which variables the map will be trained.</li>

<li><b>Map dimensions:</b> choose the number of rows (X) and columns (Y) of the
map, thus setting the number of prototypes. An advice value is to use the
square root of one tenth the number of observations.</li>

<li><b>Affectation type:</b> type of affectation used during the training.
Default type is 'standard', which corresponds to a hard affectation, and
the alternative is 'heskes', which is Heskes's soft affectation (see Heskes
(1999) for further details).</li>

<li><b>Max. iterations:</b> the number of iterations of the training process.
Default is five times the number of observations.</li>

<li><b>Distance type:</b> type of distance used to determine which prototypes of
the map are neighbors. Default type is 'Letremy' that was originally proposed in
the <a href='http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme'>SAS
programs</a> by <a href='http://samm.univ-paris1.fr/-Patrick-Letremy-'> Patrick
Letremy</a> but all methods for 'dist' are also available.</li>

<li><b>Radius type:</b> neighborhood type used to determine which prototypes of
the map are neighbors. Default type is 'letremy' as originally
implemented in the <a
href='http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme'>SAS
programs</a> by <a href='http://samm.univ-paris1.fr/-Patrick-Letremy-'> Patrick
Letremy</a> (it combines square and star-like neighborhoods along the learning)
but a Gaussian neighborhood can also be computed.</li>

<li><b>Data scaling:</b> choose how the data must be scaled before training.
Scaling is used to ensure all variables have the same importance during
training, regardless of absolute magnitude. 'none' means no scaling,
'center' means variables are shifted to have 0 mean, 'unitvar' means variables
are centered and divided by their standard deviation to ensure they all have
unit variance and '&chi;<sup>2</sup>' is used by the 'Korresp' algorithm and
'cosine' is the dissimilarity transposition of the 'cosine' transformation
performed on kernels.</li>

<li><b>Random seed:</b> Set the seed for the pseudorandom number generator used
during the training. Be sure to remember the seed used for a training if you
want to reproduce your results exactly, because running the algorithm with
different seeds will result in different maps. By default the seed is itself set
randomly when the interface is launched.</li>

<li><b>Scaling value for gradient descent:</b> This is the 'step size' parameter
used during training to determine in what proportion a winning prototype is
shifted towards an observation.</li>

<li><b>Number of intermediate backups:</b> Number of times during training a
backup of the intermediate states of the map is saved. This can be used to
monitor the progress of the training. If no backup (values 0 or 1) is saved,
the energy plot is not available.</li>

<li><b>Prototype initialization method:</b> choose how the prototypes of the map
are initialized at the beginning of training algorithm.<br /> If 'random' is
chosen, prototypes will be given random values in the range of the data. If
'obs', each prototype will be initialized to a random observation in the data.
If 'pca', prototypes are chosen along the first two PC of a PCA. Advised values
are 'random' for the 'Numeric' and the 'Korresp' algorithm and 'obs' for the
'Relational' algorithm.</li></ul>"
                             )
                           )),
           bsCollapsePanel(title = "Types of plots", value="plots", style="success",
                           p(
                             HTML(
                               "Sombrero offers many different plots to analyze your
data's topology using the self-organizing map. <br />There are two main choices
of what to plot (in the plot and superclass panels): plotting <b> prototypes
</b> uses the values of the neurons' prototypes of the map, which are the
representative vectors of the clusters. Plotting <b> observations </b> uses the
values of the observations within each cluster."
                             )
                           ),
                           p(
                             HTML(
                               "These are the standard types of plots:

<ul>
<li><b>hitmap:</b> represents circles having size proportional to the number
of observations per neuron.</li>

<li><b>color:</b> Neurons are filled with colors according to the prototype
value or the average value level of the observations for a chosen variable.</li>

<li><b>3d:</b> similar to the ‘color’ plot, but in 3-dimensions, with x and y
the coordinates of the grid and z the value of the prototypes or observations
for the considered variable.</li>

<li><b>boxplot:</b>  plots boxplots for the observations in every neuron.</li>

<li><b>lines:</b>  plots, for each neuron, the prototype value or the average
value level of the observations, with lines. Each point on the line represents a
variable. </li>

<li><b>barplot:</b>  similar to lines, here each variable value is represented
by a bar. </li>

<li><b>names:</b>  prints on the grid the names of the observations in the
neuron to which they belong. <strong>Warning!</strong> If the number of
observations or the size of the names is too large, some names may not be
representedthey are reprensented in the center of the neuron, with a warning.</li>

<li><b>poly.dist:</b> represents the distances between prototypes with polygons
plotted for each neuron. The closer the polygon point is to the border, the
closer the pairs of prototypes. The color used for filling the polygon shows the
number of observations in each neuron. A red polygon means a high number of
observations, a white polygon means there are no observations.</li>

<li><b>smooth.dist:</b>  depicts the average distance between a prototype and
its neighbors using smooth color changes, on a map where x and y are the
coordinates of the prototypes on the grid. If the topology is hexagonal, 
linear interpolation is done between neuron coordinates to get a full 
squared grid.</li>

<li><b>umatrix:</b>  is another way of plotting distances between prototypes.
The grid is plotted and filled colors according to the mean distance between the
current neuronand the neighboring neurons. Red indicates proximity.</li>

<li><b>grid.dist:</b> plots all distances on a two-dimensional map. The number
of points on this picture is equal to: <br>
<code>number_of_neurons * (number_of_neurons-1) / 2</code><br>The x axis
corresponds to the prototype distances, the y axis depicts the grid distances.
</li>

<li><b>MDS:</b> plots the number of the neurons according to a Multi Dimensional
Scaling (MDS) projection on a two-dimensional space.</li></ul>"
                             )
                           ),
                           p(
                             HTML(
                               "The plot options offered in the 'superclasses' and
'combine with  external information' panels are mostly the same as the ones
listed above, but some are specific:
<ul>
<li><b>grid:</b>  (in Superclasses panel) plots the grid of the neurons, grouped by
superclasses (color).</li>

<li><b>dendrogram:</b>  (in Superclasses panel) plots the dendrogram of the
hierarchical clustering applied to the prototypes, along with the scree plot
which shows the proportion of unexplained variance for incremental numbers of
superclasses. These are helpful in determining the optimal number of
superclasses.</li>

<li><b>dendro3d:</b>  (in Superclasses panel) similar to 'dendrogram', but in
three dimensions and without the scree plot.</li>

<li><b>pie:</b>  (in Combine panel) requires the selected variable to be a
categorical variable, and plots one pie for each neuron, corresponding to the
values of this variable.</li>

<li><b>words:</b>  (in Combine panel) needs the external data to be a
contingency table or numerical values: names of the columns will be 
used as words and printed on the map with sizes proportional to the sum of values
in the neuron.</li>

<li><b>graph:</b>  (in Combine panel) needs the external data to be the
adjacency matrix of a graph. According to the existing edges in the graph and to
the clustering obtained with the SOM algorithm, a clustered graph is built in
which vertices represent neurons and edge are weighted by the number of edges in
the given graph between the vertices affected to the corresponding neurons. This
plot can be tested with the supplied dataset <a href=
'http://nextcloud.nathalievilla.org/index.php/s/R2Vyt5Vkg3xlYPD'
>Les Miserables</a> that
corresponds to the graph those adjacency table is provided at <a href=
'http://nextcloud.nathalievilla.org/index.php/s/R2Vyt5Vkg3xlYPD'
>this link</a>.
</li></ul>")
                           ),
                           p(
                             HTML(
                               "The <b>show cluster names</b> option in the 'Plot map'
panel can be selected to show the names of the neurons on the map."
                             )
                           ),
                           p(
                             HTML(
                               "The <b>energy</b> option in the 'Plot map' panel is used
to plot the energy levels of the intermediate backups recorded during training.
This is helpful in determining whether the algorithm did converge. (This option
only works if a 'Number of intermediate backups' larger than 2 is chosen in the
'Self-Organize' panel.)"
                             )
                           )),
           
           bsCollapsePanel(title = "Grouping prototypes into Superclasses", 
                           value="superclasses", style="success",
                           
                           p(
                             HTML(
                               "Use the options on the dedicated panel to group the
prototypes of a trained map into a determined number of superclasses, using
hierarchical clustering. The 'dendrogram' plot can help you to choose a relevant
number of superclasses (or equivalently a relevant cutting height in the
dendrogram)."
                             )
                           )),
           
           bsCollapsePanel(title = "Combine with external information", 
                           value="externalinfo", style="success",
                           p(
                             HTML(
                               "Plot external data on the trained map on the dedicated
panel. The external data importation process is similar to the one described in
the <a href='#importdata'>'Data importation'</a> section, and the available
plots are described in the <a href='#plots'>'types of plots'</a> section.<br />
Note that this is the only panel in which factors can be plotted on the
self-organizing map. For instance, if the map is trained on the first four
(numeric) variables of the supplied <a href=
'http://nextcloud.nathalievilla.org/index.php/s/BWnWADSPxayGSGa'
>iris dataset</a>, you can import the dataset again as external information and
plot the iris species on the map."
                             )
                           ))
)
#)
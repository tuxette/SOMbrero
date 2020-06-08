## A single function to launch shiny GUI
## ----------------------------------------------------------------------------

#' @title Graphical Web User Interface for SOMbrero
#' @name sombreroGUI
#' @export
#' 
#' @description Start the SOMbrero GUI.
#' 
#' @return This function starts the graphical user interface with the default 
#' system browser. This interface is more lickely to work properly with Firefox
#' \url{http://www.mozilla.org/fr/firefox/new/}. In case Firefox is not your 
#' default browser, copy/paste http://localhost:8100 into the URL bar.
#' 
#' @author Élise Maigné <elise.maigne@inrae.fr>\cr
#' Julien Boelaert \email{julien.boelaert@gmail.com}\cr
#' Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @references 
#' Villa-Vialaneix N. (2017) Stochastic self-organizing map variants with the R
#' package SOMbrero. In: J.C. Lamirel, M. Cottrell, M. Olteanu, \emph{12th 
#' International Workshop on Self-Organizing Maps and Learning Vector 
#' Quantization, Clustering and Data Visualization (Proceedings of WSOM 2017)},
#' IEEE, Nancy, France.
#' 
#' RStudio and Inc. (2013). shiny: Web Application Framework for R. R package
#' version 0.7.0. \url{https://cran.r-project.org/package=shiny}

sombreroGUI <- function() {
  
  if (all(requireNamespace("shinycssloaders", quietly = TRUE),
          requireNamespace("shinyBS", quietly = TRUE), 
          requireNamespace("shinyjs", quietly = TRUE), 
          requireNamespace("shinyjqui", quietly = TRUE))) {
    shiny::runApp(system.file('shiny', package = 'SOMbrero'))
  } else {
    stop("The packages 'shinycssloaders', 'shinyBS', 'shinyjs' and 'shinyjqui' are required to launch the graphical interface.",
         call. = TRUE)
  }
  
}

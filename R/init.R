.onAttach <- function(libname, pkgname){
  packageStartupMessage("")
  packageStartupMessage("***********************************************************")
  packageStartupMessage("")
  packageStartupMessage("      This is 'SOMbrero' package, v ", packageVersion("SOMbrero"))
  packageStartupMessage("")
  packageStartupMessage("Citation details with citation('SOMbrero')")
  packageStartupMessage("")
  packageStartupMessage("Further information with help(SOMbrero)...")
  packageStartupMessage("")
  packageStartupMessage("Use sombreroGUI() to start the Graphical Interface.")
  packageStartupMessage("")
  packageStartupMessage("***********************************************************")
}

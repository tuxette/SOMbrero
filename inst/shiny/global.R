# data.frames in the global environment
# tibbles and data.tables are also returned since they are in the data.frame class
data("iris")
data("lesmis")
data("presidentielles2002")

dataframes <- ls()[sapply(ls(envir = .GlobalEnv), function(x) 'data.frame' %in% class(get(x)) | 'matrix' %in% class(get(x)))]

 

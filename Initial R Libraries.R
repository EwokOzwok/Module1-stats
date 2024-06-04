install.packages(c("reticulate", "psych", "dplyr", "caret", "caTools", "finalfit"), dependencies=T)


reticulate::conda_create(envname = "mlbase", 
                         packages = c("scikit-learn", "matplotlib", "seaborn", "tensorflow"), 
                         pip = T, 
                         python_version = '3.8')

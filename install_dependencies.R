
packages <- c("shiny", "shinyWidgets", "shinyjs",
              "irr", "GenBinomApps", "psych",
              "MazamaCoreUtils", 
              "R6", "xml2",
              "WordR", "flextable", "officer", "tidyverse")

install.packages(setdiff(packages, rownames(installed.packages())))  

# survivalContourRshiny
R shinyapp to plot predicted survival with colored contour plot using various methods

To access it in R, please type the following 
```
install_missing_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
}

packages_to_check <- c("shiny","riskRegression","prodlim","survival","intccr","viridis",
                       "zip","mets","magick","fields","spatstat.utils","rhandsontable",
                       "plotly","jsonlite","flexsurv","rlist","dplyr","randomForestSRC")

install_missing_packages(packages_to_check)
runGitHub( "survivalContourRshiny","YushuShi")
```

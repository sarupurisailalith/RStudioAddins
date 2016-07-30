# RStudioAddins
This is a package with list of R studio Addins.

* Statistical model diagnostic tools:
    + Linear regression 
    + K-means clustering
    
* Data frame operations:
    + Merge two data frames - *creates a new data frame object after merging*

## Installation

Make sure you have the latest version of [devtools](https://github.com/hadley/devtools), [htmltools](https://github.com/rstudio/htmltools), [shiny](https://github.com/rstudio/shiny) and [miniUI](https://github.com/rstudio/miniUI); then install this package

```r
devtools::install_github("sarupurisailalith/RStudioAddins")
```
Once the package is installed, addins will be avaiable under the 'Addins' menu in RStudio. 

An other way to launch via executing command in the console is as follows:
```r
RStudioAddins::lm_diagnostics()
```

#### Additional packages required to run addins:

* Linear regression model diagnostics: [plotly](https://cran.r-project.org/web/packages/plotly/index.html)
* K-means clustering model diagnostics:
[cluster](https://cran.r-project.org/web/packages/cluster/index.html),
[clValid](https://cran.r-project.org/web/packages/clValid/index.html)




*Update 1:* Added linear model diagnostics, k-means clustering diagnostics
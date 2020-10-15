![R CMD check](https://github.com/mmahmoudian/sivs/workflows/R%20CMD%20check/badge.svg)
![R CMD check --as-cran](https://github.com/mmahmoudian/sivs/workflows/R%20CMD%20check%20--as-cran/badge.svg)
![download per month](https://cranlogs.r-pkg.org/badges/sivs)

#  Stable Iterative Variable Selection (SIVS) <img src="misc/img/SIVS_logo.png" width="140" align="right" />

> **The manuscript of this work is under journal review**

SIVS is an acronym of Stable Iterative Variable Selection, and as the name suggests is a feature selection method that is robust to the variations that cross-validation can have on various methods with embedded feature selection. This method hired an iterative approach and  internally utilizes varius Machine Learning methods which have embedded feature reduction in order to shrink down the feature space into a small and yet robust set. 


## Installation

You can download and install the latest stable version from CRAN via:

```r
install.packages("sivs", repos = "https://cran.rstudio.com")
```

Alternatively, you can install it directly from github via either of the following:

```r
### First Approach
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mmahmoudian/sivs")
```

```r
### Second Approach
if (!require("remotes")) install.packages("remotes")
remotes::install_github('mmahmoudian/sivs')
```

## References

TBA

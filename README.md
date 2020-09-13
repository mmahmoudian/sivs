# sivs

> **The manuscript oof this work is under journal review**

SIVS is an acronym of Seed Independent Variable Selection, and as the name suggests is a feature selection method that is robust to the variations that cross-validation can have on various methods with embedded feature selection. This method hired an iterative approach and  internally utilizes varius Machine Learning methods which have embedded feature reduction in order to shrink down the feature space into a small and yet robust set. 


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
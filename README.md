<div align="center">
<p>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/sivs)](https://cran.r-project.org/package=sivs "Latest version in CRAN")
[![CRAN Monthly Downloads](https://cranlogs.r-pkg.org/badges/sivs)](https://cran.rstudio.com/web/packages/sivs/index.html "CRAN Monthly Downloads")
[![CRAN Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/sivs)](https://cran.rstudio.com/web/packages/sivs/index.html "CRAN Total Downloads")
[![Software Heritage Archived Badge](https://archive.softwareheritage.org/badge/origin/https://github.com/mmahmoudian/sivs/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https%3A%2F%2Fgithub.com%2Fmmahmoudian%2Fsivs)

</p>
<p>

<!--
![R CMD check](https://github.com/mmahmoudian/sivs/workflows/R%20CMD%20check/badge.svg)
![R CMD check --as-cran](https://github.com/mmahmoudian/sivs/workflows/R%20CMD%20check%20--as-cran/badge.svg)
-->
</p>
</div>

#  Stable Iterative Variable Selection (SIVS) <img src="misc/img/sivs_logo2.png" width="200" align="right" />

SIVS is an acronym of Stable Iterative Variable Selection, and as the name suggests is a feature selection method that is robust to the variations that cross-validation can have on various methods with embedded feature selection. This method hired an iterative approach and  internally utilizes varius Machine Learning methods which have embedded feature reduction in order to shrink down the feature space into a small and yet robust set.

For citation information, see the [citation section](#citation) of this document.

-------

**Table of Content**

- [Installation](#installation)
- [Quick start](#quick-start)
- [Building From Source](#building-from-source)
- [Contribution](#contribution)
- [Citation](#citation)

-------

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

Additionally, this package is also available via various package managers in Linux:


[![Packaging status](https://repology.org/badge/vertical-allrepos/r:sivs.svg)](https://repology.org/project/r:sivs/versions)


## Quick Start

There is already [a very good vignette](https://cran.r-project.org/web/packages/sivs/vignettes/vignette_simple_usage.html) that explains how `sivs` should be used and I strongly encourage you to read it, but if you need a very very short set of instructions to kick-start, here it is:

In this example let's assume you have two classes (e.g dead vs. alive) that you want to know which minimal set of features can be used to differentiate between them:

1. Prepare your data
   - have features as columns, and samples as rows
   - normalize, impute missing values, etc. as you see fit
   - if you have multiple samples from the same individual, do your best that you have the same number of samples per individual (e.g by randomly choos them) to prevent giving extra weight to specific individual
2. Run `sivs::sivs()`
    ```r
    require("sivs")
    sivs_obj <- sivs::sivs(x = data, y = class)
    ```
3. get the variable (a.k.a feature) importance. This only contains features that have importance more than 0:
    ```r
    sivs_obj$vimp
    ```
4. [optional] shrink the feature list even more. This will only return an ordered list of features which is typicaly much smaller than `vimp`:
   ```r
   sivs_suggested_features <- sivs::suggest(sivs_obj)
   ```
5. Now you can only use these features in your machine learning:
   ```r
   smaller_data <- data[, sivs_suggested_features]
   ```


## Building From Source

You can also build this package completelty from source and you are expected to get identical files as in CRAN. This can be useful for those who want to contribute to the package. I have made it easy and straight-forward to build and test the package using the [GNU make](https://www.gnu.org/software/make/). Follow these steps in order:

0. First make sure you have the `make` installed and the package building dependencies
   ```sh
   # if make is installed, you will see the version
   make --version
   
   # this will show you a general help of available commands
   make help
   
   # this will check if you have the needed R packages and if not, it will install them for you
   make deps
   ```
1. Change the code and files as needed
2. if you have changed the R code and want to test it, you can build the R code and skip building the manual and vignette:
   ```sh
   make build-noman
   ```
   if you have changed the manual:
   ```sh
   make docs
   make build
   ```
3. install the package and make sure things are in order and working as you expected:
   ```sh
   make install
   ```
4. When you confirmed that everything is in order, repeat all the building steps with CRAN checking:
   ```sh
   make docs build check-cran install
   ```
   Alternatively you can run the following which is short-form for the command above:
   ```sh
   make all-cran
   ```


## Contribution

This is a [Free and Libré OpenSource Software (FLOSS)](https://en.wikipedia.org/wiki/Free_and_open-source_software) and therefore any contribution is welcome as long as it does not violate [the license](https://github.com/mmahmoudian/sivs/blob/master/LICENSE). To contribute, follow the steps in the [Building From Source](#building-from-source) and then before creating the pull-request, make sure you have solved all ERRORs, WARNINGs and possibly all NOTEs produced by the following:

```sh
make all-cran
make check-cran
```

## Citation

This method has been published in the journal of Bioinformatics:

**Mehrad Mahmoudian**, Mikko S Venäläinen, Riku Klén, Laura L Elo, Stable Iterative Variable Selection, Bioinformatics, 2021;, btab501, **<https://doi.org/10.1093/bioinformatics/btab501>**

BibTeX entry for LaTeX users:

```bib
@article{10.1093/bioinformatics/btab501,
    author = {Mahmoudian, Mehrad and Venäläinen, Mikko S and Klén, Riku and Elo, Laura L},
    title = "{Stable Iterative Variable Selection}",
    journal = {Bioinformatics},
    year = {2021},
    month = {07},
    abstract = "{The emergence of datasets with tens of thousands of features, such as high-throughput omics biomedical data, highlights the importance of reducing the feature space into a distilled subset that can truly capture the signal for research and industry by aiding in finding more effective biomarkers for the question in hand. A good feature set also facilitates building robust predictive models with improved interpretability and convergence of the applied method due to the smaller feature space.Here, we present a robust feature selection method named Stable Iterative Variable Selection (SIVS) and assess its performance over both omics and clinical data types. As a performance assessment metric, we compared the number and goodness of the selected feature using SIVS to those selected by LASSO regression. The results suggested that the feature space selected by SIVS was, on average, 41\\% smaller, without having a negative effect on the model performance. A similar result was observed for comparison with Boruta and Caret RFE.The method is implemented as an R package under GNU General Public License v3.0 and is accessible via Comprehensive R Archive Network (CRAN) via https://cran.r-project.org/web/packages/sivs/index.html or through Github via https://github.com/mmahmoudian/sivs/Supplementary data are available at Bioinformatics online.}",
    issn = {1367-4803},
    doi = {10.1093/bioinformatics/btab501},
    url = {https://doi.org/10.1093/bioinformatics/btab501},
    note = {btab501},
    eprint = {https://academic.oup.com/bioinformatics/advance-article-pdf/doi/10.1093/bioinformatics/btab501/39070854/btab501.pdf},
}
```

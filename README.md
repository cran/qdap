qdap
====

[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![Build Status](https://travis-ci.org/trinker/qdap.svg?branch=master)](https://travis-ci.org/trinker/qdap) [![DOI](https://zenodo.org/badge/5398/trinker/qdap.svg)](http://dx.doi.org/10.5281/zenodo.11124)
[![](http://cranlogs.r-pkg.org/badges/qdap)](https://cran.r-project.org/package=qdap)


[qdap](http://trinker.github.com/qdap_dev/) (Quantitative Discourse Analysis Package) is an R package designed to assist in quantitative discourse analysis.  The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis & visualization.

## Installation

To download the development version of qdap:

Download the [zip ball](https://github.com/trinker/qdap/zipball/master) or [tar ball](https://github.com/trinker/qdap/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **pacman** package to install the development version (The user may want to install the [dev version of reports](https://github.com/trinker/reports) first):


```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(
    "trinker/qdapDictionaries",
    "trinker/qdapRegex",
    "trinker/qdapTools",
    "trinker/qdap"
)
```


## Help

- [Web Page](http://trinker.github.com/qdap/) 
- Vignettes:     
  - [HTML Vignette: Introduction to qdap](http://trinker.github.io/qdap/vignettes/qdap_vignette.html)        
  - [PDF Vignette: qdap-tm Package Compatibility](http://trinker.github.io/qdap/vignettes/tm_package_compatibility.pdf)   
  - [PDF Vignette: Cleaning Text & Debugging](http://trinker.github.io/qdap_dev/vignettes/cleaning_and_debugging.pdf)      

## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/trinker/qdap/issues>
* send a pull request on: <https://github.com/trinker/qdap/>
* compose a friendly e-mail to: <tyler.rinker@gmail.com>

*<b>Note:</b> If you are reporting a bug make sure you have first read the [Cleaning Text & Debugging vignette](http://trinker.github.io/qdap_dev/vignettes/cleaning_and_debugging.pdf)*

# qdap
====
![qdapicon](https://dl.dropbox.com/u/61803503/qdapicon.png)   
[qdap](http://trinker.github.com/qdap_dev/) (Quantitative Discourse Analysis Package) is an R package designed to assist in quantitative discourse analysis.  The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis and visualization.

## Installation

To download the development version of qdap:

Download the [zip ball](https://github.com/trinker/qdap/zipball/master) or [tar ball](https://github.com/trinker/qdap/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("qdap", "trinker")
```

**Note**: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

Additionally, Windows users currently must install `RCurl` before installing qdap.  Use the following short script:

```r
URL <- "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/3.0/"
install.packages("RCurl", contriburl = URL)
```

**Note**: Mac users might be required to install the appropriate version [XTools](https://developer.apple.com/xcode/) from the [Apple Developer site](https://developer.apple.com/) in order to install the development version.  You may need to [register as an Apple developer](https://developer.apple.com/programs/register/).  An older version of XTools may also be required.

```r
install.packages("openNLP", type = "source")
```

## Help
The qdap web page: [trinker.github.com/qdap](http://trinker.github.com/qdap/)  [improvements to come]   
For a variety of qdap help files and videos [click here](https://github.com/trinker/qdap/wiki).   
For the package pdf help manual [click here](https://dl.dropbox.com/u/61803503/qdap.pdf).

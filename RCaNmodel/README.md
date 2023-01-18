# RCaNModel
<img src="./man/figures/logo.png" align="right" width="220" />

## Installation
### Requirements
All RCaNmodel dependencies are available on [CRAN](https://cran.r-project.org/) so there are no specific requirements.

### Installation
Once these packages are installed, the process is easy using the library [remotes](https://cran.r-project.org/package=remotes).

From an R console:

    > require(remotes)
    > remotes::install_github("https://github.com/inrae/RCaNmodel.git", subdir="RCaNmodel", dependencies = TRUE)

We recommend the installation of the package [ROI.plugin.cbc](https://github.com/dirkschumacher/ROI.plugin.cbc) which enable the use of the very efficient CBC solver. This needs package [rcbc](https://github.com/dirkschumacher/rcbc) and the CBC solver to be installed first. Instructions can be found at [dirkschumacher/ROI.plugin.cbc](https://github.com/dirkschumacher/ROI.plugin.cbc).


### Usage
A vignette is provided with the package to explain how it should be used.

### Bug reporting
Please report bugs and feature request as [Issues on GitHub](https://github.com/inrae/RCaNmodel/issues).



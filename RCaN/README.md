# CaN
<img src="RCaN/man/figures/logo.png" align="right" width="220" />

## RCaN installation
### Requirements
All RCaN dependencies are available on cran so there is no specific requirements. 

### Installation
Once these packages are installed, the process is easy using the library devools. On a R console:

    > require(devtools)
    > devtools::install_github("https://github.com/inrae/RCaN.git", subdir="RCaN")
    
We recommend the installation of the package ROI.plugin.cbc which enable the use of the very efficient cbc solver. This need package rcbc and the CBC solver to be installed first. Instructions can be found [here](https://github.com/dirkschumacher/ROI.plugin.cbc).  
    

### Usage
A vignette is provided with the package to explain how it should be used.

### Bug reporting
Please, report bugs and feature request on the github site.



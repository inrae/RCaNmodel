# CaN

## RCaN installation
### Requirements
To install RCaN, you should first install two libraries that are not yet available on CRAN

To install RCaN, follow the following steps:
* [symengine](https://github.com/symengine/symengine.R)
* [cpgsR](https://github.com/Irstea/cpgsR)

### Installation
Once these packages are installed, the process is easy using the library devools. On a R console:

    > require(devtools)
    > devtools::install_git("https://gitlab.irstea.fr/hilaire.drouineau/can.git", subdir="RCaN")
    
    Don't forget to replace myusername with your Irstea Gitlab username and provide your password.
    
### Usage
A vignette is provided with the package to explain how it should be used.

### Bug reporting
Please, report bugs and feature request on the gitlab site.



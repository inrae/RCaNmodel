# CaN

## RCaN installation
### Requirements
All RCaN dependencies are available on cran so there is no specific requirements. 

### Installation
Once these packages are installed, the process is easy using the library devools. On a R console:

    > require(devtools)
    > devtools::install_git("https://gitlab.irstea.fr/hilaire.drouineau/can.git", subdir="RCaN", credentials = git2r::cred_user_pass("myusername", getPass::getPass()),dependencies=TRUE)
    
    Don't forget to replace myusername with your Irstea Gitlab username and provide your password.
    
### Usage
A vignette is provided with the package to explain how it should be used.

### Bug reporting
Please, report bugs and feature request on the gitlab site.



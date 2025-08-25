# RCaNModel
<img src="./man/figures/logo.png" align="right" width="220" />

## Installation
### Requirements
This tools facilitates the construction of RCaNfile (specification of compartments,
fluxes and constraints). Contrary to the former RCaNconstructor, it is coded in Shiny
avoiding the use of java. However, please note that:

- contrary to the java constructor, this tool is limited to the construction of the model and does not allow sampling the polytope
- and does not allow the exploration of results


### Installation
From an R console:

    > library(remotes)
    > remotes::install_github("https://github.com/inrae/RCaNmodel.git", subdir="RCaNconstructorShiny", dependencies = TRUE)
    
or 

    > require(devtools)
    > devtools::install_github("https://github.com/inrae/RCaNmodel.git", subdir="RCaNconstructorShiny", dependencies = TRUE)



### Usage
  > library(RCaNconstructorShiny)
  > launchConstructor()

### Bug reporting
Please report bugs and feature request as [Issues on GitHub](https://github.com/inrae/RCaNmodel/issues).



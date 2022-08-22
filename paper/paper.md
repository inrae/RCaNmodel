---
title: 'RCaNmodel: a R package for Chance and Necessity modelling'
tags:
  - R
  - tophic food web model
  - javafx
  - trophic controls
  - linear inverse modelling
authors:
  - name: Hilaire Drouineau 
    mail: hilaire.drouineau@inrae.fr
    orcid: 0000-0001-9206-0040
    affiliation: 1
    corresponding: true
  - name: Benjamin Planque
    orcid: 0000-0002-0557-7410
    affiliation: 2
  - name: Christian Mullon
    orcid: 0000-0003-3990-9118
    affiliation: 3
affiliations:
 - name:  INRAE, UR EABX, 50 avenue de Verdun, CEDEX, 33612 Cestas, France
   index: 1
 - name: Institute of Marine Research, Norway, P.O. Box 6606, 9296 Tromsø, Norway
   index: 2
 - name: IRD, UMR MARBEC, Avenue Jean Monnet, Sete, France
   index: 3
date: 16 August 2022
bibliography: paper.bib
---

# Summary

RCaNmodel is an R package dedicated to the development and exploration of time-dynamic food-web models.
The associated user-friendly javafx GUI RCaNconstructor is specifically developed to facilitate the
co-construction of such models with non-modellers. Together, RCaNmodel and RCaNconstructor ensure the
traceability and replicability of food-web model developments, and provide a fast and efficient solution
to handle the complex mathematical steps that arise from dynamic linear inverse modelling and polytope sampling
in high dimensions. The software includes standard graphical outputs for the exploration of food-webs
dynamics and the detection of trophic controls, which are key questions in trophic ecology.

# Statement of need

Food-web models are widely used to study the functioning of ecosystems and to explore the impact of
different pressures such as fishing or environmental changes [@addison2013; @murphy2012]. Food-web
models primarily focus on trophic exchanges, also called flows or fluxes, between prey and predators.
Estimations of trophic flows and of other model parameters is a complex task because the number of field
observations is generally far smaller than the number of estimates from model outputs. Food-web reconstruction
have no single best solutions, but rather an infinite number of flow patterns that comply with observational
data, and food-web models are said to be underdetermined [@niquil2011]. Several strategies have been
developed to tackle this issue. In Ecopath [@Polovina1984], or in its dynamic extension called Ecosim [@pauly2000; @christensen2004]
one of the most largely used trophic food-web
models [@Heymans2016] the modeller is asked to provide additional information on species diet or on their
vulnerabilities to turn the underdetermined into an overdetermined problem. This approach ignores the highly
variable and stochastic nature of trophic interactions. Linear Inverse Model (LIM) food-web models have been
proposed as an alternative approach [@vezina1988]. LIM models rely on the same key assumptions as of any
food-web models (e.g., mass conservation) and are constructed using constraints (i.e., linear
equalities/inequalities) that specify which combinations of flows are possible or not. Mathematically, the
set of constraints defines a polytope: a fraction of the space that includes all the possible solutions
satisfying the constraints (i.e. all possible trophic food web states). The polytope can be sampled using
appropriate MCMC algorithms [@kones2006; @kones2009; @vandenmeersche2009]. Like Ecopath, LIM is a static model
that describe food-webs at equilibrium, but unlike Ecosim, there is no direct dynamic extension of LIM models.
Recently, Planque and Mullon [@planque2019modelling] have developed a
time-dynamic food-web modelling approach analogous to LIM, based on a generic approach called "Chance and Necessity” 
(CaN) modelling. CaN models account for Chance, i.e. the stochasticity of nature and for Necessity, i.e.,
the existence of physical (e.g. mass conservation) or ecological constraints (e.g. inertia of populations,
satiation of individuals) that can separate food-web dynamics that are possible from those that are not.
CaN models also use observations to constrain the reconstruction of past food-web dynamics
(\autoref{fig:canprinciple}). As in LIM, building a trophic food-web CaN model starts with 1) the definition of the structure
of the food web (who eats whom), followed by 2) the specification of the constraints (what is possible and
what is not) and 3) the translation of these into the mathematical expression of a polytope.
The validity of the resulting model is then tested by 4) checking that the polytope is bounded (i.e.
infinite values are excluded) and not empty (i.e. some solutions exist). The following step is 5) to
sample solutions within the polytope (i.e. time series of biomass and flows that satisfy all-time series
of constraints). The complexity of steps 3, 4, and 5 is much greater in CaN than in LIM because the dynamic
modelling in trophic food-web CaN entails much larger polytope dimensions (often \>100 or even \>1000). CaN model outputs/samples
are then 6) analysed, often in a graphical manner, to reveal dynamical interactions between food-web components.
Because outputs from food-web models are relevant outside the modelling community, e.g., for managers and stakeholders,
there is a need for a modelling process - from model building to results interpretation - that can be conducted in
a participatory manner. RCaNmodel and RCaNconstructor provide a mathematically efficient solution to steps 3, 4, and 5
and an interactive/intuitive interface to support steps 1, 2, and 6.
 

# Methods

RCaNmodel and RCaNconstructor support the full modelling process from the specification of the model to the
exploration of the outputs (\autoref{fig:approach}), while providing user-friendly solution to each technical challenges:

-   To specify the model, the user provides several tables that define the compartments (i.e., trophic groups), their
ecological properties (species specific input parameters), the flows among the compartments (i.e., trophic
interactions), observational time-series, and user-defined explicit constraints. Tables can be provided either via
an RCaN file that consists of several spreadsheets in the xlsx open format (see supplementary material for an example),
or directly in the form of R data frames. The RCaN file format facilitates data entry for non-expert users, and exchange
of input files between users. RCaN files can be constructed using the RCaNconstructor GUI, which ensures internal consistency
between the data tables. The alternative use of R dataframes can facilitate the use of external databases or version control
through the use of plain text files. 
-   Based on the input data provided in the tables, the function *buildCaN* constructs the matrices that define the
polytope (see @planque2019modelling for the underlying mathematics). The use of symengine [@ma2020], a library for symbolic
manipulation, facilitates the automatic translation of user-specified literal constraints (see supplementary material
for details on the syntax) into  matrices.
-   The function *checkPolytopeStatus* checks if the polytope is bounded and not empty, two conditions that are
necessary for the food-web model to be valid. If the polytope is empty, the function *findingIncompatibleConstr*
can be used to identify the minimum set of constraints that need to be relaxed to make the polytope feasible
(inspired from <http://web.mit.edu/lpsolve/doc/Infeasible.htm>). Functions *getBoundParam* and *getAllBoundsParam* return
the possible minimum and maximum values of all dimensions of the polytope, i.e., of all flows at all time-steps and of all
biomasses at time-step 1. RCaNmodel uses linear programming tools to perform these tasks, using the package lpSolveAPI [@konis2020]. 
-   The function *sampleCaN* samples valid food-web trajectories using a Gibbs sampling algorithm that has proved to be
especially efficient to achieve a uniform sampling in high dimensions polytopes [@andersen2007a; @laddha2020a]. The function
returns an object of class mcmc.list that can be manipulated/explored using the package coda [@plummer2010a],
e.g. to check the convergence of the MCMC algorithm.
-   Finally, several graphical functions are available to explore the results per trophic groups and time periods
(see supplementary). For example, the function *ggTopDownBottomUp* returns a ggplot [@wickam2016] that can be used to detect
top-down or bottom-up controls on the different trophic groups (\autoref{fig:ggtopdown}).

All these steps can be done either directly through R scripts or using the RCaNconstructor GUI. An example using R commands
is presented in the supplementary material. It should be noted that most of the functions to check the status or to sample
the polytope can be used with other packages (e.g. LIM modelling package). Finally, the chance and necessity modelling approach is applicable to a wide
range of problems beyond food-web models, and RCaNmodel is currently being generalised to other kind of models that rely on
similar principles.

# Figures

![The CaN approach applied to a trophic food-web: physical and ecological constraints are used to delineate what food-web
dynamics are possible or not. These linear equalities and inequalities define a polytope, i.e. a subspace that include all
possible time-trajectories of the food-web and that can be sample using appriate MCMC algorithms.\label{fig:canprinciple}](can.png)

![Overview of the modelling process to develop and explore a CaN trophic food web model with RCaNmodel. All the steps can
be achieved using either an R script or the RCaNconstructor GUI, except the step denoted with a \*. Italicised texts denote
RCaNmodel functions. The boxes stand for the main steps, under which is mentioned their links with other R libraries.\label{fig:approach}](approaches.png)

![Example of diagrams returned by ggTopDownBottomUp which display how the yearly biomass growth of a trophic group is correlated
with the yearly predation pressure exerted on the group (in blue) or on the amount of trophic resources the group consumed (in red).
The diagram comes from the example presented in supplementary material. Here, the growth of marine mammals (MM) is positively correlated
to its feeding, suggesting a strong bottom-up control. On the other hand, the growth of pelagic fishes (PelF) is negatively correlated
to the predation pressure, suggesting a top-down control.\label{fig:ggtopdown}](ggtopdown.png)

# References

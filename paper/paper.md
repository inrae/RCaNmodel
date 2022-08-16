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
RCaNmodel is an R package dedicated to the development and exploration of time-dynamic trophic food web models. Combined with a user-friendly javafx GUI RCaNconstructor, they were specifically developed to facilitate the co-construction of such models with experts, while ensuring the traceability and repeatability of research, handling the complex mathematical steps that arise from linear modelling approaches. Thank to various graphical outputs, the package allows exploring the time trajectory of food webs and to detect trophic controls and their evolution over time, which are key questions in trophic ecology.


# Statement of need
Trophic food web models are widely used to study the functioning of ecosystems and to explore the impact of different pressures such as fishing or environmental changes [@addison2013; @murphy2012]. Such models focus on the interactions due to trophic exchanges (e.g. carbon, biomass) among preys and predators, that constitute the food web. However, the direct estimation of these flows and of other model parameters from available information is a complex task: the number of observations is generally far smaller than the number of available data; the model is said to be underdetermined [@niquil2011]. Several strategies have been developed to tackle this issue. In an Ecopath model [@Polovina1984], one of the most largely used trophic food web models [@Heymans2016], or in Ecosim [@christensen2004; @pauly2000], its time-dynamic version, the modeller is asked to provide additional information on species diet or on their vulnerabilities to turn into an overdetermined problem. However, this ignores the stochasticity of nature. Static Linear Inverse Model (LIM) trophic food web models have been proposed as an alternative [@vezina1988]. While preserving key assumptions of any trophic food-web models, such as mass conservation, LIM are based on linear equalities or inequalities of trophic flows that constraints what is feasible or not. Mathematically, the set of constraints defines a polytope: a fraction of the space that includes all the possible solutions satisfying the constraints (i.e. all possible trophic food web states), that can be sampled using appropriate MCMC algorithms [@kones2006; @kones2009; @vandenmeersche2009]. Recently, Planque and Mullon have developed a similar approach called “Chance and Necessity”, on which they based a dynamic trophic food web model [@planque2019modelling]. CaN acknowledges of the stochasticity of nature (Chance), but also the existence of physical (e.g. mass conservation) or physiological laws (e.g. inertia of populations, satiation of individuals) that constrain what is possible or not, of data that delineate things that may have indeed happened or not in the past (laws and data correspond to necessities - \autoref{fig:canprinciple}). Such as LIM, after specifying the structure of the food web, a CaN trophic food web modeller has first to specify the existing constraints and then to automatically translate them to build the underlying polytope. Then, the modeller has to ensure that the corresponding polytope is bounded (i.e. infinite values are excluded) but not empty (i.e. some solutions exist). Then, the modeller needs to sample solutions within the polytope (i.e. time series of biomass and flows that satisfy all-time series of constraints). Those last two steps are more complex in a CaN trophic food web model than with LIM, since the dynamic nature of CaN leads to much larger polytope dimensions. After, this the modeller needs to explore the high dimensional inputs to derive information the past trajectory of the food web and on trophic controls. Finally, trophic food web models are more and more used in participatory approaches and as such, user-friendly tools are required to facilitate the quick and easy construction and exploration of such complex models. RCaNmodel and RCaNconstructor are developed to facilitate of these steps and were used in recent papers [@sivel2021; @planque2022].

# Methods
RCaNmodel and RCaNconstructor support the full modelling process from the specification of the model to the exploration of the outputs (\autoref{fig:approach}), while providing user-friendly solution to all technical challenges:

* To specify the model, the user should provide several tables that define the compartments (i.e. trophic groups) and their ecological properties (used to define implicit constraints), the flows among the compartments (i.e. trophic interactions), time series of data and user-defined explicit constraints. The tables can be provided either through a xlsx spreadsheet (open format), called RCaN file (see supplementary material for an example), or directly through R data frames. The use of a xlsx file aims to facilitate data entry for non-expert users, and the exchange of input files. Moreover, an RCaN file can be constructed using the GUI that ensures its consistency. On the other hand, the use of R data frame can facilitate the use of external databases or version control through the use of plain text files. Once the tables are ready, the function *buildCaN* uses all the input data to construct all the underlying matrices that define the polytope (see @planque2019modelling for the underlying mathematics). The use of symengine [@ma2020], a library for symbolic manipulation, facilitates the automatic translation of user-specified literal constraints (see supplementary material for details on the syntax) into mathematical matrices.
* Once the model is specified, it is necessary to check that the polytope is bounded and not empty, and if required to help the user to find incompatible constraints (e.g. constraints that are not compatible simultaneously). To do so, RcaNmodel uses linear programming tools through the package lpSolveAPI [@konis2020]. The function *checkPolytopeStatus* tells the user whether the polytope is bounded and not empty. Functions *getBoundParam* and *getAllBoundsParam* can be used the minimum and possible values of parameters (e.g. a flow in a given year) resulting from the set of constraints. If the polytope is empty, the modeller can use the function *findingIncompatibleConstr* that will seek the minimum set of constraints that need to be relaxed to make the polytope feasible (inspired from http://web.mit.edu/lpsolve/doc/Infeasible.htm)
* Then, the user can sample the polytope using the function *sampleCaN*. This function implements a Gibbs sampling algorithm that has proved to be especially efficient to achieve a uniform sampling in high dimensions polytopes [@andersen2007a; @laddha2020a]. The function returns a mcmc.list that can be used with package coda [@plummer2010a] to check the convergence of the MCMC algorithm.
* Finally, several graphical functions are available to explore the results per trophic groups and time periods (see supplementary). For example, the function *ggTopDownBottomUp* returns a ggplot [@wickam2016] that can be used to detect top-down or bottom-up controls on the different trophic groups (\autoref{fig:ggtopdown}).

All these steps can be done either directly through an R script to ensure traceability and repeatability, or using the GUI that is well suited in a context of a participatory modelling exercise. An example using R commands is presented in supplementary material. It should be noted that most of the functions to check the status or to sample the polytope can be used with other packages. Finally, since the chance and necessity approach is larger than trophic food web models, RCaNmodel is currently extended to allow the development of any kind of models.

# Figures
![The CaN approach applied to a trophic food web: physiological and physical constraints are used to delineate what is ecologically possible or not. These linear equalities and inequalities define a polytope, i.e. a subspace that include all possible time-trajectories of the food-web and that can be sample using appriate MCMC algorithms.\label{fig:canprinciple}](can.png)

![Overview of the modelling process to develop and explore a CaN trophic food web model with RCaNmodel. All the steps can be achieved using either an R script or the GUI RCaNconstructor, except the step denoted with a \*. Italicised texts denote RCaNmodel functions. The boxes stand for the main steps, under which is mentioned their links with other R libraries.\label{fig:approach}](approaches.png)

![Example of diagrams returned by ggTopDownBottomUp which display how the yearly biomass growth of a trophic group is correlated with the yearly predation pressure exerted on the group (in blue) or on the amount of trophic resources the group consumed (in red). The diagram comes from the example presented in supplementary material. Here, the growth of marine mammals (MM) is positively correlated to its feeding, suggesting a strong bottom-up control. On the other hand, the growth of pelagic fishes (PelF) is negatively correlated to the predation pressure, suggesting a top-down control.\label{fig:ggtopdown}](ggtopdown.png)





# References


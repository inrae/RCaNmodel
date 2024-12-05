#' fileInteractionUI
#' ui of the file interaction visual editor
#' @param id the id of the ui
#'
#' @return nothing
#'
#' @importFrom shiny NS fluidRow tagList h3
#' @importFrom shinyjs useShinyjs 
#' @export
#'

fileInteractionUI <- function(id){
  ns <- NS(id)
  tagList(useShinyjs(),
          fluidRow(
            actionButton(ns("new"), "new"),
            actionButton(ns("open"), "open"),
            downloadButton(ns("savename"), "save")),
          fluidRow(
            h3("NAME VERSION AND AUTHORS"),
            textAreaInput(ns("modelname"), "Model Name", "", width = "90%"),
            shinyBS::bsTooltip(ns("modelname"), 
                               "Title/name of the food-web model"),
            
            textAreaInput(ns("modelversion"), "Model Version and date", "", width = "90%"),
            shinyBS::bsTooltip(ns("modelversion"),
                               "Specific code used to uniquely identify the model version"),
            
            textAreaInput(ns("authors"), "Authors", "", width = "90%"),
            shinyBS::bsTooltip(ns("authors"),
                               "Names and addresses of the principal investigators associated with the food-web model development"),
            
            
            
            h3("DOMAIN AND UNITS"),
            textAreaInput(ns("domaincoverage"), "Domain coverage", "", width = "90%"),
            shinyBS::bsTooltip(ns("domaincoverage"),
                               "Geographical and temporal extent of the model"),
            
            textAreaInput(ns("timeunit"), "Time Unit", "", width = "90%"),
            shinyBS::bsTooltip(ns("timeunit"),
                               "Time-step used in the model, e.g. daily, weekly, quarterly, annually"),
            
            textAreaInput(ns("biomassunit"), "Biomass Unit", "", width = "90%"),
            shinyBS::bsTooltip(ns("biomassunit"),
                               "Main unit for biomasses and fluxes (e.g. tonnes fresh weight, thousand tonnes, tonnes/km2, gC/m2, etc.)"),
            
            
            
            h3("COMPONENTS"),
            textAreaInput(ns("componentsin"), "Components within the model domain", "", width = "90%"),
            shinyBS::bsTooltip(ns("componentsin"),
                               "List of the trophospecies within the model domain and a description of how they were defined, e.g. are these taxonomic species? Are some divided by developmental stage or are several species grouped under a single trophospecies? Are there trophospecies that are split by geographical areas? Indication of relevant sources of data and information"),
            
            textAreaInput(ns("componentsout"), "Components outside the model domain", "", width = "90%"),
            shinyBS::bsTooltip(ns("componentsout"),
                               "List of the components outside the model domain, e.g. primary production, source/sink of migration from/to other regions, fisheries. Indication of relevant sources of data and information"),
            
            
            
            h3("FLUXES"),
            textAreaInput(ns("trophicflux"), "Trophic links", "", width = "90%"),
            shinyBS::bsTooltip(ns("trophicflux"),
                               "List of the trophic links and a description of how these were defined/selected. Trophic links are from prey to predators. Indication of relevant sources of data and information"),
            
            textAreaInput(ns("nontrophicflux"), "Non-trophic links", "", width = "90%"),
            shinyBS::bsTooltip(ns("nontrophicflux"),
                               "List of the non-trophic links and a description of how these were defined/selected. Non-trophic links are e.g. import, export, migration, production or other transfer of biomass that are not ingested by a predatory group. Indication of relevant sources of data and information"),
            
            
            
            h3("INPUT PARAMETERS, DATA AND CONSTRAINTS"),
            textAreaInput(ns("inputparam"), "Input Parameters", "", width = "90%"),
            shinyBS::bsTooltip(ns("inputparam"),
                               "Source of information (datasets, references, experimental results, theory-based, other models, [Ellipsis]) used to derive EACH input parameter values (i.e. AssimilationE,	Digestibility,	OtherLosses,	Inertia,	Satiation and	RefugeBiomass for each trophospecies within the model domain)"),
            
            textAreaInput(ns("observations"), "Observation Series", "", width = "90%"),
            shinyBS::bsTooltip(ns("observations"),
                               "Description of individual time-series, including units, source of information (surveys, assessments, [Ellipsis]) and doi when available"),
            
            textAreaInput(ns("constraints"), "Constraints", "", width = "90%"),
            shinyBS::bsTooltip(ns("constraints"),
                               "Sources of information used to define the user-defined constraints used in the model (i.e. outside satiation, inertia, positive fluxes and Biomass>Refuge Biomass, which are implicit constraints in all RCaN models)"),
            
            
            
            h3("UNCERTAINTIES AND LIMITATIONS"),
            textAreaInput(ns("uncertspeciesexhaus"), "Species Exhaustivity", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertspeciesexhaus"),
                               "How well does the list of trophospecies cover the biomass of all species within the model domain? Could/should more species be added?"),
            
            textAreaInput(ns("uncertspecieshomog"), "Species Homogeneity", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertspecieshomog"), 
                               "For each trophospecies, are all individuals sharing the same set of prey, predator and input parameters? Could/should some trophospecies be split to reflect intra-group variations?"),
            
            textAreaInput(ns("uncertspeciesparam"), "Species Parameters", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertspeciesparam"),
                               "How (un)certain are the input parameter values? Could other values be used?"),
            
            textAreaInput(ns("uncertcompoutside"), "Components outside the model domain", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertcompoutside"),
                               "How (un)certain is the information about external entities. Could/should these be formulated in alternative ways?"),
            
            textAreaInput(ns("uncerttrophiclinks"), "Trophic links", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncerttrophiclinks"),
                               "How (un)certain and how stable/variable are the trophic links? Are some links known to exist but not included in the model? Are some links rarely observed or known to be complicated to observe (e.g. no hard parts to identify certain prey)? Are some links highly variable between years/ares/studies?"),
            
            textAreaInput(ns("uncertnontrophiclinks"), "Non-trophic links", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertnontrophiclinks"),
                               "How (un)certain and how stable/variable are the non-trophic links? Are some links known to exist but not included in the model?"),
            
            textAreaInput(ns("uncertconstraints"), "Constraints", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertconstraints"),
                               "How (un)certain are the constraints? Could/should certain constraints be relaxed/strengthened/removed/added?"),
            
            textAreaInput(ns("uncertts"), "Time series Availability", "", width = "90%"),
            shinyBS::bsTooltip(ns("uncertts"),
                               "Are the time-series privately available and documented? Could more time-series be added? Are the time-series updated and maintained? Are the time-series covering the model domain appropriately (in time and space)? Are the time-series provided with appropriate uncertainty estimates? Are there additional time-series available? Are there conflicting time-series?"),
            
            
            
            h3("RESEARCH PROJECT"),
            textAreaInput(ns("projident"), "Project identity", "", width = "90%"),
            shinyBS::bsTooltip(ns("projident"),
                               "Title or name of the research project under which the food-web model was developed"),
            
            textAreaInput(ns("projper"), "Project Period", "", width = "90%"),
            shinyBS::bsTooltip(ns("projper"),
                               "Period during which the research project was conducted (start-end)"),
            
            textAreaInput(ns("projobj"), "Project Objectives", "", width = "90%"),
            shinyBS::bsTooltip(ns("projobj"),
                               "Scope and purpose of the research project"),
            
            textAreaInput(ns("projabst"), "Project Abstract", "", width = "90%"),
            shinyBS::bsTooltip(ns("projabst"),
                               "Descriptive abstract of the research project"),
            
            textAreaInput(ns("projfund"), "Project Funding", "", width = "90%"),
            shinyBS::bsTooltip(ns("projfund"),
                               "Grant and contract numbers, names and addresses of funding sources")
          ))
}

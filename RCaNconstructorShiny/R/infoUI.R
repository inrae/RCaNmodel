#' infoUI
#'
#' Info tab
#'
#' @param id the id of ui
#' @importFrom shiny actionButton fluidRow h1 textOutput textAreaInput
#' @importFrom bslib tooltip
#' @export
#'

infoUI <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          h1("NAME VERSION AND AUTHORS"),
          tooltip(textAreaInput(ns("modelname"), "Model Name", ""),
                  "Title/name of the food-web model"),
          tooltip(textAreaInput(ns("modelversion"), "Model Version and date", ""),
                  "Specific code used to uniquely identify the model version"),
          tooltip(textAreaInput(ns("authors"), "Authors", ""),
                  "Names and addresses of the principal investigators associated with the food-web model development"),
          h1("DOMAIN AND UNITS"),
          tooltip(textAreaInput(ns("domaincoverage"), "Domain coverage", ""),
                  "Geographical and temporal extent of the model"),
          tooltip(textAreaInput(ns("timeunit"), "Time Unit", ""),
                  "Time-step used in the model, e.g. daily, weekly, quarterly, annually"),
          tooltip(textAreaInput(ns("biomassunit"), "Biomass Unit", ""),
                  "Main unit for biomasses and fluxes (e.g. tonnes fresh weight, thousand tonnes, tonnes/km2, gC/m2, etc.)"),
          h1("COMPONENTS"),
          tooltip(textAreaInput(ns("componentsin"), "Components within the model domain", ""),
                  "List of the trophospecies within the model domain and a description of how they were defined, e.g. are these taxonomic species? Are some divided by developmental stage or are several species grouped under a single trophospecies? Are there trophospecies that are split by geographical areas? Indication of relevant sources of data and information"),
          tooltip(textAreaInput(ns("componentsout"), "Components outside the model domain", ""),
                  "List of the components outside the model domain, e.g. primary production, source/sink of migration from/to other regions, fisheries. Indication of relevant sources of data and information"),
          h1("FLUXES"),
          tooltip(textAreaInput(ns("trophicflux"), "Trophic links", ""),
                  "List of the trophic links and a description of how these were defined/selected. Trophic links are from prey to predators. Indication of relevant sources of data and information"),
          tooltip(textAreaInput(ns("nontrophicflux"), "Non-trophic links", ""),
                  "List of the non-trophic links and a description of how these were defined/selected. Non-trophic links are e.g. import, export, migration, production or other transfer of biomass that are not ingested by a predatory group. Indication of relevant sources of data and information"),
          h1("INPUT PARAMETERS, DATA AND CONSTRAINTS"),
          tooltip(textAreaInput(ns("inputparam"), "Input Parameters", ""),
                  "Source of information (datasets, references, experimental results, theory-based, other models, [Ellipsis]) used to derive EACH input parameter values (i.e. AssimilationE,	Digestibility,	OtherLosses,	Inertia,	Satiation and	RefugeBiomass for each trophospecies within the model domain)"),
          tooltip(textAreaInput(ns("observations"), "Observation Series", ""),
                  "Description of individual time-series, including units, source of information (surveys, assessments, [Ellipsis]) and doi when available"),
          tooltip(textAreaInput(ns("constraints"), "Constraints", ""),
                  "Sources of information used to define the user-defined constraints used in the model (i.e. outside satiation, inertia, positive fluxes and Biomass>Refuge Biomass, which are implicit constraints in all RCaN models)"),
          h1("UNCERTAINTIES AND LIMITATIONS"),
          tooltip(textAreaInput(ns("uncertspeciesexhaus"), "Species Exhaustivity", ""),
                  "How well does the list of trophospecies cover the biomass of all species within the model domain? Could/should more species be added?"),
          tooltip(textAreaInput(ns("uncertspecieshomog"), "Species Homogeneity", ""),
                  "For each trophospecies, are all individuals sharing the same set of prey, predator and input parameters? Could/should some trophospecies be split to reflect intra-group variations?"),
          tooltip(textAreaInput(ns("uncertspeciesparam"), "Species Parameters", ""),
                  "How (un)certain are the input parameter values? Could other values be used?"),
          tooltip(textAreaInput(ns("uncertcompoutside"), "Components outside the model domain", ""),
                  "How (un)certain is the information about external entities. Could/should these be formulated in alternative ways?"),
          tooltip(textAreaInput(ns("uncerttrophiclinks"), "Trophic links", ""),
                  "How (un)certain and how stable/variable are the trophic links? Are some links known to exist but not included in the model? Are some links rarely observed or known to be complicated to observe (e.g. no hard parts to identify certain prey)? Are some links highly variable between years/ares/studies?"),
          tooltip(textAreaInput(ns("uncertnontrophiclinks"), "Non-trophic links", ""),
                  "How (un)certain and how stable/variable are the non-trophic links? Are some links known to exist but not included in the model?"),
          tooltip(textAreaInput(ns("uncertconstraints"), "Constraints", ""),
                  "How (un)certain are the constraints? Could/should certain constraints be relaxed/strengthened/removed/added?"),
          tooltip(textAreaInput(ns("uncertts"), "Time series Availability", ""),
                  "Are the time-series privately available and documented? Could more time-series be added? Are the time-series updated and maintained? Are the time-series covering the model domain appropriately (in time and space)? Are the time-series provided with appropriate uncertainty estimates? Are there additional time-series available? Are there conflicting time-series?"),
          h1("RESEARCH PROJECT"),
          tooltip(textAreaInput(ns("projident"), "Project identity", ""),
                  "Title or name of the research project under which the food-web model was developed"),
          tooltip(textAreaInput(ns("projper"), "Project Period", ""),
                  "Period during which the research project was conducted (start-end)"),
          tooltip(textAreaInput(ns("projobj"), "Project Objectives", ""),
                  "Scope and purpose of the research project"),
          tooltip(textAreaInput(ns("projabst"), "Project Abstract", ""),
                  "Descriptive abstract of the research project"),
          tooltip(textAreaInput(ns("projfund"), "Project Funding", ""),
                  "Grant and contract numbers, names and addresses of funding sources")
  )
}

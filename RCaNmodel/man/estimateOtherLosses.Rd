% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateOtherLosses.R
\name{estimateOtherLosses}
\alias{estimateOtherLosses}
\title{estimateOtherLosses
Estimate other losses based on various allometric relationships}
\usage{
estimateOtherLosses(weight, method, taxon, temperature = NULL, d = 1)
}
\arguments{
\item{weight}{wet weight in gram}

\item{method}{one of "gillooly", "yodzis", "makarieva"}

\item{taxon}{one of ("unicell", "plant", "multicellular invertebrate", "fish",
"amphibian", "reptile", "bird or mammal") for gillooly, one of ("endotherm",
"vertebrate ecotherm", "invertebrate") for yodzis, one of ("prokaryote",
"protozoa", "insect", "aquatic invertebrate", "copepod or krill",
"peracarid", "decapod", "cephalopod", "gelatinous invertebrate", "ectothermic
 vertebrate", "amphibian", "fish", "reptile", "endothermic vertebrate",
 "bird", "mammal", "cyanobacteria", "eukaryotic microalgae", "eukaryotic
 macroalgae", "green leaves", "tree saplings", "seedlings")}

\item{temperature}{temperature in °C (required for Gillooly and Makarieva)}

\item{d}{fraction of days in which a species grow (1 for 365 days / year).
used in gillooly and makarieva}
}
\value{
other losses as explained in supplementary material in
\insertCite{planque2022;textual}{RCaNmodel}
}
\description{
estimateOtherLosses
Estimate other losses based on various allometric relationships
}
\details{
\insertCite{planque2022;textual}{RCaNmodel} proposed several
methods based on allometric relationships to estimate otherlosses. Method
"gillooly" is based on \insertCite{gillooly2001;textual}{RCaNmodel}, "yodzis"
is based on \insertCite{yodzis1992;textual}{RCaNmodel} and "makarieva" on
\insertCite{makarieva2008;textual}{RCaNmodel}
}
\examples{
estimateOtherLosses(240, "yodzis", taxon = "vertebrate ecotherm")
estimateOtherLosses(240, "gillooly", "fish", 13, 1)
estimateOtherLosses(240, "makarieva", "fish", 13, 1)
}
\references{
\insertAllCited{}
}

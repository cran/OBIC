% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\docType{data}
\name{nema.crop.rot.obic}
\alias{nema.crop.rot.obic}
\title{Damage and reproduction of soil-borne pathogens and pests on crops}
\format{
An object of class \code{data.table} (inherits from \code{data.frame}) with 7059 rows and 21 columns.
}
\usage{
nema.crop.rot.obic
}
\description{
This table includes information from aaltjesschema (April 2021), a website where information is collected on the vulnerability of crops to plant parasitic nematodes and diseases that use nematodes as vector.
}
\details{
\describe{
  \item{crop}{crop as called in aaltjesschema}
  \item{name_scientific}{scientific name of nematode}
  \item{propagation}{how easily a nematode can propagate on a crop given as strings with 5 classes}
  \item{damage}{strings indicating how much damage a nematode can inflict on a crop, with 5 classes}
  \item{cultivar_dependent}{boolean whether there are differences in propgation between cultivars of the crop}
  \item{serotype_dependant}{boolean whether there are differences in propagation between serotypes of the pathogen}
  \item{dalgrond}{boolean whether information is valid for soiltype 'dalgrond'}
  \item{klei}{boolean whether information is valid for soiltype 'klei'}
  \item{loess}{boolean whether information is valid for soiltype 'loess'}
  \item{zand}{boolean whether information is valid for soiltype 'zand'}
  \item{zavel}{boolean whether information is valid for soiltype 'zavel'}
  \item{info}{string whether there is information on propgation, differentiating between none, yes, and some}
  \item{name_common}{string, common name of pathogen in Dutch, if no common name is available, scientific name is given}
  \item{nema_name}{string, full name of pathogen in aaltjesschema, includes common and scientific name}
  \item{grondsoort}{string with letters indicating for which soil the information is valid}
  \item{groen_br}{boolean indicating that the crop is a green manure on fallow}
  \item{groen_vs}{boolean indicating that the crop is a green manure in early stubble}
  \item{groen_od}{boolean indicating that the crop is a green manure beneath cover crop}
  \item{groen_ls}{boolean indicating that the crop is a green manure in late stubble}
  \item{groen_st}{boolean indicating that the crop is a green manure as drifting deck}
  \item{crop_name_scientific}{string, scientific name of crop species or genus}
}
}
\keyword{datasets}

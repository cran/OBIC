% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nitrogen.R
\name{calc_nlv}
\alias{calc_nlv}
\title{Calculate the NLV}
\usage{
calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)
}
\arguments{
\item{B_LU_BRP}{(numeric) The crop code from the BRP}

\item{B_SOILTYPE_AGR}{(character) The agricultural type of soil}

\item{A_N_RT}{(numeric) The organic nitrogen content of the soil in mg N / kg}

\item{A_CN_FR}{(numeric) The carbon to nitrogen ratio}

\item{D_OC}{(numeric) The organic carbon content of the soil in kg C / ha}

\item{D_BDS}{(numeric) The bulk density of the soil in kg / m3}

\item{D_GA}{(numeric) The age of the grass if present}
}
\value{
The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value.
}
\description{
This function calculates the NLV (nitrogen producing capacity) for the soil
}
\examples{
calc_nlv(B_LU_BRP = 256, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
A_CN_FR = 11, D_OC = 86000,D_BDS = 1300, D_GA = 4)
calc_nlv(1019,'dekzand',2315,13,86000,1345,0)

}

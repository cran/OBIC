% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obic.R
\name{obic_farm}
\alias{obic_farm}
\title{Calculate the Open Bodem Index score for a series of fields belonging to a farm}
\usage{
obic_farm(dt)
}
\arguments{
\item{dt}{(data.table) A data.table containing the data of the fields to calculate the OBI}
}
\value{
The output of the Open Bodem Index Calculator for a series of agricultural fields belonging to a single farm. 
Depending on the output type, different output objects can be returned.
These include the estimated OBI scores (both total and aggregated subscores), the value of the underling indicators as well the possible recommendations to improve the soil quality.
The output is a list with field properties as well as aggregated farm properties
}
\description{
This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI).
In contrast to obic_field, this wrapper uses a data.table as input.
}
\details{
The data.table should contain all required inputs for soil properties needed to calculate OBI score. Management information is optional as well as the observations from the visual soil assessment.
The threshold values per category of soil functions need to have an equal length, with fractions defining the class boundaries in increasing order. 
The lowest boundary value (zero) is not needed.
}
\examples{
 
\dontrun{
obic_farm(dt = data.table(B_SOILTYPE_AGR = 'rivierklei',B_GWL_CLASS = "II",
B_GWL_GLG = 75,B_GWL_GHG = 10,
B_GWL_ZCRIT = 50,B_SC_WENR = '2',B_HELP_WENR = "MOb72",B_AER_CBS = 'LG01',
B_LU_BRP = c( 1010, 1010,263,263, 263,265,265,265),A_SOM_LOI = 3.91,A_SAND_MI = 66.3,
A_SILT_MI = 22.8,A_CLAY_MI = 7.8,A_PH_CC = 5.4,A_N_RT = 1528.33,A_CN_FR = 13.02,
A_S_RT = 321.26,A_N_PMN = 63.3,A_P_AL = 50.2,A_P_CC = 2.9,A_P_WA = 50.5,
A_CEC_CO = 56.9,A_CA_CO_PO = 66.87,A_MG_CO_PO = 13.97,A_K_CO_PO = 3.06,
A_K_CC = 58.6,A_MG_CC = 77.53,A_MN_CC = 7586.61,A_ZN_CC = 726.2,A_CU_CC = 68.8,
A_C_BCS = 1,A_CC_BCS = 1,A_GS_BCS = 1,A_P_BCS = 1,A_RD_BCS = 1,A_EW_BCS = 1,
A_SS_BCS = 1,A_RT_BCS = 1,A_SC_BCS = 1,M_COMPOST = 0,M_GREEN = FALSE,M_NONBARE =FALSE,
M_EARLYCROP = FALSE,M_SLEEPHOSE = FALSE,M_DRAIN = FALSE,M_DITCH = FALSE,
M_UNDERSEED = FALSE,M_LIME = FALSE,M_MECHWEEDS = FALSE,M_NONINVTILL = FALSE,
M_PESTICIDES_DST = FALSE,M_SOLIDMANURE = FALSE,M_SSPM = FALSE,M_STRAWRESIDUE = FALSE))
}

}

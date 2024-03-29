#' Calculate the phosphate availability (PBI)
#' 
#' This function calculates the phosphate availability. This value can be evaluated by \code{\link{ind_phosphate_availability}}
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The P-CaCl2 content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' 
#' @import data.table
#' 
#' @examples 
#' calc_phosphate_availability(B_LU_BRP = 265, A_P_AL = 45, A_P_CC = 2.5)
#' calc_phosphate_availability(c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#' 
#' @return 
#' The phosphate availability index estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
calc_phosphate_availability <- function(B_LU_BRP, A_P_AL = NULL, A_P_CC = NULL, A_P_WA = NULL) {
  
  # Load in the crops data set
  crop_code = crop_phosphate = id = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check length of desired input
  arg.length <- max(length(B_LU_BRP),length(A_P_AL),length(A_P_CC),length(A_P_WA))

  # check P input parameters for grassland 
  arg.length.grass <- max(length(A_P_AL),length(A_P_CC))
  if(arg.length.grass > 0){
    checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = arg.length.grass)
    checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = FALSE, len = arg.length.grass)
  }
  
  # check P input parameters for arable soils
  arg.length.arable <- length(A_P_WA)
  if(arg.length.arable > 0){
    checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = arg.length.arable)  
  }
  
  # check crop input
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    id = 1:arg.length,
    A_P_AL = A_P_AL,
    A_P_CC = A_P_CC,
    A_P_WA = A_P_WA,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  setkey(dt, B_LU_BRP)
  dt <- crops.obic[dt]
  setorder(dt, id)
  
  # Calculate the phosphate availability for grass (PBI)
  dt[crop_phosphate == "gras", value := log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2]
  
  # force negative values to be 0 (negative values occur when PAL is large relative to P-CaCl2)
  dt[crop_phosphate == "gras" & value  < 0, value := 0] 
  
  # Calculate the phosphate availability for maize (PBI)
  dt[crop_phosphate == "mais", value := A_P_CC + 0.05 * (A_P_AL / A_P_CC)]
  
  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[crop_phosphate == "arable", value := A_P_WA * 0.1]
  
  # nature 
  dt[crop_phosphate == "nature", value := 0]
  
  # return value
  value <- dt[, value]
  
  return(value)

}

#' Calculate the indicator for the the phosphate availability
#' 
#' This function calculates the indicator for the phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @param D_PBI (numeric) The value of phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @examples 
#' ind_phosphate_availability(D_PBI = 3.5)
#' ind_phosphate_availability(D_PBI = c(0.5,0.8,2.5,5,15,35,75))
#'  
#' @return 
#' The evaluated score for the soil function to supply and buffer phosphorus for crop uptake. A numeric value between 0 and 1.
#' 
#' @export
ind_phosphate_availability <- function(D_PBI) {
  
  # Check inputs
  checkmate::assert_numeric(D_PBI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Evaluate the phosphate availability
  value <- OBIC::evaluate_logistic(D_PBI, b = 1.3, x0 = 1.3, v = 0.35)
  
  # return output
  return(value)
}

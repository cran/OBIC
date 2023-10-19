## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(OBIC)

## ----table--------------------------------------------------------------------
desc <- OBIC::column_description.obic
knitr::kable(desc)


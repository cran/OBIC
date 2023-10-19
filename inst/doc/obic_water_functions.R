## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(OBIC);library(ggplot2);library(data.table)
setDTthreads(1)

## ----GW recharge, fig.width = 4,fig.height = 3, fig.align = 'center'----------

  x = seq(0,600,10)
  y = OBIC::evaluate_logistic(x,0.05,300,2.5)
  y2 = OBIC::evaluate_logistic(x,0.04,130,1.5)
  
  scoring_range <- data.table(class = c("Very low","Low", "Average", "High","Very high"),
                              lower = c(0,0.25,0.5,0.75,1),
                              upper = c(0.25,0.5,0.75,1,1.25))
  
  ggplot() +
    theme_bw() +
    geom_rect(data = scoring_range,aes(xmin = 0, xmax = 600, ymin = lower, ymax = upper, fill = class), alpha = 0.4) +
    geom_line(aes(x,y2), size = 1.2, color = "grey45") +
    geom_line(aes(x,y), size = 1.2) +
    scale_x_continuous(minor_breaks = c(0,100,200,300,400,500,600), breaks =  c(0,100,200,300,400,500,600)) +
    scale_y_continuous(limits = c(0,1.25), minor_breaks = c(0,0.25,0.5,0.75,1,1.25), breaks =  c(0,0.25,0.5,0.75,1,1.25)) +
    labs(x = "Precipitation surplus (mm)", y = "Index score" ) +
    scale_fill_manual(values = c("Very high" = "royalblue1","High" = "limegreen","Average"="yellow","Low"= "orange2", "Very low" = "tomato3"),
                      breaks = c("Very high","High", "Average", "Low","Very low"),
                      name = "")
  
  

## ----include image of formula unsaturated permeability, echo=FALSE, out.width = '45%', out.height = '45%', fig.align = 'center'----
# include graphic
knitr::include_graphics('Formula_K-unsaturated.png')

## ----Unsaturated permeability, fig.width = 4,fig.height = 3, fig.align = 'center'----

  x = seq(0,200,10)
  y = evaluate_logistic(x,0.08,50,0.4)

  ggplot() +
    theme_bw() +
    geom_rect(data = scoring_range,aes(xmin = 0, xmax = 200, ymin = lower, ymax = upper, fill = class), alpha = 0.4) +
    geom_line(aes(x,y), size = 1.2) +
    scale_x_continuous(minor_breaks = c(0,50,100,150,200), breaks =  c(0,50,100,150,200)) +
    scale_y_continuous(limits = c(0,1.25), minor_breaks = c(0,0.25,0.5,0.75,1,1.25), breaks =  c(0,0.25,0.5,0.75,1,1.25)) +
    labs(x = "Unsaturated permeability (cm/d)", y = "Index score" ) +
    scale_fill_manual(values = c("Very high" = "royalblue1","High" = "limegreen","Average"="yellow","Low"= "orange2", "Very low" = "tomato3"),
                      breaks = c("Very high","High", "Average", "Low","Very low"),
                      name = "")


## ----N-efficiency, fig.width = 4,fig.height = 3, fig.align = 'center'---------

  x = seq(0,50,1)
  y = ind_nretention(x,'gw')
  
  scoring_range <- data.table(class = c("Very low","Low", "Average", "High","Very high"),
                              lower = c(0,0.25,0.5,0.75,1),
                              upper = c(0.25,0.5,0.75,1,1.25))
  
  ggplot() +
    theme_bw() +
    geom_rect(data = scoring_range,aes(xmin = 0, xmax = 50, ymin = lower, ymax = upper, fill = class), alpha = 0.4) +
    geom_line(aes(x,y), size = 1.2) +
    scale_x_continuous(minor_breaks = c(0,10,20,30,40,50), breaks = c(0,10,20,30,40,50)) +
    scale_y_continuous(limits = c(0,1.25), minor_breaks = c(0,0.25,0.5,0.75,1,1.25), breaks =  c(0,0.25,0.5,0.75,1,1.25)) +
    labs(x = "Nitrogen leaching (mg NO3/L", y = "Index score" ) +
    scale_fill_manual(values = c("Very high" = "royalblue1","High" = "limegreen","Average"="yellow","Low"= "orange2", "Very low" = "tomato3"),
                      breaks = c("Very high","High","Average","Low","Very low"),
                      name = "")


## ----Pesticide leaching, fig.width = 4,fig.height = 3, fig.align = 'center'----

  x = seq(0,1,0.01)
  y = ind_pesticide_leaching(x)
  
  scoring_range <- data.table(class = c("Very low","Low", "Average", "High","Very high"),
                              lower = c(0,0.25,0.5,0.75,1),
                              upper = c(0.25,0.5,0.75,1,1.25))
  
  ggplot() +
    theme_bw() +
    geom_rect(data = scoring_range,aes(xmin = 0, xmax = 1, ymin = lower, ymax = upper, fill = class), alpha = 0.4) +
    geom_line(aes(x,y), size = 1.2) +
    scale_x_continuous(minor_breaks = c(0,0.25,0.5,0.75,1), breaks =  c(0,0.25,0.5,0.75,1)) +
    scale_y_continuous(limits = c(0,1.25), minor_breaks = c(0,0.25,0.5,0.75,1,1.25), breaks =  c(0,0.25,0.5,0.75,1,1.25)) +
    labs(x = "Leaching risk", y = "Index score" ) +
    scale_fill_manual(values = c("Very high" = "royalblue1","High" = "limegreen","Average"="yellow","Low"= "orange2", "Very low" = "tomato3"),
                      breaks = c("Very high","High","Average","Low","Very low"),
                      name = "")
  

## ----include=FALSE------------------------------------------------------------
knitr::write_bib(c(.packages()), "packages.bib")
knitr::write_bib(file = 'packages.bib')


## ----setup, include=FALSE-----------------------------------------------------
require(data.table)
require(ggplot2)
require(OBIC)
require(patchwork)
knitr::opts_chunk$set(echo = FALSE,
                      collapse = TRUE,
                      comment = "#>")
options(rmarkdown.html_vignette.check_title = FALSE)

## ----make data----------------------------------------------------------------
# make data table
 dt <- data.table(field = factor(seq(1,4,1)),
      A_CLAY_MI = c(15.6, 22.6, 2.9, 3.1),
      A_SILT_MI = c(16.7, 36.6, 8.6, 10.6),
      B_LU_BRP = c(233, 256, 265, 265),
      B_SOILTYPE_AGR = c('zeeklei', 'zeeklei',  'dekzand', 'veen'),
      B_GWL_GLG = c(173, 144, 115, 65),
      B_GWL_GHG = c(21, 70, 49,  9),
      B_GWL_ZCRIT = c(400, 400, 400, 400)
    )
# merge with crop names
dt <- merge(dt, crops.obic[,.(crop_code, crop_name)], by.x = 'B_LU_BRP', by.y = 'crop_code')

## ----show dt------------------------------------------------------------------
knitr::kable(dt, format = 'html')

## ----calc_workability indicator and score-------------------------------------

# calc rsl/D_WO
dt[,D_WO := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR,B_GWL_GLG, B_GWL_GHG, B_GWL_ZCRIT)]

# calculate score
dt[, I_P_WO := ind_workability(D_WO, B_LU_BRP)]


## ----plot regime curve, eval=FALSE, fig.width = 7, fig.height = 4,fig.fullwidth = TRUE----
#  
#  # this is not yet clear (comment Gerard. Please, check and update)
#  #x <- 46:227
#  
#  # try to plot the regime curve
#  #ggplot(data = dt[field == 4], aes(46:227,138-(asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024), color = field))+
#  #  geom_point()
#  
#  #dt4 <- data.table(dt[field == 4],x = 46:227)
#  #dt4[,y := 138 + (asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024) ]
#  #plot(x~y,ylim = c(-100,100),data=dt4)
#  

## ----plot baseline, fig.width = 7, fig.height = 4,fig.fullwidth = TRUE--------
# plot the initial rsl and score
gg <- ggplot(data = dt, 
             aes(x= field, fill = field)) +  geom_col(aes(y = D_WO)) + 
  theme_bw() +theme(axis.text = element_text(size = 10, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10, face = 'bold'),
        legend.text = element_text(size = 10, color = 'black'),
        plot.title = element_text(size = 11)) + ylab('Relative season length')

gg2 <- ggplot(data = dt, 
             aes(x= field, fill = field)) +  geom_col(aes(y = I_P_WO)) + 
  theme_bw() +theme(axis.text = element_text(size = 10, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10, face = 'bold'),
        legend.text = element_text(size = 10, color = 'black'),
        plot.title = element_text(size = 11)) + ylab('Workability score')

(gg|gg2) + plot_layout(guides = 'collect') + plot_annotation(caption = 'Baseline workability scores.',
                                                             theme = theme(plot.caption = element_text(hjust = 0)))


## ----scoring with peas, fig.width = 7, fig.height = 4,fig.fullwidth = TRUE----

# calc rsl/D_WO with peas instead of initial crops
dt[, D_WO_pea := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP = rep(308,4), B_SOILTYPE_AGR,B_GWL_GLG, B_GWL_GHG, B_GWL_ZCRIT)]

# calculate score
dt[, I_P_WO_pea := ind_workability(D_WO_pea, B_LU_BRP = rep(308,4))]

# plot scores
gg <- ggplot(data = dt, 
             aes(x= field, fill = field)) +  geom_col(aes(y = D_WO_pea)) + 
  theme_bw() +theme(axis.text = element_text(size = 10, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10, face = 'bold'),
        legend.text = element_text(size = 10, color = 'black'),
        plot.title = element_text(size = 11)) + ylab('Relative season length')

gg2 <- ggplot(data = dt, 
             aes(x= field, fill = field)) +  geom_col(aes(y = I_P_WO_pea)) + 
  theme_bw() +theme(axis.text = element_text(size = 10, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10, face = 'bold'),
        legend.text = element_text(size = 10, color = 'black'),
        plot.title = element_text(size = 11)) + ylab('Workability score')

(gg|gg2) + plot_layout(guides = 'collect') + plot_annotation(caption = 'Workability scores when crop is peas.',
                                                             theme = theme(plot.caption = element_text(hjust = 0)))


## ----table season lengths-----------------------------------------------------
seasondt <- season.obic[landuse %in% c('suikerbieten', 'wintertarwe', 'erwten, bonen', 'beweid bemaaid gras')&soiltype.m == 'klei',
                               .(landuse, req_days_pre_glg, req_days_post_glg, total_days)]
knitr::kable(seasondt, format = 'html')

## ----show working depth determining code, include= TRUE, echo=TRUE,  fig.width = 7, fig.height = 4,fig.fullwidth = TRUE----
## merge with OBIC crop and soil table
  
  # load other tables
  crops.obic <- OBIC::crops.obic
  season.obic <- OBIC::season.obic
  
  # merge tables
  dt <- merge(dt, crops.obic[, list(crop_code, crop_waterstress, crop_season)], 
              by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.m)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  dt <- merge(dt, season.obic, by.x = c('crop_season','soiltype.m'), by.y = c('landuse', 'soiltype.m'))
  
## determine workability key numbers

    # new parameters to be added
    cols <- c('gws_sub_workingdepth','spring_depth')
    
  # sandy soils with variable silt content
    dt[soiltype.m == 'zand' & A_SILT_MI < 10, c(cols) := list(45,35)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & A_SILT_MI < 20, c(cols) := list(55,30)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 20, c(cols) := list(60,30)]
      
    # loess and peat soils
    dt[soiltype.m == 'loess',c(cols) := list(65,12)]
    dt[soiltype.m == 'veen',c(cols) := list(55,22)]
    
    # clay soils
    dt[soiltype.m == 'klei' & A_CLAY_MI < 12, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 12 & A_CLAY_MI < 17, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 17 & A_CLAY_MI < 25, c(cols) := list(75,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 25 & A_CLAY_MI < 35, c(cols) := list(65,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 35, c(cols) := list(45,15)]
    
    # Overwrite spring working depth for perennial crops
  crops.p <- c('boomteelt', 'overig boomteelt', 'groot fruit','grasland zonder herinzaai', 'grasland met herinzaai')
  dt[crop_waterstress %in% crops.p,spring_depth := 0]


## ----add soil params----------------------------------------------------------
  
  ## determine workability key numbers

    # new parameters to be added
    cols <- c('gws_sub_workingdepth','spring_depth')
    
    # sandy soils with variable silt content
    dt[soiltype.m == 'zand' & A_SILT_MI < 10, c(cols) := list(45,35)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & A_SILT_MI < 20, c(cols) := list(55,30)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 20, c(cols) := list(60,30)]
      
    # loess and peat soils
    dt[soiltype.m == 'loess',c(cols) := list(65,12)]
    dt[soiltype.m == 'veen',c(cols) := list(55,22)]
    
    # clay soils
    dt[soiltype.m == 'klei' & A_CLAY_MI < 12, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 12 & A_CLAY_MI < 17, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 17 & A_CLAY_MI < 25, c(cols) := list(75,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 25 & A_CLAY_MI < 35, c(cols) := list(65,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 35, c(cols) := list(45,15)]

## ----show soil working params-------------------------------------------------

# make table with understandable names
dtt <- copy(dt)
setnames(dtt, c('gws_sub_workingdepth', 'spring_depth'), c('Water lvl below workingdepth', 'Spring working depth'))

# calculate required depth
dtt[,required_depth := `Water lvl below workingdepth`+`Spring working depth`]
dtt <- dtt[,.(field, B_GWL_GLG, B_GWL_GHG, `Water lvl below workingdepth`, `Spring working depth`, required_depth)]

# display table
knitr::kable(dtt, format = 'html', caption = 'Ground water parameters for workability when cultivating peas')

## ----show lower water , fig.width = 7, fig.height = 4,fig.fullwidth = TRUE----

# calc rsl/D_WO with peas instead of initial crops
dt[,D_WO_dry := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR,
                            B_GWL_GLG*1.3, B_GWL_GHG*1.3, B_GWL_ZCRIT)]
# calculate score
dt[, I_P_WO_dry := ind_workability(D_WO_dry, B_LU_BRP)]

# plot scores
gg <- ggplot(data = dt,aes(x= field, fill = field)) +  
      geom_col(aes(y = D_WO_dry)) + 
      theme_bw() +
      theme(axis.text = element_text(size = 10, color = 'black'),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_text(size = 10),
            legend.title = element_text(size = 10, face = 'bold'),
            legend.text = element_text(size = 10, color = 'black'),
            plot.title = element_text(size = 11)) + ylab('Relative season length')

gg2 <- ggplot(data = dt, aes(x= field, fill = field)) +  
       geom_col(aes(y = I_P_WO_dry)) + 
       theme_bw() +
       theme(axis.text = element_text(size = 10, color = 'black'),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.title = element_text(size = 10),
             legend.title = element_text(size = 10, face = 'bold'),
             legend.text = element_text(size = 10, color = 'black'),
             plot.title = element_text(size = 11)) + ylab('Workability score')

# plot combined plot
(gg|gg2) + plot_layout(guides = 'collect') + plot_annotation(caption = 'Workability with lowered GLG and GHG.',
                                                             theme = theme(plot.caption = element_text(hjust = 0)))


## ----plot with low zcrit , fig.width = 7, fig.height = 4,fig.fullwidth = TRUE----

# calc rsl/D_WO with peas instead of initial crops
dt[,D_WO_lzcrit := calc_workability(A_CLAY_MI, A_SILT_MI, 
                                    B_LU_BRP, B_SOILTYPE_AGR,
                                    B_GWL_GLG, B_GWL_GHG, 
                                    B_GWL_ZCRIT = rep(50,4))]
# calculate score
dt[, I_P_WO_lzcrit := ind_workability(D_WO_lzcrit, B_LU_BRP)]

# plot scores
gg <- ggplot(data = dt, aes(x= field, fill = field)) +  
      geom_col(aes(y = D_WO_lzcrit)) + 
      theme_bw() +
      theme(axis.text = element_text(size = 10, color = 'black'),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_text(size = 10),
            legend.title = element_text(size = 10, face = 'bold'),
            legend.text = element_text(size = 10, color = 'black'),
            plot.title = element_text(size = 11)) + ylab('Relative season length') 

gg2 <- ggplot(data = dt,aes(x= field, fill = field)) +  
       geom_col(aes(y = I_P_WO_lzcrit)) + 
       theme_bw() +
       theme(axis.text = element_text(size = 10, color = 'black'),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.title = element_text(size = 10),
             legend.title = element_text(size = 10, face = 'bold'),
             legend.text = element_text(size = 10, color = 'black'),
             plot.title = element_text(size = 11)) + ylab('Workability score')

# combine plots
(gg|gg2) + plot_layout(guides = 'collect') + plot_annotation(caption = 'Workability scores with B_GWL_ZCRIT set to 50.',
                                                             theme = theme(plot.caption = element_text(hjust = 0)))



## ---- include=FALSE-----------------------------------------------------------
knitr::write_bib(c(.packages()), "packages.bib")
knitr::write_bib(file = 'packages.bib')


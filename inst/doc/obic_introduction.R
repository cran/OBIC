## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include = FALSE---------------------------------------------------
  # load packages
  library(OBIC); library(data.table); library(ggplot2);require(patchwork)
  setDTthreads(1)

  # load binnenveld data.table
  binnenveld <- as.data.table(OBIC::binnenveld)

## ----showbinnenveld-----------------------------------------------------------
  dim(binnenveld)
  binnenveld[1]

## ----echo=FALSE, out.width = '85%', out.height = '85%', fig.cap = 'Figure 1. Graphic representation of how measured soil properties are aggregated to scores.'----
# ![](OBIC_score_integratie.png){widht = 25%, height = 20%}
knitr::include_graphics('../vignettes/OBIC_score_integratie_2.png')

## -----------------------------------------------------------------------------
  # select the relevant columns without management measures and data from Visual Soil Assessment
  cols <- colnames(binnenveld)[!grepl('_BCS$|^M_',colnames(binnenveld))]
  
  # select the first field, a grassland field 
  dt <- binnenveld[ID==1,mget(cols)]

  # run the obic_field with default management measures and no visual assessment data

  # test the obic field function via obic_field and give only the final score
  obic_field(B_SOILTYPE_AGR =  dt$B_SOILTYPE_AGR, B_GWL_CLASS =  dt$B_GWL_CLASS,
             B_SC_WENR = dt$B_SC_WENR, B_HELP_WENR = dt$B_HELP_WENR, B_AER_CBS = dt$B_AER_CBS,
             B_GWL_GLG = dt$B_GWL_GLG, B_GWL_GHG = dt$B_GWL_GHG, B_GWL_ZCRIT = dt$B_GWL_ZCRIT,
             B_DRAIN = FALSE, B_FERT_NORM_FR = 1,
             B_LU_BRP = dt$B_LU_BRP, A_SOM_LOI = dt$A_SOM_LOI, A_SAND_MI = dt$A_SAND_MI,
             A_SILT_MI = dt$A_SILT_MI, A_CLAY_MI = dt$A_CLAY_MI, A_PH_CC = dt$A_PH_CC,
             A_N_RT = dt$A_N_RT, A_CN_FR = dt$A_CN_FR,
             A_S_RT = dt$A_S_RT, A_N_PMN = dt$A_N_PMN,
             A_P_AL = dt$A_P_AL, A_P_CC = dt$A_P_CC, A_P_WA = dt$A_P_WA, A_CEC_CO = dt$A_CEC_CO,
             A_CA_CO_PO = dt$A_CA_CO_PO, A_MG_CO_PO = dt$A_MG_CO_PO, A_K_CO_PO = dt$A_K_CO_PO,
             A_K_CC = dt$A_K_CC, A_MG_CC = dt$A_MG_CC, A_MN_CC = dt$A_MN_CC,
             A_ZN_CC = dt$A_ZN_CC, A_CU_CC = dt$A_CU_CC, output = 'obic_score')
  
  # test the obic field function via obic_field_dt and give only the final score
  obic_field_dt(dt,output = 'obic_score')

## ----results = FALSE----------------------------------------------------------
  # run obic_field to retrieve indicators
  obic_field_dt(dt, output = 'indicators')

  # run obic_field to retrieve aggregated scores 
  # for chemistry, biology, fysics, management and environment
  obic_field_dt(dt, output = 'scores')
  
  # the default option is to retrieve all output
  obic_field_dt(dt, output = 'all')

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE, echo=FALSE-----------
  # make a data.table for sandy and clay soil
  dt.test <- data.table(B_SOILTYPE_AGR = c(rep('dekzand',10),rep('rivierklei',10)),
                        A_SOM_LOI = c(seq(0.1,10,length.out = 10), seq(0.1,10,length.out = 10)),
                        A_CLAY_MI = c(rep(5,10),rep(25,10)))

  # estimate bulk density (D_BDS)
  dt.test[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]
  
  # plot output
  ggplot(data = dt.test,
         aes(x = A_SOM_LOI, y = D_BDS, group = B_SOILTYPE_AGR, color = B_SOILTYPE_AGR)) + 
    geom_line() + geom_point(size=3) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
    ylab('Bulk density (kg / m3)') + xlab('Soil organic matter content (%)') + 
    theme(legend.position = c(0.8,0.8)) + ggtitle('Estimate bulk density from SOM and clay content') 

## ----results = FALSE----------------------------------------------------------
  # estimate soil bulk density
  dt[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]

  # estimate soil sampling depth
  dt[, D_RD := calc_root_depth(dt$B_LU_BRP)]

  # estimate Carbon pool
  dt[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  
  # estimate age of grassland
  dt[,D_GA := calc_grass_age(ID, B_LU_BRP)]
  
  # estimate crop rotation fraction for sugar beet
  dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]

## ----results = FALSE----------------------------------------------------------
  # estimate nitrogen supply (kg N / ha)
  dt[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two land uses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate default properties needed to estimate NLV
  dt.test[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]
  dt.test[, D_RD := calc_root_depth(B_LU_BRP)]
  dt.test[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt.test[, D_GA := calc_grass_age(ID, B_LU_BRP)]

  # estimate nitrogen supply (kg N / ha)
  dt.test[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged NLV for both soils
  dt.test1 <- dt.test[,list(D_NLV = mean(D_NLV)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_NLV, x = luse, fill = luse)) + geom_col() +  theme_bw() + ylim(0,200)+
        ylab('N supplying capacity (kg N / ha)') + xlab('Landuse') + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(legend.position = c(0.2,0.85),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10),
              axis.text = element_text(size=10)) + ggtitle('N supplying capacity')
     
  
  # estimate NLV for grassland soil over range of N-total levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite N levels of the soil and the soil texture to sand with a range to illustrate impact of N total on NLV
  dt.test2[,B_LU_BRP := 265]
  dt.test2[, A_N_RT := seq(100,5000,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_GA := 5]
  dt.test2[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_N_RT, y = D_NLV)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
    theme(plot.title = element_text(size=10),
              legend.text = element_text(size=10),
              axis.text = element_text(size=10))+
    ylab('N supplying capacity (kg N / ha)') + xlab('Total Nitrogen content (mg / kg)') +
    ggtitle('Relationship NLV (grasland) and N total') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate phosphate availability index (unitless)
  dt[, D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grass and continue maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate PBI
  dt.test[, D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged PBI for both soils
  dt.test1 <- dt.test[,list(D_PBI = mean(D_PBI)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_PBI, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('P availability index (-)') + xlab('Landuse') + ylim(0,7)+
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('P availability index')
     
  
  # estimate PBI for grassland soil over range of P-CaCl2 levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite P-CaCl2 levels
  dt.test2[, A_P_CC := seq(0.5,10,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_P_CC, y = D_PBI)) + geom_line() + geom_point(size=3) +
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('P availability index (-)') + xlab('Available P-CaCl2 (mg / kg)') +
        ggtitle('Relationship PBI (grasland) and P-CaCl2') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate potassium availability index (unitless)
  dt[, D_K :=  calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                                           A_PH_CC, A_CEC_CO, A_K_CO_PO, A_K_CC)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate K-availability index
  dt.test[, D_K :=  calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                                           A_PH_CC, A_CEC_CO, A_K_CO_PO, A_K_CC)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged K-availability indices for both soils
  dt.test1 <- dt.test[,list(D_K = mean(D_K)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_K, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('K availability index (-)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('K availability index')
     
  
  # estimate K-supply for grassland soil over range of K-CaCl2 levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite K-CaCl2 levels
  dt.test2[, A_K_CC := seq(5,500,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_K :=  calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                                           A_PH_CC, A_CEC_CO, A_K_CO_PO, A_K_CC)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_K_CC, y = D_K)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('K availability index (-)') + xlab('Available K-CaCl2 (mg / kg)') +
        ggtitle('Relationship K-availability (grasland) and K-CaCl2') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate S supplying capacity
  dt[, D_SLV := calc_slv(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate bulk density
  dt.test[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]

  # estimate Mg-availability index
  dt.test[,D_SLV := calc_slv(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged SLV for both soils
  dt.test1 <- dt.test[,list(D_SLV = mean(D_SLV)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_SLV, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('S supplying capacity (kg S / ha)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('S supplying capacity (kg S / ha)')
     
  
  # estimate SLV for grassland soil over range of total S levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite A_S_RT levels
  dt.test2[, A_S_RT := seq(500,5000,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_SLV := calc_slv(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_S_RT, y = D_SLV)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('S supplying capacity (kg S / ha)') + xlab('total S (mg / kg)') +
        ggtitle('Relationship SLV (grasland) and total S') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate magnesium availability index (unitless)
  dt[, D_MG := calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, 
                                           A_CLAY_MI, A_PH_CC, A_CEC_CO,
                                           A_K_CO_PO, A_MG_CC, A_K_CC)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate Mg-availability index
  dt.test[, D_MG := calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, 
                                           A_CLAY_MI, A_PH_CC, A_CEC_CO,
                                           A_K_CO_PO, A_MG_CC, A_K_CC)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged Mg-availability indices for both soils
  dt.test1 <- dt.test[,list(D_MG = mean(D_MG)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_MG, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('Mg availability index (-)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('Mg availability index')
     
  
  # estimate Mg-supply for grassland soil over range of Mg-CaCl2 levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite Mg-CaCl2 levels
  dt.test2[, A_MG_CC := seq(5,500,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_MG := calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, 
                                           A_CLAY_MI, A_PH_CC, A_CEC_CO,
                                           A_K_CO_PO, A_MG_CC, A_K_CC)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_MG_CC, y = D_MG)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('Mg availability index (-)') + xlab('Available Mg-CaCl2 (mg / kg)') +
        ggtitle('Relationship Mg-availability (grasland) and Mg-CaCl2') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate Cu availability index
  dt[, D_CU := calc_copper_availability(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, A_K_CC, A_MN_CC, A_CU_CC)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate Cu-availability index
  dt.test[, D_CU := calc_copper_availability(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, A_K_CC, A_MN_CC, A_CU_CC)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged Cu-availability indices for both soils
  dt.test1 <- dt.test[,list(D_CU = mean(D_CU)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_CU, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('Cu availability index (-)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('Cu availability index')
     
  
  # estimate Cu-supply for grassland soil over range of Cu-CaCl2 levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite Cu-CaCl2 levels
  dt.test2[, A_CU_CC := seq(5,500,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_CU := calc_copper_availability(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, A_K_CC, A_MN_CC, A_CU_CC)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_CU_CC, y = D_CU)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('Cu availability index (-)') + xlab('Available Cu-CaCl2 (ug / kg)') +
        ggtitle('Relationship Cu-availability (grasland) and Cu-CaCl2') 

  # plot side by side
  p1 + p2

## ----results = FALSE----------------------------------------------------------
  # estimate Zn availability index
  dt[,  D_ZN := calc_zinc_availability(B_LU_BRP, B_SOILTYPE_AGR, A_PH_CC, A_ZN_CC)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # estimate Zn-availability index
  dt.test[, D_ZN := calc_zinc_availability(B_LU_BRP, B_SOILTYPE_AGR, A_PH_CC, A_ZN_CC)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged Zn-availability indices for both soils
  dt.test1 <- dt.test[,list(D_ZN = mean(D_ZN)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_ZN, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('Zn availability index (-)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('Zn availability index')
     
  
  # estimate Zn-supply for grassland soil over range of Zn-CaCl2 levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite Zn-CaCl2 levels
  dt.test2[, A_ZN_CC := seq(50,5000,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_ZN := calc_zinc_availability(B_LU_BRP, B_SOILTYPE_AGR, A_PH_CC, A_ZN_CC)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(x = A_ZN_CC, y = D_ZN)) + geom_line() + geom_point(size=3) + 
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('Zn availability index (-)') + xlab('Available Zn-CaCl2 (ug / kg)') +
        ggtitle('Relationship Zn-availability (grasland) and Zn-CaCl2') 

  # plot side by side
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # estimate distance to required pH
#    dt[, D_PH_DELTA := calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
#                                     D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # define a field with two landuses: grassland and maize
  dt.test <- binnenveld[ID %in% c(1,11)]
  
  # Calculate the crop rotation fraction
  dt.test[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
  dt.test[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
  dt.test[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
  dt.test[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
  dt.test[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
  dt.test[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
    
  # estimate delta-pH
  dt.test[, D_PH_DELTA := calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                   D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
  
  # add group for figure
  dt.test[,luse := fifelse(ID==11,'arable','grasland')]
  
  # estimate averaged delta-pH for both soils
  dt.test1 <- dt.test[,list(D_PH_DELTA = mean(D_PH_DELTA)), by = 'luse']
  
   # plot output
  p1 <- ggplot(data = dt.test1,aes(y = D_PH_DELTA, x = luse, fill = luse)) + 
        geom_col(show.legend = FALSE) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        ylab('desired pH-change (-)') + xlab('Landuse') + 
        theme(plot.title = element_text(size=10),
              axis.text = element_text(size=10)) + 
        ggtitle('desired pH-change (-)')
     
  
  # estimate desired pH-change for grassland soil over range of pH levels in soil
  dt.test2 <- dt.test[ID==1]
  
  # overwrite initial pH
  dt.test2[, A_PH_CC := seq(3,10,length.out = .N)]
  dt.test2[, B_SOILTYPE_AGR := 'dekzand']
  dt.test2[, D_PH_DELTA := calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                   D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
    
  # plot output
  p2 <- ggplot(data = dt.test2,
               aes(y = D_PH_DELTA, x = A_PH_CC)) + geom_line() + geom_point(size=3) +  
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+
        ylab('desired pH-change (-)') + xlab('pH value') +
        ggtitle('Relationship desired pH-change and pH') 

  # plot side by side
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # estimate importance of CEC supporting crop development
#    dt[, D_CEC := calc_cec(A_CEC_CO)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE, eval = FALSE----
#    # Make a data table with CEC levels between 1 and 100 mmol+/kg
#    dt <- data.table(A_CEC_CO = seq(1,100,1))
#  
#    # Add D_CEC
#    dt[,D_CEC := calc_cec(A_CEC_CO)]
#  
#    # plot output
#    p1 <- ggplot(data = dt,aes(y = D_CEC, x = A_CEC_CO)) +
#          geom_point(show.legend = FALSE) + geom_line()+ theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
#          ylab('CEC evaluation') + xlab('Cation exchange capacity') +
#          theme(plot.title = element_text(size=10),
#                axis.text = element_text(size=10)) +
#          ggtitle('')
#  
#     p1

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # reformat GWL_CLASS
#    dt[, B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
#  
#    # estimate risk on yield reduction to drought stress or wetness stress
#    dt[, D_WSI_DS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress')]
#    dt[, D_WSI_WS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wetnessstress')]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # subset a series of agricultural fields
  dt.test <- binnenveld[ID %in% c(1,181,8,125,5,11)]
  
  # set ID to 1:5 for plotting purpose
  dt.test[,pID := .GRP, by = ID]
  
  # reformat GWL_CLASS
  dt.test[, B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
  
  # estimate delta-pH
  dt.test[, D_WSI_DS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress'),by=ID]
  dt.test[, D_WSI_WS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wetnessstress'),by=ID]
  
  # estimate averaged moisture stress for the different fields
  dt.test1 <- dt.test[,list(D_WSI_DS = mean(D_WSI_DS),
                            D_WSI_WS = mean(D_WSI_WS)), by = 'pID']
  
  # melt dt.test1
  dt.test1 <- melt(dt.test1,id.vars = 'pID', variable.name = 'stresstype')
  
  # plot output
  p1 <- ggplot(data = dt.test1,aes(y = value, x = pID, fill = stresstype)) + 
        geom_bar(stat='identity') +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ ylim(0,55)+
        theme(legend.position = c(0.3,0.8),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10)) +
        ylab('yield reduction (%)') + xlab('Field') + 
        ggtitle('Soil water stress index')
     
  
  # estimate moisture stress with variable Gt for an arable field
  dt.test2 <- dt.test[ID==11][1]
  dt.test2 <- dt.test2[rep(1,4)]
  dt.test2[,B_GWL_CLASS := c('GtIII','GtIV','GtV','GtVI')]
  
  # update WSI
  dt.test2[, D_WSI_DS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress')]
  dt.test2[, D_WSI_WS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wetnessstress')]
  
   # estimate averaged moisture stress for the different fields
  dt.test2 <- dt.test2[,list(D_WSI_DS = mean(D_WSI_DS),
                            D_WSI_WS = mean(D_WSI_WS)), by = 'B_GWL_CLASS']
  
  # melt dt.test2
  dt.test2 <- melt(dt.test2,id.vars = 'B_GWL_CLASS', variable.name = 'stresstype')
    
  # plot output
  p2 <- ggplot(data = dt.test2,aes(y = value, x = B_GWL_CLASS, fill = stresstype)) + 
        geom_bar(stat='identity') +  
        theme_bw() + 
        scale_fill_viridis_d()+ scale_color_viridis_d()+ ylim(0,30)+
        theme(legend.position = c(0.3,0.8),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10)) +
        ylab('yield reduction (%)') + xlab('GWL_CLASS') + 
        ggtitle('Soil water stress index')

  # plot side by side
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # estimate the presence / risk for surface sealing and wind erodibility
#    dt[, D_SE := calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
#    dt[, D_WE := calc_winderodibility(B_LU_BRP, A_CLAY_MI, A_SILT_MI)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # make a data.table for sandy and clay soil with potatoes
  dt.test1 <- data.table(B_LU_BRP = rep(859,20),
                        A_CLAY_MI = c(seq(1,25,length.out = 10), seq(1,25,length.out = 10)),
                        A_SOM_LOI = c(rep(3.5,10),rep(3.5,10)),
                        A_SILT_MI = c(rep(5,10),rep(5,10)))

  # make a data.table for sandy and clay soil with grassland
  dt.test2 <- data.table(B_LU_BRP = rep(265,20),
                        A_CLAY_MI = c(seq(1,25,length.out = 10), seq(1,25,length.out = 10)),
                        A_SOM_LOI = c(rep(5,10),rep(5,10)),
                        A_SILT_MI = c(rep(5,10),rep(5,10)))
  
  # combine both
  dt.test <- rbind(dt.test1,dt.test2)
  dt.test[, B_SOILTYPE_AGR := fifelse(A_CLAY_MI < 15,'dekzand','rivierklei')]
  
  # estimate the presence / risk for surface sealing and wind erodibility
  dt.test[, D_SE := calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  dt.test[, D_WE := calc_winderodibility(B_LU_BRP, A_CLAY_MI, A_SILT_MI)]
  
  # set land use
  dt.test[,luse := fifelse(B_LU_BRP==265,'grass','cropland')]
   
  # plot output sealing
  p1 <- ggplot(data = dt.test,
               aes(x = A_CLAY_MI, y = D_SE, color = luse,group = luse)) + 
        geom_line() + geom_point(size=3) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        ylab('Soil Sealing (-)') + xlab('Clay content (%)') + ylim(4,12)+
        theme(legend.position = c(0.4,0.8),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10)) + 
        ggtitle('Soil sealing given SOM and clay content') +
        guides(color = guide_legend(title="land use"))

  # plot output wind erodibility
  p2 <- ggplot(data = dt.test,
               aes(x = A_CLAY_MI, y = D_WE, color = luse)) + 
        geom_line() + geom_point(size=3) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        ylab('Wind Erodibility (-)') + xlab('Clay content (%)') + ylim(0,1)+
        theme(legend.position = c(0.7,0.8),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10)) + 
        ggtitle('Wind erodibility given soil texture and landuse') +
        guides(color = guide_legend(title="land use"))
  
  # plot side by side
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # assess the crumbleability and aggregate stability
#    dt[, D_CR := calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]
#    dt[, D_AS := calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # make a data.table for crumbleability
  dt.test1 <- data.table(A_CLAY_MI = c(seq(1,25,length.out = 10), seq(1,25,length.out = 10)),
                        A_SOM_LOI = c(rep(3.5,10),rep(3.5,10)),
                        A_PH_CC = rep(6,20))

  # make a data.table for aggregate stability
  dt.test2 <- data.table(B_SOILTYPE_AGR = rep('dekzand', 8), A_SOM_LOI = rep(3.5,8), A_K_CO_PO = seq(28,0,-4),
                         A_CA_CO_PO = seq(30,100,10), A_MG_CO_PO = seq(28,0,-4))

  # make second data.table for aggregate stability with clay soil
  dt.test3 <- data.table(B_SOILTYPE_AGR = rep('rivierklei', 8), A_SOM_LOI = rep(3.5,8), A_K_CO_PO = seq(28,0,-4),
                         A_CA_CO_PO = seq(30,100,10), A_MG_CO_PO = seq(28,0,-4))

  dt2 <- rbindlist(list(dt.test2, dt.test3))
  
  # Make a random aggragate stabilty data table with variying cec values
  dt4 <- data.table(B_SOILTYPE_AGR = rep('rivierklei', 460), A_SOM_LOI = rep(3.5,460),
                    A_CA_CO_PO = rep(seq(50,95,1),10), A_K_CO_PO = c(runif(460, min = 0, max = 10)))
  dt4 <- dt4[A_K_CO_PO+ A_CA_CO_PO >= 100, A_K_CO_PO := 100 - A_CA_CO_PO]
  dt4 <- dt4[,A_MG_CO_PO := 100 - A_CA_CO_PO - A_K_CO_PO]

  # crumbleability and aggregatestability
  dt.test1[, D_CR := calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]
  dt4 <- dt4[, D_AS := calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]
 
   
  # plot output crumbleabilty
  p1 <- ggplot(data = dt.test1,
               aes(x = A_CLAY_MI, y = D_CR)) + 
        geom_line() + geom_point(size=3) +  theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        ylab('Crumbleability') + xlab('Clay content (%)') + ylim(4,12)+
        theme(legend.position = c(0.4,0.8),
              plot.title = element_text(size=10),
              legend.text = element_text(size=10)) + 
    ggtitle('Soil crumbleability given SOM and clay content')   

  # plot output aggregate stability
    p4 <- ggplot(data = dt4,
               aes(x = A_CA_CO_PO, y = D_AS, colour = A_MG_CO_PO)) + 
        geom_point(size=2) + theme_bw() + scale_fill_viridis_d()+ scale_colour_viridis_b()+ 
        ylab('Distance to optimum CEC occupation') + xlab('Calcium ocuppation') + ylim(0,1)+
        theme(plot.title = element_text(size=10),
              legend.position = c(0.35,0.75),
              legend.text = element_text(size=10),
              legend.title = element_text(size=10)) +
    ggtitle('Aggregate stability\ngiven variation in Ca, Mg and K')+
      guides(colour = guide_legend(title="Mg occupation (%)"))
    
  
  # plot side by side
  p1 + p4

## ----results = FALSE, eval = FALSE--------------------------------------------
#      # overwrite soil physical functions for compaction when BCS is available
#      dt[,D_P_CO := (3 * A_EW_BCS + 3 * A_SC_BCS + 3 * A_RD_BCS  - 2 * A_P_BCS - A_RT_BCS)/18]
#      dt[,D_P_CO := pmax(0, D_P_CO)]
#      dt[,I_P_CO := fifelse(is.na(D_P_CO),I_P_CO,D_P_CO)]
#  
#      # overwrite soil physical functions for aggregate stability when BCS is available
#      dt[,D_P_CEC := (3 * A_EW_BCS + 3 * A_SS_BCS - A_C_BCS)/12]
#      dt[,D_P_CEC := pmax(0, D_P_CEC)]
#      dt[,I_P_CEC := fifelse(is.na(D_P_CEC),I_P_CEC,D_P_CEC)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
    # make a data table with some BCS values
    dt <- binnenveld[ID == 1]
    dt[,year := 1:.N, by=ID]
    dt <- dt[year==1]
    
    # reformat B_SC_WENR
    dt[, B_SC_WENR := format_soilcompaction(B_SC_WENR)]
    
    # calculate soil compaction and aggregate stability
    dt[, I_P_CO := ind_compaction(B_SC_WENR)]
    dt[, D_AS := calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]
    dt[, I_P_CEC:= ind_aggregatestability(D_AS)]

    # make three duplicates
    dt <- dt[rep(1,3)][,id:=.I]
    
    # set BCS all from bad quality to good quality
    
      # colnames
      bcscols <- names(dt)[grep('_BCS$',names(dt))]
    
      # set BCS
      dt[id==1, c(bcscols) := 0][,c('A_P_BCS','A_C_BCS','A_RT_BCS') := 2]
      dt[id==2, c(bcscols) := 1][,c('A_P_BCS','A_C_BCS','A_RT_BCS') := 1]
      dt[id==3, c(bcscols) := 2][,c('A_P_BCS','A_C_BCS','A_RT_BCS') := 0]
   
    # overwrite soil physical functions for compaction when BCS is available
    dt[,D_P_CO := (3 * A_EW_BCS + 3 * A_SC_BCS + 3 * A_RD_BCS  - 2 * A_P_BCS - A_RT_BCS)/18]
    dt[,D_P_CO := pmax(0.05, D_P_CO)]
    dt[,I_P_CO2 := fifelse(is.na(D_P_CO),I_P_CO,D_P_CO)]
    
    # overwrite soil physical functions for aggregate stability when BCS is available
    dt[,D_P_CEC := (3 * A_EW_BCS + 3 * A_SS_BCS - A_C_BCS)/12]
    dt[,D_P_CEC := pmax(0.05, D_P_CEC)]
    dt[,I_P_CEC2 := fifelse(is.na(D_P_CEC),I_P_CEC,D_P_CEC)]
    
    # melt 
    dt2 <- melt(dt[,.(id,I_P_CEC,I_P_CO,I_P_CEC2,I_P_CO2)],
                id.vars = c('id'))
    dt2[,treatment := fifelse(grepl('2$',variable),'corrected','uncorrected')]
    
    p1 <- ggplot(data = dt2[grepl('CEC',variable)],
          aes(y = value, x = id-1, fill = treatment)) + 
          geom_bar(stat='identity',position=position_dodge())+
          theme_bw() + scale_fill_viridis_d()+ 
          ylab('Soil compaction index') + xlab('Fieldscore VSA')+
          theme(legend.position = 'none',
                plot.title = element_text(size=8),
                legend.text = element_text(size=10),
                axis.text = element_text(size=10)) + 
          ggtitle('Soil compaction before and after\ncorrection with VSA data\nfor a soil with low compaction')
    
    p2 <- ggplot(data = dt2[grepl('CO',variable)],
          aes(y = value, x = id-1, fill = treatment)) + 
          geom_bar(stat='identity',position=position_dodge())+
          theme_bw() + scale_fill_viridis_d()+ 
          ylab('Aggregate stability index') + xlab('Fieldscore VSA')+
          theme(legend.position = c(0.3,0.8),
                plot.title = element_text(size=8),
                axis.text = element_text(size=10),
                legend.text = element_text(size=8)
                ) + 
          ggtitle('Aggregate stability before and\nafter correction withVSA data\nfor a soil with a moderate aggregate stability')
        
    # print both figures
    p1+p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # estimate the plant available water in topsoil
#    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'plant available water')]
#  
#    # estimate the water holding capacity in topsoil
#    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'water holding capacity')]
#  
#    # estimate the moisture content of the wilting point in topsoil
#    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'wilting point')]
#  
#    # estimate the moisture content of the field capacity in topsoil
#    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'field capacity')]
#  
#    # estimate the saturated permeability in topsoil
#    dt[, D_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'Ksat')]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
    # Create data
    dt <- data.table(A_CLAY_MI = c(seq(1,36,2.5), rep(5,45)),
                     plot = c(rep('klei',15), rep('zand',15), rep('silt', 15), rep('som',15)),
                     A_SAND_MI = c(rep(20,15), seq(1,36,2.5), rep(20,30)),
                     A_SILT_MI = c(rep(20,30), seq(1,36,2.5), rep(20,15)),
                     A_SOM_LOI = c(rep(2, 45), seq(0.1,5.7,0.4))
                     )

    # Calculate water retention values
    dt <- dt[, paw := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'plant available water')]
    dt <- dt[, whc := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'water holding capacity')]
    dt <- dt[, wp := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'wilting point')]
    dt <- dt[, fc := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'field capacity')]
    dt <- dt[, ksat:= calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'Ksat')]

    
    pclay <- ggplot(data = dt[plot == 'klei'],
               aes(x = A_CLAY_MI, y = paw)) + geom_line() + geom_point(size=3) +  theme_bw() + 
              scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+ ylim(50,80)+
        ylab('PAW (mm)') + xlab('Clay content (%)') +
        ggtitle('Relation between PAW and clay') 

    pcsand <- ggplot(data = dt[plot == 'zand'],
               aes(x = A_SAND_MI, y = paw)) + geom_line() + geom_point(size=3) +  theme_bw() + 
            scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+ ylim(50,80)+
        ylab('PAW (mm)') + xlab('Sand content (%)') +
        ggtitle('Relation between PAW and sand')
    
    pcsilt <- ggplot(data = dt[plot == 'silt'],
               aes(x = A_SILT_MI, y = paw)) + geom_line() + geom_point(size=3) +  theme_bw() + 
      scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+ ylim(50,80)+
        ylab('PAW (mm)') + xlab('Silt content (%)') +
        ggtitle('Relation between PAW and silt')
    
    pcsom <- ggplot(data = dt[plot == 'som'],
               aes(x = A_SOM_LOI, y = paw)) + geom_line() + geom_point(size=3) +  theme_bw() + 
      scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text = element_text(size=10))+ ylim(50,80)+
        ylab('PAW (mm)') + xlab('Soil organic matter content (%)') +
        ggtitle('Relation between PAW and SOM')
    
    # compose figure in 2 x 2 layout
    (pclay + pcsand) / (pcsilt + pcsom)

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # calculate the index for the potential mineralizable nitrogen pool
#    dt[, D_PMN := calc_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE ,warning=FALSE----
  # define test data table with increasing PMN and different land uses
  dt.test <- data.table(B_LU_BRP = c(rep(265,12), rep(234,12)), 
                        B_SOILTYPE_AGR = 'rivierklei',
                        A_N_PMN = rep(seq(0,550,50),2), 
                        A_N_PMN2 = rep(seq(0,220,20),2))
  dt.test2 <- data.table(B_LU_BRP = c(rep(265,12), rep(234,12)), 
                         B_SOILTYPE_AGR = 'dekzand',
                         A_N_PMN = rep(seq(0,550,50),2), 
                         A_N_PMN2 = rep(seq(0,220,20),2))
  dt.test <- rbindlist(list(dt.test, dt.test2))
  
  # Add unique identifyer
  dt.test[B_LU_BRP == 265 & B_SOILTYPE_AGR == 'rivierklei',landuse := 'grassland on clay']
  dt.test[B_LU_BRP == 265 & B_SOILTYPE_AGR == 'dekzand',landuse := 'grassland on sand']
  dt.test[B_LU_BRP == 234 & B_SOILTYPE_AGR == 'rivierklei',landuse := 'arable on clay']
  dt.test[B_LU_BRP == 234 & B_SOILTYPE_AGR == 'dekzand',landuse := 'arable on sand']

  # Calculate PMN index
  dt.test <- dt.test[,D_PMN := calc_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN)]

  # plot output
  p1 <- ggplot(data = dt.test,
               aes(y = D_PMN, x = A_N_PMN, group = landuse, colour = landuse)) + 
        geom_line() + geom_point(size=3) +  ylim(0,800)+
        theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
              legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.text = element_text(size=10),
              legend.position = c(0.33,0.75))+
        ylab('PMN index') + xlab('Microbial activity') +
        ggtitle('Relationship between measured PMN and\nadjusted PMN index') 

  # calculate indicator with A_PMN2 (which has lower numbers)
  dt.test <- dt.test[, I_PMN2 := ind_pmn(calc_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN2))]
  
  # Plot indicator
  p2 <- ggplot(data = dt.test[A_N_PMN2<=150],
               aes(y = I_PMN2, x = A_N_PMN2, group = landuse, colour = landuse)) + 
        geom_line() + geom_point(size=3) + xlim(0,150)+ 
        theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
        theme(plot.title = element_text(size=10),
              legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.text = element_text(size=10),
              legend.position = c(0.6,0.3))+ 
        ylab('PMN indicator score') + xlab('Measured Microbial activity') +
        ggtitle('Conversion of measured PMN to OBI score') 
    
  # plot side by side
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # Calculate disease resistance
#    dt[, I_B_DI := ind_resistance(A_SOM_LOI)]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
    # Make data table
    dt <- data.table(A_SOM_LOI = seq(0.1, 6.9,0.4))

    # Calculate disease resistance indicator
    dt[, I_B_DI := ind_resistance(A_SOM_LOI)]
    
    # make a plot
    p1 <- ggplot(data = dt,aes(y = I_B_DI, x = A_SOM_LOI)) + 
          geom_line() + 
          geom_point(size=3) +  
          theme_bw() + 
          scale_fill_viridis_d() + scale_color_viridis_d()+ 
          theme(plot.title = element_text(size=10),
                legend.text = element_text(size=10),
                axis.text = element_text(size=10),
                legend.position = c(0.75,0.15))+
          ylab('Disease resistance indicator score') + 
          xlab('Soil organic matter content (%)') +
          ggtitle('Relationship between SOM and disease resistance indicator') 
    
    # plot figure
    p1

## ----hidden text nematodes, eval = FALSE, include = FALSE, echo=FALSE---------
#  # hide text in unevaluated code block till text is finished
#  ### Nematodes (WIP)
#  Nematodes are small animals occurring in all ecotypes on Earth, so also in soil. There is a large variety of nematodes in soils, they vary amongst other things, in size, feeding habits, reproduction speed, and life cycle. Plant parasitic nematodes (PPN) can reproduce in a range of host plants. Some nematodes have a specific host preference while other can reproduce in a large variety of plants. PPN can severely depress crop yields in vulnerable crops. Therefore, farmers can have their fields sampled to analyse which and how many nematodes occur.
#  
#  A nematode parameter that is not entered is assumed to be 0. The severity of an infection depends on the number of individuals and differs per parameter. For example: five Pratylenchus fallax (A_RLN_PR_FAL) will barely reduce the indicator score while five Ditylenchus destructor (A_SN_DI_DES) will severly reduce the score. It is assumed that the most severe infection is most limiting for crop production, therefore, the lowest score for a nematode parameter will determine the indicator score.

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
   dt <- data.table(nempar = c('A_RLN_PR_FAL', 'A_SN_DI_DES'), nr = 5, nem_ind = c(ind_nematodes(265, A_RLN_PR_FAL=5),ind_nematodes(265, A_SN_DI_DES=5)))

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # calculate potential for N leaching to groundwater
#    dt[,D_NGW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "gw")]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # subset a series of agricultural fields
  dt.test <- binnenveld[ID %in% c(1,2,3,4,5)]
  dt.test[,year := 1:.N, by = ID]
  dt.test <- dt.test[year==1]
    
  # check B_GWL_class
  
  # estimate default properties needed to estimate NLV
  dt.test[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI),by=ID]
  dt.test[, D_RD := calc_root_depth(B_LU_BRP),by=ID]
  dt.test[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD),by=ID]
  dt.test[, D_GA := calc_grass_age(ID, B_LU_BRP),by=ID]

  # estimate nitrogen supply (kg N / ha)
  dt.test[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA),by=ID]

  # estimate potential for N leaching
  dt.test[, D_NGW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "gw"),by=ID]
  
  # estimate potential for N runoff
  dt.test[, D_NSW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "ow"),by=ID]
  
  # plot output
  p1 <- ggplot(data = dt.test, aes(y = D_NGW, x = ID, fill = ID)) +  
        geom_col() +  
        theme_bw() +  scale_fill_viridis_b() + theme(legend.position = 'none') +
        xlab('Field') + ggtitle('Potential for N leaching')
   
  p2 <- ggplot(data = dt.test, aes(y = D_NSW, x = ID, fill = ID)) + 
        geom_col() + 
        theme_bw() +  scale_fill_viridis_b() + theme(legend.position = 'none') +
        xlab('Field') + ggtitle('Potential for N runoff')
  
  # Make example with different gwt's
  dt.test2 <- binnenveld[c(1,1,1,1,1)]
  dt.test2 <- dt.test2[,B_GWL_CLASS := c('GtIII','GtIV', 'GtV', 'GtVI', 'GtVII')]

  # estimate default properties needed to estimate NLV
  dt.test2[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI)]
  dt.test2[, D_RD := calc_root_depth(B_LU_BRP)]
  dt.test2[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt.test2[, D_GA := calc_grass_age(ID, B_LU_BRP)]

  # estimate nitrogen supply (kg N / ha)
  dt.test2[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate potential for N leaching
  dt.test2[, D_NGW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "gw")]
  
  # estimate potential for N runoff
  dt.test2[, D_NSW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "ow")]
  
  p3 <- ggplot(data = dt.test2, aes(y = D_NGW, x = B_GWL_CLASS, fill = B_GWL_CLASS)) + 
        geom_col() +  
        theme_bw() +  scale_fill_viridis_d() + theme(legend.position = 'none') +
        xlab('B_GWL_CLASS') + ggtitle('Potential for N leaching')
  p4 <- ggplot(data = dt.test2, aes(y = D_NSW, x = B_GWL_CLASS, fill = B_GWL_CLASS)) + 
        geom_col() +  
        theme_bw() +  scale_fill_viridis_d() + theme(legend.position = 'none') +
        xlab('B_GWL_CLASS') + ggtitle('Potential for N runoff')
  
  # plot figures
  p3 + p4

## ----fig.width = 7, fig.height = 2.2,fig.fullwidth = TRUE,echo=FALSE, eval = TRUE----
  # Table of field properties
  dt.table <- copy(dt.test2)
  setnames(dt.table, 'ID', 'field')
  
  # estimate N supplying capacity
  dt.table <- dt.table[,D_NLV := round(D_NLV, 1)]

  # select only relevant columns
  dt.table <- dt.table[,.(field,
                          soiltype = B_SOILTYPE_AGR, 
                          crop = B_LU_BRP, 
                          groundwaterclass = B_GWL_CLASS, 
                          nlv = round(D_NLV), 
                          regio = B_AER_CBS)]
  
  knitr::kable(dt.table,
               caption = 'Examples of Field properties to calculate N leaching and runoff')

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # create example data
  dt.test <- data.table(ngw = seq(0,30,2))
  dt.test <- dt.test[, I_NGW := ind_nretention(ngw, 'gw')]
  
    # plot output
  p <- ggplot(data = dt.test, aes(y = I_NGW, x = ngw)) + geom_line()  +
    theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ xlab('Potential N leaching')
  
  # Plot figure
  p

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # print figures p2 and p5
  p1 + p2

## ----results = FALSE, eval = FALSE--------------------------------------------
#      # Calculate organic matter balance
#      dt[, D_SOM_BAL := calc_sombalance(B_LU_BRP,A_SOM_LOI, A_P_AL, A_P_WA, M_COMPOST, M_GREEN)]
#  
#      # add management when input is missing
#      cols <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_COMPOST','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
#                'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
#      dt[, c(cols) := add_management(ID,B_LU_BRP, B_SOILTYPE_AGR,
#                                     M_GREEN, M_NONBARE, M_EARLYCROP,M_COMPOST,M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED,
#                                     M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST)]
#  
#      # calculate grass age
#       dt[, D_GA := calc_grass_age(ID, B_LU_BRP)]
#  
#       # Calculate the crop rotation fraction
#      dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
#      dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
#      dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
#      dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
#      dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
#      dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
#      dt[, D_CP_RUST := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewas")]
#      dt[, D_CP_RUSTDEEP := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewasdiep")]
#  
#      # Calculate series of management actions
#      dt[, D_MAN := calc_management(A_SOM_LOI,B_LU_BRP, B_SOILTYPE_AGR,B_GWL_CLASS,
#                                    D_SOM_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
#                                    M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN,
#                                    M_DITCH, M_UNDERSEED, M_LIME, M_NONINVTILL, M_SSPM,
#                                    M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST
#                                    )]

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
    # subset binnenveld for first 5 fields
    dt <- binnenveld[ID %in% c(1:5)]
    dt <- dt[,ID := as.factor(ID)]
    
    # calculate SOM balance for all crops in crop rotation plan
    dt[, D_SOM_BAL := calc_sombalance(B_LU_BRP,A_SOM_LOI, A_P_AL, A_P_WA, M_COMPOST, M_GREEN)]
    
    # calculate the sum per field
    dt2 <- dt[,list(D_SOM_BAL = sum(D_SOM_BAL)), by = ID]
    
    # plot figure
    p2 <- ggplot(data = dt2,aes(y = D_SOM_BAL, x = ID, fill = ID)) + 
          geom_col() +  
          theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
          ylab('SOM balance') + xlab('Field') + 
          theme(legend.position = c('none'),
                plot.title = element_text(size=10),
                legend.text = element_text(size=10),
                axis.text = element_text(size=10)) + 
          ggtitle('Soil organic matter balance for five fields')
    
    # add management when input is missing
    cols <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_COMPOST','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
              'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
    dt[, c(cols) := add_management(ID,B_LU_BRP, B_SOILTYPE_AGR,
                                   M_GREEN, M_NONBARE,M_EARLYCROP,M_COMPOST,
                                   M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED,
                                   M_LIME, M_NONINVTILL, M_SSPM,M_SOLIDMANURE,
                                   M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST)]
    
    # calculate grass age
    dt[, D_GA := calc_grass_age(ID, B_LU_BRP)]

    # Calculate the crop rotation fraction
    dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
    dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
    dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
    dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
    dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
    dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
    dt[, D_CP_RUST := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewas")]
    dt[, D_CP_RUSTDEEP := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewasdiep")]
    
    # calculate management per field per year
    dt[, D_MAN := calc_management(A_SOM_LOI,B_LU_BRP, B_SOILTYPE_AGR,B_GWL_CLASS,
                                  D_SOM_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                                  M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN,
                                  M_DITCH, M_UNDERSEED, M_LIME, M_NONINVTILL, M_SSPM,
                                  M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST
                                  )]
    
    # calculate mean management score per field     
    dt2 <- dt[,list(D_MAN = mean(D_MAN)), by = ID]

    # make plot
    p3 <- ggplot(data = dt2,aes(y = D_MAN, x = ID, fill = ID)) + 
          geom_col() +  
          theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+ 
          ylab('Management score') + xlab('Field') + 
          theme(legend.position = c('none'),
                plot.title = element_text(size=10),
                legend.text = element_text(size=10),
                axis.text = element_text(size=10)) + 
          ggtitle('Management')
    
    # print figures
    p2 + p3

## ----results = FALSE, eval = FALSE--------------------------------------------
#      # evaluate measures
#      dt.measure <- OBIC::obic_evalmeasure(dt.score, extensive = FALSE)
#  
#      # make recommendations of top 3 measures
#      out.recom <- OBIC::obic_recommendations(dt.measure)

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # Be aware these functions below are not given here to be evaluated.
#    # They illustrate how the different indices can be evaluated (do not execute them here)
#  
#    # Calculate indicators for soil chemical functions
#    dt[, I_C_N := ind_nitrogen(D_NLV, B_LU_BRP)]
#    dt[, I_C_P := ind_phosphate_availability(D_PBI)]
#    dt[, I_C_K := ind_potassium(D_K,B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI)]
#    dt[, I_C_MG := ind_magnesium(D_MG, B_LU_BRP, B_SOILTYPE_AGR)]
#    dt[, I_C_S := ind_sulfur(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]
#    dt[, I_C_PH := ind_ph(D_PH_DELTA)]
#    dt[, I_C_CEC := ind_cec(D_CEC)]
#    dt[, I_C_CU := ind_copper(D_CU,B_LU_BRP)]
#    dt[, I_C_ZN := ind_zinc(D_ZN)]
#  
#    # Calculate indicators for soil physical functions
#    dt[, I_P_CR := ind_crumbleability(D_CR, B_LU_BRP)]
#    dt[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
#    dt[, I_P_DS := ind_waterstressindex(D_WSI_DS)]
#    dt[, I_P_WS := ind_waterstressindex(D_WSI_WS)]
#    dt[, I_P_DU := ind_winderodibility(D_WE)]
#    dt[, I_P_CO := ind_compaction(B_SC_WENR)]
#    dt[, I_P_WRI := ind_waterretention(D_WRI)]
#    dt[, I_P_CEC := ind_aggregatestability(D_AS)]
#    dt[, I_P_WO := ind_workability(D_WO)]
#  
#    # Calculate indicators for soil biological functions
#    dt[, I_B_DI := ind_resistance(A_SOM_LOI)]
#    dt[, I_B_SF := ind_pmn(D_PMN)]
#  
#    # Calculate indicators for environment
#    dt[, I_E_NGW := ind_nretention(D_NGW, leaching_to = "gw")]
#    dt[, I_E_NSW := ind_nretention(D_NSW, leaching_to = "ow")]

## ----plot relation function and index values,fig.width = 7, fig.height = 16,fig.fullwidth = TRUE,echo=FALSE----
  # create waterstress index relation figure
  # dt <- data.table(D_WRI = seq(0,100,1))
  # dt[, I_P_WRI := ind_waterretention(D_WRI)]
  # gg <- ggscatter(dt, x = 'D_WRI', y = 'I_P_WRI', title = 'Waterstress evaluation') +xlab('Indicator value') + ylab('Indicator score') +theme_bw()
  # gg

  # define a series with numbers reflecting variation in derivatives
  dt <- data.table(id = 1:202)

  # add all derivatives
  dt[,D_PH_DELTA := rep(seq(0,2.5,0.025),2)]
  dt[,c('D_NLV','D_MG','D_CEC','D_ZN','D_PMN') := rep(seq(0,100,1),2)]
  dt[,c('D_PBI','D_K','D_SLV') := rep(seq(0,50,0.5),2)]
  dt[,c('D_CU') := rep(seq(0,20,.2),2)]
  dt[,c('D_WSI_DS','D_WSI_WS','D_WRI') := rep(seq(0,50,.5),2)]
  dt[,c('D_NGW','D_NSW') := rep(seq(0,50,.5),2)]
  dt[,c('D_WE','D_AS') := seq(0,1,length.out = 202)]
  dt[,D_CR := rep(seq(0,10,0.1),2)]
  dt[,D_SE := rep(seq(0,50,0.5),2)]
  dt[,B_LU_BRP := c(rep(265,101),rep(256,101))]
  dt[,B_SOILTYPE_AGR := rep('dekzand',202)]
  dt[,A_SOM_LOI := rep(3.5,202)]
  dt[,D_DUMMY_SOM_LOI := rep(seq(0,35,0.35),2)]
  dt[,B_AER_CBS := rep("Centraal Veehouderijgebied",202)]
  dt[,B_SC_WENR := "Matig"]
  dt[,D_WO := rep(seq(0,1,0.01),2)]
  
  # Calculate indicators for soil chemical functions
  dt[, I_C_N := ind_nitrogen(D_NLV, B_LU_BRP)]
  dt[, I_C_P := ind_phosphate_availability(D_PBI)]
  dt[, I_C_K := ind_potassium(D_K,B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, I_C_MG := ind_magnesium(D_MG, B_LU_BRP, B_SOILTYPE_AGR)]
  dt[, I_C_S := ind_sulfur(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]
  dt[, I_C_PH := ind_ph(D_PH_DELTA)]
  dt[, I_C_CEC := ind_cec(D_CEC)]
  dt[, I_C_CU := ind_copper(D_CU,B_LU_BRP)]
  dt[, I_C_ZN := ind_zinc(D_ZN)]
    
  # Calculate indicators for soil physical functions
  dt[, I_P_CR := ind_crumbleability(D_CR, B_LU_BRP)]
  dt[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
  dt[, I_P_DS := ind_waterstressindex(D_WSI_DS)]
  dt[, I_P_WS := ind_waterstressindex(D_WSI_WS)]
  dt[, I_P_DU := ind_winderodibility(D_WE)]
  dt[, I_P_CO := ind_compaction(B_SC_WENR)] 
  dt[, I_P_WRI := ind_waterretention(D_WRI)]
  dt[, I_P_CEC := ind_aggregatestability(D_AS)]
  dt[, I_P_WO := ind_workability(D_WO,B_LU_BRP)] 
  
  # Calculate indicators for soil biological functions
  dt[, I_B_DI := ind_resistance(D_DUMMY_SOM_LOI)]
  dt[, I_B_SF := ind_pmn(D_PMN)]
    
  # Calculate indicators for environment
  dt[, I_E_NGW := ind_nretention(D_NGW, leaching_to = "gw")]
  dt[, I_E_NSW := ind_nretention(D_NSW, leaching_to = "ow")]
    
  indcols <- colnames(dt)[grep('^I_',colnames(dt))]
  cols <- colnames(dt)[grep('^I_|^B_|^A_|^id$',colnames(dt))]
  
  dd.melt <- melt.data.table(dt, id.vars = cols,variable.name = 'funct', value.name = 'function_value')
  
  dd.melt <- melt.data.table(dd.melt, id.vars = c(cols[!cols%in%indcols],'funct', 'function_value'),
                               measure.vars = indcols, variable.name = 'indicator', value.name = 'index_value')
    
    # Reset b_lu_brp to something readable
    dd.melt <- dd.melt[, B_LU_BRP := as.character(fifelse(B_LU_BRP == 265,'grass','arable'))]
    
    # Select correct rows from dd.melt
    dd.melt <-  dd.melt[(funct == 'D_NLV'& indicator == 'I_C_N')|
                        (funct == 'D_PBI'& indicator == 'I_C_P')|
                        (funct == 'D_K'& indicator == 'I_C_K')|
                        (funct == 'D_MG'& indicator == 'I_C_MG')|
                        (funct == 'D_SLV'& indicator == 'I_C_S')|
                        (funct == 'D_PH_DELTA'& indicator == 'I_C_PH')|
                        (funct == 'D_CEC'& indicator == 'I_C_CEC')|
                        (funct == 'D_CU'& indicator == 'I_C_CU')|
                        (funct == 'D_ZN'& indicator == 'I_C_ZN')|
                        (funct == 'D_WSI_DS'& indicator == 'I_P_DS')|
                        (funct == 'D_WSI_WS'& indicator == 'I_P_WS')|
                        (funct == 'D_WRI'& indicator == 'I_P_WRI')|
                        (funct == 'D_CEC'& indicator == 'I_P_CEC')|
                        (funct == 'D_PMN'& indicator == 'I_B_SF')|
                        (funct == 'D_NGW'& indicator == 'I_E_NGW')|
                        (funct == 'D_NSW'& indicator == 'I_E_NSW')|
                        (funct == 'D_CR'& indicator == 'I_P_CR')|
                        (funct == 'D_SE'& indicator == 'I_P_SE')|
                        (funct == 'D_WE'& indicator == 'I_P_DU')|
                        (funct == 'D_DUMMY_SOM_LOI'& indicator == 'I_B_DI')]
    
    # make plot
    ggsp <- ggplot(dd.melt, aes(x= function_value, y= index_value,color=B_LU_BRP)) +
            geom_line(size = 1.5) + 
            facet_wrap(facets = 'indicator', ncol = 4, scales = 'free_x') + 
            theme_bw() +
            scale_color_manual(values = c("#440154FF", "#FDE725FF"))+
            theme(legend.position = 'bottom')
       
    #plot
    ggsp  + labs(caption = 'Relation between function values and their corresponding evaluated index values.')

## ----fig.width = 7, fig.height = 4,fig.fullwidth = TRUE,echo=FALSE------------
  # create example data
  dt.arabl <- data.table(landuse = c(rep('arable on clay',5),rep('arable on peat',5),rep('arable on sand',5),rep('maize',5)),
                         points = c(rep(c(0,4,9,14,18),3),c(0,3,6,9,12)),
                         soiltype = c(rep('rivierklei', 5), rep('veen',5), rep('dekzand',10)),
                         B_LU_BRP = c(rep(234,15),rep(259,5)))
  dt.gras <- data.table(landuse = c(rep('grass on clay',5),rep('grass on sand',5),rep('grass on peat',5)),
                        points = c(rep(c(0,3,5,8,11),2),c(0,4,9,14,17)),
                        soiltype = c(rep('rivierklei',5),rep('dekzand',5),rep('veen',5)))
  # calculate indicator
  dt.gras <- dt.gras[,score := ind_management(points, rep(265,15),soiltype)]
  dt.arabl <- dt.arabl[,score := ind_management(points, B_LU_BRP,soiltype)]
  
  # make plots
  p.ar <- ggplot(data = dt.arabl,
                 aes(points,score, col = landuse, group = landuse, shape = landuse)) + geom_point(size = 3)+ geom_line() + theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
    ylab('Indicator value') + xlab('Soil management points')+theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10), axis.text = element_text(size=10), legend.position = c(0.75,0.22))
  p.gr <- ggplot(data = dt.gras,
                 aes(points,score, col = landuse, group = landuse, shape = landuse)) + geom_point(size = 3)+ geom_line() + theme_bw() + scale_fill_viridis_d()+ scale_color_viridis_d()+
    ylab('Indicator value') + xlab('Soil management points')+theme(plot.title = element_text(size=10),
                  legend.text = element_text(size=10), axis.text = element_text(size=10), legend.position = c(0.75,0.2))
p.ar+p.gr

## ----results = FALSE, eval = FALSE--------------------------------------------
#      # load weights.obic (set indicator to zero when not applicable)
#      w <- as.data.table(OBIC::weight.obic)
#  
#      # Add years per field
#      dt[,year := .I, by = ID]
#  
#      # Select all indicators used for scoring
#      cols <- colnames(dt)[grepl('I_C|I_B|I_P|I_E|I_M|year|crop_cat|SOILT',colnames(dt))]
#  
#      # Melt dt and assign main categories for OBI
#      dt.melt <- melt(dt[,mget(cols)],
#                      id.vars = c('B_SOILTYPE_AGR','crop_category','year'),
#                      variable.name = 'indicator')
#  
#      # add categories relevant for aggregating
#      # C = chemical, P = physics, B = biological, BCS = visual soil assessment
#      # indicators not used for integrating: IBCS and IM
#      dt.melt[,cat := tstrsplit(indicator,'_',keep = 2)]
#      dt.melt[grepl('_BCS$',indicator) & indicator != 'I_BCS', cat := 'IBCS']
#      dt.melt[grepl('^I_M_',indicator), cat := 'IM']
#  
#      # Determine number of indicators per category
#      dt.melt.ncat <- dt.melt[year==1 & !cat %in% c('IBCS','IM')][,list(ncat = .N),by='cat']
#  
#      # add weighing factor to indicator values
#      dt.melt <- merge(dt.melt,w[,list(crop_category,indicator,weight_nonpeat,weight_peat)],
#                       by = c('crop_category','indicator'), all.x = TRUE)
#  
#      # calculate correction factor for indicator values (low values have more impact than high values, a factor 5)
#      dt.melt[,cf := cf_ind_importance(value)]
#  
#      # calculate weighted value for crop category
#      dt.melt[,value.w := value]
#      dt.melt[grepl('veen',B_SOILTYPE_AGR) & weight_peat < 0,value.w := -999]
#      dt.melt[!grepl('veen',B_SOILTYPE_AGR) & weight_nonpeat < 0,value.w := -999]

## ----results=FALSE, eval=FALSE------------------------------------------------
#      # subset dt.melt for relevant columns only
#      out.score <-  dt.melt[,list(cat, year, cf, value = value.w)]
#  
#      # remove indicator categories that are not used for scoring
#      out.score <- out.score[!cat %in% c('IBCS','IM','BCS')]
#  
#      # calculate weighted average per indicator category
#      out.score <- out.score[,list(value = sum(cf * pmax(0,value) / sum(cf[value >= 0]))), by = list(cat,year)]
#  
#      # for case that a cat has one indicator or one year and has NA
#      out.score[is.na(value), value := -999]

## ----aggreagate indicators per category, results= FALSE, eval=FALSE-----------
#      # calculate correction factor per year; recent years are more important
#      out.score[,cf := log(12 - pmin(10,year))]
#  
#      # calculate weighted average per indicator category per year
#      out.score <- out.score[,list(value = sum(cf * pmax(0,value)/ sum(cf[value >= 0]))), by = cat]

## ----holistic obi score, results = FALSE, eval = FALSE------------------------
#    # merge out with number per category
#    out.score <- merge(out.score,dt.melt.ncat, by='cat')
#  
#    # calculate weighing factor depending on number of indicators
#    out.score[,cf := log(ncat + 1)]
#  
#    # calculated final obi score
#    out.score <- rbind(out.score[,list(cat,value)],
#                       out.score[,list(cat = "T",value = sum(value * cf / sum(cf)))])

## ----results = FALSE, eval = FALSE--------------------------------------------
#    # For example
#    OBIC::obic_field_dt(binnenveld[ID == 1], output = 'scores')
#  
#    OBIC::obic_field_dt(binnenveld[ID == 1], output = 'obic_score')

## ----eval = TRUE, echo=FALSE--------------------------------------------------
  OBIC::obic_field_dt(binnenveld[ID == 1], output = 'scores')
  OBIC::obic_field_dt(binnenveld[ID == 1], output = 'obic_score')

## ----echo=FALSE, fig.height= 6, fig.width=6.8---------------------------------
    # Load data
    dt <- binnenveld
    
    dts <- obic_field_dt(dt[ID== unique(dt$ID)[1]])
    dts <- dts[,field :=  unique(dt$ID)[1]]
    for(i in unique(dt$ID)[2:4]){
      dts2 <- obic_field_dt(dt[ID==i])
      dts2 <- dts2[,field := i]
      dts <- rbindlist(list(dts, dts2))
    }
    dts <- dts[, field := as.factor(field)]
    
    # g1 <- ggradar(dts[,.(field,S_T_OBI_A, S_C_OBI_A, S_P_OBI_A, S_B_OBI_A, S_M_OBI_A)], values.radar = c(0,0.5,1),
    #           axis.label.size = 4, legend.position = "bottom", legend.title = 'field',
    #           legend.text.size = 12,
    #           grid.label.size = 4,
    #           plot.extent.x.sf = 1.2,
    #           axis.label.offset = 1.2)
    
    # pre prepare for lollipop chart
    cols <- colnames(dts)[grepl('^S_T_|^S_C_|^S_P_|^S_B_|^S_E_|^S_M_|^field', colnames(dts))]
dts2 = dts[,mget(cols)]
bar <- melt(dts2, id.vars = 'field')
bar[grepl('^S_T_', variable), cat := "Total"]
bar[grepl('^S_C_', variable), cat := "Chemical"]
bar[grepl('^S_B_', variable), cat := "Biological"]
bar[grepl('^S_P_', variable), cat := "Physical"]
bar[grepl('^S_M_', variable), cat := "Management"]
bar[grepl('^S_E_', variable), cat := "Environmental"]


   # change field for plotting
   bar <- bar[,field:= paste0('Field ', field)]
   
   # make lollipop chart (cleveland dotchart) without ggpubr
   g2p <- ggplot(bar, aes(x= cat, y= value, group = cat, color = cat)) +
     geom_col(fill = 'grey', color = NA, width = 0.05) + geom_point(size = 2) + ylab('OBI-score') + xlab('') +
     theme_bw(12) + theme(legend.position = 'none',
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_line(size = 0.5),
                        panel.grid.minor.x = element_blank()) +
     coord_flip() +
     facet_wrap(~field) +
     scale_fill_viridis_d()+ scale_color_viridis_d()
   
 g2p

## ----include=FALSE------------------------------------------------------------
knitr::write_bib(c(.packages()), "packages.bib")
knitr::write_bib(file = 'packages.bib')


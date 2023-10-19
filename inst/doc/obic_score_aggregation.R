## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----include score integration image, echo=FALSE, out.width = '85%', out.height = '85%', fig.cap = 'Figure 1. Graphic representation of how measured soil properties are aggregated to scores.'----
# include graphic
knitr::include_graphics('../vignettes/OBIC_score_integratie_2.png')

## ----load binnenveld----------------------------------------------------------

  # load packages
  library(OBIC); library(data.table); library(ggplot2); library(patchwork)
  setDTthreads(1)

  # load data
  dt <- OBIC::binnenveld[ID==1]
  
  

## ----reformatting code, echo = TRUE, eval=FALSE-------------------------------
#    # Step 3 Reformat dt given weighing per indicator and prepare for aggregation  ------------------
#  
#      # load weights.obic (set indicator to zero when not applicable)
#      w <- as.data.table(OBIC::weight.obic)
#  
#      # Add years per field
#      dt[,year := 1:.N, by = ID]
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
#  

## ----eval = FALSE, include = FALSE--------------------------------------------
#  # YF: I think this paragraph is not necessary
#  # After reformatting the data in step 3, an indicator data.table is created in step 4. This data.table uses the soil function scores adjusted for their applicability for the soiltype and crop category. In step 4 indicators are calculated to display them as output. In step 5 the total OBI score is calculated, since part of step 5 overlaps with step 4, we only discuss step 5 onwards.

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#   # Step 5 Add scores ------------------
#  
#      # subset dt.melt for relevant columns only
#      out.score <-  dt.melt[,list(cat, year, cf, value = value.w)]
#  
#      # remove indicator categories that are not used for scoring
#      out.score <- out.score[!cat %in% c('IBCS','IM','BCS')]

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#      # calculate weighted average per indicator category
#      out.score <- out.score[,list(value = sum(cf * pmax(0,value) / sum(cf[value >= 0]))), by = list(cat,year)]
#  
#        # for case that a cat has one indicator or one year and has NA
#        out.score[is.na(value), value := -999]

## ----output year cf-----------------------------------------------------------
  # create data
  y <- 1:11
  cf <- log(12 - pmin(10, y))
  cat(round(cf, 3))

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  
#      # calculate correction factor per year; recent years are more important
#      out.score[,cf := log(12 - pmin(10,year))]
#  
#      # calculate weighted average per indicator category per year
#      out.score <- out.score[,list(value = sum(cf * pmax(0,value)/ sum(cf[value >= 0]))), by = cat]

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#        # merge out with number per category
#        out.score <- merge(out.score,dt.melt.ncat, by='cat')

## ----eval = FALSE, include = FALSE--------------------------------------------
#  # this text is probably wrong
#  # Furthermore, by aggregating indicators to categories and then to a score rather than directly from indicators to a score; individual indicators from categories with few underlying indicators, affect the holistic score more than indicators in categories with many indicators. For example, if there is one biological indicator, its weight in affecting the holistic score is log(1+1)= `r round(log(1+1),2)`, while a indicator within a chemical indicator with nine indicators individually only weighs log(9+1)/9= `r round(log(9+1)/9,2)`. While on category level, biology only weighs `r round(log(1+1),2)` and chemical `r round(log(9+1),2)`.

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#      # calculate weighing factor depending on number of indicators
#      out.score[,cf := log(ncat + 1)]
#      # calculated final obi score
#      out.score <- rbind(out.score[,list(cat,value)],
#                         out.score[,list(cat = "T",value = sum(value * cf / sum(cf)))])

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#      # update element names
#      out.score[,cat := paste0('S_',cat,'_OBI_A')]
#      out.score[, value := round(value,3)]

## ----make mock data, eval= TRUE-----------------------------------------------

# make mock data and calculate scores with different aggregation methods (averaging without weight and averaging with linearly changing weight)
# data like:
# soil_function_value|indicator|group|year|cf_base|cf_noweight|cf_linearweight|score_base|score_noweight|score_linearweight

# visualise differences

# make veldnr
fieldid <- 1

# define standard deviation 
std <- 0.2

# make indicator
inds <- c('I_C_CEC', 'I_C_CU', 'I_C_K', 'I_C_MG', 'I_C_N', 'I_C_P', 'I_C_PH', 'I_C_S', 'I_C_ZN',
          'I_B_DI', 'I_B_SF', 
          'I_E_NGW', 'I_E_NSW',
          'I_M',
          'I_P_CEC', 'I_P_CO', 'I_P_CR', 'I_P_DS', 'I_P_DU', 'I_P_SE', 'I_P_WRI', 'I_P_WS')

# make jaar
year <- 1:10

# combine in dt
dt <- data.table(field = sort(rep(fieldid,length(inds)*length(year))),
                 indicator = sort(rep(inds, length(year)*length(fieldid))),
                 year = rep(year, length(inds)*length(fieldid))
                 )

# add category
dt <- dt[,cat := tstrsplit(indicator,'_',keep = 2)]

# iteratively add fields
dto <- data.table(field = NULL, indicator = NULL, year = NULL)
for(i in 1:100){
  dtn <- dt
  dtn <- dtn[,field := i]
  dto <- rbindlist(list(dto, dtn))
}

# dto is a almost ready set of 100 fields, only values and value description need to be added
set.seed(222)

# helper function to make random values within 0:1
rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
    bounds <- pnorm(c(min, max), mean, sd)
    u <- runif(n, bounds[1], bounds[2])
    qnorm(u, mean, sd)
}

# make baseline
dt1 <- copy(dto)
dt1 <- dt1[,field := field+100-1]
dt1 <- dt1[,treatment := 'baseline']
dt1 <- dt1[,value := rtnorm(n = nrow(dt1),mean = 0.7, sd = std)]

# make treatment where c = 0.3
dt2 <- copy(dto)
dt2 <- dt2[,field := field+200-1]
dt2 <- dt2[,treatment := 'low C values']
dt2 <- dt2[cat == 'C',value := rtnorm(n = nrow(dt2[cat=='C']),mean = 0.3, sd = std)]
dt2 <- dt2[!cat == 'C',value := rtnorm(n = nrow(dt2[!cat=='C']),mean = 0.7, sd = std)]

# make treatment where B = 0.3
dt3 <- copy(dto)
dt3 <- dt3[,field := field+300-1]
dt3 <- dt3[,treatment := 'low B values']
dt3 <- dt3[cat == 'B',value := rtnorm(n = nrow(dt3[cat=='B']),mean = 0.3, sd = std)]
dt3 <- dt3[!cat == 'B',value := rtnorm(n = nrow(dt3[!cat=='B']),mean = 0.7, sd = std)]

# make treatment where one C indicator = 0
dt4 <- copy(dto)
dt4 <- dt4[,field := field+400-1]
dt4 <- dt4[,treatment := 'one low C']
dt4 <- dt4[indicator == 'I_C_CEC',value := 0]
dt4 <- dt4[!indicator == 'I_C_CEC',value := rtnorm(n = nrow(dt4[!indicator == 'I_C_CEC']),mean = 0.7, sd = std)]

# make where one B indicator = 0
dt5 <- copy(dto)
dt5 <- dt5[,field := field+500-1]
dt5 <- dt5[,treatment := 'one low B']
dt5 <- dt5[indicator == 'I_B_DI',value := 0]
dt5 <- dt5[!indicator == 'I_B_DI',value := rtnorm(n = nrow(dt5[!indicator == 'I_B_DI']),mean = 0.7, sd = std)]

# make treatment where recent years score low and old years high
dt6 <- copy(dto)
dt6 <- dt6[,field := field+600-1]
dt6 <- dt6[,treatment := 'Recent years low']
dt6 <- dt6[year %in% 1:5, value := rtnorm(n = nrow(dt6[year %in% 1:5]), mean = 0.3, sd = std)]
dt6 <- dt6[!year %in% 1:5, value := rtnorm(n = nrow(dt6[!year %in% 1:5]), mean = 0.7, sd = std)]

# make treatment where recent years score high and old years low
dt7 <- copy(dto)
dt7 <- dt7[,field := field+700-1]
dt7 <- dt7[,treatment := 'Recent years high']
dt7 <- dt7[!year %in% 1:5, value := rtnorm(n = nrow(dt7[!year %in% 1:5]), mean = 0.3, sd = std)]
dt7 <- dt7[year %in% 1:5, value := rtnorm(n = nrow(dt7[year %in% 1:5]), mean = 0.7, sd = std)]

# combine all data
dta <- rbindlist(list(dt1, dt2, dt3, dt4, dt5, dt6, dt7))

# make sure all values are between 0 and 1
if(any(dta$value >1|dta$value<0)){cat('values outside acceptable bounds')}


## ----plot orignal values histogram,fig.width = 7, fig.height = 4,fig.fullwidth = TRUE, fig.cap = 'Figure 5. Distribution of indicator values per scenario as histogram'----
ggplot(dta, aes(x = value, fill = cat)) +
  geom_histogram(bins = 40) + 
  theme_bw() +facet_wrap(~treatment, ncol = 4)


## ----table with mock data before aggregation----------------------------------
# get relevant data from dta
dtat <- dta[, mean(value), by = c( 'cat', 'treatment')]

# rounc V1
dtat <- dtat[, V1 := round(V1, digits = 3)]

# improve category descrition
dtat <- dtat[,cat := paste('mean', cat)]

# dcast
dtat <- dcast(dtat ,treatment~cat, value.var = 'V1')

# factorise and order treatment
dtat <- dtat[, treatment := factor(treatment, levels = c('baseline', 'low B values', 'low C values',
                                                         'one low B', 'one low C',
                                                         'Recent years high', 'Recent years low'))]

# order dtat
setorder(dtat)

# make table 
  knitr::kable(dtat,
               caption = 'Mean scores per category for each scenario')


## ----plot mock cf, fig.width = 7, fig.height = 3,fig.fullwidth = TRUE, fig.cap = 'Figure 2. Correction factors calculated with linear or logarithmic methods per aggregation step.'----

  # plot correction factors
  pdtlog <- data.table(x = c(seq(0,1,0.1),rep(0:10,2)),
                       cf_type = c(rep('value',11),rep('year',11), rep('ncat', 11)))
  # calc cf's log
  pdtlog <- pdtlog[cf_type == 'value', cf := OBIC::cf_ind_importance(x)]
  pdtlog <- pdtlog[cf_type == 'year', cf := log(12 - pmin(10,x))]
  pdtlog <- pdtlog[cf_type == 'ncat', cf := log(x + 1)]
  pdtlog[,cf_method := 'log']

  # calc cf's linear
  pdtlin <- data.table(x = c(seq(0,1,0.1),rep(0:10,2)),
                      cf_type = c(rep('value',11),rep('year',11), rep('ncat', 11)))
  pdtlin <- pdtlin[cf_type == 'value', cf := 5-4.17*x]
  pdtlin <- pdtlin[cf_type == 'year', cf := 2.59-0.19*x]
  pdtlin <- pdtlin[cf_type == 'ncat', cf := x*log(11)/10]
  pdtlin[,cf_method := 'linear'] 

  # combine
  pdt <- rbindlist(list(pdtlog, pdtlin))

  # format pdt
  pdt <- pdt[,cf_type := factor(cf_type, levels = c('value', 'ncat', 'year'))]

# plot
gg <- ggplot(pdt, aes(x = x, y = cf, color = cf_method, group = cf_type))+
      geom_point() +
      theme_bw() + 
      facet_wrap(~cf_type, ncol = 3, scales = 'free') + 
      scale_colour_viridis_d()+
      xlab('') + ylab('cf (weight)')

# plot a line in each
for(i in 1:uniqueN(pdt$cf_method)){
  gg <- gg + geom_line(data = pdt[cf_method == unique(pdt$cf_method)[i]], color = c('#FDE725FF', '#440154FF')[i])
}

# plot gg
gg

## ----calc correction factors--------------------------------------------------

# Use three ways to calc correction factors (giving weight to each value), log (standard in OBIC), lin (linearly increasing/decreasing), non (everything has the same weight)

# value correction factors ======
dt <- copy(dta)

# function to add  correction factors
addcf <- function(dt){
  
  # copy input
  dt.int <- copy(dt)
  
  # add correction factor for indicator value
	dt.int[,log := OBIC::cf_ind_importance(value)]
	dt.int[,lin := 5-4.17*value]
	dt.int[,non := 1]
	
	# melt dt by cf method
	dt.int <- melt(dt.int, measure.vars = c('log', 'lin', 'non'), value.name = 'v_cf', variable.name = 'cf_method')
	
	# calculate cf for cat ====
	dt.int[,ncat := .N,by=c('field','year','cat','cf_method')]
	
	# add correction factor for number of categories
	dt.int[cf_method == 'log',c_cf := log(ncat + 1)]
	dt.int[cf_method == 'lin',c_cf := ncat*log(10 + 1)/10]
	dt.int[cf_method == 'non',c_cf := 1]
	
	# dd correction factor for number of years
	dt.int[cf_method == 'log',y_cf := log(12 - pmin(10,year))]
	dt.int[cf_method == 'lin',y_cf := 2.59-0.19*year]
	dt.int[cf_method == 'non',y_cf := 1]
}

# add correction factors
dt <- addcf(dt)


## ----calc correction factors 2------------------------------------------------

# Use three ways to calc correction factors (giving weight to each value), log (standard in OBIC), lin (linearly increasing/decreasing), non (everything has the same weight)
# for log and lin, 4 scenarios are added (c_cf only, v_cf only, y-cf only, or all)

# value correction factors ======
dtp <- copy(dta)

# function to add  correction factors
addcf2 <- function(dt){
  
  # copy input
  dt.int <- copy(dt)
  
  # add correction factor for indicator value
	dt.int[,log_all := OBIC::cf_ind_importance(value)]
	dt.int[,log_vcf := OBIC::cf_ind_importance(value)]
	dt.int[,log_ccf := 1]
	dt.int[,log_ycf := 1]
	dt.int[,lin_all := 5-4.17*value]
	dt.int[,lin_vcf := 5-4.17*value]
	dt.int[,lin_ccf := 1]
	dt.int[,lin_ycf := 1]
	dt.int[,non := 1]
	
	# melt dt by cf method
	dt.int <- melt(dt.int, measure.vars = c('log_all', 'log_vcf', 'log_ccf', 'log_ycf',
	                                        'lin_all', 'lin_vcf', 'lin_ccf', 'lin_ycf', 
	                                        'non'), 
	               value.name = 'v_cf', variable.name = 'cf_method')
	
	# calculate cf for cat ====
	dt.int[,ncat := .N,by=c('field','year','cat','cf_method')]
	
	# add correction factor for number of categories
	dt.int[cf_method == 'log_all',c_cf := log(ncat + 1)]
	dt.int[cf_method == 'log_ccf',c_cf := log(ncat + 1)]
	dt.int[cf_method == 'log_vcf',c_cf := 1]
	dt.int[cf_method == 'log_ycf',c_cf := 1]
	dt.int[cf_method == 'lin_all',c_cf := ncat*log(10 + 1)/10]
	dt.int[cf_method == 'lin_ccf',c_cf := ncat*log(10 + 1)/10]
	dt.int[cf_method == 'lin_vcf',c_cf := 1]
	dt.int[cf_method == 'lin_ycf',c_cf := 1]
	dt.int[cf_method == 'non',c_cf := 1]
	
	# dd correction factor for number of years
	dt.int[cf_method == 'log_all',y_cf := log(12 - pmin(10,year))]
	dt.int[cf_method == 'log_ycf',y_cf := log(12 - pmin(10,year))]
	dt.int[cf_method == 'log_vcf',y_cf := 1]
	dt.int[cf_method == 'log_ccf',y_cf := 1]
	dt.int[cf_method == 'lin_all',y_cf := 2.59-0.19*year]
	dt.int[cf_method == 'lin_ycf',y_cf := 2.59-0.19*year]
	dt.int[cf_method == 'lin_vcf',y_cf := 1]
	dt.int[cf_method == 'lin_ccf',y_cf := 1]
	dt.int[cf_method == 'non',y_cf := 1]
	
	return(dt.int)
}

# add correction factors
dt3 <- addcf2(dtp)


## ----aggregate scores---------------------------------------------------------

# make function to aggregate scores
aggscores <- function(dt) {
  
  # copy input
  dt.agg <- copy(dt)
  
  # calculate weighted value per category and year
	dt.agg <- dt.agg[,list(w.value = sum(v_cf* pmax(0,value) / sum(v_cf[value >= 0])),
	                       y_cf = mean(y_cf),
	                       c_cf = mean(c_cf)),
	                  by = .(treatment,field, cf_method, cat, year)]
	
	# calculated weighted average value per category (so mean over years)
	dt.agg <- dt.agg[,list(wy.value = sum(y_cf * pmax(0, w.value) / sum(y_cf[w.value >= 0])),
	                       c_cf = mean(c_cf)),
	                  by = .(treatment,field, cf_method, cat)] 
	
	# calculated weighted average value per field (so mean over categories)
	dt.agg.tot <- dt.agg[,list(value = sum(wy.value * c_cf / sum(c_cf))), 
	                      by = .(treatment,field, cf_method)]
	
	# output
	dt.out <- rbind(dt.agg[,.(treatment,field,cf_method,cat,value = wy.value)],
	                dt.agg.tot[,.(treatment,field,cf_method,cat='total',value)])
	
	}

# add scores to dt
dt.out <- aggscores(dt)
dt3.out <- aggscores(dt3)


## ----orig score funs of brent, eval=TRUE--------------------------------------


aggscores_brent <- function(dt) {
  # copy input
  dt.agg <- copy(dt)
  
  	# calculate weighted value per category and year
	dt.agg <- dt.agg[,w.value := sum(v_cf* pmax(0,value) / sum(v_cf[value >= 0])), by = .(field, cf_method, cat, year)]
	
	# calculated weighted average value per category (so mean over years)
	dt.agg <- dt.agg[,wy.value := sum(y_cf * pmax(0, w.value) / sum(y_cf[w.value >= 0])), by = .(field, cf_method, cat)] # scores per category
	
	# calculate total obi score (log method)
	dt.agg <- dt.agg[,S_T := sum(wy.value * c_cf / sum(c_cf)), by = .(field, cf_method)]
	# calculate total obi score (lin method)
	dt.agg[, c_cf_lin := ncat*log(10 + 1)/10]
	dt.agg <- dt.agg[,S_T_catlin := sum(wy.value * c_cf_lin / sum(c_cf_lin)), by = .(field, cf_method)]
	# calculate total obi score (non method)
	dt.agg[, c_cf_non := 1]
	dt.agg <- dt.agg[,S_T_catnon := sum(wy.value * c_cf_non / sum(c_cf_non)), by = .(field, cf_method)]
	
	# calculate total obi score if not aggregated by cat
	dt.agg <- dt.agg[, nocat.value := sum(v_cf* pmax(0,value) / sum(v_cf[value >= 0])), by = .(field, cf_method, year)]
	dt.agg <- dt.agg[, S_T_nocat := sum(y_cf * pmax(0, nocat.value) / sum(y_cf[nocat.value >= 0])), by = .(field, cf_method)]
	
	# select data for scores
	dts <- unique(dt.agg[,.(field, indicator,cat, wy.value, S_T ,treatment, cf_method, S_T_nocat, S_T_catlin, S_T_catnon)])
	
	# reshape dts so total scores are in same column as cat scores (with T being a cat)
	dts1 <- unique(dts[,.(field, cat, wy.value, treatment, cf_method)])
	dts2 <- unique(dts[,.(field, S_T, treatment, cf_method)])
	dts3 <- unique(dts[,.(field, S_T_nocat, treatment, cf_method)])
	dts4 <- unique(dts[,.(field, S_T_catlin, treatment, cf_method)])
	dts5 <- unique(dts[,.(field, S_T_catnon, treatment, cf_method)])
	
	# rename  cols
	setnames(dts1, 'wy.value', 'score')
	setnames(dts2, 'S_T', 'score')
	setnames(dts3, 'S_T_nocat', 'score')
	setnames(dts4, 'S_T_catlin', 'score')
	setnames(dts5, 'S_T_catnon', 'score')
	# add cat column to dts2
	dts2$cat <- 'T_cf_log'
	dts3$cat <- 'T_nocat'
	dts4$cat <- 'T_cf_lin'
	dts5$cat <- 'T_cf_non'
	
	# bind scores dt's
	dt.agg <- rbindlist(list(dts1, dts2, dts3, dts4, dts5), use.names = TRUE)
	
	# update element names
	dt.agg[,cat := paste0('S_',cat)]
	dt.agg[, score := round(score,3)]
	
	# factorise cat and cf_method
	dt.agg <- dt.agg[, cat := factor(cat, levels = c('S_T_cf_log', 'S_C', 'S_P',
											'S_B', 'S_E','S_M',
											"S_T_cf_lin", "S_T_cf_non", "S_T_nocat"))]
	dt.agg <- dt.agg[, cf_method := factor(cf_method, levels = c('log', 'lin', 'non'))]
}

dt2 <- aggscores_brent(dt)

## ----plot baselines scores, fig.width = 7, fig.height = 7,fig.fullwidth = TRUE, fig.cap = 'Figure 3. Total OBI score boxplots per aggregation method for each scenario.'----

# arithmetric mean of all indicators (this should be equal to lin_ccf?)
arimean <- dt[cf_method == "non", arimean := mean(value), by = c("field", "treatment")]
arimean <- dta[,arimean := mean(value), by = c("field", "treatment")]
arimean2 <- unique(arimean[, .(field, treatment, arimean)])
arimean3 <- arimean2[, median(arimean), by = treatment]

# plot
ggplot(dt.out[cat == 'total'], aes(x = value, y = cf_method)) +
        geom_vline(data = arimean3, aes(xintercept = V1), col = "red", lty = 2) +
        geom_boxplot() +
        theme_bw() + scale_colour_viridis_d() + scale_y_discrete(limits = rev) +
        coord_cartesian(xlim = c(0,1)) +
        facet_wrap(~treatment, ncol = 1)



## ----OBI score summary table--------------------------------------------------

  # get relevant data from dt
  dtt <- dt.out[, list(value = round(mean(value),3)), by = c( 'cat', 'cf_method', 'treatment')]

  # improve category discretion
  dtt <- dtt[,ct := paste('mean', cat)]

  # dcast
  dtt <- dcast(dtt ,treatment+cf_method~cat, value.var = 'value')

  # factorise and order treatment
  dtt <- dtt[, treatment := factor(treatment, levels = c('baseline', 'low B values', 'low C values',
                                                         'one low B', 'one low C',
                                                         'Recent years high', 'Recent years low'))]

  # order 
  setorder(dtt, treatment, cf_method)

# make table 
  knitr::kable(dtt,
               caption = 'Mean scores per category and total per aggregation method')
  
  # make dtt version with just total scores to use for in text reporting
  dttt <- dtt[,.(treatment, cf_method, S_T_OBI_A = total)]

## ----plot original values, eval=FALSE, fig.width = 7, fig.height = 20,fig.fullwidth = TRUE, fig.cap = 'Figure 4. Distribution of indicator values  per scenario.'----
#  # factorise dta cat levels
#  # dta <- dta[, cat := factor(cat, levels = c('C', 'P', 'B', 'E'))]
#  # dta <- dta[, indicator := factor(indicator, levels = c("I_C_CEC","I_C_CU", "I_C_K",  "I_C_MG", "I_C_N",  "I_C_P",  "I_C_PH", "I_C_S",  "I_C_ZN",
#  #                                                        "I_P_CR", "I_P_DS", "I_P_DU", "I_P_SE", "I_P_WRI", "I_P_WS","I_P_CEC","I_P_CO",
#  #                                                        "I_B_DI", "I_B_SF", "I_E_NGW","I_E_NSW"))]
#  #
#  # # plot
#  # ggplot(dta, aes(x = value, y = indicator, color = cat))+
#  #   geom_boxplot() +
#  #   theme_bw() + coord_cartesian(xlim = c(0,1)) + scale_y_discrete(limits = rev)+
#  #   facet_wrap(~treatment, ncol = 1)
#  

## ----plot baselines scores zoomed in, fig.width = 7, fig.height = 2,fig.fullwidth = TRUE, fig.cap = 'Figure 4. Total OBI score per aggregation method for baseline sceinario.'----

# plot (different colors per method, 'lin' and 'log' are divided into 4 variation)
dt3.out[, method_main := substr(cf_method, 1, 3)]
dt3.out[, alpha := 1]
dt3.out[!grepl("_all|non", cf_method), alpha := 0.5]

ggplot(dt3.out[cat == 'total' & treatment =="baseline"],
       aes(x = value, y = cf_method, fill = method_main, alpha = alpha)) +
  # geom_vline(data = arimean3, aes(xintercept = V1), col = "red", lty = 2) +
  geom_boxplot() +
  theme_bw() + scale_colour_viridis_d() + scale_y_discrete(limits = rev) +
  labs(fill = "")+
  guides(alpha = F)+ xlab("total score") + ylab("scenario") +
  coord_cartesian(xlim = c(0,1))

# check standard deviation
#dt3.out[cat == 'total' & treatment =="baseline", sd(value), by = cf_method]

## ----plot scores with and without cat, fig.width = 7, fig.height = 5,fig.fullwidth = TRUE, fig.cap = 'Figure 5. Scores when categories are ignored during aggregation (S_Tnocat_OBI_A) and regular aggregation (S_T_OBI_A).'----

#dt <- copy(dta)
#dt <- addcf(dt)
#dt2 <- aggscores_brent(dt)
dt2[, colv := "no category aggregation"]
dt2[grepl("S_T_cf", cat), colv := "with category aggregation"]
gg3 <- ggplot(dt2[cat %in% c('S_T_cf_log', 
                             #'S_T_cf_lin', 'S_T_cf_non', 
                             'S_T_nocat') & 
                  cf_method == 'log' & 
                  !treatment %in% c('Recent years low', 'Recent years high')],
              aes(x = score, y = cat)) +
  geom_boxplot(aes(fill = colv)) +
  labs(fill="") + xlab("total score") + ylab("")+
  theme_bw() + scale_y_discrete(limits = rev) +
  coord_cartesian(xlim = c(0,1)) +
  facet_wrap(~treatment, ncol = 1) #+ geom_boxplot(data = dt[cat == 'S_Tnocat_OBI_A'], mapping = aes(fill = 'blue')) 
gg3 

## ----eval=FALSE---------------------------------------------------------------
#    obic_field(B_SOILTYPE_AGR =  dt$B_SOILTYPE_AGR, B_GWL_CLASS =  dt$B_GWL_CLASS,
#               B_SC_WENR = dt$B_SC_WENR, B_HELP_WENR = dt$B_HELP_WENR, B_AER_CBS = dt$B_AER_CBS,
#               B_LU_BRP = dt$B_LU_BRP, A_SOM_LOI = dt$A_SOM_LOI, A_SAND_MI = dt$A_SAND_MI,
#               A_SILT_MI = dt$A_SILT_MI, A_CLAY_MI = dt$A_CLAY_MI, A_PH_CC = dt$A_PH_CC,
#               A_N_RT = dt$A_N_RT, A_CN_FR = dt$A_CN_FR,
#               A_S_RT = dt$A_S_RT, A_N_PMN = dt$A_N_PMN,
#               A_P_AL = dt$A_P_AL, A_P_CC = dt$A_P_CC, A_P_WA = dt$A_P_WA, A_CEC_CO = dt$A_CEC_CO,
#               A_CA_CO_PO = dt$A_CA_CO_PO, A_MG_CO_PO = dt$A_MG_CO_PO, A_K_CO_PO = dt$A_K_CO_PO,
#               A_K_CC = dt$A_K_CC, A_MG_CC = dt$A_MG_CC, A_MN_CC = dt$A_MN_CC,
#               A_ZN_CC = dt$A_ZN_CC, A_CU_CC = dt$A_CU_CC, output = 'obic_score')

## ----get binnenveld indicator values, eval=TRUE-------------------------------
# cleanup bini if required
if(exists('bini')){rm(bini)}

# select columns
bcols <- names(binnenveld)[!grepl('BCS$', names(binnenveld))]

# get indicator values per field, for first 10 fields
for(i in unique(binnenveld$ID)[1:10]){
  # calc OBIC inidicators for i
  bini.n <- obic_field_dt(binnenveld[ID == i,..bcols], output = 'unaggregated')
  # re add ID
  bini.n <- bini.n[,ID := i]
  if(!exists('bini')){
    # if bini doesn't exist yet make it
    bini <- bini.n
  } else{
    # if bini exists, add bini.n to bini (binnenveld indicators)
    bini <- rbindlist(list(bini, bini.n))
  }
}

# remove inidicators not used for scoring
bini <- bini[!cat %in% c('BCS', 'IBCS', 'IM')]

# remove irrelevant columns
rmcols <- names(bini)[!grepl('^weight|cf|w$', names(bini))]
bini <- bini[,..rmcols]

# rename ID to field
setnames(bini, 'ID', 'field')
# add treatment
bini$treatment <- bini$field


## ----aggregate binneveld, eval=TRUE-------------------------------------------
# add correction factors
bini <- addcf(bini)

# add scores
bini <- aggscores(bini)


## ----plot binnenveld scores, eval=TRUE, message = FALSE, fig.height= 9, fig.width= 8 ,fig.cap="Figure 6. Total and category OBI scores of binnenveld fields aggregated with 'log', 'lin' or 'non' method, as well as total scores when disregarding categories in aggregating scores."----
# make labels
ldt <- binnenveld[,.(ID, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_CLASS)]
ldt <- ldt[ID %in% unique(ID)[1:10]]

# get most occurring soil type and crop type
ldt <- ldt[, lapply(.SD, function (x) names(sort(table(x),decreasing = TRUE)[1])), 
              .SDcols = c('B_LU_BRP','B_SOILTYPE_AGR', 'B_GWL_CLASS'),by = ID]
ldt[, B_LU_BRP := as.integer(B_LU_BRP)]

# add crop name
ldt <- merge(ldt, crops.obic[,.(crop_code, crop_name)], by.x = 'B_LU_BRP', by.y = 'crop_code')

# order ldt
setorder(ldt, ID)

# make cat labels more readable
bini[grepl('^T', cat), lcat := "Total"]
bini[grepl('^C', cat), lcat := "Chemical"]
bini[grepl('^B', cat), lcat := "Biological"]
bini[grepl('^P', cat), lcat := "Physical"]
bini[grepl('^M', cat), lcat := "Management"]
bini[grepl('^E', cat), lcat := "Environmental"]
bini[, lcat := factor(lcat, levels = c('Chemical', 'Physical', 'Biological', 'Environmental',
                                       'Management','Total'))]


# make plot
gg <- ggplot(bini, aes(x= lcat, y= value, color = cf_method)) +
  geom_point(size = 2,alpha = 0.5) +
  ylab('OBI-score') + xlab('') +
  theme_bw(12) + theme(panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(size = 0.5),
                       panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim = c(0,1))+ 
  scale_x_discrete(limits = rev)+ scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))+
  coord_flip() +
  facet_wrap(~field, ncol = 3) +
  theme(legend.position = c(0.8,0.1))

# show gg
gg

# show table with data on binnenveld fields
knitr::kable(ldt[,.(ID, B_SOILTYPE_AGR, B_GWL_CLASS, crop_name)], caption = 'Soiltype, groundwaterclass and most frequent crop per binnenveld field.')

## ----graphical representation of EPX , fig.cap= "NAC scores where one variable varies and the others are set to Maximum ecological potential (MEP), variable values range from 5th percentile to 95th percentile reported by Rutgers (2008). Type indicates whether standard calculation (i) is used or not(j) where j improves on the NAC."----
# make table with reference data from Rutgers2008
ac.mep <- data.table(var = c('FMA', 'PotC', 'PotN', 'SOM', 'pH', 'PAL'),
                     mep.v = c(2700, 18, 2.0, 2.2, 7.6, 47),
                     soiltype = rep('klei', 6),
                     landuse = rep('akkerbouw', 6),
                     nl.mean = c(1150,22, 2, 2.5, 7.5, 47),
                     nl5perc = c(14, 9, 0.5, 1.6, 7.3, 31),
                     nl95perc = c(3960, 48, 3.7, 3.6, 7.7, 62))

# make data
dtS <- data.table(x = seq(1.6,3.6,length.out = 100), var = 'SOM')
dtF <- data.table(x=seq(10,3990,length.out = 100), var = 'FMA')
dtF <- dtF[x>200]
dtH <- data.table(x=seq(7.3,7.7,length.out = 100), var = 'pH')
dtP <- data.table(x=seq(31,62,length.out = 100), var = 'PAL')
dtC <- data.table(x=seq(9,48,length.out = 100), var = 'PotC')
dtN <- data.table(x=seq(0.5,3.7,length.out = 100), var = 'PotN')

# combine data
dta <- rbindlist(list(dtS, dtF, dtH, dtP, dtC, dtN))

# add mep values
dta <- merge(dta, ac.mep[,.(var, mep.v)], by = 'var')

# indicate when to use i or j type
dta[,type := 'i']
dta[(var == 'FMA' & x<mep.v) | var == 'SOM'& x>mep.v, type := 'j']

# calc y
dta[,y := fifelse(type == 'i',10^-(abs(log10(x/mep.v)/6)), 10^(abs(log10(x/mep.v)/6)))]

# plot
ggplot(dta) + 
  geom_line(aes(x = x, y = y, col = type))+
  ylab("NAC score if all other variables are at MEP") +
  xlab("Variable value") +
  # labs(title = 'j (red) and i (black) type plot, j-type is applied') +
  facet_wrap(facets = ~var, scales = 'free_x', ncol = 3) +
  theme_bw()

## ----eval = FALSE, include=FALSE----------------------------------------------
#  # e.g. (van Wijnen et. al. 2012 and  Rutgers et. al. 2012) or Moebius-Clune 2016


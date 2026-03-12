##################################################################
# Script developed by Jen Cruz to estimate SSFs and iSSFs          #
# approach derived from Fieberg et al. 2021 and Signer et al. 2019 #
# using code from Appendices B and C                             #
# also vignette here:
# https://conservancy.umn.edu/server/api/core/bitstreams/63727072-87b1-4b35-b81c-8fd31b8f1e57/content #
# Vegetation cover  was downloaded from Rangeland Analysis Platform #
# https://rangelands.app/products/ for 2021 and includes        #
# % cover for shrub, perennial herbaceous, annual herbaceous    #
# tree, litter and bare ground                                   #
# coordinate system is WGS84 EPSG:4326, spatial resolution is 30m #
#                                                                #
# Prairie Falcon data was thinned to 20 seconds for 9 individuals #
# tracked in 2021 and uses NAD83 UTM zone 11N +                   #
# which is the same as the NCA polygon                           #
# Jen plotted 5 sec, 10 sec, 15 sec, etc. to see which time interval was
# good and found 20 seconds was good for smoothed data
#two-step approach
###################################################################

################## prep workspace ###############################
#install relevant packages
install.packages( "circular" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt )
library( circular ) #for plotting von mises distribution (turning angle)

#####################################################################
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------

# Clean your workspace to reset your R environment. #
rm( list = ls() )
load('SSF_results.RData')
#load 30m steps estimated for all individuals and habitat 
# variables extracted for each step
df_steps <- read.csv( "Data/df_steps20.csv" )
#5 second data was too tortuous, so Jen checked 10 sec, 15 sec, 20 sec etc.
#mapped and decided 20 sec was good compromise and thinned out enough of the most 
#tortuous movement (she did this "off screen" and just sent us the data) because she's 
#looking at foraging
#she also removed points in buffer around nests to exlude territory defending movements
NCA_Shape <- sf::st_read("Data/BOPNCA_Boundary.shp")
lines20sec <- read_rds( "Data/lines20sec" )
#Jen didn't send to us, so skip this first visualization for hw
#she joined all tracks in a line to create lines20sec
#import akde ranges you created which includes  thinned (30min) data
ranges <- read_rds( "Data/akde_all" ) 

#######################################################################
######## preparing data ###############################################
#let's remind ourselves what data we are looking at
# extract akde ranges
akdes <- ranges %>%
  hr_to_sf( hr_akde_all, id ) %>% #turns akde ranges into sf objects
  dplyr::filter( what == "estimate" ) #gives mean estimates of the areas of home ranges
#otherwise it will give us confidence intervals

#hr_isopleths( akde_all$hr_akde_all )
#skip this visualization
ggplot(lines20sec ) +
  theme_bw( base_size = 15 ) + 
  theme( legend.position = "none" ) +
  geom_sf( aes( color = as.factor(id) ),
           linewidth = 1 ) +
  # geom_sf(data = NCA_Shape, linewidth = 1.5,
  #         fill=NA ) +
  #extract isopleths for ouf model using thinned data from amt
  geom_sf( data = akdes,
           color = "black",
           linewidth = 1, fill=NA ) +
  facet_wrap( ~id)
#see notes for the result

#view steps
head( df_steps) #2 points per row bc steps, ta = turning angle, altitude here is not
#corrected for ground, so you would have to change that if you were actually using alt
#burst id = how many burst? + individual id
#t1, t2 is time between first and second steps (20 sec)
#used step and 9 available steps that match it (true vs false case)
#perennial, annual, shrub at 30 m resolution (reasonable for 20 sec intervals)
#create vector of potential predictors
prednames <- c( "annual", "perennial", "shrub" )

#check for missing values
colSums( is.na( df_steps[,prednames] ) )

# Scale predictors 
#minus mean and divide by one standard deviation for each value
#create new dataframe to hold scaled predictors/covariates, while keeping 
# unscaled ones for plotting later (by making a copy to scale)
#puts all covariates on same scale
#makes data comparable, improves convergence, should be standard protocol for all analysis
#ex: if you have one covariate where one is say distance to water, which is hundreds of km
#and one where it's on the meter scale, it would be hard to compare without scaling and 
#models would have trouble converging. also good for interpretation when everything is standardized
df_scl <- df_steps

#scale only those columns:
df_scl[, prednames] <- apply( df_scl[,prednames], 2, scale )
#view
head( df_scl)
#now replace small amount of missing values with 0,which represents 
# the mean for scaled predictors
# df_scl$annual[is.na(df_scl$annual)] <- 0
# df_scl$perennial[is.na(df_scl$perennial)] <- 0
# df_scl$shrub[is.na(df_scl$shrub)] <- 0

#for the movement parameters we also calculate the log of sl and the cos of ta
df_scl$log_sl_ <- log( df_scl$sl_ ) #shape parameter of gamma distribution
df_scl$cos_ta_ <- cos( df_scl$ta_ ) #relates to concentration parameter of Von mises (what makes the shape fat or long)
# we also turn our step lengths to km instead of meters to match it to the scale/values of our scaled env predictors
df_scl$sl_ <- df_scl$sl_ / 1000
#PRFA can move up to 800 m/0.8 km in 20 sec, so like half a mile 
#check
hist(df_scl$sl_ )
# we also assign weights to available points to be much greater than used points bc logistic reg
df_scl$weight <- 1000 ^( 1 - as.integer(df_scl$case_ ) )
#turns case column into 1 (used) or 0 (available) and weighs available
#1000^1 for available (so 1000) and 1000^0 for used (so 0)
#check
head( df_scl )
#this was similar to rsf prep except we also prep movement parameters

#### end data prep #############
###########################################################################
#####   running STEP SELECTION FUNCTIONS          ##########
# We start with a traditional step selection function
#use step_id as random intercept to account for conditional likelihood
# we fit separate models for each individual
mi1 <- df_scl %>% dplyr::filter( id == 1 ) %>% 
  fit_ssf( 
    case_ ~ #response variable (used or available column without weights, y)
      #habitat variable (no movement in ssf)
      annual + perennial + shrub +
      #stratum to ensure random steps match to each point to make it into conditional logistic regression
      strata( step_id_ ), #id column gives the same id to the sets of matched points
    model = TRUE )   

summary( mi1 ) #runs fast bc we scaled it
#n = how many points, exponentiates coeff for us, gives p value (pr), calculates confidence intervals for the 
#exponentiated metric
#from p-values, we can see perennial and shrub are selected for strongly

#now we fit the same model for all individuals (fit sep models for each ind and put 
#them together for analysis post-hoc, two-step):
mall <- df_scl %>% 
  nest( data = -id ) %>% 
  dplyr::mutate( ssf = lapply( data, function(x) { #creates new column to put analyses
    x %>%  amt::fit_ssf(  case_ ~ annual + perennial + shrub +
                            strata( step_id_ ) )
  } ) )

mall #nested data (you can see sample size) and new set of tables with the sep analyses
#id, data, and ssf columns
#we clean up and combine results to get the average selection
# across all individuals
#calculate population level estimates
d2 <- mall %>% 
  dplyr::mutate( coef = map( ssf, #creates new coef column
                             ~broom::tidy(.x$model) ) ) %>% #summarizes information about model components
  dplyr::select( id, coef ) %>% #now it's just the id and coeff columns 
  unnest( cols = c(coef) ) %>% #unnest coeff
  dplyr::mutate( id = factor(id) ) %>% #turn id into categories
  dplyr::group_by( term )%>% #group by annual, perr, shrub
  dplyr::summarize( 
    mean = mean( estimate ), 
    #calculate 95% CIs
    ymin = exp(mean - 1.96 *sd(estimate)), #1.96 is the standard ballpark estimate when you're trying to add standard error
    #and you're assuming data is normally distributed
    ymax = exp(mean + 1.96 *sd(estimate) ),
    mean = exp(mean ) )

d2$x <- 1:nrow( d2 ) #new x column that has 1, 2, 3 for annual, perr, shrub
d2

# visualizing model results #
# extract coefficients for each individual
#diff is we're not summarizing by term
coefsall <- mall %>% 
  dplyr::mutate( coef = map( ssf, 
                             ~broom::tidy(.x$model) ) ) %>% 
  dplyr::select( id, coef ) %>% 
  unnest( cols = c(coef) ) %>% 
  dplyr::mutate( id = factor(id),
                 #model was logistic so have to exponentiate
                 conf.low = exp(estimate - 1.96 * std.error),
                 conf.high = exp(estimate + 1.96 * std.error),
                 estimate = exp(estimate) )
tail(coefsall)

#we plot individual differences 
pall <- coefsall %>%
  ggplot(., aes(x = term, y = estimate, 
                group = id, col = id ) ) +
  #add individual results
  geom_pointrange( aes( ymin = conf.low, 
                        ymax = conf.high ),
                   position = position_dodge( width = 0.7 ), size = 0.8 ) +
  #draw line at 0
  geom_hline( yintercept = 1, lty = 2 ) +
  #start with population level averages we calculated earlier
  geom_rect( mapping = aes(xmin = x - 0.4, xmax = x + 0.4, 
                           ymin = ymin, ymax = ymax ), 
             data = d2, 
             inherit.aes = FALSE, fill = "grey90", alpha = 0.5) +
  geom_segment(mapping = aes(x = x - 0.4, xend = x + 0.4, 
                             y = mean, yend = mean ), 
               data = d2, inherit.aes = FALSE, size = 1 ) +
  #Add the labels to each axis
  labs(x = "Habitat", y = "Relative Selection Strength") + 
  theme_light()

pall
#y = 1: using predictors proportionate to what's available, so above is selecting for, 
#below is selecting against, black line is population average
#gray squares are population conf intervals
# How do you interpret the results from this figure?
# Answer:
# annual is not strongly selected for or against
#(average line is pretty close to one and individuals are clustered close to the average)
# perennial has some individuals that strongly suggest for or against
# shrub has more individuals that select very strongly for or against (even more variable than perennial)
# just looking at averages doesn't give you this whole picture

# could selection be due to the amount of habitat available for each 
# individual?
#what's happening in the bigger picture (e.g. are individuals selecting for
#shrub at the third order bc they don't have a lot in the home range?)
#looking at multi-scale habitat selection = functional responses
#how are animals selecting and does that relate to availability?
#we can calculate across all available and all used points for each vegetation 
#type, what was the mean value?
# To answer this question we extract additional details from our steps
# dataframe including sex and the average amount of veg cover available
# for each individual

#we create a new id df with those summary values
iddf <- df_steps %>% 
  group_by( id, territory, sex ) %>% 
  summarise( annual_mean = mean( annual, na.rm = TRUE),
             perennial_mean = mean( perennial, na.rm = TRUE),
             shrub_mean = mean( shrub, na.rm = TRUE)
  )
iddf
#turn into a long format to combine with coefs dataframe
id_long <- iddf %>% 
  pivot_longer( cols = ends_with( "mean" ),
                names_to = "term",
                values_to = "cover" )
#view
head( id_long )
#modify term to match coefs by removing _mean from labels
id_long$term <- str_split_i( id_long$term, "_",1 )
#view
head( id_long )
#turn id to factor to match
id_long$id <- as.factor( id_long$id)
#combine with our resource selection strength estimates
coefs_df <- left_join( id_long, coefsall, by = c("id", "term" ) )
#view
head(coefs_df)

#plot resource selection strength by vegetation cover 
ggplot( coefs_df,aes( x = cover , y = estimate, color = id ) ) +
  theme_classic( base_size = 15 ) +
  labs( x = "Mean cover (%)", 
        y = "Resource selection strength" ) +
  geom_point(aes(shape = sex)) +
  geom_errorbar( aes( ymin = conf.low, 
                      ymax = conf.high ) ) +
  geom_hline( yintercept = 1, linewidth = 1, lty = 2 ) + 
  facet_wrap( ~term, scales = "free", ncol = 1 )

#x axis is percent cover (that's the difference from prev plot)
#y = resource selection strength
#annual = mostly cheatgrass, PRFA seem to mostly ignore it and don't select for or against

#### what do you interpret from this plot?
# is amount of vegetation influencing results?
# Answer:
# no, otherwise you'd see individuals selecting less or more as the percent cover increases
#or there's a "sweet spot" of percent cover where individuals seem to like and cluster around
#availability doesn't seem like a driver 
#other explanations: maybe different prey preferences or whether they are sitting on the nest or
#hunting more or less (sex-based differences), different hunting styles even for same prey
#if they have different hunting styles, maybe we'd see different movement patterns/behaviors,
#or they are moving throug certain habitat, but ssfs don't capture that
# What about sex?
# Answer:
# Nope. Still seems very randomly distributed

##### end of ssf analysis #####
############################################################
#### iSSF analysis                                  #####
#####################################################################
## We saw that there are differences in how individuals are selecting#
# habitat based on our previous analysis BUT we do not know yet #
# the relationship between habitat and how individuals move. To #
# explore those we shift to iSSFs using the same data            #
#ie if hunting styles are different between vegetation types, we may
#be able to see that. also are they just passing over certain vegetation
#types more to get to other vegetation types
################## single individual iSSF ########################
# For homework choose a different one by modifying code below:
mi <- df_scl %>% dplyr::filter( id == 4 ) %>% 
  fit_issf( #response variable
    case_ ~ 
      #add habitat variables
      annual + perennial + shrub +
      #add movement variables
      log_sl_ + cos_ta_ + sl_ + 
      #log of sl only modifies either shape or scale (jen can't remember which, it's the alpha parameter), 
      #sl captures the other parameter. modifying both is unnecessary, but you do need both in the model
      #cos_ta modifies shape (how tortuous), but not mean
      # add movement interactions (what makes this issf)
      log_sl_:shrub + cos_ta_:shrub +
      log_sl_:perennial + cos_ta_:perennial +
      log_sl_:annual + cos_ta_:annual +
      #add stratum to ensure random steps are matched to corresponding used step
      strata( step_id_ ), model = TRUE )

summary( mi )
#focus on p values and how coeffs changed compared to ssf model
#individual 2 (ind we analyzed in class) had crazy strong selection for annual (exp coeff = 10)
#bc interaction terms that were signficant were annual:log_sl and annual:cos_ta
#suggesting this individual is moving differently in annual but not in other veg types
#differently how? will need to plot


# We calculate the tentative distributions from empirical data 
# for that same individual
# Start with step length fitted as a gamma with shape and scale parameters (empirical)
emp_d_sl <- df_scl %>% 
  #select step lengths for that individual
  dplyr::filter( id == 4 ) %>% 
  dplyr::select( sl_ ) %>% 
  #fit a gamma distribution using empirical data
  amt::fit_distr(., dist_name = "gamma" )
emp_d_sl
#calculates empirical distribution regardless of habitat

#Fit a von misses to the turning angles for that individual (empirical)
emp_d_ta <- df_scl %>% 
  #select turning angles for that individual
  dplyr::filter( id == 4) %>% 
  dplyr::select( ta_ ) %>% 
  #use the amt fit_dist function
  amt::fit_distr(., dist_name = "vonmises" )
emp_d_ta
#regardless of habitat
#kappa = concentration parameter, makes it fat or skinny

#Assign the empirical distributions to model object,
#so give it the empirical distributions we calculated:
#we had to do this bc the data was saved as a csv, which auto-removed empirical metrics
mi$sl_ <- emp_d_sl
mi$ta_ <- emp_d_ta
# view
mi$sl_
mi$ta_

# Now we can use coefficients associated with movement parameters
# to update our movement related distributions for the same individual. #
# Refer to appendix c in Fieberg et al. 2021 to see the equations that
# are used to update distribution parameters #

# we need to first relabel coefficients since we have interactions
# Start by extracting coefficients of the model 
b <- coef( mi )
b
#choose significant interaction with habitat
# Here I choose annual.
# Change it depending on your individual results
#update empirical distribution so it becomes distribution in annual
# Modify code accordingly:
summary(mi)
#extracts coefficients
b_log_l <- b["log_sl_"] 
b_log_h <- b["log_sl_"] + b["annual:log_sl_"] 
b_sl <-  b["sl_"] 
# Update step length distribution to the baseline when annual doesn't interact 
# with step length:
updated_sl_l <- update_gamma( mi$sl_, #empirical
                              beta_sl = b_sl, #updated coefficients when annual is not high (before we put in interaction term)       
                              beta_log_sl = b_log_l )
# Update step length distribution of how habitat alters step distribution
updated_sl_h <- update_gamma( mi$sl_, 
                              beta_sl = b_sl,     #updated coefficients when habitat is high (adds interaction term)    
                              beta_log_sl = b_log_h )

#view estimated parameters
updated_sl_l;updated_sl_h
#scale is same for both, shape is updated by the interaction
# Are any of the parameters negative? If so then the model is ill fitted. 
# Tal Avgar recommends to try a different step-length distribution
# include different interactions 
# remove non-movement steps (based on a step-length threshold )
# resample data to coarser resolution

#For turning angle, choose significant interaction if present for that 
# individual 
# Modify code accordingly:
b_costa_l <- b["cos_ta_"]
b_costa_h <- b[ "cos_ta_" ] + b["annual:cos_ta_"]
#update turning angle distribution:
updated_ta_l <- update_vonmises( mi$ta_,
                                 beta_cos_ta = b_costa_l )
updated_ta_h <- update_vonmises( mi$ta_,
                                 beta_cos_ta = b_costa_h )

#View results
updated_ta_l
updated_ta_h
#Is the von Mises concentration parameter (kappa) negative? #
# If so Tal Avgar indicates that the adjusted turn angle distribution
# is centred at pi (180) rather than 0, meaning that the animal is 
# more likely to turn back. 

# What results did you get for your individual? 
# Answer:
# ind 4 also had significant p-values only for annual, but it was a much lower coeff than 2 (0.66)
# so some selection but not strongly
# also strong siginicant positive selection for perennial, log_sl, shrub: cos_ta, perennial: log_sl
#perennial:cos_ta, annual:log_sl, and annual:cos_ta
# step length scale is 1.77 for low and 1.85 for high annual
# turning angle kappa is 1.47 for low and 1.59 for high annual

###### plot changes in step length distribution ###
# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

#check step lengths of chosen individual to choose your x
hist(df_scl[ which(df_scl$id == 4),'sl_'])

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 1, length.out = 100)
#0 - 1 km, evenly spaced 100 points

# y-axis is the probability density under the given gamma distribution
# For the updated distribution when habitat is low
plot_sl$updated_l <- dgamma(
  x = plot_sl$x,
  shape = updated_sl_l$params$shape,
  scale = updated_sl_l$params$scale)
#when habitat is high
plot_sl$updated_h <- dgamma(
  x = plot_sl$x,
  shape = updated_sl_h$params$shape,
  scale = updated_sl_h$params$scale)

# Pivot from wide data to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

tail(plot_sl)
# Plot
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlim(0,1) +
  xlab("Step Length (km)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution", 
                     breaks = c("updated_l", "updated_h"),
                     values = c("blue", "orange")) +
  theme_bw()

#How did the distribution change with model results?
# Answer:
#1.72 shape for empirical step length, so shape value shifted higher for both high and low annual updated models
#(implying slightly faster in annual vegetation, especially high annual vegetation)
#1.48 for empirical turning angle which close to the kappa value for low annual but lower than the value for high annual,
#which means slightly more directed flight in high annual vegetation

####  Plot turning angle distribution changes ###
# data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -1 * pi, to = pi, length.out = 100)

# y-axis is the probability density under the given von Mises distribution
# For low habitat
plot_ta$updated_l <- circular::dvonmises(
  x = plot_ta$x, 
  mu = updated_ta_l$params$mu,
  kappa = updated_ta_l$params$kappa)

# For high habitat
plot_ta$updated_h <- circular::dvonmises(
  x = plot_ta$x, 
  mu = updated_ta_h$params$mu,
  kappa = updated_ta_h$params$kappa)

# Pivot from wide data to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

tail(plot_ta)
# Plot
ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  xlab("Relative Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  scale_color_manual(name = "Distribution", 
                     breaks = c("updated_l", "updated_h"),
                     values = c("blue", "orange")) +
  theme_bw()

#How do you interpret output for these last two plots?
# Did habitat and movement parameters interact to alter how #
# Prairie falcons use their landscape ?
# Answer:
# for ind 2, less directed and slower in high annual vegetation
#yes. looking at just habitat can tell us that an individual selects more or less strongly 
#for a habitat but looking at turning angle and step lengths tells us more about why they
#might be selecting for or against that habitat
#it can also tell us how they're moving in a habitat even if they don't select for it
# Add answer for the new individual that you tried for homework
# ind 4 was slightly faster (longer step lengths) and slightly more directed 
# (narrower shape) in high annual cover but it's not a lot
#
##### end of single individual issf ####
########################################################################
######### All individual issfs ####################################

# Repeat the analysis for all individuals at the same time #
miall2 <- df_scl %>% 
  nest( data = -id ) %>% 
  dplyr::mutate( issf = lapply( data, function(x) {
    x %>%  amt::fit_issf(  case_ ~ 
                             #add habitat variables
                             annual + perennial + shrub +
                             #add movement variables
                             log_sl_ + cos_ta_ + sl_ +
                             # add movement interactions with shrub
                             log_sl_:shrub + cos_ta_:shrub +
                             log_sl_:perennial + cos_ta_:perennial +
                             log_sl_:annual + cos_ta_:annual +
                             #add stratum to ensure random steps are matched to corresponding used step
                             strata( step_id_ ), model = TRUE )
  } ) )

miall2

# Extract coefficients for all indviduals
coefs_issf2  <- miall2 %>% 
  dplyr::mutate( coef = map( issf, 
                             ~broom::tidy(.x$model) ) ) %>% 
  dplyr::select( id, coef ) %>% 
  unnest( cols = c(coef) ) %>% 
  dplyr::mutate( id = factor(id),
                 conf.low = exp( estimate - 1.96 * std.error),
                 conf.high = exp(estimate + 1.96 * std.error),
                 estimate = exp(estimate))
#view results
coefs_issf2

#average across individuals
d4 <- miall2 %>% 
  dplyr::mutate( coef = map( issf, 
                             ~broom::tidy(.x$model) ) ) %>% 
  dplyr::select( id, coef ) %>% 
  unnest( cols = c(coef) ) %>% 
  dplyr::mutate( id = factor(id) ) %>%
  dplyr::group_by( term )%>% 
  dplyr::summarize( 
    mean = mean( estimate ), 
    #calculate 95% CIs
    ymin = exp(mean - 1.96 *sd(estimate)), 
    ymax = exp(mean + 1.96 *sd(estimate)),
    mean = exp( mean ), )

d4$x <- 1:nrow( d4 )

# Plot individual differences and population averages 
pissfs2 <- coefs_issf2 %>%
  #dplyr::filter(id %in% 3:9 ) %>% 
  ggplot(., aes(x = term, y = estimate, 
                group = id, col = id ) ) +
  #add individual results
  geom_pointrange( aes( ymin = conf.low, 
                        ymax = conf.high ),
                   position = position_dodge( width = 0.7 ), size = 0.8 ) +
  #draw line at 0
  geom_hline( yintercept = 1, lty = 2 ) +
  #start with population level averages we calculated earlier
  geom_rect( mapping = aes(xmin = x - 0.4, xmax = x + 0.4, 
                           ymin = ymin, ymax = ymax ), 
             data = d4, 
             inherit.aes = FALSE, fill = "grey90", alpha = 0.5) +
  geom_segment(mapping = aes(x = x - 0.4, xend = x + 0.4, 
                             y = mean, yend = mean ), 
               data = d4, inherit.aes = FALSE, size = 1 ) +
  #Add the labels to each axis
  labs(x = "Predictors", y = "Relative Selection Strength") + 
  theme_light() +
  #ylim( c( 0,5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


pissfs2
#model may have failed bc some individuals have crazy confidence intervals and values
#produced are unrealistic
#(should start to worry if ci is more than 10)
#you may have overparameterized and model is too complicated
#either have to simplify for everyone or take out the two individuals 
#(BUT our sample size is too small already)
#OR put all individuals in same model with random slopes (will do that next week)
#Note: can't fit a model with just interaction effects (when there's two coeff together) 
#without main effects ever


#view
head( id_long )
#combine with our resource selection strength estimates
coefs_df <- left_join( id_long, coefs_issf2, by = c("id", "term" ) )
#view
head(coefs_df)

#plot resource selection strength by vegetation cover (right now we will just remove 2 ind)
coefs_df %>%
  dplyr::filter(id %in% 3:9 ) %>% 
  ggplot( .,aes( x = cover , y = estimate, color = id ) ) +
  theme_classic( base_size = 15 ) +
  labs( x = "Mean cover (%)", 
        y = "Resource selection strength" ) +
  geom_point() +
  geom_errorbar( aes( ymin = conf.low, 
                      ymax = conf.high ) ) +
  geom_hline( yintercept = 1, linewidth = 1, lty = 2 ) + 
  facet_wrap( ~term, scales = "free", ncol = 3 )
#Interpretation example:
#for shrub, you can see that individuals that had more shrub cover, esp at 16%, did not select for it
#selection seemed to be strongest for area with less cover
#(16% vs 8% shrub is actually a pretty significant difference in the landscape)
#16% is heavy shrub and 8% is just shrubs here or there. Likely there's more ground squirrels in areas with less shrub


### what are the weaknesses of averaging separate individual models #
# in this way? #
# Answer:
# if you have uneven sample sizes (some individuals are data hungry and some are data poor),
#your model may fail. You have to have enough data on all individuals to compare them properly
#because you can't just fit simpler models for the few individuals with low data (bc you're running
#the same model with everyone's data in it)

# What is a possible solution to the weakness you stated above?
# Answer:
# Remove the individuals (costly in our case with just 9 individuals) or use random slopes
##########################################################################
### Save desired results   #
#we save the scaled dataframe so that we can use it for our random effects
write.csv(df_scl, "Data/df_scl.csv"  )
#save workspace if in progress
save.image( 'SSF_results.RData'  )

############# end of script  ##################################
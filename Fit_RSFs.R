##################################################################
# Script developed by Jen Cruz to estimate resource selection     #
# functions. Code adapted from atm vignette:                      #
#https://cran.r-project.org/web/packages/amt/vignettes/p3_rsf.html #
# Vegetation cover  was downloaded from Rangeland Analysis Platform #
# https://rangelands.app/products/ for 2021 and includes        #
# % cover for shrub, perennial herbaceous, annual herbaceous    #
# tree, litter and bare ground                                   #
# coordinate system is WGS84 EPSG:4326,                          #
# We perform analyses in amt (Signer et al. 2019) and glmmTMB    #
# Prairie Falcon data was thinned to 30minutes for 9 individuals #
# tracked in 2021.                                               #
###################################################################

################## prep workspace ###############################
# we will be using new packages:
install.packages( "glmmTMB" )
# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt )
library( glmmTMB ) # for analysis

#####################################################################
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#set working directory

#if you already started the analysis then load workspace:
#load('RSFresults.RData')
#load our clean data frame to assess 1st order selection
df_sa <- read.csv( "Data/df_sa.csv" )
#load data for 2nd order selection
df_hr <- read.csv( "Data/df_hr.csv" )
#import polygon of the NCA as sf spatial file:
NCA_Shape <- sf::st_read("Data/BOPNCA_Boundary.shp")
#this one has the same CRS as our used/available points
#######################################################################
######## preparing data ###############################################
#create vector of predictors taking advantage of naming commonality 
# to automatically extract them:
prednames <- grep('0m', colnames(df_sa), value = TRUE)
#"0m" is what it should look for at the end of character strings
#value = TRUE gives exact characters from df_sa without turning them into integers
#view
prednames
# Scale predictors create new dataframes to hold scaled predictors, while keeping 
# unscaled ones for plotting later
sa_scl <- df_sa
#scale only those columns:
sa_scl[, prednames] <- apply( sa_scl[,prednames], 2, scale )
#2 is apply  to second row
#view
head( sa_scl)
# why do we scale predictors?
# Answer:
# makes data comparable, improves convergence, should be standard protocol for all analysis
#ex: if you have one covariate where one is say distance to water, which is hundreds of km
#and one where it's on the meter scale, it would be hard to compare without scaling and 
#models would have trouble converging. also good for interpretation when everything is standardized

#now check for missing values
colSums( is.na( sa_scl[,prednames] ) )
#no missing values in this instance. 
# we also assign weights to available points to be much greater than used points
sa_scl$weight <- 1000 ^( 1 - as.integer( sa_scl$case_ ) )
#case = used or available. FALSE = available
#check
head( sa_scl )
tail( sa_scl )

# We repeat the process for second order selection where
# available points were extracted within each individual's range
#extract individual id numbers:
idnos <- sort( unique( df_hr$territory )) 
#duplicate dataframe
hr_scl <- df_hr
#scale only those columns:
hr_scl[, prednames] <- apply( hr_scl[,prednames], 2, scale )
#view
head( hr_scl)
#now check for missing values
colSums( is.na( hr_scl[,prednames] ) )
#no missing values in this instance. 
# we also assign weights to available points to be much greater than used points
hr_scl$weight <- 1000 ^( 1 - as.integer( hr_scl$case_ ) )
#check
head( hr_scl )

#########################################################################
#################### Population-level RSF #########################
# We want to determine use within the NCA assuming 9 prairie falcons #
# are a representative sample of the population. #

# When would this not be the case? #
# Answer:
# 9 being too small a sample size. Depends on how many falcons are in the NCA.

# We start with our finest resolution of predictors:
msa_100m <- glmmTMB::glmmTMB( case_ ~ 1 + annual_100m + perennial_100m +
                       shrub_100m,
                     family = binomial(), data = sa_scl, 
                     weights = weight ) 
#comparing between used and available data by each covariate, 1 is SD I think
#view results
summary( msa_100m )
# How do you interpret the summary table of results?
# Answer:
# can't yet since we didn't exponentiate
# Now we look at 500m resolution
msa_500m <- glmmTMB::glmmTMB( case_ ~ 1 + annual_500m + perennial_500m +
                       shrub_500m,
                     family = binomial(), data = sa_scl, 
                     weights = weight ) 

summary( msa_500m )
#Did inference change at the bigger scale?
# Answer:
# About the same

#which scale has the most support? We compare model fit using AIC
anova( msa_100m, msa_500m)

# Which scale is most supported by model selection?
# Answer:
# 100 m

# Interpreting results of the top model ###
# we start by exponentiating the coefficients:
exp( glmmTMB::fixef( msa_100m )$cond )
# this reflects the relative selection strength for choosing each
# vegetation cover when the remaining vegetation covers are kept 
# at their mean values
# Thus prairie falcons are 1.415 times more likely to choose
# shrub with cover that is 1 SD higher when annual and perennial are 
# kept at their mean
#Results:
#(Intercept)    annual_100m perennial_100m     shrub_100m 
#0.0001870258   1.3904774512   0.9972005499   1.4156360576 

# To remind ourselves what the SD for our predictor is
apply( df_sa[,prednames], 2, sd )
#Results:
#perennial_100m    annual_100m     shrub_100m perennial_500m    annual_500m 
#9.791397      10.820083       7.512654       8.512755       9.369981 
#shrub_500m 
#6.711714
# And now the mean values for each habitat:
apply( df_sa[,prednames], 2, mean )
#Results:
#perennial_100m    annual_100m     shrub_100m perennial_500m    annual_500m 
#13.48428       19.06490       10.34567       13.49770       18.93876 
#shrub_500m 
#10.21466

# So a Prarie Falcon will be 1.4 times more likely to use an area with 10 %
# shrub than 2.8 % shrub when annual is 19% and perennial is 13.5 % (mean values)

#What would be the equivalent statement for annual?
# Answer: 
#PRFA would be 1.4x more like to use use an area with 19% annual than 8.3% 
#(mean annual - sd annual?) annual when shrub is 10.3% and perennial is 13.5%.

# plot differences in distribution between used and available #
# locations for our predictor of choice. To plot on the real scale we #
# combine unscaled data first:
ggplot( df_sa ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = shrub_100m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  ) 
#now for perennial
ggplot( df_sa ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = perennial_100m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  )  
#for annual:
ggplot( df_sa ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = annual_100m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  )  

#What do these plots tell us about how prairie falcons select habitat?
# Is it reasonable to assume that all individuals are selecting 
# habitat similarly?
# Answer:
#They have a fairly strong preference for shrub and annual 
#(less overlap than there is for perennial).
# No because we know from initial data exploration that they don't.
#########
################ 2nd order RSFs ####################
######
# We replicate our approach at the home-range scale starting with 100m#
mhr_100m <- glmmTMB::glmmTMB( case_ ~ 1 + annual_100m + perennial_100m +
                       shrub_100m,
                     family = binomial(), data = hr_scl, 
                     weights = weight ) 
#view results
summary( mhr_100m )
exp( glmmTMB::fixef( mhr_100m )$cond )
#Results:
#   (Intercept)    annual_100m perennial_100m     shrub_100m 
#0.0001904805   0.8946200836   1.0530546370   1.3900924017 
#prefer perennial and shrub

# Now we look at 500m
mhr_500m <- glmmTMB::glmmTMB( case_ ~ 1 + annual_500m + perennial_500m +
                       shrub_500m,
                     family = binomial(), data = hr_scl, 
                     weights = weight ) 

summary( mhr_500m )
exp( glmmTMB::fixef( mhr_500m )$cond )
#Results:
#(Intercept)    annual_500m perennial_500m     shrub_500m 
#0.0001890328   0.7810880573   1.0734497800   1.2919772145 
#We compare models using AIC
anova( mhr_100m, mhr_500m)

# Which scale is most supported by model selection?
# Answer:
#500 m (doesn't give it in order. AIC for 500m is 62169, 62237 for 100 m)
# Is the scale the same as that chosen for the study area analysis?
# If not, why would that be?
# Answer:
# No, maybe because the falcon choose a certain point more due to the surrounding
#landscape than what was immediately around that point at the home range scale.


# Interpret results of the top model ###
exp( glmmTMB::fixef( mhr_500m )$cond )
#Results:
#  (Intercept)    annual_500m perennial_500m     shrub_500m 
#0.0001890328   0.7810880573   1.0734497800   1.2919772145 
# To remind ourselves what the SD for our predictor is
apply( df_hr[,prednames], 2, sd )
#Results:
#perennial_100m    annual_100m     shrub_100m perennial_500m    annual_500m 
#8.073329      12.041058       7.813842       6.865333      10.468136 
#shrub_500m 
#7.228348 
# And now the mean values for each habitat:
apply( df_hr[,prednames], 2, mean )
#Results
#perennial_100m    annual_100m     shrub_100m perennial_500m    annual_500m 
#12.501716      23.935045       9.634850      12.497218      23.829956 
#shrub_500m 
#9.553448 

#Using the same approach as we did for study area rephrase selection
# for the most significant predictor?
# Answer:
# PRFA prefer an area 1.2x more when there is 9.55% shrub than when there is 
#2.35% shrub when annual is 23.8% and perennial is 12.5%.

# we also plot differences in distribution between used and available #
# locations for our predictor of choice. This time we look at #
# potential differences among individual ranges vs used habitat: 
ggplot( df_hr ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = shrub_500m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  ) +
  facet_wrap( ~ territory )

# Are all individuals using shrub in higher proportions than what 
# is available inside their range? 
# Describe and contrast selection for each individual:
# Answer: 
# No
#Most are using shrub in higher proportions than what is available in their
#range, but Mac is not really, and some individuals, like HHGS_US and SDTP, don't seem
#to have that strong of a preference

# Are their ranges filled with similar amounts
# Describe differences here:
# No
# HHGS_US, PR_II, SDTP, and SG still use more shrub than is available, but not by
#much. Mac barely preferes shrub at all. 
# Tally individuals using more shrub cover than what 
# is available in their range:
# Answer:
# CFR, CRW, CRW_new, HHGS_DS, HHGS_US, PR_II, SDTP, SG
# 8 individuals (Mac has a little bit of non-overlap as well)

# For homework also add similar figures for the other
# two vegetation types at the same scale of the top model
# and interpret differences and similarities
# Add code and responses here:
#Annual
ggplot( df_hr ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = annual_500m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  ) +
  facet_wrap( ~ territory )
#Some individuals have a strong preference for annual vegetation, but the
#number who prefer annual is less than for shrub. I would say only CFR, CRW,
#CRW_new, PR_II, and SG really use more annual vegetation than available.
#Perennial
ggplot( df_hr ) +
  theme_bw( base_size = 15 ) +
  geom_density( aes( x = perennial_500m, 
                     fill = case_, group = case_ ),
                alpha = 0.5  ) +
  facet_wrap( ~ territory )
#All seem to show some preference for perennial habitat. Only HHGS_US and SG
#show smaller amounts of preference.

# Did the interpretation of which habitats are selected by #
# Prairie falcons differ between the 1st order and 2 order selection?
# Answer:
#Yes. They select for shrub at both orders, but otherwise more strongly prefer annual 
#vegetation at the 1st order and more strongly prefer perennial at the second.



###########################################################
### Save desired results                                  #
# we can save the movement model results
#save workspace if in progress
save.image( 'RSFresults.RData' )
############# end of script  ##################################
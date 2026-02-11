##################################################################
# Script developed by Jen Cruz to calculate home ranges          # 
# We rely on amt vignette here:                                  #
# https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html #
# as well as: Signer & Fieberg (2021) PeerJ9:e11031              #
# http://doi.org/10.7717/peerj.11031                             #
###################################################################

################## prep workspace ###############################7

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library(dplyr)
library( amt )
library( sf )

#####################################################################
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# load workspace 
#load( "homerangeresults.RData" )
# Jen says she'll put like a ##### stop here ### when she stops working and saves the workspace as an rdata so she knows where it'll pick up next time

#load the thinned (30min) data
trks.thin <- read_rds( "Data/trks.thin" )

#load 5 sec data for comparison
trks.breed <- read_rds( "Data/trks.breed" )

#import polygon of the NCA as sf spatial file:
NCA_Shape <- sf::st_read( "Data/BOPNCA_Boundary.shp" )

###############################################################
##### Comparing different estimators for occurrence      #######
###   distributions for all individuals at once.          ####
## We evaluate Minimum Convex Polygons (MCP),             ####
### Kernel density estimators (KDE).                         ##
## Individuals are often sampled for different time periods ##
# so we also standardize time periods to evaluate the       ##
### effects of sampling period on home range estimates.     ##
##############################################################
#check if object has coordinate system in the right format
get_crs( trks.thin )
#view data
head( trks.thin, 10)
tail(trks.breed )
# MCP and KDE rely on data with no autocorrelation. But
# how do we check for autocorrelation to determine if # 
# we need to thin data further?
# One approach is by calculating autocorrelation functions (ACF)
# We do so for each individual using our 30min sampled data:
par( mfrow = c( 2,3 ) )
#based on direction
for( i in 1:dim(trks.thin)[1] ){
  #select x location of each individual
  x <- trks.thin %>% dplyr::filter( id == i ) %>% 
    #select( x_ )
    select( y_ )
  #select(speed )
  #calculate autocorrelation function:
  acf( x, lag.max = 40,
       main = paste0( "individual = ", i ) )
  #Note you can modify the lag.max according to your data 
}
#within the blue lines = not autocorrelated or very little 
#You want to autocorrelation to get removed as you move along the x axis because the first few bars say the data is autocorrelated with itself, which is expected
#assesses autocorrelation at different lags, from 0 (no lag), bars for individual 1 goes 30 min, 1 h, 2 h, 4 h, 8 hr, etc.
#if you were to remove enough data for individual 2 for it to not be autocorrelated, the data would be 20x coarser
#if this were real and you didn't have AKDE, you'd just have to raise your threshold for your autocorrelation cutoff and 
#acknowledge that you left some autocorrelation in the data
#Confused about this.
# Are our data autocorrelated?
# Answer:
# yes

#if you are struggling to determine that maybe we can compare
# against our 5 second data (only captured 3 days of the week, as a reminder), which we know will be correlated:
par( mfrow = c( 2,3 ) )
for( i in 1:dim(trks.breed)[1] ){
  #select x location of each individual
  x <- trks.breed %>% dplyr::filter( id == i ) %>% 
    #select( x_ )
    select( y_ )
  #select(speed )
  #select( alt )
  #calculate autocorrelation function:
  acf( x, lag.max = 500,
       main = paste0( "individual = ", i ) )
  #Note we modified the lag.max 
}
#What do you see regarding autocorrelation?
#Answer:
# Very highly autocorrelated.

# How much would you have to thin your data to remove autocorrelation?
# Answer:
# Would have to thin most of the data.

# we keep the 30min interval and not thin data (as we will make use of autocorrelation)
# next week and calculate MCP (minimum convex polygon) and KDE (kernel density estimator) for each individual:
ranges <- trks.thin %>% 
  #we group tibbles for each individual:
  nest( data = -"id" ) %>% #advantage of nesting approach is all the data stays together and you can group it by some 
  #criteria (individual level in this case). Can also do multiple analysis methods at once with the data
  #then add estimates from two home range measures:
  #Jen normally doesn't recommend autocorrelated data for these methods because it'll crash them. 
  #This would also not be publishable. We are doing this for the sake of the class exercise
  mutate(
    #Minimum Convex Polygon
    hr_mcp = map(data, ~ hr_mcp(., levels = c(0.5, 0.95)) ),
    #levels = isopleths (line drawn on map through all points of the same value). We are doing 50 and 95 percentiles
    #Kernel density estimator
    hr_kde = map(data, ~ hr_kde(., levels = c(0.5, 0.95)) ),
    #also calculate the sample size for each individual
    n = map_int( data, nrow )
  )  
#view
ranges

#plot KDEs:
ranges %>%
  hr_to_sf( hr_kde, id, n ) %>% 
  ggplot( . ) +
  theme_bw( base_size = 17 ) + 
  geom_sf( aes(color= as.factor(id) ) )  +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE, 
          fill = NA ) +
  theme( legend.position = "none" ) +
  facet_wrap( ~id )
# smaller circles in the center are 50 percentile of data (where the bird spent 50% of it's time I think)

#save the MCP only so that we can plot it against the KDE:
mcps #just the sf object without plotting
mcps <- ranges %>%
  hr_to_sf( hr_mcp, id, n )

#plot both methods:
#select tibble 
ranges %>%
  #choose one home range method at a time
  hr_to_sf( hr_kde, id, n ) %>%  #AMT object to sf (vector) object, also stores ids and sample sizes
  #plot with ggplot
  ggplot( . ) +
  theme_bw( base_size = 17 ) + 
  geom_sf( aes( fill = as.factor(id)), 
           linewidth = 0.8, alpha = 0.6 ) +
  geom_sf( data = mcps, colour = "black", #just added this extra line to plot mcps above kdes
           linewidth = 1, fill = NA ) +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE, fill=NA ) +
  theme( legend.position = "none" ) +
  #plot separate for each individual
  facet_wrap( ~id )

# adding polygons make the home range bigger or smaller depending on the individual
# MCP makes it so each individual only has 1 50% area and doesn't necessarily match up with 50% area in KDEs
# MCP may not be super reliable method
#which individuals show the biggest variation between the two methods?
#Answer:
# 1, 3, 4, 6

#Is there evidence that sample size is affecting differences among 
#individuals? How are you deciding your answer?
#Answer:
#Not necessarily. 1 has the lowest sample size and 4 has the largest and they both are pretty different between the two methods.

# Using KDE we can see large variation among individuals#
# during the breeding season. Is it real? We know our sampling wasn't #
# consistent. To account for our variable sampling, we can plot #
# estimated occurrence distributions weekly. #

# recalculate n and weekly range estimates for kdes:
hr_wk <- trks.thin %>%  
  # we nest by id and week
  nest( data = -c(id, wk, sex ) ) %>%
  mutate( n = map_int(data, nrow ) ) %>% #recalculate sample size
  #remove weeks without enough points
  filter( n > 15 ) %>% 
  mutate( #now recalculate weekly home range
    hr_kde = map(data, ~ hr_kde(., levels = c(0.5, 0.95)) )) #just kde this time

#plot weekly ranges for each individual at a time so that
# we can focus on within-individual differences
#define a vector with individual ids to loop through
ids <- hr_wk %>% 
  group_by( id ) %>% 
  slice( 1 ) %>% 
  dplyr::select( id, sex )
ids #can see more females than males

# this way you can loop through each individual
for( i in ids$id ){
  # #filter data for one individual at a time:
  wp <- hr_wk %>% filter( id == ids$id[i] ) %>% 
    #turn range estimates to sf objects, keeping relevant id details
    hr_to_sf( hr_kde, id, sex, wk, n ) %>% 
    #plot with ggplot
    ggplot( . ) +
    theme_bw( base_size = 13 ) + 
    # color code weekly ranges by week
    geom_sf( aes( fill = as.factor(wk),color = as.factor(wk) ), 
             alpha = 0.3, linewidth = 1 ) +
    #add used locations from 5 sec data as a check on top (remember that 5 sec data was only captured 3 days a week):
    geom_sf( data = as_sf_points( trks.breed %>% 
                                    filter( id == ids$id[i] ) ),
             size = 0.5 ) +
    #add NCA polygon
    #  geom_sf(data = NCA_Shape, inherit.aes = FALSE, fill=NA ) +
    #add labels including individual ID and sex as title
    labs( title = paste( ids$id[i], ids$sex[i] ),
          fill = "week", color = "week", x = "lat" ) + 
    #choose legend location
    theme( legend.position = "bottom" ) +
    #plot separate for each week
    facet_wrap( ~wk )
  # prints each individual separately
  print( wp )
}
# week 26 is end of june so there's only a few points bc we filtered out data after june bc that's migration time
# some weeks don't have data probably because battery died that week (maybe was in nest cavity without sunlight)

# to help us work out where we are in the season we extract initial 
# dates for each week
trks.thin %>% group_by(wk ) %>% 
  slice( 1 )
#How are the range distributions changing on a weekly basis?
# Answer:
# Home ranges pretty consistent between weeks. You can kind of tell when females start sitting on eggs in some individuals because 
#all their locations are suddenly clustered in one area. They seem to more or less use the same 50% areas (nest and foraging areas
#probably), but some of them do explore new areas some weeks.

#Let's compare how our 30m and 5 sec tracks compare tracks
ggplot( trks.breed, aes( x = x_, y = y_ ) ) +
  theme_bw( base_size = 15 ) +
  geom_point( size = 0.5 ) +
  geom_point( data = trks.thin, color = "red",
              size = 1 ) +
  facet_wrap( ~id, scales = "free" )

###### Estimating home range area ##########################
# We can also calculate the range size area.
hr_area <- ranges %>%  select( -data ) %>% 
  pivot_longer( hr_mcp:hr_kde, names_to = "estimator", 
                values_to = "hr" )

#view
hr_area
# The we calculate area for each method 
hr_area <- hr_area %>%  
  mutate( hr_area = map( hr, ~hr_area(.)) ) %>% 
  unnest( cols = hr_area )
#convert area in m^2 to area in km^2 bc m was way too big
hr_area$area_km <- hr_area$area / 1e6

#add sex attribute 
hr_area <- left_join( hr_area, ids, by = "id" )
#check
head(hr_area)
#plot 
hr_area %>% 
  #choose desired level 
  filter( level  == 0.95 ) %>% 
  ggplot( aes(x = as.character(id), y = area_km, 
              color = sex ) ) + 
  geom_point(size = 4) +
  theme_light(base_size = 15) + 
  facet_wrap( ~estimator, nrow = 2, 
              scales = "free_y" )
# Comment on this graph
#Answer:
# mcps are mostly smaller than kdes. males seem to go further (but we only have 3 males).

# Replot this with 50% area size.
# Add Code:
hr_50 <- hr_area %>% 
  #choose desired level 
  filter( level == 0.5 ) %>% 
  ggplot( aes(x = as.character(id), y = area_km, 
              color = sex ) ) + 
  geom_point(size = 4) +
  theme_light(base_size = 15) + 
  facet_wrap( ~estimator, nrow = 2, 
              scales = "free_y" )
hr_50

# How do individuals and sexes differ between 95 and 50% 
# breeding ranges? # what could you tentatively say about 
# their ecology based on these results # 
# Answer:
# males and females have more comparable 50% ranges (males still seem to use slightly bigger areas than females), 
#but males seem to have bigger 95% ranges in general. 
#Tentatively, I could say that males have bigger breeding ranges than females and both sexes possibly sit on nests, though females
#probably do more nest sitting (explaining why they have more comparable 50% areas), but maybe males do most of the foraging, 
#which is why they roam further. The two females with extremely small 95% ranges also have very small 50% ranges, so they probably
#spent almost 100% of the breeding season nest sitting.

# optional question:
# Adapt area size code to plot weekly changes in area size 
# for the weekly ranges you subsetted 
# Code Answer: 
hr_weekly <- hr_wk %>%  select( -data ) %>% 
  pivot_longer( hr_kde, names_to = "estimator", 
                values_to = "hr" )

#view
hr_weekly
# The we calculate each method 
hr_weekly <- hr_weekly %>%  
  mutate( hr_weekly = map( hr, ~hr_area(.)) ) %>% 
  unnest( cols = hr_weekly )
#convert area in m^2 to area in km^2 bc m was way too big
hr_weekly$area_km <- hr_weekly$area / 1e6

#add sex attribute 
hr_weekly <- left_join( hr_weekly, ids, by = "id" )
#check
head(hr_weekly)
#plot 
hw <- hr_weekly %>% 
  filter( level  == 0.95 ) %>% 
  #plot with ggplot
  ggplot( aes(x = as.character(wk), y = area_km, 
              color = sex.x ) ) + 
  geom_point(size = 4) +
  theme_light(base_size = 15) + 
  facet_wrap( ~id, nrow = 2, 
              scales = "free_y" )
print( hw )

    

#### end of area ##############

###########################################################
### Save desired results                                  #
#if you are still getting through this script then save 
# the workspace here so you don't have to rerun your code
save.image( 'homerangeresults.RData' )

# Once you are finished:
#save breeding ranges
write_rds( ranges, "Data/ranges" )
#save weekly home ranges
write_rds( hr_wk, "Data/hr_wk" )

# save the homework plots and upload them to github 
# Here:
write_rds( hr_50, "Data/hr_50" )
write_rds( hw, "Data/hw" )

############# end of script  ###########################################
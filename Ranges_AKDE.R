##################################################################
# Script developed by Jen Cruz to estimate ranges using AKDE     # 
# For this script we rely on Fleming et al.(2015) Ecology 96(5):1182-1188#
# We use ctmm first, and then use amt                           #
# For instructions on how to use ctmm directly check out:       #
# https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html #
# https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html #
###################################################################

################## prep workspace ###############################

# Clean your workspace to reset your R environment. #
rm( list = ls() )
#uncomment and install the package if you haven't got it
#install.packages( "ctmm" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt )
library( sf )
library(ctmm ) #for more detailed functionality, don't model home range in ctmm
#from Flemings paper
#####################################################################
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------

#load workspace if you have already started working through this script#
load( "AKDEresults.RData")

#if you are starting from scratch load cleaned data:
#download the thinned (30min) data
trks.thin <- read_rds( "Data/trks.thin" )
#this is breeding points at 30 min
#view
head( trks.thin )
#download breeding ranges we estimated last week
ranges <- read_rds( "Data/ranges" )
#view import
head( ranges )

#import polygon of the NCA as sf spatial file:
NCA_Shape <- sf::st_read( "Data/BOPNCA_Boundary.shp" )

####################################################################
############## estimating ranges in CTMM ###########################
###################################################################

######### variograms using ctmm ###############
# We use ctmm to explore autocorrelation in our data #
# but using estimates of semi-variance instead of the acf() from 
# last week.

##### We start with a single individual to test  ######
#choose an individual id (can choose a different one to check):
i <- 2
# filter tracks to select that individual's data
t <- trks.thin %>% filter( id == i )
#convert to ctmm object
ctmm.t <- as_telemetry( t ) #if downloading data straight from movebank: it's already 
#in this format so you don't need this line
#estimate empirical variograms
svf.t <- variogram( ctmm.t )
#now plot them side by side
par(mfrow = c(2,1) ) #how many rows and columns plots should be displayed at
plot(svf.t, fraction = 1, level = 0.95)
#lag = timescale, so it's showing how far the bird goes from a center point
#at each possible timescale 
#ie how far they travel from nest on average when it's been 1 day into breeding vs 5 vs 10, 
#which is why the confidence intervals (gray) get much worse as time goes on
#closer to dispersal at higher lags, so traveling further on average
#cycling bc they sometime travels less, sometimes more
#now zoom in to starting time lags
plot( svf.t, xlim = c(0,2 %#% "day"), #zoom to 2 days
      fraction = 1, level = 0.95 )
#we want to look at beginning of variograms bc it's more reliable
#also big differences are at the start of the variogram, and it does plateau 
#to an average (asymptotic)
#if there was an upward trajectory in distance travelled over time, it would suggest
#this isn't place-based movement, so like not home range movement

################
##################################################################
##### ALL individuals using ctmm     #############################
###################################################################
#Plot variograms for all individuals
# extract names for individuals first into an object
ids <- sort(unique( trks.thin$id ))
#create objects to store results
#ctmm was built with base r, so it uses lists instead of nested data.
svf.t <- list()
ctmm.t <- list() #turns data into as.telemetry object
xlimz <- c(0,36 %#% "hour" )
#set plot parameters, base R
par( mfrow = c(3,3)) #creates 3 by 3 plots
#loop through all individuals. this can help us determine if our data is reliable for 
#home range analysis visually
for( i in ids ){
  #print progress
  print( i )
  # extract data for individual i
  t <- trks.thin %>% filter( id == i )
  #convert to ctmm object and add to list. storing points for one individual
  ctmm.t[[i]] <- as_telemetry( t )
  #Calculate empirical variograms:
  svf.t[[i]] <- variogram( ctmm.t[[i]] )
  #plot variograms for each individual
  plot( svf.t[[i]], xlim =  xlimz ) #limits x to 36 hours (xlimz object earlier)
  #how much we zoom in for our own data depends on period of time tracked and fixed rates
}

# How are they unique for each individual?
# Answer:
# The distances travelled are pretty unique for each individual. Some tend to go
#further while some don't, so some home ranges will be bigger than others. 
#There is also some variability in the cycling of the  distances traveled, 
#but they all do cycle between traveling longer and shorter distances.

#########
##############################################################
# automate the process of estimating a suitable movement     #
# model for the observed data using the empirical            # 
# variogram as a guide.                                     #
# options are "iid" (KDE): for uncorrelated independent data,      #
#  "ou": Ornstein-Uhlenbeck process (home range),  #
# "ouf": Ornstein-Uhlenbeck forage process,                  #
# "auto": uses model selection with AICc to find bets model  #
################################################################

### we try the model selection method for our class example ###
### YOU DO NOT NEED TO DO THIS SECTION FOR HOMEWORK ######

#create and object to store results 
m.best <- list()
#loop through each individual
#this won't be fast...remember that we are estimating all #
# possible movement models for each individual and then #
# using AIC to pick a best model from the model choices #
# we also plot the empirical variograms vs the model results #
for( i in 1:length(ids)){
  print( i )
  #use empirical variogram estimated in the previous step to guess starting parameters
  # as a way of guiding the choice of movement model
  guess <- ctmm.guess(data = ctmm.t[[i]], variogram = svf.t[[i]],
                      interactive = FALSE ) #"This function plots a variogram object 
  #overlayed with a continuous-time movement model guesstimated from the variogram's
  #shape."
  #here we actually calculate and compare among 6 movement model options 
  # and compare fit using AIC to select the top model
  m.best[[i]] <- ctmm.select( ctmm.t[[i]], guess, verbose = TRUE,
                              trace = 2 ) 
  #trace = "Report progress updates. Can be among 0:3 with increasing detail." 
  #Verbose TRUE returns additional information
  #ctmm.t is just points for each individual as a ctmm object
  #view summary output for model comparison for each individual
  print(summary( m.best[[i]] ))
}
#use individual names to label each list:
names( m.best ) <- ids#[1:2]
#see notes for a screenshot/notes of what it looks like to just plot one ind
#some individuals only plot less than 6 models bc they have less data

#define plotting parameters:
par(mfrow = c(2,2))
#Now compare top model choice against traditional KDE
for( i in 1:length(ids) ){
  #trace progress:
  print(i)
  # add basic IID model to model list
  m.best[[i]]$"IID isotropic" <- ctmm.fit( ctmm.t[[i]],
                                           ctmm(isotropic = TRUE) )
  #get row id for IDD model
  iidid <- length(rownames(summary( m.best[[i]])))
  #extract model name for top two models (this is code updated after class):
  an <- strsplit( rownames(summary( m.best[[i]][1])), " ")[[1]][1]
  ab <- strsplit( rownames(summary( m.best[[i]][2])), " ")[[1]][1]
  #plot top model, second best, and IID models
  ctmm::plot( svf.t[[i]], m.best[[i]][c(1,2,iidid)],
              col.CTMM=c("orange","blue","red"),
              #define a short lag to zoom in on differences
              xlim = c(0,6 %#% "hour" ),
              # label with individual id and color for top two models
              main = paste( ids[i], "orange =", an,
                            "blue =", ab ) )
}  
#Code used in class:
  #extract model name for top model, m(best) is all the models ordered based on aic selection for each ind
  #an <- rownames(summary( m.best[[i]][1]))
  #plot best model: empirical variogram, best model, zoom to 36 hours, label
  #ctmm::plot( svf.t[[i]], m.best[[i]][[1]], 
              #xlim =  xlimz ,
              #main = paste( ids[i], an ) )#best model
  #plot two most common models: plot second top model
  #a2 <- rownames(summary( m.best[[i]][2]))
  #plot 2nd best model
  #ctmm::plot( svf.t[[i]], m.best[[i]][[2]], 
              #xlim =  xlimz ,
              #main = paste( ids[i], a2 ) )
  # #plot against traditional KDE
  # ctmm::plot( svf.t[[i]], m.best[[i]]$"IID isotropic", 
  #             xlim = xlimz,
  #       main = paste( ids[i], "IID isotropic" ) ) 
#}  
#We are looking for: differences between the two top models and between individuals
#OU anisotrophic and OUF anisotrophic are consistently top models
#the two top models are indistinguishable to us
#OU is top model for most (one on the left for each individual)
#See notes

# Comment on the differences in the variance model assumptions
# between akde and traditional kde ###
# Answer:
#didn't plot kde against empirical variogram in class, so will answer this question 
#for the home ranges plotted using OU, OUF, KDE
#kde tends to be fatter than ou and ouf, but ou and ouf tend to be very similar
#kde inflates range sometimes.


# How consistent was the top model chosen among individuals?
# For which individuals did it vary most? How?
# Answer:
# OU anisotrophic was chosen for most individuals. 
#4 varied the most: the top model, OUF anisotrophic, had a gentler curve as the time
#lags increased than the second model, OUf anisotrophic

# Now that we have estimated top movement models for each #
# individual we are ready to apply those models to our estimates #
# of ranges. 

# We also have an extra option to choose from #
# we can weight points based on high utilisation to correct the range #
# estimate.  Weighing may be helpful if there are large data gaps or multiple #
# sampling rates #

# Here we compare ranges from 3 options: (1) top movement model 
# (2) 2nd top movement model  (3) traditional kde no weighing

# we create objects to store output from our 3 options:
akde.ou <- list()
akde.ouf <- list()
kde.iid <- list()
# We loop through each individual to estimate ranges for each option:
for( i in 1:length(ids) ){
  print(i)
  #extract most common movement model
  akde.ou[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]]$"OU anisotropic",
                              weights = FALSE  )
  # extract second most comment movement model
  akde.ouf[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]]$"OUF anisotropic", 
                               weights = FALSE )
  #extract the IID movement model for comparison
  kde.iid[[i]] <- ctmm::akde( ctmm.t[[i]], m.best[[i]]$"IID isotropic" )
}

#plot estimate ranges comparing output for each option:
par(mfrow = c(3,2))
for( i in 1:length(ids) ){# 2
  print(i)
  plot( ctmm.t[[i]], akde.ouf[[i]] )
  title( paste("OUF model", ids[i]) )
  plot( ctmm.t[[i]], akde.ou[[i]] )
  title("OU model")
  plot( ctmm.t[[i]], kde.iid [[i]])
  title("Traditional KDE" )
}
#kde is fatter than ou and ouf, but ou and ouf tend to be very similar
#kde inflates range sometimes, added to show us the differences between the models
#Jen would choose OU bc the more complicated model (OUF) might struggle with more
#data hungry individuals but doesn't seem to have a different fit for more data ind
#which model is best really depends on biological context
#see notes
#ctmm plots are ugly and replots in km from center instead of easting northings
#making it harder to visualize
#code is also clunky: multiple for loops and lists. Annotate heavily if you use ctmm
#now plot with amt (which can't calculate or choose semi-variograms, which is why we used ctmm for that part,
#but amt is better for visualization)

#first we are going to convert our ctmm objects to sf objects and plot (this is still the ctmm output)
#if you like ctmm better, you can just do all the ctmm stuff and then turn into sf objects and plot
#and ignore the amt stuff after
#extract mean HR estimates for weighted and unweighted approaches 
# as sf polygon and combine 
ouf_list <- list()
ou_list <- list()
i_list <- list()
for( i in 1:length(ids) ){
  #extract home range for each animal and turn into sf object
  sf.ouf <- as.sf( akde.ouf[[i]] )
  sf.ou <- as.sf( akde.ou[[i]] )
  sf.i <- as.sf( kde.iid[[i]] )
  # convert crs to back to study area/original crs (otherwise their crs won't match, ctmm auto-changes crs)
  #so changing it back to original crs
  sf.ouf.t <- st_transform( sf.ouf, crs = get_crs( trks.thin ) ) 
  sf.ou.t <- st_transform( sf.ou, crs = get_crs( trks.thin ) ) 
  sf.i.t <- st_transform( sf.i, crs = get_crs( trks.thin ) ) 
  #extract only the point estimate (mean range) and add to list in rows
  ouf_list[[i]] <- sf.ouf.t[2,]
  ou_list[[i]] <- sf.ou.t[2,]
  i_list[[i]] <- sf.i.t[2,]
}

ouf_akdes <-  ouf_list %>%  dplyr::bind_rows()
ou_akdes <-  ou_list %>%  dplyr::bind_rows()
kde_akdes <-  i_list %>%  dplyr::bind_rows()
#now they are sf multipolygons, but names are given as est.....id (see notes)
#re-add attributes for each individual
iddf <- trks.thin %>% 
  group_by( id ) %>% 
  select( id, territory, sex ) %>% 
  slice(1 )
#view
iddf

#adding id, territory, and sex attributes to sf object
class( ouf_akdes)
head(ouf_akdes)
ouf_akdes$name <- iddf$territory
ouf_akdes$id <- iddf$id
ouf_akdes$sex <- iddf$sex
ou_akdes$name <- iddf$territory
ou_akdes$id <- iddf$id
ou_akdes$sex <- iddf$sex
kde_akdes$name <- iddf$territory
kde_akdes$id <- iddf$id
kde_akdes$sex <- iddf$sex

head( ouf_akdes)

#Plot comparisons from the different model choices
ggplot() +
  theme_bw( base_size = 15 ) +
  #compare against  ouf model using all data from ctmm
  geom_sf( data = ouf_akdes,
           fill = NA, col = "purple", linewidth = 2 ) +
  #compare against  ou model using all data from ctmm
  geom_sf( data = ou_akdes,
           fill = NA, col = "orange", linewidth = 1 ) +
  #compare against kde model using all data from ctmm
  geom_sf( data = kde_akdes,
           fill = NA, col = "black", linewidth = 1 ) +
  facet_wrap( ~id )
#each panel is individual, black is kde, ou is orange, ouf is purple (see notes)
#we're going to choose ou for reasons mentioned above even in amt bc ou and ouf look the same
#usually would have stopped when we did semivariograms and saw ou and our were similar
#and just went straight to ou in amt, but this is to show us

## How do the options compare?
#Answer:
##ou and ouf still look the same (kde can look different), so decision of ou or ouf doesn't matter for this dataset.

#####

##############################################################
# Estimating AKDE using atm package                          #
##############################################################
### DO THIS SECTION FOR HOMEWORK   #

# once we have decided which movement model to use we can use 
# amt directly to plot all home ranges at once: 

#make sure individuals are in order (nested) so that they can be compared to ctmm results
nested.thin <- trks.thin %>% 
  arrange( id ) %>% 
  nest( data = -"id" ) #nest tibbles
nested.thin
#ind 4 has most points, 1 has least

#calculate home range using your chosen movement model:
#specify ou is model (notice lowercase)
akde_all <- nested.thin %>% 
  mutate( hr_akde_all = map( data, ~hr_akde( ., 
                                             model = fit_ctmm(., model = "ou", 
                                                              #          uere = uere, ctmm( isotropic = FALSE) 
                                                              #this is the default, so it does anisotropic by default
                                             ),
                                             levels = 0.95 ) ) )
#hr_akde_all is a new set of tibbles
#for our purposes we're not doing 50% home ranges but you could easily add that in if needed: levels = c(0.95, 0,5)
# 
akde_all

#plot amt vs ctmm
for( i in 1:length(ids) ){
  #Plot for all individuals against equivalent from ctmm
  a <- ggplot() +
    theme_bw( base_size = 8 ) +
    #extract isopleths for ouf model using thinned data from amt
    geom_sf( data = hr_isopleths( akde_all$hr_akde_all[[i]] ),
             #specifying hr_akde_all tibbles bc that's our amt stuff
             col = "black", linewidth = 1, fill=NA ) +
    ### you won't have this object because it was created 
    # on the ctmm model selection section, see notes for output
    geom_sf( data = ou_akdes %>% filter(id == i),
             fill = NA, 
             col = "purple", linewidth = 1 )
  labs( title = ids[i] ) 
  print(a)
}

# What is the discrepancy between the two outlines?
# Answer:
# pretty close but not exact
#slightly different sizes between the 2: ctmm falls in middle of two lines that denote amt.
#amt gives us some extra bits of range on some individuals

# now we plot them with the study area underneath
akde_all %>%
  #choose one home range method at a time
  hr_to_sf( hr_akde_all, id ) %>% 
  #plot with ggplot
  ggplot( . ) +
  theme_bw( base_size = 17 ) + 
  geom_sf( aes( fill = as.factor(id)) ) +  
  #,linewidth = 0.8, alpha = 0.6 ) +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE, fill=NA ) +
  theme( legend.position = "none" ) +
  #plot separate for each individual
  facet_wrap( ~id )
#pretty big differences in home ranges between individuals
#Jen says would be pretty comfortable with amt versions because 
#the ranges are similar regardless of package

###########################################################
### Save desired results #
#save range for your selected individual using your preferred 
# movement model 
#save range for all individuals in atm
write_rds( akde_all, "Data/akde_all" )

# I save range for all individuals estimated with ctmm and 
#converted to sf object. But you don't need to 
# since you did not run that part of the code
# write_rds( ou_akdes, "Data/ou_akdes" )
# write_rds( ouf_akdes, "Data/ouf_akdes" )


#save workspace if in progress
save.image( 'AKDEresults.RData' )
############# end of script  ##################################
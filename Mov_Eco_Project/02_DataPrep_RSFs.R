#To extract vegetation data
################## Prep. workspace ###############################
#install packages
install.packages( "tmap") 
install.packages( "corrplot" )
# load packages relevant to this script:
library( tidyverse ) #easy data manipulation and plotting
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt ) #handling tracks from location data
library( sf ) #handling vector data
library( raster ) #handle raster data
library( tmap ) #visualize raster and vector data together
library( corrplot ) #for plotting correlations
## end of package load ###############

#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# If this is not the first time working on this script load workspace
# to pick up where you left off
#need to save stuff from last time
load( "02_DataCleanRSFs.RData" )

#ADAPT
#RESOLUTION
#We cropped the vegetation cover raster to our study area so that #
# the image could be shared via github, which has size restrictions #
# Load the cropped raster here:
#IMPORT RASTER
cover_NCA <- raster::stack( "Data/RAPcover2021_NCA.img" )
#Land cover classifications
#want bigger raster area than stopover polygon (bc that area might just be ag), 
#also should think about if I want veg cover vs land use
#class vs even crop type maybe (crop type, can do stopover area)? May want to make stopover polygon bigger too
#eventually

#import akde ranges you created which includes  thinned (30min) data and home range 
akde_all <- read_rds( "Data/akde_all" )

#load high resolution data for comparison?
trks.breed <- read_rds( "Data/trks.breed" )

# We will derive available data at (1) the study area level (1st-order #
# selection ) because my research will focus on population-level resource selection,
#even though I am only using 2 birds for the class project.

# For scales (1) and (2) of inferences we do not need high resolution #
# data so we used our data resampled at 30 min intervals
trks.thin <- akde_all %>% 
  dplyr::select( id, data ) %>% 
  unnest( cols = data ) 

#define how many random points we want to draw per individual:
# as factor that total points will get multiplied by
rn <- 5
# for publication, want at least 10, but it takes too long for class. maybe will try 10

#For scale/order 1 We create random points inside study area
sa_pnts <- random_points( NCA_Shape, n = nrow( trks.thin )*rn,
                          type = "random", presence = trks.thin )
#How we are defining available habitat. Random gps points within study area of the same number
#as used points, multiply by 5, randomly assign these points, append used points.
#1st Order: Available space = study area

sa_pnts
#case: FALSE for available, TRUE for used

######################
### Extract values of vegetation cover #######################
##########
######## Prepare raster and polygon data ############################

#convert your objects with used and available points to sf objects
# defining the coordinate column to be the same as study area
# since in previous scripts we made them match
#start with study area scale
sa_sf <- sf::st_as_sf( sa_pnts, coords = c("x_", "y_"), #which col are coords
                       crs = st_crs(NCA_Shape) ) #which crs system, for own data, need to give own easting northing

#To extract raster data at used and available points, we need #
# to turn our vector crs to crs used by the raster #
# We ALWAYS TURN VECTOR PROJECTIONS TO RASTERS NOT THE OTHER WAY AROUND
#otherwise you'll distort the shape of your rasters

#now for points at scale 1:
sa_trans  <- sf::st_transform( sa_sf, st_crs( cover_NCA ) ) #now in lat longs

###### Let's check that our raster is suitable for use ###
#view raster attributes
cover_NCA
#note that names of vegetation layers did not save so we add them:
names(cover_NCA) <- c( "annual", "perennial", "shrub" )
#processes data at all layers of vegetation cover at the same time, but the layers don't have names

#we visualize vegetation rasters
#ggplot now also lets you plot rasters
#tm_shape let's you select the object you want to plot
tm_shape( cover_NCA ) +
  #how you want to plot it. plot raster and call it "cover"
  tm_raster( title = "cover (%)" ) + 
  #here you select the NCA polygon to overlay
  tm_shape( NCA_Shape ) +
  #choose to plot the outline in black (giving study area border)
  tm_borders( lwd = 3, col = "black" )

#######
#################################################################
# Extract cover around each point. #
#We have to think about scale here again #
# We choose 3 scales 
##########
#the finest resolution is 30 x 30 m cells extracting value at the cell
sa_cover_30m <- raster::extract( x = cover_NCA, sa_trans, #specify raster package bc terra also has an extract function
                                 #sa_trans is the raster stack used
                                 method = "simple" ) #whichever cell point falls under, it extracts at the finest resolution (30 m) there
colnames(sa_cover_30m) <- paste( colnames(sa_cover_30m),
                                 "30m", sep = "_" )
#check
head( sa_cover_30m )

#larger buffers?

df_sa <- cbind( sa_pnts, sa_cover_30m )
head( df_sa )

#ind attributes? and correlations
#last week's code has been updated. update own r script and work on class project and send Jen what you did.
# Install new packages from "CRAN" repository if you don't have them. # 
install.packages( "tidyverse" ) #actually a collection of packages 
install.packages( "amt" )
#trying to install amt directly from github
# install.packages( "devtools" )
# devtools::install_github("jmsigner/amt")
install.packages( "sf" )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation and plotting
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( amt ) #creating tracks from location data
library( sf ) #handling spatial data
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()

# load workspace 
#load( "02_Cleaning_Data_Script.RData" )

# set path to where you can access your data #
# Note that the path will be different for your.#
datapath <- "C:/Users/rosey/OneDrive/Documents/Movement Ecology/Movement-Ecology/Mov_Eco_Project/01_Data_CTT/"
#import GPS data# 
# Fixes are stored as separate CSV files for each individual
## We therefore create a function that imports multiple files at once:
load_data <- function( path ){
  # extract all file names in the folder
  myfiles <- dir( path, pattern = '\\.csv', full.names = TRUE )
  for( i in 1:length(myfiles) ){
    mydata <- read.csv( file = myfiles[i], 
                        #remove white spaces  
                        strip.white =TRUE, 
                        #include column headers
                        header = TRUE, 
                        # read the id columns as a character instead of number:
                        colClasses = c("id" = "character")) 
    # create df for first file and append rows for other files
    ifelse( i == 1,
            df <- mydata, 
            df <- bind_rows(df, mydata) )
  } 
  #return df for all individuals
  return( df )
}

#apply function to import all files as list of databases:
dataraw <- load_data( paste0(datapath, '01_Raw_Data_CTT/') )
#Note that the files are all in a subdirectory
head(dataraw)

#import polygon of the stopover as sf spatial file:
Stopover_Shape <- sf::st_read( paste0( datapath, "Stopover.shp") )

###################################################################
# Clean GPS data
# Column names for gps csvs#
colnames( dataraw )

# GPS units often provide information on the quality of the fixes they #
# obtained.#
# The units from Ornitela provide HDOP and #
# time to fix information # 
# Start by viewing what those look like in the dataset #
hist( dataraw$pdop, breaks = 50 )
hist( dataraw$time_to_fix, main = "Time to fix" )

# Remove 2D fixes and fixes where PDOP ≥10 following #
# D’eon and Delparte (2005).#
# Also those where time to fix > 50min (based on the histogram) or with 0 satellites:

#start by creating a new dataframe to store cleaned location records:
datadf <- dataraw 
#which columns do we have?
colnames( datadf )
# Filter to remove inaccurate locations
datadf <- datadf %>% dplyr::filter( pdop < 10 ) %>%
  dplyr::filter( time_to_fix <= 50 ) %>% 
  dplyr::filter( latitude > 0 ) %>% 
  dplyr::filter( fix_method == "GPS" ) %>% 
  #remove superfluous columns
  dplyr::select( -hardware_id, -alias, -cog, -hdop, -vdop, -fix_quality, -error, -activity, -battery_mv, -instantaneous_solar_mv, 
                 -solar_current_since_last_fix_ma, -temperature_c, -connect_at, -ctt_device_type, -session_id)
#Excel turned hardware_id into the weird E thing. Make sure to not convert it in future. Doesn't matter rn since id is in separate 
#id column at end, so we are deleting hardware_id
#removing hdop, vdop, cog, since they're blank anyways

#view
head( datadf ); dim( datadf )

# We also need to set a time column containing date and time information #
# in POSIX format (as required by amt)#
# We rely on lubridate for this. If you haven't used lubridate before #
# go here: https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
# to learn more about how to easily manipulate time and dates in R #
# Data are stored in year, month, day, hour, minute, second format in our data. 
# We define correct format with lubridate 
datadf$date <- lubridate::ymd_hms( datadf$time_at_fix,
                                   tz = "UTC" )
# time should be UTC (via ornitela website)
datadf$date <- lubridate::with_tz( datadf$date, tz = "America/Chicago" )
#birds are captured in N Dakota and generally stay in Central Time for migration
# and create new column where we convert it to posixct
datadf$ts <- as.POSIXct( datadf$date )
#view
head( datadf ); dim( datadf )

# # check if any data are missing
all( complete.cases( datadf ) )
# # missing! What do I do?

# we also add month, week, and day of year information using lubridate
datadf <- datadf %>% 
  dplyr::mutate( mth = lubridate::month(date),
                 wk = lubridate::week(date),
                 jday = lubridate::yday(date) )

# Don't know exact dates and times of capture, will have to ask Jay.

##################################################################
### Define coordinate system and projection for the data ######
# location data were recorded using WGS84 in lat long #
# We use the epsg code to define coordinate system for our sites #
# How? Google epsg WGS84 # First result should  take you here: #
# where you can find that epgs = 4326 for this coordinate system #
# If you are not familiar with geographic data, vector, rasters & #
# coordinate systems go here: 
# https://bookdown.org/robinlovelace/geocompr/spatial-class.html #
# to learn more. #

# For amt, crs need to be provided using sp package so:
crsdata <- 4326#
# We also want to transform the lat longs to easting and northings #
# using UTM. For this we need to know what zone we are in. Go: #
# http://www.dmap.co.uk/utmworld.htm
# We are in zone 13.

# Alternatively we can crs of the polygon of our study area, which is already 
# in eastings and northings. check that is the case for your own data
sf::st_crs( Stopover_Shape )
#extract crs value for the study area (in easting northings)
crstracks <- sf::st_crs( Stopover_Shape )

######################################################################
######## Visual checks of raw locations ################################
#######################################################################

#Check sample size #
table( datadf$id, datadf$wk )

# Not all transmitters work according to our expectations.
# We can get an idea of the data collected for each individual
# by plotting histograms

#sampling duration
ggplot( datadf, aes( x = jday, group = id ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ id )

#speeds traveled
ggplot( datadf, aes( x = speed, group = id ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ id )
# completely blank

################
#######################################################################
###### Creating tracks, calculating step lengths and turning angles ###
####              for all individuals at the same time:           #####
########################################################################

#amt requires us to turn data into tracks for further analyses.
trks <- datadf %>% 
  #make track. Note you can add additional columns to it
  amt::make_track( .y = latitude, .x = longitude, .t = ts, 
                   #define columns that you want to keep, relabel if you need:
                   id = id, mth = mth, wk = wk,
                   jday = jday, #speed = speed, alt = altitude, 
                   #assign correct crs in lat longs (WGS84)
                   crs = crsdata )
# remember you need to give it the original CRS first!!!!!
#check
head(trks)

# Reproject to UTM to convert lat lon to easting northing
#because it is an amt object now, we use an amt functions:
trks <- amt::transform_coords( trks, crs_to = crstracks )
#note that we are still using the study area crs 

#check
head(trks)

#Turn into a tibble list by grouping and nest by individual IDs so that 
# we can use map function for faster processing

trks.tib <- trks %>%  amt::nest( data = -"id" )
#view
trks; trks.tib

# Remember we have multiple types of data including detailed data for flights #
# We plot overall paths for each individual:
for( i in 1:dim(trks.tib)[1]){
  a <- as_sf_points( trks.tib$data[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', trks.tib$id[i]) ) +
    geom_sf(data = Stopover_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
} 

# Here we rely on stopover polygon, removing records that exist East of the #
# site. We can extra the extent of a polygon:
sf::st_bbox( Stopover_Shape )

#Then use the Eastern-most and Western-most coordinates to filter out data 
xmax <- as.numeric(st_bbox(Stopover_Shape)$xmax) #627081.5
xmin <- as.numeric(st_bbox(Stopover_Shape)$xmin)
#Then use the Northern-most and South-most coordinate to filter out data 
ymax <- as.numeric(st_bbox(Stopover_Shape)$ymax) + 10000 #627081.5
ymin <- as.numeric(st_bbox(Stopover_Shape)$ymin)

#subset those tracks less than as breeding and those > as migrating:
trks.tib <- trks.tib %>% mutate(
  stopover = map( data, ~ filter(., x_ < xmax ) ) )

trks.tib <- trks.tib %>% mutate(
  stopover = map( data, ~ filter(., x_ < xmin ) ) )

trks.tib <- trks.tib %>% mutate(
  stopover = map( stopover, ~ filter(., y_ < ymax ) ) )

trks.tib <- trks.tib %>% mutate(
  stopover = map( stopover, ~ filter(., y_ < ymin ) ) )

#stopover = feb-april
trks.tib <- trks.tib %>% mutate(
  stopover = map( stopover, ~filter(., mth >= 2 ) ),
  stopover = map( stopover, ~filter(., mth <= 4 ) )
  #migrating = map( data, ~filter(., mth > 6 ) ), this is for the future
)

#check 
trks.tib

# Visualization. 
for( i in 1:dim(trks.tib)[1]){
  a <- as_sf_points( trks.tib$stopover[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', trks.tib$id[i]) ) +
    geom_sf(data = Stopover_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
} 

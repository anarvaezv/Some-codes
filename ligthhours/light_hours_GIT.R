#### Code to calculate the sunlingth of every cell in a raster
# based on a DSM (Digital surface model)  giving a particular day of the year and location
# this code does not take into account shadow depth
# The output can be use in different ecological studies,but
# it is also gives a very nice visual representation of the surface
#Notes
# it is wiser to use a wider area than the area of interest due to shadows coming from the neighbouring obstacles

#### loading libraries
library(insol)
library(raster)
library(suncalc)

#### loading requiered data
dsm <- raster("pathtodsm_or_ sampledata:dsm") 

#### Settings####### 

# here the coordinates of the place of interes. Must be within the dsm. lon, lat in decimal degrees 
lonlat<- c(9.18204,48.77431)

# Give the prefered date  as example : "2021-08-25", or leave it as it is to take the current day
date <- Sys.Date() 


#### Preprocessing raster #####
# rescale unit. the ndsm must be in meters, so in this case is use to rescale from cm to m 
dsm<- dsm/100


#### START ###

# formating the geocoordinates
location_geo <- matrix(lonlat, ncol = 2)
# get the sunrise and sunset for the particular date and extract them into time1 and time 2
ImpoPoint<- getSunlightTimes(date = as.Date(date), lat=location_geo[,2],lon=location_geo[,1], keep=c("sunrise", "sunset"), tz="CET")
time1 <- as.POSIXct(ImpoPoint[,4], tz = "Europe/Berlin", origin="1970-01-01") 
time2 <- as.POSIXct(ImpoPoint[,5], tz = "Europe/Berlin", origin="1970-01-01") 


# divide the day hourly from sunrise to sunset
time<-seq(time1,time2, by = "hour")

# get the solar position for time of the day and to the rigth formatting 

solar_pos <- maptools::solarpos(location_geo, time)
solar_pos<- cbind(90-solar_pos[,2], solar_pos[,1])

# Function for calculating the shaded areas and visualizing them 
shade<- function (x){
  sh<-doshade(dsm, normalvector(x[1],x[2]),1)
  plot(sh) # comment the plot if you do not want to see the progression
  return(sh)
}

# applying shade function to every day
shadRe<-apply(solar_pos,1,shade)

# add all the time steps to get the final results  
sunpositives<-calc(stack(shadRe), sum)

plot(sunpositives)



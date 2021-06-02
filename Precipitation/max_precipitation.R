# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  # #
# time series analysis
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  # #

# Spatial identification of the month 
# with the maximum precipitation 

####   load library #############################

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

loadandinstall("raster")
loadandinstall ("rasterVis")


#### Load data #######################################
# load raster with precipitation data one layer for everymonth. here the data are already stacked

prec <- brick("C:/Users/Pirmin/Documents/RGitHub/Some-codes/Precipitation/prec.tif")


#######################################################                 
#  use this part to use only a subset of your data. 
# draw an rectangle in the plot when the red stop bottom appears
######################################################
par(mfrow=c(1,1))
par(mar= (c(5, 4, 4, 2) + 0.1))
plot(prec[[1]])
region<-drawExtent()
prec.r <- crop(prec, region)	# subset prec 

#######################################################                 
####  find the month with the maximum precipitaion per pixel 
######################################################

max.mes<-function(x){
  max.val<-max(x) # max value?
  maxi<-which(x==max.val) # in which month?
  
  if (max.val==0 & length (maxi) >1 ) {  # if 0 precipitation in all months then = 0
    maxi<-0
    
  }else if (max.val!=0 & length (maxi) >1){ # if two months with maximum precipitation then = 13
    
    maxi<-13
    
  }else if (length (maxi)==1){ # assign the number of the month with the maximum precipitation to the cell
    maxi<-maxi
  }else{
    maxi<-NA
  }
  return(as.numeric(maxi))
}


# Apply the function

m.max.prec<-calc(prec.r,max.mes) 


# plot
m.max.prec1f<-as.factor(m.max.prec)
(niveles<-levels(m.max.prec1f)[[1]]) 
(niveles$Mes <- as.character (niveles$ID))
niveles$Mes [nchar(niveles$Mes) == 1] <- paste0(0, niveles$Mes [nchar(niveles$Mes) == 1])
levels(m.max.prec1f)<- niveles
levelplot(m.max.prec1f, col.regions=rev(rainbow(15)), xlab="", ylab="")


 

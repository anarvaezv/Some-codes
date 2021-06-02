##### GET ALL LANdSAT products for temperature
# This code has part of the analyisis of landclsses and its relationship 
# with temperature
# 1. extract the temperature from Landsat ST product for for all polygons. Each 
# polygon represents an entity with a class. There are many polygons with the same class
# 2. maskes grouping in biggger classes. GITplus
# 3. makes boxplot s of the classes vs temperature and saves them 
# 4. does a correlation analysis and saves the result. 
# 5. makes boxplots of the percentages of landcover classes vs temperature and saves them 
# 6. does basic statistics analysis on the results and saved them
# 7. save the most important geodata

### loading the libraries

 library(sp)
 library(rgdal)
 library(raster)
 library(sf)
 library(rgeos)
 library(fasterize)
 library(dplyr)
 library(tidyverse)


#### Genral settings ####
# define the temporal folder for the rasters
rasterOptions()
rasterOptions(tmpdir="E:/R/Temp")

# define working  and putput directory
GITPath<- "D:/Data/01_Processing/Frankfurt/01_Vector/Ecognition subset/classes/correct_bush_trees/mosaic"
setwd(GITPath)

outputdocu<- "D:/Data/01_Processing/Frankfurt/00_documents/LST"

# define the grid size of interest. To fetch the data

resolution<-60 # cambiar 30,60,90

# name the analysis to do
layer.sel<- "2019NFG"


#### load the data

# vector grid with information of green infraastructure classes
Coverture0<- readOGR(paste0("GIT_",layer.sel,resolution,".shp" ))

# list the temperature files raster data of interest
dates<-list.dirs("D:/Data/01_Processing/Frankfurt/02_Raster/Temperatur")
dates<- grep("2019", dates, value=T)

#### START #########


for (i in dates){
  
  # accesing the folder with the temperature information, load the infromation 
  # and extract relevant parts of the path for the naming of files
  setwd(i)
  year<- strsplit(i, "/")[[1]][7]
  fileTemp<-list.files(pattern=paste0("^LST.*",resolution,"_.*.tif$"))
  Coverture<-Coverture0
  LST<-raster(fileTemp)

 
  ## EXTRACTING lst DATA  ######################
  
  # rasteritze for the zonal analysis
  
  Coverture$DID<- 1:nrow(Coverture)
  Coverture_sf<-st_as_sf(Coverture)
  r_Coverture<- fasterize (Coverture_sf,LST,  field = "DID")
  
  
  # extract the values per pixel 
  
  # Do the zonal analysis
  Coverture$meanst<-zonal(LST, r_Coverture, mean)[,"value"]
  
  ### Organize the order of the plots #########
  
  Coverture$GIT<- as.factor(Coverture$GIT)
  Coverture$GIT<- factor(Coverture$GIT, levels= c( "IM1",  "IM2" , "IM3" , "IM4",  "IM4+" ,"IM5" , "IM5+", "IM6" , "IM6+" , 
                                                   "MX2" ,    "MX2+",    "MX3",  "MX4" , "MX5" ,   "MX5+", "MX6" , "MX7",  "MX8",  "MX9" , "MX10" ,
                                                   "PV2" , "PV3" , "PV4" , "PV6" , "PV8" , "PV10", "PV11",
                                                   "AQ1",  "AQ2",  "AQ3",  "AQ4", "AQ4+", "AQ5",  "AQ6" , "AQ7",  "HA" ,  "X" ))
  
  # create the plus classes code
  
  Coverture$GITplus<-Coverture$GIT
  
  Coverture$GITplus[Coverture$GITplus=="MX5"]<- as.factor("MX5+")
  Coverture$GITplus[Coverture$GITplus=="MX2"]<- as.factor("MX2+")
  Coverture$GITplus[Coverture$GITplus=="IM4"]<- as.factor("IM4+")
  Coverture$GITplus[Coverture$GITplus=="IM5"]<- as.factor("IM5+")
  Coverture$GITplus[Coverture$GITplus=="IM6"]<- as.factor("IM6+")
  Coverture$GITplus[Coverture$GITplus=="AQ4"]<- as.factor("AQ4+")
  
  
  
  ######## make bloxplot of the GITs vs the Temperature  #########################
  
  Covertureplus<-Coverture[Coverture$GITplus %in% c("AQ1",  "AQ2",  "AQ3",  "AQ4+", "AQ5",  "AQ6" , "AQ7",  "HA"  , "IM1",  "IM2" , "IM3" , "IM4+", "IM5+" , "IM6+", "MX10", "MX2+" ,
                                                    "MX3",  "MX4" , "MX5+" ,  "MX6" , "MX7",  "MX8",  "MX9" ,"PV10", "PV11", "PV2" , "PV3" , "PV4" , "PV6" , "PV8"  ),]
  
  Covertureplus$GITplus<-droplevels(Covertureplus$GITplus)
  
  #try(dev.off())
  jpeg(file=paste0(outputdocu,"/P",layer.sel,"_LSTvsGIT", year, "_",resolution,".jpeg"), width= 700, height=500)
  
  # boxplot
  
  par(mar=c( 5.1, 4.1 ,4.1 ,2.1))
  bx<-boxplot(meanst ~ GITplus, Covertureplus, las=2, ylab="LST",
              pch=20, cex=.7, outcol="grey", cex.axis=.8)# yaxp=c(25,50,10), ylim= c(23,45), pch=20, cex=.7, outcol="grey", cex.axis=.8)
  abline(v=1: 29, lty = 2, col = "grey65")
  grid()
  bx<-boxplot(meanst ~ GITplus, Covertureplus, las=2, ylab="LST",
              pch=20, cex=.7, outcol="turquoise3", cex.axis=.8, add=T)# ylim= ylim= c(24,50)
  dev.off()
  
  
  #### #1 first do the correlation analysis ######################
  
  
  ### divide by ranges
  
  Coverture$rangesT <-cut(Coverture$X_trees, 10, labels=seq(10, 100, 10))
  Coverture$rangesB <-cut(Coverture$X_bush, 10, labels=seq(10, 100, 10))
  Coverture$rangesG <-cut(Coverture$X_gras, 10, labels=seq(10, 100, 10))
  Coverture$TVegPer<- Coverture$X_bush+ Coverture$X_gras+Coverture$X_trees
  Coverture$rangesTV <-cut(Coverture$TVegPer, 10, labels=seq(10, 100, 10))
  Coverture$rangesBui <-cut(Coverture$X_noveg, 10, labels=seq(10, 100, 10))
  
  
  Cor_spear<-cor(Coverture$meanst,Coverture@data[,c("X_bush", "X_gras", "X_noveg", "X_trees","TVegPer" )], method="spearman", use="complete.obs")
  
  
  #### #2 plot percentage of vegetation class cover vs temperature (boxplot) ############
  
  jpeg(file=paste0(outputdocu,"/P_",layer.sel ,"_4cor", year, "_",resolution,".jpeg"), width= 700, height=500)
  
  par(mfcol=c(2,2))
  par(mar=c(4,4,1,1))
  
  box.stat<-boxplot( meanst ~ rangesT  , data=Coverture, xlab= " % Tree cover in ranges", 
                     ylab= "LST", pch=".")
  grid(lty = 2)
  box.stat<-boxplot( meanst ~ rangesB  , data=Coverture, xlab= " % Bush cover in ranges", 
                     ylab= "LST", pch=".")
  grid(lty = 2)
  box.stat<-boxplot( meanst ~ rangesG  , data=Coverture, xlab= " % Grass cover in ranges", 
                     ylab= "LST", pch=".")
  grid(lty = 2)
  box.stat<-boxplot( meanst ~ rangesBui  , data=Coverture, xlab= " % impervious/bare cover in ranges", 
                     ylab= "LST", pch=".")
  grid(lty = 2)
  
  dev.off()
  
  # get basic statistic data
  
  genStap<-filter(Coverture@data,complete.cases(Coverture@data))%>%
    group_by(GITplus)%>%
    summarize(  median = median(meanst),
                mean= mean(meanst),
                count=length(meanst),
                sdev=sd(meanst,na.rm=T))
  
  genStap$percentage<-prop.table(genStap$count)*100
  
  #Save the results 
  writeOGR(Coverture, ".",paste0("Deffish5",layer.sel,resolution,"_",year), driver="ESRI Shapefile", overwrite_layer = T) #, overwrite_layer = T
  write.csv2(Coverture@data,paste0(outputdocu,"/GIT_LST",layer.sel,resolution,"_",year))
  write.csv2(genStap,paste0(outputdocu,"/lst_GITplus",layer.sel,resolution, "_",  year))
  write.csv2(Cor_spear,paste0(outputdocu,"/correlation",layer.sel,resolution, "_",  year))
  
}

#### Extract and crop#
# Extract landsat C2L2 products of selected bands and 
# crop them to a area of interest overwriting the originally extracted files


library(raster)

# define the temporal folder for the rasters
rasterOptions()
rasterOptions(tmpdir="E:/R/Temp")


# set the working directory. where the landsat data are
setwd("C:/Users/ac133546/AppData/Local/Programs/bda/Bulk Order Stuttgart/Landsat 8 OLI_TIRS C2 L2")
getwd()

####  Extract selected files from the tar

# get a list of the files
lsList<- list.files(pattern =".tar")
# get a shorter version of the name
rfproductname <- substr(lsList, 1, nchar(lsList) - 10)
# do the extraction on selected layers  2,3,4,5 and ST_B10
for(i in 1:length(lsList)){
  files2Extract<-untar(lsList[i],list=T)[c(8:11,17)]# this takes only especific bands of landsat
  untar(lsList[i],files=files2Extract , exdir = rfproductname[i])
}


#### crop the files to Frankfurt extension and save overwrite ###########

Frank<- readOGR("G:/INTERESS/03_Daten/original/Daten_Stuttgart/02_Vector/Stuttgart_Bezirke.shp")

DirList<- list.dirs()

file<- files[1]

for (d in 1:length(DirList)){
  files<-list.files(DirList[d], pattern=".TIF$")
  for (file in files){
    eco<-raster(paste0(DirList[d],"/",file))
    eco_F<- crop (eco, Frank)
    writeRaster (eco_F, paste0(DirList[d],"/",file),  format = 'GTiff', datatype = 'INT2U', overwrite=T ) 
  }
}


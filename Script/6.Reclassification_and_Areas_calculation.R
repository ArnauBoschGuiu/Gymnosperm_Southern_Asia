# ==============================================================================
# Load libraries
# ==============================================================================

library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(geosphere)
library(dplyr)


# ==============================================================================
# Set output directory
# ==============================================================================

main.directory <- "Rdata/Gymnos/models"
setwd(main.directory)

# ==============================================================================
# Area calculation of each model and each species
# ==============================================================================


sps <- list.files("/present")
sps <- sps[-1]


models <- list.files("/models")
models <- models[1:13]

## ________Model current____________________
## source: https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/

for (i in sps) {

# Define the output file names
input_present <- paste("/present/",i, sep = "")

# Open the output raster from Maxent in ASCII format
present <- raster (input_present)

# assign all pixels that equal 0 to NA or no data value
present[present == 0] <- NA

#get sizes of all cells in current distribution raster
#note my original layers were 30 seconds or 1 km2. 
cell_size<-area(present, na.rm=TRUE, weights=FALSE)

#delete NAs from all raster cells. It looks like these come back when switching from dataframe to raster
cell_size1<-cell_size[!is.na(cell_size)]

#compute area [km2] of all cells in raster
skm_present<-length(cell_size1)*median(cell_size1)
skm_present


## ________Create Table with values_____________

table_values<-data.frame(Model="Present",
              Predicted_area=skm_present,
              Difference=0,
              Overlap=0,
              Lost_area=0,
              Gained_area=0)


## ________Calculate Future/past area_____________

#loop of all the models for one species

for(j in models) {

  
input_model <- paste("/models/",j,"/",i, sep = "")
model <- raster (input_model)
model[model == 0] <- NA
cell_size<-area(model, na.rm=TRUE, weights=FALSE)
cell_size1<-cell_size[!is.na(cell_size)]
skm_model<-length(cell_size1)*median(cell_size1)
skm_model  

if (length(skm_model)==0) {
 
dif_area <- 0 - skm_present
dif_area

z<-data.frame(Model=j,
              Predicted_area=0,
              Difference=dif_area,
              Overlap=0,
              Lost_area=dif_area,
              Gained_area=0)

}
 
else  {
  
dif_area <- skm_model - skm_present
dif_area

mi <- overlay(present, model, fun=function(x,y){return(x*y)})
mi.df <- as.data.frame(mi, xy=TRUE)
names(mi.df)[3] <- "valor"
mi.df1 <- na.omit(mi.df)

if (dim(mi.df1)[1]==0) {
  
  z<-data.frame(Model=j,
                Predicted_area=skm_model,
                Difference=dif_area,
                Overlap=0,
                Lost_area=dif_area,
                Gained_area=skm_model)  
  
}

else
{

mi.df2 <- mi.df1[mi.df1$valor> 0,]
mi.raster <-rasterFromXYZ(mi.df2)
cell_size<-area(mi.raster, na.rm=TRUE, weights=FALSE)
cell_size1<-cell_size[!is.na(cell_size)]
skm_intersect<-length(cell_size1)*median(cell_size1)
skm_intersect

##calculate change in area
lost_area <- skm_present - skm_intersect
lost_area

gain_area <- skm_model - skm_intersect
gain_area


#include the results in the table

z<-data.frame(Model=j,
              Predicted_area=skm_model,
              Difference=dif_area,
              Overlap=skm_intersect,
              Lost_area=lost_area,
              Gained_area=gain_area)
}
}

table_values<-rbind(table_values,z) 


rm(lost_area)
rm(gain_area)
rm(skm_intersect)
rm(skm_model)
rm(z)
rm(model)
rm(mi.raster)
rm(mi.df)
rm(mi.df1)
rm(mi.df2)
rm(mi)
rm(cell_size)
rm(cell_size1)

}

View(table_values)

#save the table with all area calculations

output_file <- paste("table_values_",i,".txt", sep = "") #change if necessary

write.table(table_values, output_file, sep = "\t", quote = F, row.names = F)

rm(table_values)
rm(skm_present)
rm(present)
rm(cell_size)
rm(cell_size1)

}


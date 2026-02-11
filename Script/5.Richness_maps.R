# ==============================================================================
# Load libraries
# ==============================================================================

library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(geosphere)
library(dplyr)


# ==============================================================================
# Set output directory
# ==============================================================================
setwd("/Rdata/Gymnos")


# ==============================================================================
# Overlap of all original models for each species
# ==============================================================================

#list of models
models <- c("lgm_sh_cc","lgm_sh_mr",
            "lgm_sh_me","mh_sh_cc",
            "mh_sh_mr","mh_sh_me",
            "GFDL_ESM4_ssp126_2081_2100",
            "GFDL_ESM4_ssp370_2081_2100",
            "IPSL_CM6A_LR_ssp126_2081_2100",
            "IPSL_CM6A_LR_ssp370_2081_2100",
            "MRI_ESM2_0_ssp126_2081_2100",
            "MRI_ESM2_0_ssp370_2081_2100",
            "lig","present")

for(i in models) {

#list of raster files
  
file1 <- paste("./models/",i,"/",sep = "") #change if necessary
listado = list.files(file1, pattern = "asc", full.names = T) 
datos = lapply(listado, FUN=raster)

#combination of all rasters

datos_raster_stack = stack(datos)

#Sum

options(scipen = 999)
suma_raster = sum(datos_raster_stack)
plot(suma_raster)

#Save file

file2 <- paste("./suma/species_richness_",i,".asc",sep = "") #change if necessary
writeRaster(suma_raster, file2)

plot(suma_raster)

rm(file1)
rm(file2)
rm(listado)
rm(datos)
rm(datos_raster_stack)
rm(suma_raster)

}

# ==============================================================================
# Overlap of time periods and scenarios for each species
# ==============================================================================

#_______________________________________________________________________________
#FUTURE SSP126 SCENARIO (THREE MODELS)

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species

for(i in sps) {
  
main.directory <- "/models"
setwd(main.directory)

model1 = raster(paste("./MRI_ESM2_0_ssp126_2081_2100/",i,".asc", sep = "") )
model2 = raster(paste("./IPSL_CM6A_LR_ssp126_2081_2100/",i,".asc", sep = "") )
model3 = raster(paste("./GFDL_ESM4_ssp126_2081_2100/",i,".asc", sep = "") )

datos_raster_stack = stack(model1,model2,model3)

intersect_raster = overlay(datos_raster_stack, fun=min)

outfile_name_model <- paste("./_futureSSP126/",i, ".asc", sep = "") 
writeRaster(intersect_raster, outfile_name_model)

plot(intersect_raster)

}


#_______________________________________________________________________________
#FUTURE SSP370 SCENARIO (THREE MODELS)

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  
  main.directory <- "/models"
  setwd(main.directory)
  
  model1 = raster(paste("./MRI_ESM2_0_ssp370_2081_2100/",i,".asc", sep = "") )
  model2 = raster(paste("./IPSL_CM6A_LR_ssp370_2081_2100/",i,".asc", sep = "") )
  model3 = raster(paste("./GFDL_ESM4_ssp370_2081_2100/",i,".asc", sep = "") )

  datos_raster_stack = stack(model1,model2,model3)

  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./_futureSSP370/",i, ".asc", sep = "") 
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}

#_______________________________________________________________________________
#PAST SCENARIOS (THREE MODELS)

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species

for(i in sps) {
  
  main.directory <- "/models"
  setwd(main.directory)
  
  model1 = raster(paste("./mh_sh_me/",i,".asc", sep = "") )
  model2 = raster(paste("./mh_sh_mr/",i,".asc", sep = "") )
  model3 = raster(paste("./mh_sh_cc/",i,".asc", sep = "") )

  datos_raster_stack = stack(model1,model2,model3)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./_mh/",i, ".asc", sep = "") #cambiar el escenario cuando termine  
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}

#_______________________________________________________________________________
#PAST SCENARIOS (THREE MODELS)

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  
  main.directory <- "/models"
  setwd(main.directory)
  
  model1 = raster(paste("./lgm_sh_me/",i,".asc", sep = "") )
  model2 = raster(paste("./lgm_sh_mr/",i,".asc", sep = "") )
  model3 = raster(paste("./lgm_sh_cc/",i,".asc", sep = "") )
  
  datos_raster_stack = stack(model1,model2,model3)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./_lgm/",i, ".asc", sep = "") 
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}

#_______________________________________________________________________________
#PRESENT AND PAST SCENARIOS

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  
  main.directory <- "C:/Rdata/Gymnos/models"
  setwd(main.directory)
  

    model1 = raster(paste("./present/",i,".asc", sep = ""))
    model2 = raster(paste("./lig/",i,".asc", sep = ""))
    model3 = raster(paste("./_lgm/",i,".asc", sep = ""))
    model4 = raster(paste("./_mh/",i,".asc", sep = ""))
  
  datos_raster_stack = stack(model1,model2,model3,model4)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./__present-past/",i, ".asc", sep = "") 
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}


#_______________________________________________________________________________
#PRESENT AND FUTURE SSP126 SCENARIOS 

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species

models <- c("present","_futureSSP126")

for(i in sps) {
  
  main.directory <- "C:/Rdata/Gymnos/models"
  setwd(main.directory)
  
    model1 = raster(paste("./present/",i,".asc", sep = ""))
    model2 = raster(paste("./_futureSSP126/",i,".asc", sep = ""))

  datos_raster_stack = stack(model1,model2)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./__present-future126/",i, ".asc", sep = "") 
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}


#_______________________________________________________________________________
#PRESENT AND FUTURE SSP370 SCENARIOS

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  
  #Hacemos un listado de los archivos raster.
  main.directory <- "C:/Rdata/Gymnos/models"
  setwd(main.directory)

    model1 = raster(paste("./present/",i,".asc", sep = ""))
    model2 = raster(paste("./_futureSSP370/",i,".asc", sep = ""))
    
  datos_raster_stack = stack(model1,model2)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)

  outfile_name_model <- paste("./__present-future370/",i, ".asc", sep = "") 
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}


#_______________________________________________________________________________
#PRESENT, PAST AND FUTURE SSP126 SCENARIO

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  
  main.directory <- "C:/Rdata/Gymnos/models"
  setwd(main.directory)

    model1 = raster(paste("./__present-past/",i,".asc", sep = ""))
    model2 = raster(paste("./__present-future126/",i,".asc", sep = ""))

  datos_raster_stack = stack(model1,model2)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./___present-past-future126/",i, ".asc", sep = "")  
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}

#_______________________________________________________________________________
#PRESENT, PAST AND FUTURE SSP370 SCENARIO

# Set working directory
main.directory <- "/sps"
setwd(main.directory)
sps <- dir()


#Loop for each species
for(i in sps) {
  

  main.directory <- "C:/Rdata/Gymnos/models"
  setwd(main.directory)
  
    model1 = raster(paste("./__present-past/",i,".asc", sep = ""))
    model2 = raster(paste("./__present-future370/",i,".asc", sep = ""))

  datos_raster_stack = stack(model1,model2)
  
  intersect_raster = overlay(datos_raster_stack, fun=min)
  
  outfile_name_model <- paste("./___present-past-future370/",i, ".asc", sep = "") #cambiar el escenario cuando termine  
  writeRaster(intersect_raster, outfile_name_model)
  
  plot(intersect_raster)
  
}


# ==============================================================================
# Richness maps
# ==============================================================================


#Models list
models = list.files("./", pattern = "^_", full.names = T) #usamos el pattern para que solo escoga los archivos tif y fullnames para que tome toda la direcci?n de donde se encuentran los archivos.
models
models <- models[-1]

models <- c("./___present-past-future370", "./__present-future370", "./__present-future126", "./__present-past")

for(i in models) {
  
  listado = list.files(i, pattern = "asc", full.names = T) 
  
  datos = lapply(listado, FUN=raster)
  
  datos_raster_stack = stack(datos)
  
  options(scipen = 999)
  suma_raster = sum(datos_raster_stack)
  plot(suma_raster)
  
  model <- sub('./','',i)
  file2 <- paste("C:/Rdata/Gymnos/suma/species_richness_",model,".asc",sep = "") 
  writeRaster(suma_raster, file2)
  
  plot(suma_raster)
  
  rm(file2)
  rm(listado)
  rm(datos)
  rm(datos_raster_stack)
  rm(suma_raster)
  
}

# ==============================================================================
# Reclassification of the richness maps for area calculation
# ==============================================================================


main.directory <- "/suma"
setwd(main.directory)

#Maps list

mapas <- list.files()
mapas <- mapas[-1]  
  
#Loop for area of each species

for(i in mapas) {

  main.directory <- "/suma"
  setwd(main.directory)  

#load rasters

mapa <- raster(i)
plot(mapa)

#Reclassification matrices

reclass_1.73   <- matrix(c(0, 73, 1)
                , ncol = 3
                , byrow = TRUE)
reclass_1.15    <- matrix(c(0, 15, 1,
                15, 73, 0)
                , ncol = 3
                , byrow = TRUE)
reclass_16.26    <- matrix(c(0, 15, 0,
                15, 26, 1,
                26, 73, 0)
                , ncol = 3
                , byrow = TRUE)
reclass_27.39    <- matrix(c(0, 26, 0,
                26, 39, 1,
                39, 73, 0)
                , ncol = 3
                , byrow = TRUE)
reclass_40.51  <- matrix(c(0, 39, 0,
                39, 51, 1,
                51, 73, 0)
                , ncol = 3
                , byrow = TRUE)
reclass_52.73  <- matrix(c(0, 51, 0,
                51, 73, 1)
                , ncol = 3
                , byrow = TRUE)

#New reclassification map
mapa_reclass_1.73 <- reclassify(mapa, reclass_1.73)
plot(mapa_reclass_1.73)
mapa_reclass_1.15 <- reclassify(mapa, reclass_1.15)
plot(mapa_reclass_1.15)
mapa_reclass_16.26 <- reclassify(mapa, reclass_16.26)
plot(mapa_reclass_16.26)
mapa_reclass_27.39 <- reclassify(mapa, reclass_27.39)
plot(mapa_reclass_27.39)
mapa_reclass_40.51 <- reclassify(mapa, reclass_40.51)
plot(mapa_reclass_40.51)
mapa_reclass_52.73 <- reclassify(mapa, reclass_52.73)
plot(mapa_reclass_52.73)


#Save file
setwd("/Reclass")
dir.create(i)
file_name_carpeta <- paste("/Reclass/",i, sep = "") 
setwd(file_name_carpeta)

writeRaster(mapa_reclass_1.73, "./species_richness_1-73.asc")
writeRaster(mapa_reclass_1.15, "./species_richness_1-15.asc")
writeRaster(mapa_reclass_16.26, "./species_richness_16-26.asc")
writeRaster(mapa_reclass_27.39, "./species_richness_27.39.asc")
writeRaster(mapa_reclass_40.51, "./species_richness_40-51.asc")
writeRaster(mapa_reclass_52.73, "./species_richness_52-73.asc")


}


# ==============================================================================
# Areas calculation
# ==============================================================================

main.directory <- "/Reclass"
setwd(main.directory)

#Maps list

mapas <- list.files()
mapas
mapas <- mapas[3:14]

for (j in mapas) {

file_name <- paste("/Reclass/",j, sep = "") 

setwd(file_name)

list <- c(
        "1-73.asc",
        "1-15.asc",
        "16-26.asc",
        "27.39.asc",
        "40-51.asc",
        "52-73.asc")

#Create Table with values

table_values<-data.frame(Model="xx",
                         Total=0
)

for (i in list) {
  
# Input file
input_file <- paste("./species_richness_",i, sep = "")

# Define the projection of the polygon
selected_projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Open the output raster from Maxent in ASCII format
reclass_binary_ras <- raster(input_file)

# assign all pixels that equal 0 to NA or no data value
reclass_binary_ras[reclass_binary_ras == 0] <- NA

#get sizes of all cells in current distribution raster
#note my original layers were 30 seconds or 1 km2. 
cell_size<-area(reclass_binary_ras, na.rm=TRUE, weights=FALSE)

#delete NAs from all raster cells. It looks like these come back when switching from dataframe to raster
cell_size1<-cell_size[!is.na(cell_size)]

#compute area [km2] of all cells in raster
total_skm<-length(cell_size1)*median(cell_size1)
total_skm

if (length(total_skm)==0){
  z<-data.frame(Model=i,
                Total=0              )}

else {

z<-data.frame(Model=i,
              Total=total_skm              )}


table_values<-rbind(table_values,z)

rm(z)
rm(reclass_binary_ras)
rm(total_skm)

}

output_file <- paste("table_values_",j,".txt", sep = "") #change if necessary

write.table(table_values, output_file, sep = "\t", quote = F, row.names = F)
}

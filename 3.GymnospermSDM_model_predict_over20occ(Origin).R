# ==============================================================================
# Load libraries
# ==============================================================================
{
{
  library(raster)
  library(rgdal)
  library(ENMeval)
  library(maxnet)
  library(ggplot2)
  library(rasterVis) 
  library(tidyverse)
  library(dplyr)
  library(gt)
  library(dismo)
  library(flexsdm)
  library(fuzzySim)
  library(ggspatial)
  library(SDMtune)
  library(blockCV)
  library(zeallot)
  library(marmap)
  library(ggspatial)
  library(grid)
  library(ecospat)
  library(gridExtra)
}

# ==============================================================================
# Set output directory
# ==============================================================================
newdir = "/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/result20230123"
# dir.create(newdir)
setwd(newdir)

# ==============================================================================
# Study area & seed settings
# ==============================================================================
ext = c(50, 180, 0, 90)
seed = 186546
set.seed(seed)

# ==============================================================================
# Environmental Data Processing
# ==============================================================================
#Worldclim 1.4 bioclim of present
env = list.files("/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/WorldClim1.4/bio_2-5m_bil", pattern = '\\.bil$', full.names = TRUE) %>%
  stack() %>%
  crop(., ext)
names(env) <- gsub('wc2.1_2.5m_bio_', 'bio', names(env))

# ==============================================================================
# Unit Adjustment (Scaling Bioclim Variables)
# ==============================================================================
for(y in c(1,2,5,6,7,8,9,10,11)){
  print(y)
  eval(parse(text=paste0("env$bio",y," = (env$bio",y,"/10)")))
}

for(j in c(3,4)){
  print(j)
  eval(parse(text=paste0("env$bio",j," = (env$bio",j,"/100)")))
}

# ==============================================================================
# LGM bioclim data processing
# ==============================================================================
{
  lgm_land = raster("/Users/gyoiko/Desktop/AisanConifer_dataset/high_longlat_lgm_NA.tif") %>%
    crop(., ext) %>%
    resample(., env)

  for(d in c("cc","mr","me")){
    print(d)
    eval(parse(text=paste0("bio_lgm_",d," = list.files('/Users/gyoiko/Desktop/AisanConifer_dataset/WorldClim1.4/",d,"lgm',
  pattern = '.tif', full.names = TRUE)")))
    eval(parse(text=paste0("bio_lgm_",d," = stack(bio_lgm_",d,") %>%
  raster::crop(., ext)%>%
  raster::mask(., lgm_land)")))
    eval(parse(text=paste0("names(bio_lgm_",d,") = gsub('",d,"lgmbi', 'bio', names(bio_lgm_",d,"))")))
  }

  for(d in c("cc","mr","me")){
    for(y in c(1,2,5,6,7,8,9,10,11)){
      print(d)
      print(y)
      eval(parse(text=paste0("bio_lgm_",d,"$bio",y," = (bio_lgm_",d,"$bio",y,"/10)")))
    }}

  for(d in c("cc","mr","me")){
    for(j in c(3,4)){
      print(d)
      print(j)
      eval(parse(text=paste0("bio_lgm_",d,"$bio",j," = (bio_lgm_",d,"$bio",j,"/100)")))
    }}

  for(d in c("cc","mr","me")){
    print(d)
    eval(parse(text=paste0("env_lgm_",d," = stack(bio_lgm_",d,")")))
  }
}

# ==============================================================================
# Mid-Holocene bioclim data processing
# ==============================================================================
{
  for(d in c("cc","mr","me")){
    print(d)
    eval(parse(text=paste0("bio_mh_",d," = list.files('/Users/gyoiko/Desktop/AisanConifer_dataset/WorldClim1.4/",d,"midbi_2-5m',
  pattern = '.tif', full.names = TRUE)")))
    eval(parse(text=paste0("bio_mh_",d," = stack(bio_mh_",d,") %>%
  raster::crop(., ext)")))
    eval(parse(text=paste0("names(bio_mh_",d,") = gsub('",d,"midbi', 'bio', names(bio_mh_",d,"))")))
  }
  for(d in c("cc","mr","me")){
    for(y in c(1,2,5,6,7,8,9,10,11)){
      print(d)
      print(y)
      eval(parse(text=paste0("bio_mh_",d,"$bio",y," = (bio_mh_",d,"$bio",y,"/10)")))
    }}

  for(d in c("cc","mr","me")){
    for(j in c(3,4)){
      print(d)
      print(j)
      eval(parse(text=paste0("bio_mh_",d,"$bio",j," = (bio_mh_",d,"$bio",j,"/100)")))
    }}

  for(d in c("cc","mr","me")){
    print(d)
    eval(parse(text=paste0("env_mh_",d," = stack(bio_mh_",d,")")))
  }
}


# ==============================================================================
# Future bioclim (CMIP6) data processing
# ==============================================================================
scenario = c("GFDL_ESM4", "IPSL_CM6A_LR", "MRI_ESM2_0")
rcp = c(126, 370)
times = c("2081_2100")

for(d in scenario){
  for(r in rcp){
    for(t in times){
      print(d)
      print(r)
      print(t)
      eval(parse(text=paste0("bio_",d,"_",r,"_",t," = list.files('/Users/gyoiko/Desktop/Gymnosperm_wc2-5_future_cmip6/',
    pattern = '",d,"_ssp",r,"_",t,".tif', full.names = TRUE)")))
      eval(parse(text=paste0("bio_",d,"_",r,"_",t," = stack(bio_",d,"_",r,"_",t,") %>% 
    raster::crop(., ext)"))) 
      eval(parse(text=paste0("names(bio_",d,"_",r,"_",t,") = gsub('wc2_', 'bio', names(bio_",d,"_",r,"_",t,"))")))
    }}}


# ==============================================================================
# LIG bioclim data processing
# ==============================================================================
env_lig = list.files("/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/lig_30s_bio", pattern = '\\.bil$', full.names = TRUE) %>%
  stack() %>%
  resample(., env$bio1)%>% 
  crop(., ext) 
names(env_lig) <- gsub('lig_30s_bio_', 'bio', names(env_lig))


for(y in c(1,2,5,6,7,8,9,10,11)){
  print(y)
  eval(parse(text=paste0("env_lig$bio",y," = (env_lig$bio",y,"/10)")))
}

for(j in c(3,4)){
  print(j)
  eval(parse(text=paste0("env_lig$bio",j," = (env_lig$bio",j,"/100)")))
}

# ==============================================================================
# Species Occurrence Data Preparation
# ==============================================================================
g = openxlsx::read.xlsx("/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/EastAsianGymnosperms_record20230130_revised.xlsx")
g = g[!duplicated(g[,c(1,3)]),]
g$occ = 1
g = g[g$Error == 0, ] # remove error points 
colnames(g)[c(2:3)] = c("x", "y")
g = mutate(g, Species = gsub(Species, pattern = " ", replacement = "_", ignore.case = T))

sp_list = g %>% distinct(g[, c("Species")]) 
sp_list = sp_list$`g[, c("Species")]`

# out put of species over 20 locations 
g_list = g %>% group_by(Species, occ) %>%
  summarize(occ_n=sum(occ))
g_list = g_list[, c("Species", "occ_n")]
g_list_sel = g_list[g_list$occ_n >= 20,]
g_list_sel = g_list_sel[g_list_sel$occ_n >= 20,]

g_data = left_join(g, g_list, by = "Species")
g_data_sel = g_data[g_data$occ_n >= 20, ]
g_data_sel = g_data_sel[g_data_sel$occ_n >= 20, ]
g_data_sel = g_data_sel %>% rowid_to_column()

# ==============================================================================
# Sampling Bias File
# ==============================================================================
xext = raster("/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/EastAsiaBias_20230130.asc")
EAbias <- xext
values(EAbias) <- values(xext)/nrow(g_data)
values(EAbias)[values(EAbias)==0] <- NA

specieslist = g_list_sel$Species

# #Remove species (Picea_maximowiczii), Total 152 species with over 20 occurrnce
specieslist
specieslist = specieslist[!c(specieslist == "Picea_maximowiczii")]


{
# ==============================================================================
# Setting output Folder
# ==============================================================================
newdir2 = "/Volumes/Extreme SSD/GymnospermSDM/AisanConifer_dataset/result2023_revised_CMIP6"
dir.create(newdir2)
setwd(newdir2)

dir.create("modelresult")
dir.create("Projection")

fclist = openxlsx::read.xlsx("/Volumes/Extreme SSD/GymnospermSDM/fclist.xlsx")
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

# ==============================================================================
# Project of suitable habitats based on optimized model using dismo package
# ==============================================================================

for(names in specieslist[1:length(specieslist)]){
  print(names)
  eval(parse(text=paste0("dir.create('modelresult/",names,"')")))
  eval(parse(text=paste0("xy = as.data.frame(g_data_sel[g_data_sel$Species == '",names,"', c('rowid', 'Species', 'x', 'y'), ])")))
  eval(parse(text=paste0("a <- openxlsx::read.xlsx('",newdir,"/",names,"/",names,"_abs.xlsx')")))
  eval(parse(text=paste0("write_csv(a,'modelresult/",names,"/",names,"_abs.csv')")))
  eval(parse(text=paste0("a = read.csv('",newdir2,"/modelresult/",names,"/",names,"_abs.csv')")))
  
  ###Enviromental filter
  sp_filt = occfilt_env(
    data = xy,
    x = "x",
    y = "y",
    id = "rowid",
    env_layer = terra::rast(env),
    nbins = 25
  )
  eval(parse(text=paste0("write_csv(xy[, c('x', 'y')],'modelresult/",names,"/",names,"_occ.csv')")))
  eval(parse(text=paste0("write_csv(sp_filt[, c('x', 'y')],'modelresult/",names,"/",names,"_filt_occ.csv')")))
  eval(parse(text=paste0("sp_filt = read.csv('",newdir2,"/modelresult/",names,"/",names,"_filt_occ.csv')")))

eval(parse(text=paste0("var = openxlsx::read.xlsx('",newdir,"/",names,"/",names,"_varImp.xlsx')")))
var = var[, c("Variable")]

env_sel = stack()
for(r in var){
  eval(parse(text=paste0("env_sel = stack(env_sel, env$'",r,"')")))
}

#Final model construction
eval(parse(text=paste0("data = prepareSWD(species = '",names,"', 
                           sp_filt[, c('x','y')], a = a, 
                           env = env_sel)")))

eval(parse(text=paste0("score = openxlsx::read.xlsx('",newdir,"/",names,"/",names,"_score.xlsx')")))
rm = score[4,2]
fc = score[5,2]

fold <- kfold(sp_filt, k=5)
occtest <- sp_filt[fold == 1, ]
occtrain <- sp_filt[fold != 1, ]
occtrain = occtrain[,c('x','y')]
occtrain
fc_code = fclist[fclist$fc == fc ,]

eval(parse(text=paste0("me = dismo::maxent(env_sel, p = as.data.frame(occtrain), a = a, path='",newdir2,"/modelresult/",names,"',
  args=c('responsecurves=TRUE','pictures=TRUE','jackknife=TRUE','outputformat=logistic',
  'outputfiletype=asc',
  'betamultiplier=",rm,"','replicates=1','randomseed=FALSE','",fc_code[2],"', '",fc_code[3],"',
                         '",fc_code[4],"', '",fc_code[5],"', '",fc_code[6],"'))")))

#Predict present suitable habitat
eval(parse(text=paste0("
sh = dismo::predict(me, env_sel, type ='logistic', file = 'modelresult/",names,"/",names,"_present_sh.asc', overwrite=TRUE)
                       ")))
sh = dismo::predict(me, env_sel, type ='logistic')

eval(parse(text=paste0("
sh = writeRaster(sh, file = 'Projection/",names,"_present_sh.asc', overwrite=TRUE)
                       ")))


#Predict LGM suitable habitat
for(d in c("cc","mr","me")){
  eval(parse(text=paste0("pred_lgm",d," = stack()")))
}

for(d in c("cc","mr","me")){
  for(p in var){
    eval(parse(text=paste0("pred_lgm",d," = stack(pred_lgm",d,", env_lgm_",d,"$'",p,"')")))
  }}

for(d in c("cc","mr","me")){
  eval(parse(text=paste0("sh_lgm_",d," = dismo::predict(me, pred_lgm",d,", type='logistic', file = 'modelresult/",names,"/",names,"_lgm_sh_",d,".asc', overwrite=TRUE)")))
  # eval(parse(text=paste0("sh_lgm_",d," = mean(sh_lgm_",d,")")))
  eval(parse(text=paste0("sh_lgm_",d," = writeRaster(sh_lgm_",d,", file = 'Projection/",names,"_lgm_sh_",d,".asc', overwrite=TRUE)")))
  }


#Predict mid-Holocene suitable habitat
for(d in c("cc","mr","me")){
  eval(parse(text=paste0("pred_mh",d," = stack()")))
}

for(d in c("cc","mr","me")){
  for(p in var){
    eval(parse(text=paste0("pred_mh",d," = stack(pred_mh",d,", env_mh_",d,"$'",p,"')")))
  }}

for(d in c("cc","mr","me")){
  eval(parse(text=paste0("sh_mh_",d," = dismo::predict(me, pred_mh",d,", type='logistic', file = 'modelresult/",names,"/",names,"_mh_sh_",d,".asc', overwrite=TRUE)")))
  eval(parse(text=paste0("sh_mh_",d," = writeRaster(sh_mh_",d,", file = 'Projection/",names,"_mh_sh_",d,".asc', overwrite=TRUE)")))
}

eval(parse(text=paste0("maxentResults <- read.csv('modelresult/",names,"/maxentResults.csv')")))
auc <- maxentResults$Training.AUC %>%
  as.data.frame(.)%>%
  round(., 3)
colnames(auc) = "Train AUC"

e1 <- evaluate(me, p=as.data.frame(occtest[,c("x", "y")]), a=a, x=env_sel)
e1 = e1@auc %>%
  as.data.frame() %>%
  round(., 3)
colnames(e1) = "Test AUC"

cbi = ecospat::ecospat.boyce(fit = sh, sp_filt[, c('x', 'y')], PEplot = TRUE, res = 100,
                             rm.duplicate = TRUE, method = 'spearman')
cbi = cbi$cor %>%
  as.data.frame() %>%
  round(., 3)
colnames(cbi) = "CBI"

sp_name = g_list_sel[g_list_sel$Species == names, ]
colnames(sp_name) = c("Species", "Occurrence")
sp_name = mutate(sp_name, Species = gsub(Species, pattern = "_", replacement = " ", ignore.case = T))

final_score = cbind(sp_name, auc) %>%
  cbind(., e1) %>%
  cbind(., cbi)
eval(parse(text=paste0("write.csv(final_score, 'modelresult/",names,"/",names,"_eval.csv')")))

#Predict LIG suitable habitat
pred_lig = stack()
for(r in var){
  eval(parse(text=paste0("pred_lig = stack(pred_lig, env_lig$'",r,"')")))
}

eval(parse(text=paste0("sh_lig = dismo::predict(me, pred_lig, type='logistic', file = 'modelresult/",names,"/",names,"_lig.asc', overwrite=TRUE)")))
eval(parse(text=paste0("sh_lig = writeRaster(sh_lig, file = 'Projection/",names,"_lig.asc', overwrite=TRUE)")))

#Predict Future suitable habitat
for(d in scenario){
  for(r in rcp){
    for(t in times){
  eval(parse(text=paste0("pred_",d,"_",r,"_",t," = stack()")))
}}}

for(d in scenario){
  for(r in rcp){
    for(t in times){
      for(p in var){
    eval(parse(text=paste0("pred_",d,"_",r,"_",t," = stack(pred_",d,"_",r,"_",t,", bio_",d,"_",r,"_",t,"$'",p,"')")))
  }}}}

for(d in scenario){
  for(r in rcp){
    for(t in times){
  eval(parse(text=paste0("sh_",d,"_",r,"_",t," = dismo::predict(me, pred_",d,"_",r,"_",t,", type='logistic', file = 'modelresult/",names,"/",names,"_",d,"_ssp",r,"_",t,".asc', overwrite=TRUE)")))
  eval(parse(text=paste0("sh_",d,"_",r,"_",t," = writeRaster(sh_",d,"_",r,"_",t,", file = 'Projection/",names,"_",d,"_ssp",r,"_",t,".asc', overwrite=TRUE)")))
    }}}
}
}
}

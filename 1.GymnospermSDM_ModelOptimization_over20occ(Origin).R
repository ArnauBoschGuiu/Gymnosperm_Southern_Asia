
# ==============================================================================
# Load libraries
# ==============================================================================
{
  library(raster)
  library(rgdal)
  library(ENMeval)
  library(dplyr)
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
newdir = "/Users/Desktop/EastAsianConifers/result20230123"
dir.create(newdir)
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
env = list.files("/Volumes/Extreme SSD/WorldClim1.4/bio_2-5m_bil", pattern = '\\.bil$', full.names = TRUE) %>%
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

#Worldclim 1.4 bioclim of LGM
{
lgm_land = raster("/Volumes/Extreme SSD/high_longlat_lgm_NA.tif") %>% 
  crop(., ext) %>% 
  resample(., env)

for(d in c("cc","mr","me")){
  print(d)
  eval(parse(text=paste0("bio_lgm_",d," = list.files('/Volumes/Extreme SSD/WorldClim_data/",d,"lgm', 
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

#Worldclim 1.4 bioclim of mid-holocene
{
  for(d in c("cc","mr","me")){
    print(d)
    eval(parse(text=paste0("bio_mh_",d," = list.files('/Volumes/Extreme SSD/WorldClim_data/",d,"midbi_2-5m', 
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
# Species Occurrence Data Preparation
# ==============================================================================
g = openxlsx::read.xlsx("/Users/Desktop/EastAsianConifers/EastAsianGymnosperms_record20230130_revised.xlsx") %>% 
  na.omit
g = g[!duplicated(g[,c(1,3)]),]
g$occ = 1
g = g[g$Error == 0, ]
colnames(g)[c(2:3)] = c("x", "y")
g = mutate(g, Species = gsub(Species, pattern = " ", replacement = "_", ignore.case = T))

sp_list = g %>% distinct(g[, c("Species")]) 
sp_list = sp_list$`g[, c("Species")]`


g_list = g %>% group_by(Species, occ) %>%
  summarize(occ_n=sum(occ))
g_list = g_list[, c("Species", "occ_n")]
g_list_sel = g_list[g_list$occ_n >= 20,]

g_data = left_join(g, g_list, by = "Species") 
g_data_sel = g_data[g_data$occ_n >= 20,]
g_data_sel = g_data_sel %>% rowid_to_column()


# ==============================================================================
# Create Sampling Bias File
# ==============================================================================
mx <- matrix(0,4320,8640)
j <- ceiling((90-g_data$y)*24)
i <- ceiling((g_data$x+180)*24)

df <- data.frame(table(i,j))
df2 <- df[df$Freq > 0,]

for(k in 1:nrow(df2)){
  jj <- as.numeric(as.character(df2$j[k]))
  ii <- as.numeric(as.character(df2$i[k]))
  mx[jj,ii] <- df2$Freq[k]
  }

x <- raster()
res(x) <- 1/24

values(x) <- mx
xext <- crop(x,ext)

EAbias <- xext
values(EAbias) <- values(xext)/nrow(g_data_sel)
values(EAbias)[values(EAbias)==0] <- NA
# writeRaster(xext,file='EastAsiaBias_20230130.asc',format='ascii',overwrite=TRUE)

# ==============================================================================
# Initialize Storage for Model Results
# ==============================================================================
varImp_all = data.frame()
score_all = data.frame(matrix(ncol = 9)[0, ])
colnames(score_all) = c("Species name", "Train data", "Test data",       
                        "Regularization multiplier", "Feature classes", "AUC",                      
                        "TSS", "CBI", "Threshold")

# ==============================================================================
# Filtering species based on occurrence counts (over 20 occurrences)
# ==============================================================================
g_list_sel$Species

g_gbif_onlylist = g[g$GBIF == 1,]
gbif_onlylist = g_gbif_onlylist %>% distinct(g_gbif_onlylist[, c("Species")]) 
gbif_onlylist = gbif_onlylist$`g_gbif_onlylist[, c("Species")]`

gbif_onlylist_sel = g_gbif_onlylist %>% group_by(Species, occ) %>%
  summarize(occ_n=sum(occ))
gbif_onlylist_sel = gbif_onlylist_sel[, c("Species", "occ_n")]
gbif_onlylist_sel = gbif_onlylist_sel[gbif_onlylist_sel$occ_n >= 20,]

gbif_onlylist_sel$Species

# ==============================================================================
# Main SDM Loop: Model Construction and Optimization
# ==============================================================================
for(names in gbif_onlylist_sel$Species){
  print(names)
  eval(parse(text=paste0("dir.create('",names,"')")))
  eval(parse(text=paste0("xy = as.data.frame(g_data_sel[g_data_sel$Species == '",names,"', c('rowid', 'Species', 'x', 'y'), ])")))
EAbias@data
  set.seed(123456)
  a <- randomPoints(EAbias, 5000, prob = T, lonlatCorrection = TRUE, p = xy[,c('x', 'y')], excludep = T)
  eval(parse(text=paste0("openxlsx::write.xlsx(a, '",names,"/",names,"_abs.xlsx')")))
  
  ####Enviromental filter
  sp_filt = occfilt_env(
    data = xy,
    x = "x",
    y = "y",
    id = "rowid",
    env_layer = terra::rast(env),
    nbins = 25
    )

  ###Data-driven select variables for Maxent
  selval = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11",
             "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
  varsel_pred = stack()
  for(r in selval){
    eval(parse(text=paste0("varsel_pred = stack(varsel_pred, env$'",r,"')")))
  }
  
  eval(parse(text=paste0("varsel_data = prepareSWD(species = '",names,"', p = sp_filt[,c('x','y')], a = a, env = varsel_pred)")))
  #SWD format only background
  bgSWD <- prepareSWD(species = "Bgs", a = a, env = varsel_pred)
  
  c(train_data, test_data) %<-% trainValTest(varsel_data, test = 0.2, only_presence = T,
                                             seed = seed)

  train_model <- train("Maxent", data = train_data)
  
  #Remove highly correlated variables
  selvar <- SDMtune::varSel(train_model, metric = "auc", bg4cor = bgSWD, test=test_data,
                            method = "spearman", env = varsel_pred,
                            cor_th = 0.7, permut = 1)
  
  select.val = names(selvar@data@data)
  names(selvar@data@data)
  select.val
  
  # predict variable dataset
  predictors = stack()
  for(p in select.val){
    eval(parse(text=paste0("predictors = stack(predictors, 
                             env$",p,")")))
  }

  ## Maxnet experiment hyperparameters---------------------------------------
  eval(parse(text=paste0("data = prepareSWD(species = '",names,"', 
                           sp_filt[, c('x','y')], a = a, 
                           env = predictors)")))
  c(train_data2, test_data2) %<-% trainValTest(data, test = 0.2, only_presence = FALSE, seed = seed)
  
  # Train starting model
  train = train("Maxent", data = train_data2)  

  # hyperparameters' combinations
  h = list(reg = seq(1, 10, 0.5), fc = c("l", "lh", "lq", "lqh", "lqp", "lqph", "lqpht"))
  nrow(expand.grid(h)) # Make sure there are combinations  

  # Genetic Algorithm
  om_aicc = gridSearch(train, hypers = h, metric = "aicc", env= predictors)   
  om_auc = gridSearch(train, hypers = h, metric = "auc", env = predictors, test = test_data2)

  options(scipen=10)
  om_auc@results$'ID' = 1:length(om_auc@results$train_AUC)
  AUC_testlist = (om_auc@results[order(-om_auc@results$diff_AUC), ])
  AUC_testlist = AUC_testlist %>% mutate('fc_reg' = paste(!!!rlang::syms(c('fc', 'reg')), sep='-'))
  AICc_testlist = (om_aicc@results[order(-om_aicc@results$AICc), ])
  AICc_testlist = AICc_testlist %>% mutate('fc_reg' = paste(!!!rlang::syms(c('fc', 'reg')), sep='-'))
  AUC_testlist_2 = left_join(AUC_testlist, AICc_testlist[, c('AICc', 'delta_AICc', 'fc_reg')], by = 'fc_reg')
  
  eval(parse(text=paste0("openxlsx::write.xlsx(AUC_testlist_2, '",names,"/",names,"_RMFClist.xlsx')")))
  GS_testlist = left_join(AUC_testlist, AICc_testlist[, c('AICc', 'AICc', 'fc_reg')], by = 'fc_reg')
  GS_testlist = GS_testlist[, c('ID', 'fc_reg', 'fc', 'reg', 'train_AUC', 'test_AUC', 'diff_AUC', 'AICc')]
  GS_testlist$diff_AUC = abs(GS_testlist$diff_AUC)
  GS_testlist = GS_testlist %>% mutate_all(funs(ifelse(is.infinite(.),NA,.))) %>% 
    na.omit()
  
  maxtestAUC = GS_testlist[GS_testlist$test_AUC == max(GS_testlist$test_AUC),]
  maxtestAUC['Setting'] = 'Optimized(maximum AUC)'
  minAICc = GS_testlist[GS_testlist$AICc == min(GS_testlist$AICc),]
  minAICc['Setting'] = 'Optimized(minimum AICc)'
  
  
  CompDefOp = rbind(minAICc[1,], maxtestAUC[1,])

  CompDefOp = CompDefOp[, c('ID','Setting', 'fc', 'reg', 'train_AUC', 'test_AUC', 'diff_AUC', 'AICc')]
  names(CompDefOp) = c('ID','Setting','Feature combination','Regularization Multiplier','TrainAUC','TestAUC','Diff.AUC','AICc')
  
  eval(parse(text=paste0("openxlsx::write.xlsx(CompDefOp, '",names,"/",names,"_CompDefOp.xlsx')")))

  # Final model
  best_difAUC = CompDefOp[CompDefOp$Diff.AUC == min(CompDefOp$Diff.AUC),]
  best_difAUC = min(best_difAUC$ID)
  index = which(abs(om_auc@results$ID) == best_difAUC)
  new_train = om_auc@models[[index[1]]]@data
  
  final_folds = randomFolds(new_train, k = 5, only_presence = TRUE, seed = 123)
  reg = om_auc@results[index, 2]
  fc = om_auc@results[index, 1]
  final_model = train("Maxent", data = new_train, fc = om_auc@results[index, 1], reg = om_auc@results[index, 2], folds=final_folds)
  
  ##Veriable importance and reponse curves
  varImp = varImp(final_model, permut = 5)
  eval(parse(text=paste0("openxlsx::write.xlsx(varImp, '",names,"/",names,"_varImp.xlsx')")))
  eval(parse(text=paste0("varImp$Species = '",names,"'")))
  varImp_all = rbind(varImp_all, varImp)
  select.val2 = varImp[varImp$Permutation_importance >0, c("Variable")]
  for(rr in select.val2){
    eval(parse(text=paste0("rc_",rr," = plotResponse(final_model, var = '",rr,"', type = 'logistic',
                         only_presence = TRUE, marginal = FALSE, rug = TRUE, color = 'black')+ylim(0.0, 1.0)")))
  }
  plots <- list()
  for(rr in select.val2){
    eval(parse(text=paste0("plots[[rr]] <- rc_",rr,"")))
  }
  res.cur = cowplot::plot_grid(plotlist = plots, nrow = 1, labels = "auto")+ 
    theme(plot.background = element_rect(fill="white", color = NA))
  eval(parse(text=paste0("ggplot2::ggsave(file='",names,"/",names,"_res.cur.tiff', plot=res.cur, dpi = 200, width = 15, height = 3)")))
  
  #Predict present suitable habitat
  eval(parse(text=paste0("sh = SDMtune::predict(final_model, predictors, type ='logistic', file = '",names,"/",names,"_present_sh', format = 'GTiff', overwrite=TRUE)")))
  
  #Habitat of over threshold
  final_model_PA = train("Maxent", data = new_train, fc = om_aicc@results[index, 1], reg = om_aicc@results[index, 2])
  ths = thresholds(final_model_PA, type = "logistic", test = test_data2)
  ths
  th = ths[3, 2]
  eval(parse(text=paste0("plotPA_e  = plotPA(sh, th = th, filename = '",names,"/",names,"_pa_map', format = 'GTiff', overwrite=TRUE)")))
  sh_values = getValues(sh)
  sh_th = ifelse(sh_values >= th, sh_values, 0)
  sh_th = setValues(sh, sh_th)
  eval(parse(text=paste0("writeRaster(sh_th, '",names,"/",names,"_present_shth.tif', overwrite=TRUE)")))

  #Score
  auc = SDMtune::auc(final_model, test= TRUE) %>%
    as.data.frame(.)%>% 
    round(., 3)
  rownames(auc) = "AUC"
  colnames(auc) = "Data"
  tss = SDMtune::tss(final_model, test = TRUE)%>%
    as.data.frame(.)%>% 
    round(., 3)
  rownames(tss) = "TSS"
  colnames(tss) = "Data"
  
  cbi = ecospat::ecospat.boyce(fit = sh, sp_filt[, c('x', 'y')], PEplot = TRUE, res = 100,
                               rm.duplicate = TRUE, method = 'spearman')
  cbi = cbi$cor %>% 
    as.data.frame() %>% 
    round(., 3)
  rownames(cbi) = "CBI"
  colnames(cbi) = "Data"
  
  sp_name = as.data.frame(data@species)
  rownames(sp_name) = "Species name"
  colnames(sp_name) = "Data"
  train_n = as.data.frame(sum(new_train@pa == 1)) 
  rownames(train_n) = "Train data"
  colnames(train_n) = "Data"
  test_n = as.data.frame(sum(nrow(sp_filt)) - sum(new_train@pa == 1))
  rownames(test_n) = "Test data"
  colnames(test_n) = "Data"
  
  score = rbind(sp_name, train_n) %>%
    rbind(., test_n) %>%
    rbind(., reg) %>%
    rbind(., fc)  %>%
    rbind(., auc) %>%
    rbind(., tss) %>%
    rbind(., cbi) %>%
    rbind(., round(th,3))
  rownames(score)[c(4,5,9)] = c("Regularization multiplier", "Feature classes", "Threshold")
  score = rownames_to_column(score)
  colnames(score) = c("Score", "Data")  
  eval(parse(text=paste0("openxlsx::write.xlsx(score, '",names,"/",names,"_score.xlsx', fileEncoding = 'CP932')")))  
  
  score_t = t(score) %>% 
    as.data.frame()
  colnames(score_t) = c("Species name", "Train data", "Test data",       
                        "Regularization multiplier", "Feature classes", "AUC",                      
                        "TSS", "CBI", "Threshold")
  score_all = rbind(score_all, score_t[2, ])
  
  #Predict LGM suitable habitat (Test)
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("pred_lgm",d," = stack()")))
  }
  
  for(d in c("cc","mr","me")){
    for(p in select.val){
      eval(parse(text=paste0("pred_lgm",d," = stack(pred_lgm",d,", env_lgm_",d,"$'",p,"')")))
    }}
  
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("predict_lgm_",d," = SDMtune::predict(final_model, pred_lgm",d,", type='logistic', file = '",names,"/",names,"_lgm_sh_",d,"', format = 'GTiff', overwrite=TRUE)")))
    eval(parse(text=paste0("PA_lgm_",d," = plotPA(predict_lgm_",d,", th = ths[3, 2], filename = '",names,"/",names,"_pa_lgm_",d,"', format = 'GTiff', overwrite=TRUE)")))
  }
  mean_lgm_sh = mean(predict_lgm_cc, predict_lgm_mr, predict_lgm_me)
  eval(parse(text=paste0("writeRaster(mean_lgm_sh, '",names,"/",names,"_lgm_sh_mean.tif', overwrite=TRUE)")))
  
  #Habitat of over threshold
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("sh_values_",d," = getValues(predict_lgm_",d,")")))
    eval(parse(text=paste0("sh_lgm_th_",d," = ifelse(sh_values_",d," >= th, sh_values_",d,", 0)")))
    eval(parse(text=paste0("sh_lgm_th_",d," = setValues(predict_lgm_",d,", sh_lgm_th_",d,")")))
    eval(parse(text=paste0("writeRaster(sh_lgm_th_",d,", '",names,"/",names,"_lgm_shth_",d,".tif', overwrite=TRUE)")))
  }
  mean_lgm_shth = mean(sh_lgm_th_cc, sh_lgm_th_mr, sh_lgm_th_me)
  eval(parse(text=paste0("writeRaster(mean_lgm_shth, '",names,"/",names,"_lgm_shth_mean.tif', overwrite=TRUE)")))
  eval(parse(text=paste0("save(final_model, file = '",names,"/",names,"_finalmodel.rda')")))
  
  #Predict Mid-Holocene suitable habitat (Test)
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("pred_mh",d," = stack()")))
  }
  
  for(d in c("cc","mr","me")){
    for(p in select.val){
      eval(parse(text=paste0("pred_mh",d," = stack(pred_mh",d,", env_mh_",d,"$'",p,"')")))
    }}
  
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("predict_mh_",d," = SDMtune::predict(final_model, pred_mh",d,", type='logistic', file = '",names,"/",names,"_mh_sh_",d,"', format = 'GTiff', overwrite=TRUE)")))
    eval(parse(text=paste0("PA_mh_",d," = plotPA(predict_mh_",d,", th = ths[3, 2], filename = '",names,"/",names,"_pa_mh_",d,"', format = 'GTiff', overwrite=TRUE)")))
  }
  mean_mh_sh = mean(predict_mh_cc, predict_mh_mr, predict_mh_me)
  eval(parse(text=paste0("writeRaster(mean_mh_sh, '",names,"/",names,"_mh_sh_mean.tif', overwrite=TRUE)")))
  
  
  #Habitat of over threshold
  for(d in c("cc","mr","me")){
    eval(parse(text=paste0("sh_values_",d," = getValues(predict_mh_",d,")")))
    eval(parse(text=paste0("sh_mh_th_",d," = ifelse(sh_values_",d," >= th, sh_values_",d,", 0)")))
    eval(parse(text=paste0("sh_mh_th_",d," = setValues(predict_mh_",d,", sh_mh_th_",d,")")))
    eval(parse(text=paste0("writeRaster(sh_mh_th_",d,", '",names,"/",names,"_mh_shth_",d,".tif', overwrite=TRUE)")))
  }
  mean_mh_shth = mean(sh_mh_th_cc, sh_mh_th_mr, sh_mh_th_me)
  eval(parse(text=paste0("writeRaster(mean_mh_shth, '",names,"/",names,"_mh_shth_mean.tif', overwrite=TRUE)")))
  eval(parse(text=paste0("save(final_model, file = '",names,"/",names,"_finalmodel.rda')")))
}



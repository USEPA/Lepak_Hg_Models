# MeHg model with THg and LOI as predictors

# library(party) # 1.3-11 on VM
# library(permimp) # permimp_1.0-2 
library(randomForest) # randomForest_4.7-1.1
library(tidyverse) # tidyverse_1.3.2
library(foreach)
library(doParallel)
library(pdp) # 0.8.1
library(colorspace)
library(vegan)
library(maps)

# library(remotes)
# install_version( "tidyverse",version = "1.3.2")
# install_version("randomForest", version = "4.7-1.1")
# install_version("pdp", version = "0.8.1")


output_dir <- "Model_Output/MeHgTHg_wLOI/"
model_dir <- "Saved_Models/MeHgTHg_wLOI/"
fig_dir <- "Figures/MeHgTHg_wLOI/"

dir.create(paste0(output_dir, "CV"), showWarnings = F)
dir.create(paste0(output_dir, "PDP"), showWarnings = F)
dir.create(paste0(output_dir, "PDP/Bivariate"), showWarnings = F)
dir.create(paste0(fig_dir, "PDP"), showWarnings = F)
dir.create(paste0(fig_dir, "PDP/Bivariate"), showWarnings = F)

# Load imputed training and test data - does not contain ecoregion
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")

# Which predictors were imputed
# Imputed_Preds <- read.csv("Tables/List_Imputed_Training_Preds_THg_MHg.csv")



# Make LOI a percent doesn't matter for this model
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100

# Compute MeHg/THg ratio for modeling
Train_Dat$log10MeHgTratio <- log10(Train_Dat$SMHG_ng_g/Train_Dat$STHG_ng_g)
Test_Dat$log10MeHgTratio <- log10(Test_Dat$SMHG_ng_g/Test_Dat$STHG_ng_g)

# Toss lakes where MeHg > THg
Train_Dat %>% filter(STHG_ng_g <SMHG_ng_g) %>% dplyr::select(STHG_ng_g, SMHG_ng_g,  NLA12_ID) # NLA12_WY-R02
Test_Dat %>% filter(STHG_ng_g <SMHG_ng_g) %>% dplyr::select(STHG_ng_g, SMHG_ng_g,  NLA12_ID) # No test lakes have this issue

Train_Dat <- Train_Dat %>% filter(!(STHG_ng_g <SMHG_ng_g)) # Now 974


## *****************  Change this for different models  ******************* ####
## DO SEARCH/REPLACE!!!!!!!!!!!!!!!!!!!!
response_var <- "log10MeHgTratio"
mtry_par <- 0.20

# Let's go with nodesize=1, mtry=.2*p  


##### Do 10-fold CV for all models #####

# Do stratified partitioning by space

Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83, HUC2, HUC8, Omernik_I, Omernik_II, Omernik_III)

sort(table(Lake_Geo$Omernik_II))
ecoregions <- unique(Lake_Geo$Omernik_II)

Train_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Train_Dat$NLA12_ID)
Test_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Test_Dat$NLA12_ID)

Train_ecoregions <- unique(Train_Geo$Omernik_II) # Has all 19 ecoregions
Test_ecoregions <- unique(Test_Geo$Omernik_II)   # Has 15 ecoregions - Does not have 4 ecoregions with <10 lakes

# Number lakes in each ecoregion
ecoregion_n <- Train_Geo %>% group_by(Omernik_II) %>% summarize(n=n()) %>% arrange(desc(n))
# Max number lakes in each ecoregion is 135

min_n <- floor(nrow(Train_Geo)/10) # min number lakes in each fold
# 10-fold CV will result in 5 folds with 97 lakes, 5 folds with 98 lakes

# Randomly put lakes in folds, attempting to allocate lakes in ecoregions evenly across folds
set.seed(37) 
Lake_folds <- NULL
fold_breakdown <- rep(1,10) # initiate

for(i in 1:length(ecoregion_n$Omernik_II)){
  
  # Lakes in ecoregion i
  lakes.i <-  Train_Geo %>% filter(Omernik_II==ecoregion_n$Omernik_II[i])
  
  # Sort lakes by Omernik_III, then state (state is in NLA12_ID), and then randomly distribute in order to attempt to evenly distribute ecoregions and nearby lakes across folds
  lakes.i <- lakes.i %>% arrange(Omernik_III, NLA12_ID)
  
  # Create vector of random permutations of fold numbers for assignment
  # Note that lake IDs are grouped spatially by state, so makes sense
  random.folds <- as.vector(replicate(n = ceiling(nrow(lakes.i)/10), expr = {sample(1:10, 10)}))
  # random.folds <- c(sample(1:10, 10), sample(1:10, 10), sample(1:10, 10)) 
  
  
  if(fold_breakdown[1]>min_n) random.folds <- random.folds[!random.folds==1]
  if(fold_breakdown[2]>min_n) random.folds <- random.folds[!random.folds==2]
  if(fold_breakdown[3]>min_n) random.folds <- random.folds[!random.folds==3]
  if(fold_breakdown[4]>min_n) random.folds <- random.folds[!random.folds==4]
  if(fold_breakdown[5]>min_n) random.folds <- random.folds[!random.folds==5]
  if(fold_breakdown[6]>min_n) random.folds <- random.folds[!random.folds==6]
  if(fold_breakdown[7]>min_n) random.folds <- random.folds[!random.folds==7]
  if(fold_breakdown[8]>min_n) random.folds <- random.folds[!random.folds==8]
  if(fold_breakdown[9]>min_n) random.folds <- random.folds[!random.folds==9]
  if(fold_breakdown[10]>min_n) random.folds <- random.folds[!random.folds==10]
  
  lakes.i$folds <- random.folds[1:nrow(lakes.i)]
  
  Lake_folds <- rbind(Lake_folds, lakes.i)
  fold_breakdown <- table(Lake_folds$folds)
  
}

table(Lake_folds$folds) # Goal: 5 97s, 5 98s
sum(table(Lake_folds$folds)) # Should be 974

# Randomly reassign NLA12_IDs to folds with <min_n chems
while(sum(table(Lake_folds$folds)<min_n)>0){
  fold.assign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)<min_n])[1]
  
  folds.reassign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)>min_n])
  lake.reassign <- Lake_folds %>% filter(folds %in% folds.reassign) %>% sample_n(1) %>% pull(NLA12_ID)
  Lake_folds$folds[Lake_folds$NLA12_ID==lake.reassign] <- fold.assign
}

table(Lake_folds$folds) # Goal: 5 97s, 5 98s
sum(table(Lake_folds$folds)) # Should be 974



### Do 10-fold CV to estimate error

# # Keep LOI
# Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g) # , -LOI_PERCENT, -log10THg
# p <- ncol(Train_Dat_run)-1 # 125 preds 



## *****************  Change RESPONSE in loop below!!!!!!!!!! **************

# Vars to remove
RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)


# numCores <- detectCores()
# numCores <- detectCores()-1
# registerDoParallel(numCores)

registerDoParallel(30) # 48 min on server

start.time <- Sys.time()

foreach(i = (1:nrow(RFE_info)), .packages=c('dplyr', 'randomForest')) %dopar% {
  All_dat_preds_loop <- Train_Dat[, colnames(Train_Dat) %in% c(RFE_info$Worst_Var[i:nrow(RFE_info)], response_var, "NLA12_ID")]
  
  nump <- ncol(All_dat_preds_loop)-2    # Number predictors (subtracting response, NLA12_ID)
  
  All_dat_Pred <- NULL
  
  for(j in 1:max(Lake_folds$folds)){
    print(paste("Iteration:", i, "out of", nrow(RFE_info), "... Fold", j))
    
    TestLakes <- Lake_folds$NLA12_ID[which(Lake_folds$folds==j)]
    
    TrainData <- All_dat_preds_loop %>% filter(!NLA12_ID %in% TestLakes) %>% dplyr::select(-NLA12_ID)
    
    TestData <- All_dat_preds_loop %>% filter(NLA12_ID %in% TestLakes)
    
    set.seed(73) 
    rf  <- randomForest(log10MeHgTratio ~ ., data=TrainData, 
                        mtry=max(floor(mtry_par*nump), 1), 
                        ntree=5000,
                        nodesize=1,
                        keep.forest=T,
                        keep.inbag = F,
                        importance=F)
    
    rf_pred_j <- predict(rf, newdata=TestData)
    
    TestData <- TestData %>% mutate(Pred = rf_pred_j, Fold=j)
    
    All_dat_Pred <- rbind(All_dat_Pred, TestData)
    
  }
  
  
  write.csv(All_dat_Pred, paste0(output_dir, "CV/CV_preds_Iter",i,".csv"), row.names = F)
  
  
}

stopImplicitCluster()  


end.time <- Sys.time()
end.time-start.time


# results <- mclapply(i_s, fx, mc.cores = numCores)
# mclapply(i_s, cv_function, mc.cores = numCores)




#### CV error estimates for RFE #####

## ******* NEED TO CHANGE RESPONSE VARIABLE IN LOOP BELOW FOR OTHER RESPONSES!!!!! **********

RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)

RFE_info$MeanCV_rmse <- RFE_info$MeanCV_mse <- RFE_info$MeanCV_mae <- RFE_info$MeanCV_bias <- NA
RFE_info$SE_CV_rmse <- RFE_info$SE_CV_mse <- RFE_info$SE_CV_mae <- RFE_info$SE_CV_bias <- NA


for(i in 1:nrow(RFE_info)){
  
  All_dat_Pred <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))
  
  CV_Stats <- All_dat_Pred %>% group_by(Fold) %>% summarize(
    MAE=mean(abs(log10MeHgTratio-Pred)), 
    RMSE=sqrt(mean((log10MeHgTratio-Pred)^2)), 
    MSE=mean((log10MeHgTratio-Pred)^2),
    Bias=mean(Pred-log10MeHgTratio), 
    n=n()) 
  
  RFE_info$MeanCV_rmse[i] <- mean(CV_Stats$RMSE)
  RFE_info$MeanCV_mse[i] <- mean(CV_Stats$MSE)
  RFE_info$MeanCV_mae[i] <- mean(CV_Stats$MAE)
  RFE_info$MeanCV_bias[i] <- mean(CV_Stats$Bias)
  
  RFE_info$SE_CV_rmse[i] <- sd(CV_Stats$RMSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$SE_CV_mse[i] <- sd(CV_Stats$MSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$SE_CV_mae[i] <- sd(CV_Stats$MAE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$SE_CV_bias[i] <- sd(CV_Stats$Bias)/sqrt(max(All_dat_Pred$Fold))
  
}


# write.csv(RFE_info, paste0(output_dir, "rf_RFE_info_wCVerrors.csv"), row.names = F)


RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info_wCVerrors.csv"))


# MAE iteration with least variables that has error within 1 SE of min error
min_it_mae <- which(RFE_info$MeanCV_mae==min(RFE_info$MeanCV_mae))
best_it_mae <- max(which(RFE_info$MeanCV_mae <= RFE_info$MeanCV_mae[min_it_mae] + RFE_info$SE_CV_mae[min_it_mae]))
# Remaining variables
RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)]

RFE_info$Predictor <- RFE_info$Worst_Var
RFE_info$Predictor[RFE_info$Predictor=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
RFE_info$Predictor[RFE_info$Predictor=="WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s"] <- "WetLossLS"
RFE_info$Predictor[RFE_info$Predictor=="SECCHI_m_use_depth_for_clear_to_bottom"] <- "Secchi"

cat(rev(RFE_info$Predictor[best_it_mae:nrow(RFE_info)]), sep = "\n")


RFE_info$Subset_MAE <- "Out"
RFE_info$Subset_MAE[best_it_mae:nrow(RFE_info)] <- "In"
RFE_info$Point_Col <- "Black"
RFE_info$Point_Col[best_it_mae:nrow(RFE_info)] <- "firebrick3"

RFE_info  %>% filter(NumVars %in% 1:50) %>% 
  ggplot(aes(x=NumVars, y=MeanCV_mae, label=Predictor)) + 
  geom_point(size=3, aes(col=Subset_MAE)) + 
  geom_line(size=1.2) +
  coord_cartesian(ylim=c(.25,.4)) +
  geom_hline(yintercept=min(RFE_info$MeanCV_mae), size=1) +
  geom_hline(yintercept=RFE_info$MeanCV_mae[min_it_mae] + RFE_info$SE_CV_mae[min_it_mae], lty=2, size=1) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  annotate(geom = "text", x=rev(1:50), y=.4, label=RFE_info$Predictor[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4, col=RFE_info$Point_Col[RFE_info$NumVars %in% 1:50]) +
  ylab("CV MAE") + xlab("Number variables") + 
  theme(legend.position="none") +
  scale_color_manual(values = c("firebrick3", "Black"))
ggsave(paste0(fig_dir, "/RFE_CV_MAE.png"), width=10, height=6)



# RMSE iteration with least variables that has error within 1 SE of min error
min_it_rmse <- which(RFE_info$MeanCV_rmse==min(RFE_info$MeanCV_rmse))
# min_it_mse <- which(RFE_info$MeanCV_mse==min(RFE_info$MeanCV_mse)) # same
best_it_rmse <- max(which(RFE_info$MeanCV_rmse <= RFE_info$MeanCV_rmse[min_it_rmse] + RFE_info$SE_CV_rmse[min_it_rmse]))
# Remaining variables
RFE_info$Worst_Var[best_it_rmse:nrow(RFE_info)]

RFE_info$Subset_RMSE <- "Out"
RFE_info$Subset_RMSE[best_it_rmse:nrow(RFE_info)] <- "In"
RFE_info$Point_Col_rmse <- "Black"
RFE_info$Point_Col_rmse[best_it_rmse:nrow(RFE_info)] <- "firebrick3"

RFE_info  %>% filter(NumVars %in% 1:50) %>% 
  ggplot(aes(x=NumVars, y=MeanCV_rmse, label=Predictor)) + 
  geom_point(size=3, aes(col=Subset_RMSE)) + 
  geom_line(size=1.2) +
  coord_cartesian(ylim=c(.32,.5)) +
  geom_hline(yintercept=min(RFE_info$MeanCV_rmse), size=1) +
  geom_hline(yintercept=RFE_info$MeanCV_rmse[min_it_mae] + RFE_info$SE_CV_rmse[min_it_mae], lty=2, size=1) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  annotate(geom = "text", x=rev(1:50), y=.5, label=RFE_info$Predictor[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4, col=RFE_info$Point_Col_rmse[RFE_info$NumVars %in% 1:50]) +
  ylab("CV RMSE") + xlab("Number variables") + 
  theme(legend.position="none") +
  scale_color_manual(values = c("firebrick3", "Black"))
ggsave(paste0(fig_dir, "/RFE_CV_RMSE.png"), width=10, height=6)


# Plot OOB error with new CV error - almost the same
# RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=MeanCV_mae, label=Worst_Var)) + 
#   geom_point(size=3, col="red") + 
#   geom_line(size=1.2, col="red") +
#   geom_point(aes(x=NumVars, y=OOB_mae), col="black", size=3) + 
#   coord_cartesian(ylim=c(.15,.4)) +
#   geom_hline(yintercept=min(RFE_info$OOB_mae), col="black") +
#   geom_hline(yintercept=min(RFE_info$MeanCV_mae), col="red") +
#   theme_minimal(base_size = 19) +
#   scale_x_continuous(breaks=seq(2,50,2)) +
#   annotate(geom = "text", x=rev(1:50), y=.40, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
#   ylab("CV MAE") + xlab("Number variables") +
#   geom_errorbar(aes(ymin=MeanCV_mae-SE_CV_mae, ymax=MeanCV_mae+SE_CV_mae), width=.2, col="red")


# Bias
RFE_info  %>% filter(NumVars %in% 1:50) %>% 
  ggplot(aes(x=NumVars, y=MeanCV_bias, label=Predictor)) + 
  geom_point(size=4) + 
  geom_line(size=1.2) +
  geom_hline(yintercept=0, lty=2, col="black", size=1.2) +
  coord_cartesian(ylim=c(-.001,.01)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  annotate(geom = "text", x=rev(1:50), y=.01, label=RFE_info$Predictor[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("CV Mean Bias") + xlab("Number variables") 
ggsave(paste0(fig_dir, "/RFE_CV_Bias.png"), width=10, height=6)







# Visualize errors of best MAE model 
i <- best_it_mae
Top_MAE_Mod <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))

range(Top_MAE_Mod$log10MeHgTratio)
range(Top_MAE_Mod$Pred)

Top_MAE_Mod %>% ggplot(aes(x=log10MeHgTratio, y=Pred)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  geom_point(size=2) +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,-.25), ylim=c(-3.5,-.25))+
  theme(text=element_text(size=20)) +
  xlab("Observed log10MeHgTratio") + ylab("Predicted log10MeHgTratio")
ggsave(paste0(fig_dir, "/Best_MAE_CVPred_vs_Obs.png"), width=10, height=6)


# Visualize errors of best RMSE model 
i <- best_it_rmse
Top_RMSE_Mod <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))

Top_RMSE_Mod %>% ggplot(aes(x=log10MeHgTratio, y=Pred)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_point(size=2) +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,-.25), ylim=c(-3.5,-.25))+
  theme(text=element_text(size=20)) +
  xlab("Observed log10MeHgTratio") + ylab("Predicted log10MeHgTratio")


# What about errors of full model
i <- 1
Full_Mod <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))

Full_Mod %>% ggplot(aes(x=log10MeHgTratio, y=Pred)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_point(size=2) +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,-.25), ylim=c(-3.5,-.25))+
  theme(text=element_text(size=20)) +
  xlab("Observed log10MeHgTratio") + ylab("Predicted log10MeHgTratio")

# Full model also sucks!!

# Using MAE has more variables and more bias
# Using RMSE has fewer variables and least bias but higher variance
# summary(lm(Top_MAE_Mod$Pred~Top_MAE_Mod$log10MeHgTratio)) # 0.1861 
# summary(lm(Top_RMSE_Mod$Pred~Top_RMSE_Mod$log10MeHgTratio)) # 0.1881 





# Then REFIT FINAL MODEL to all training data, test on holdout test set, do PDPs ####

##### ********** CHANGE RESPONSE IN FINAL MODEL FIT!!!!!! ****************** 


# Do  with best MAE model
# Using MAE instead of RMSE because more conservative in retaining predictors than RMSE and tracks with initial visual assessment

# MAE iteration with least variables that has error within 1 SE of min error
min_it_mae <- which(RFE_info$MeanCV_mae==min(RFE_info$MeanCV_mae))
best_it_mae <- max(which(RFE_info$MeanCV_mae <= RFE_info$MeanCV_mae[min_it_mae] + RFE_info$SE_CV_mae[min_it_mae]))

# Remaining variables
RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)]
final.preds <- c(RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)])

Train_run <- Train_Dat[, colnames(Train_Dat) %in% c(final.preds, response_var)]

nump <- ncol(Train_run)-1    # Number predictors (subtracting response, NLA12_ID)

# Fit final model  
set.seed(73) 
rf.final  <- randomForest(log10MeHgTratio ~ ., data=Train_run, 
                          mtry=max(floor(mtry_par*nump), 1), 
                          ntree=5000,
                          nodesize=1,
                          keep.forest=T,
                          keep.inbag = F,
                          importance=T)


# saveRDS(rf.final, paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))


# Predict test set
rf_predict_Test <- predict(rf.final, newdata=Test_Dat)

# Add predictions to Test set and write out
Test_Dat <- Test_Dat %>% mutate(Pred = rf_predict_Test)

write.csv(Test_Dat, paste0(output_dir, "Test_Final_Mod_Preds.csv"), row.names = F)



### Compute Error tables ########

# Compute errors on original log scale
Test_Errors <- Test_Dat  %>% summarize(
  MAE=mean(abs(log10MeHgTratio-Pred)), 
  RMSE=sqrt(mean((log10MeHgTratio-Pred)^2)), 
  Bias=mean(Pred-log10MeHgTratio)) 
#         MAE      RMSE        Bias
#   0.2779883 0.347239 0.02076987

# Compare to CV error
CV_Errors <- RFE_info[best_it_mae,] %>% dplyr::select(MeanCV_mae, MeanCV_rmse, MeanCV_bias) %>% rename(MAE=MeanCV_mae, RMSE=MeanCV_rmse, Bias=MeanCV_bias)
OOB_Errors <- RFE_info[best_it_mae,] %>% dplyr::select(OOB_mae, OOB_rmse   , OOB_bias ) %>% rename(MAE=OOB_mae, RMSE=OOB_rmse, Bias=OOB_bias)

Errors <- rbind(Test_Errors, CV_Errors, OOB_Errors)
Errors$Dataset <- c("Test_Set_101", "Train_CV_Select", "Train_Set_OOB")
Errors <- Errors %>% relocate(Dataset)
Errors_Exp <- Errors %>% mutate(MAE=10^MAE, RMSE=10^RMSE, Bias=10^Bias) # Fold-difference
write.csv(Errors, paste0(output_dir, "Error_Table.csv"), row.names = F)




# Compute relative errors

# Naive test errors
naive_pred <- mean(Train_Dat$log10MeHgTratio)

Naive_Test_Errors <- Test_Dat  %>% summarize(
  Naive_MAE=mean(abs(log10MeHgTratio-naive_pred)), 
  Naive_RMSE=sqrt(mean((log10MeHgTratio-naive_pred)^2))) 

Relative_Test_Errors <- data.frame(RAE=Test_Errors$MAE/Naive_Test_Errors$Naive_MAE,
                                   RRSE=Test_Errors$RMSE/Naive_Test_Errors$Naive_RMSE)


# Naive CV errors
All_dat_Pred_Naive <- NULL
for(j in 1:max(Lake_folds$folds)){
  print(paste("Iteration:", i, "out of", nrow(RFE_info), "... Fold", j))
  TestLakes <- Lake_folds$NLA12_ID[which(Lake_folds$folds==j)]
  TrainData <- Train_Dat %>% filter(!NLA12_ID %in% TestLakes) %>% dplyr::select(-NLA12_ID)
  TestData <- Train_Dat %>% filter(NLA12_ID %in% TestLakes)
  TestData <- TestData %>% mutate(Pred = mean(TrainData$log10MeHgTratio), Fold=j)
  All_dat_Pred_Naive <- rbind(All_dat_Pred_Naive, TestData)
  
}



NaiveCV_Stats <- All_dat_Pred_Naive %>% group_by(Fold) %>% summarize(
  MAE=mean(abs(log10MeHgTratio-Pred)), 
  RMSE=sqrt(mean((log10MeHgTratio-Pred)^2)), 
  MSE=mean((log10MeHgTratio-Pred)^2),
  Bias=mean(Pred-log10MeHgTratio), 
  n=n()) 

Naive_MeanCV_mae <- mean(NaiveCV_Stats$MAE)
Naive_MeanCV_rmse <- mean(NaiveCV_Stats$RMSE)

Relative_CV_Errors <- data.frame(RAE=CV_Errors$MAE/Naive_MeanCV_mae,
                                   RRSE=CV_Errors$RMSE/Naive_MeanCV_rmse)


Relative_Errors <- rbind(Relative_Test_Errors, Relative_CV_Errors)
Relative_Errors$Dataset <- c("Test_Set_101", "Train_CV_Select")
Relative_Errors <- Relative_Errors %>% relocate(Dataset)
write.csv(Relative_Errors, paste0(output_dir, "Relative_Error_Table.csv"), row.names = F)



####################



# Visualize errors 
Test_Dat %>% ggplot(aes(x=log10MeHgTratio, y=Pred)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  geom_point(size=2) +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,-.25), ylim=c(-3.5,-.25))+
  theme(text=element_text(size=20)) +
  xlab("Observed log10MeHgTratio") + ylab("Predicted log10MeHgTratio")
ggsave(paste0(fig_dir, "/Best_MAE_TEST_Pred_vs_Obs.png"), width=10, height=6)


# Plot residuals spatially
Test_Dat$Residual <- Test_Dat$log10MeHgTratio - Test_Dat$Pred
Top_MAE_Mod$Residual <- Top_MAE_Mod$log10MeHgTratio - Top_MAE_Mod$Pred


Train_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Train_Dat$NLA12_ID)
Test_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Test_Dat$NLA12_ID)


Test_Dat <- left_join(Test_Dat, Test_Geo)
Top_MAE_Mod <- left_join(Top_MAE_Mod, Train_Geo)

MainStates <- map_data("state")

# Spatial distribution of residuals
# ggplot(Test_Dat, aes(col=Residual, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()

ggplot(Top_MAE_Mod, aes(fill=Residual, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray50", shape=21) + 
  theme_void() +
  geom_point(data=Test_Dat, size=3.5, aes(fill=Residual, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-2,2,1), limits=c(-2.3,2.3))
ggsave(paste0(fig_dir, "/Best_MAE_Residuals_Space.png"), width=10, height=6)

# ArmyRose, Earth, Fall, Geyser, TealRose, Temps, Tropic, PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu,  Spectral, Zissou 1, Cividis, Roma



### Test spatial autocorrelation for final model
CV_Resids <- Top_MAE_Mod %>% dplyr::select(Residual, LON_DD83, LAT_DD83)

dists <- as.matrix(dist(cbind(CV_Resids$LAT_DD83, CV_Resids$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids$Residual))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=0.02, p=.05 There is NOT spatial autocorrelation in residuals



##### Partial dependence ####

final.preds <- rev(final.preds)
cat(final.preds, sep = "\n")

# Single variable partial dependence plots
for(i in 1:length(final.preds)){
  
  print(i)
  
  pred_dat <- Train_run %>% dplyr::select(final.preds[i])
  
  lim.i <- round(quantile(pred_dat[,1], probs=c(0.05, 0.95)), 4)
  grid.i <- data.frame(PlaceHold=seq(lim.i[1],lim.i[2], (lim.i[2]-lim.i[1])/20))
  names(grid.i) <- names(pred_dat)
  
  cl <- makeCluster(5) 
  doParallel::registerDoParallel(cl)
  
  # partial1 <- partial(rf.final, pred.var=paste0(final.preds[i]), quantiles=T, probs=seq(0.05, 0.95, 0.05))
  partial1 <- partial(rf.final, pred.var=paste0(final.preds[i]),  pred.grid = grid.i, parallel=TRUE,  paropts=list(.packages = "randomForest"))
  
  stopCluster(cl)
  
  pred_lab <- final.preds[i]
  if(pred_lab == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred_lab <- "WetLossConv"
  if(pred_lab == "WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s") pred_lab <- "WetLossLS"
  if(pred_lab == "SECCHI_m_use_depth_for_clear_to_bottom") pred_lab <- "Secchi"
  
  
  saveRDS(partial1, paste0(output_dir, "PDP/", paste0(pred_lab), "_PDP.rds"))
  
  print(pred_lab)
  
  autoplot(partial1, size=1.2) + theme_minimal() + xlab(paste0(pred_lab)) + ylab("log10MeHgTratio") +
    theme(text=element_text(size=20))  #+
  # scale_x_continuous(breaks=seq(-2,6,2)) #+
  ggsave(paste0(fig_dir, "PDP/PDP_", pred_lab, ".png"), width=7, height=5)
}






# Unique predictor combos
pred_combos <- combn(final.preds, 2)

cl <- makeCluster(5) 
doParallel::registerDoParallel(cl)

# On i=6
for (i in 1:ncol(pred_combos)){
  
  print(i)
  
  xvar <- pred_combos[1,i] 
  yvar <- pred_combos[2,i]
  
  pred_lab_x <- xvar
  pred_lab_y <- yvar
  
  if(pred_lab_x == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred_lab_x <- "WetLossConv"
  if(pred_lab_x == "WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s") pred_lab_x <- "WetLossLS"
  
  if(pred_lab_y == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred_lab_y <- "WetLossConv"
  if(pred_lab_y == "WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s") pred_lab_y <- "WetLossLS"
  
  if(pred_lab_x == "SECCHI_m_use_depth_for_clear_to_bottom") pred_lab_x <- "Secchi"
  if(pred_lab_y == "SECCHI_m_use_depth_for_clear_to_bottom") pred_lab_y <- "Secchi"
  
  
  
  pred_dat <- Train_run %>% dplyr::select(paste0(xvar), paste0(yvar))
  
  lim.i <- round(quantile(pred_dat[,1], probs=c(0.05, 0.95)), 4)
  grid.i <- data.frame(PlaceHold=seq(lim.i[1],lim.i[2], (lim.i[2]-lim.i[1])/20))
  names(grid.i) <- names(pred_dat)[1]
  
  lim.j <- round(quantile(pred_dat[,2], probs=c(0.05, 0.95)), 4)
  grid.j <- data.frame(PlaceHold=seq(lim.j[1],lim.j[2], (lim.j[2]-lim.j[1])/20))
  names(grid.j) <- names(pred_dat)[2]
  
  grid.ij <- expand.grid(cbind(grid.i, grid.j), KEEP.OUT.ATTRS = FALSE)
  
  
  
  rf.2pd <- partial(rf.final, train=Train_run, pred.var = c(paste0(xvar), paste0(yvar)),  pred.grid = grid.ij,  parallel=TRUE,  paropts=list(.packages = "randomForest"))
  
  saveRDS(rf.2pd, paste0(output_dir, "PDP/Bivariate/", paste0(pred_lab_x), "_", paste0(pred_lab_y),   "_PDP.rds"))
  # doParallel::stopImplicitCluster()
  
  # rf.2pd <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred_lab_x), "_", paste0(pred_lab_y),   "_PDP.rds"))
  
  suppressWarnings(print(
    autoplot(rf.2pd,  contour = T, legend.title = paste0(response_var)) +
      theme_minimal() +
      scale_fill_continuous_diverging(name=paste0(response_var), palette = 'Blue-Red', mid=mean(range(rf.2pd$yhat)), alpha=1, rev=F) +
      theme(text=element_text(size=20))  +
      xlab(paste0(pred_lab_x)) +
      ylab(paste0(pred_lab_y))
  ))
  
  ggsave(paste0(fig_dir, "PDP/Bivariate/", paste0(pred_lab_x), "_", paste0(pred_lab_y),   "_PDP.png"), width=7, height=5)
}

stopCluster(cl)

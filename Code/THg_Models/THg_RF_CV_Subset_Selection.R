# THg model including LOI as PREDICTOR

# library(party) # 1.3-11 on VM
# library(permimp) # permimp_1.0-2 
library(randomForest) # randomForest_4.7-1.1
library(tidyverse) # tidyverse_1.3.2
library(foreach)
library(doParallel)

# library(remotes)
# install_version(
#   "tidyverse",
#   version = "1.3.2")
# install_version(
#   "randomForest",
#   version = "4.7-1.1")

output_dir <- "Model_Output/THg/"
model_dir <- "Saved_Models/THg/"
fig_dir <- "Figures/THg/"

dir.create(paste0(output_dir, "CV"), showWarnings = F)


# Load imputed training and test data - does not contain ecoregion
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")

# Which predictors were imputed
# Imputed_Preds <- read.csv("Tables/List_Imputed_Training_Preds_THg_MHg.csv")


# log-transform THg for all modeling
Train_Dat$log10THg <- log10(Train_Dat$STHG_ng_g)
Test_Dat$log10THg <- log10(Test_Dat$STHG_ng_g)

# Make LOI a percent (doesn't matter for this model, but need for THg/LOI model
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100





# Try tuning mtry and nodesize first at 5000 trees
# grid_search <- read.csv("Tables/Grid_Search/THg_grid_srch.csv")
# ggplot(grid_search, aes(x=mtry, y=OOB_error, col=nodesize))+geom_point()

# Let's go with nodesize=1, mtry=.5*p



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
sum(table(Lake_folds$folds)) # Should be 975

# Randomly reassign NLA12_IDs to folds with <min_n chems
while(sum(table(Lake_folds$folds)<min_n)>0){
  fold.assign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)<min_n])[1]
  
  folds.reassign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)>min_n])
  lake.reassign <- Lake_folds %>% filter(folds %in% folds.reassign) %>% sample_n(1) %>% pull(NLA12_ID)
  Lake_folds$folds[Lake_folds$NLA12_ID==lake.reassign] <- fold.assign
}

table(Lake_folds$folds) # Goal: 5 97s, 5 98s
sum(table(Lake_folds$folds)) # Should be 975



### Do 10-fold CV to estimate error
# Need to do mean CV with standard error. Not standard error with OOB. So need to do for all iterations.
# Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g) 
# p <- ncol(Train_Dat_run)-1 # 125 preds - including LOI!!!


## ********  Change this for different models  ******** ####
response_var <- "log10THg"
## * Also change mtry for different models

# Vars to remove
RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)



# Try i=1
# 15.15665 mins for i=105
# 1.121855 hours for i=1

# for(i in 1:nrow(RFE_info)){
# Make loop a function
# cv_function <- function(i=is, Train_Dat=Train_Dat, RFE_info=RFE_info,  Lake_folds=Lake_folds){



# numCores <- detectCores()
# numCores <- detectCores()-1
# registerDoParallel(numCores)

registerDoParallel(30)

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
    rf  <- randomForest(log10THg ~ ., data=TrainData, 
                        mtry=max(floor(nump/2), 1), 
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


# Add QSAR code for calculating CV errors















########### START HERE WITH RFE #############




# RFE with unconditional permutation importance

# # Pull out response and predictors
# All_dat_preds_loop <- Train_Dat_run 
# 
# p <- ncol(Train_Dat_run)-1 # 
# 
# # Save OOB error, variable importance
# VI.list <- NULL
# var.list <- rep(NA,p)
# 
# OOB.rmse <- rep(NA,p)
# OOB.mae <- rep(NA,p)
# OOB.bias <- rep(NA,p)
# 
# train.rmse <- rep(NA,p)
# train.mae <- rep(NA,p)
# train.bias <- rep(NA,p)
# 
# 
# 
# start.time <- Sys.time()
# 
# for(i in 1:p){
#   print(i)
#   
#   nump <- ncol(All_dat_preds_loop)-1 # Number predictors
#   
#   set.seed(13) 
#   rf  <- randomForest(log10THg ~ ., data=All_dat_preds_loop, 
#                       mtry=max(floor(nump/2), 1), 
#                       ntree=5000,
#                       nodesize=1,
#                       keep.forest=F,
#                       keep.inbag = F,
#                       importance=F)
#   
#   rf.pred <- rf$predicted # OOB
#   rf.pred.tr <- predict(rf, newdata=Train_Dat_run) # training
#   
#   # OOB error
#   OOB.rmse[i] <- sqrt(mean((Train_Dat_run$log10THg-rf.pred)^2)) # OOB rmse
#   OOB.mae[i] <- mean(abs(Train_Dat_run$log10THg-rf.pred)) # oob mae
#   OOB.bias[i] <- mean(rf.pred-Train_Dat_run$log10THg) # oob bias
#   
#   # Training error
#   train.rmse[i] <- sqrt(mean((Train_Dat_run$log10THg-rf.pred.tr)^2)) # training rmse
#   train.mae[i] <- mean(abs(Train_Dat_run$log10THg-rf.pred.tr)) #  training mae
#   train.bias[i] <- mean(rf.pred.tr-Train_Dat_run$log10THg) # training bias
#   
#   
#   # rf_imp <- data.frame(Variable=row.names(rf$importance), Importance=rf$importance[,1])
#   # row.names(rf_imp) <- NULL
#   # rf_imp <- rf_imp %>% arrange(Importance)
# 
#   
#   # VI.list[[i]] <- rf_imp
#   # var.list[i] <- rf_imp$Variable[1] # Least informative variable to remove
#   
#   All_dat_preds_loop <- All_dat_preds_loop[,!colnames(All_dat_preds_loop)==var.list[i]] # remove variable
#   
#   rm(rf)
# }
# 
# 
# # RFE info needs to be saved before do the CV errors!
# RFE_info <- data.frame(Worst_Var=var.list, OOB_rmse=OOB.rmse, OOB_mae=OOB.mae, OOB_bias=OOB.bias, Train_rmse=train.rmse, Train_mae=train.mae, Train_bias=train.bias)
# # write.csv(RFE_info, "Model_Output/THg/rf_RFE_info.csv", row.names = FALSE)
# 
# end.time <- Sys.time()
# end.time-start.time
# 
# 
# RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
# 
# RFE_info$Iteration <- 1:nrow(RFE_info)
# RFE_info$NumVars <- rev(RFE_info$Iteration)













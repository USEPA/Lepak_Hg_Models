# Generate predictions from D202-only model 


#################################################################################
### Refit final-final model on training+test set, imputed together
# Then generate predictions for full set of lakes
#################################################################################


library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0

output_dir <- "Model_Output/Iso_D202/"
model_dir <- "Saved_Models/Iso_D202/"
fig_dir <- "Figures/Iso_D202/"

Isos <- c("d202_Avg")
# # The processes governing D199 and D201 are the same. The processes governing D200 and D204 are the same. So please use d202, D199 and D200. 


# Read in imputed data for all 1112 lakes
All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
All_Dat$LOI_PERCENT <- All_Dat$LOI_PERCENT/100

# Subset lakes with iso data (combine original train+test sets) to train final-final model
unique(All_Dat$TrainTest)
Train_Dat <- All_Dat %>% filter(TrainTest=="Iso_Train" | TrainTest=="Iso_Test") # All 410 iso lakes

# Standardize all isos and remove original variables
Iso_stats <- data.frame(D202=c(NA, NA))
row.names(Iso_stats) <- c("Mean", "SD")

Iso_stats$D202 <- c(mean(Train_Dat$d202_Avg), sd(Train_Dat$d202_Avg))


# Standardize isos in train set
Train_Dat$D202 <- (Train_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]

# Standardize isos in full set using training set stats
All_Dat$D202 <- (All_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]




######### Refit final-final model #########
response_var <- c("D202")
mtry_par <- 1/3

# Read in best reduced model to get predictors
rf.reduced <- readRDS(paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))
preds <- rf.reduced$xvar.names
rm(rf.reduced)

Train_run <- Train_Dat[, colnames(Train_Dat) %in% c(preds, response_var)]

nump <- ncol(Train_run)-1    # Number predictors (subtracting responses)

# Fit final-final model  
set.seed(73) 
rf.final  <- rfsrc(D202 ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mse", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
# saveRDS(rf.final, paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))






All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


# Predict all lakes
rf_predict_All <- predict(rf.final, newdata=All_Dat_sub)

# Predicted isos in SD
All_Dat_sub <- All_Dat_sub %>% mutate(Pred_D202_SD = rf_predict_All$predicted)

# Obs isos in SD
All_Dat_sub$Obs_D202_SD <- All_Dat$D202

# Predicted isos in original units
All_Dat_sub$Pred_D202_origUnits <- ( All_Dat_sub$Pred_D202_SD * Iso_stats$D202[2] ) + Iso_stats$D202[1]

All_Dat_sub <- All_Dat_sub %>% rename(Obs_D202_origUnits=d202_Avg) %>% 
  relocate(c("Obs_D202_origUnits"), .after = last_col())

# Isos with _Avg are obs values in original units
# TrainTest == "Impute_Data" | "MissTHg" are lakes with iso predictions, no observed data


All_Dat_sub <- All_Dat_sub %>% rename(LakeRole = TrainTest)

All_Dat_sub$LakeRole[All_Dat_sub$LakeRole=="Iso_Train" | All_Dat_sub$LakeRole=="Iso_Test"] <- "Iso_Lake_Trained_Model"
All_Dat_sub$LakeRole[All_Dat_sub$LakeRole=="Impute_Data"] <- "New_Predicted_Lake"
All_Dat_sub$LakeRole[All_Dat_sub$LakeRole=="MissTHg"] <- "New_Predicted_Lake_MissingTHgOrLOI"

unique(All_Dat_sub$LakeRole)

Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83, HUC2, HUC8, Omernik_I, Omernik_II, Omernik_III)

All_Dat_sub <- left_join(All_Dat_sub, Lake_Geo)


write.csv(All_Dat_sub, paste0(output_dir, "D202_Predictions_All_Lakes_FINALFINALMOD_2024-01-24.csv"), row.names = F)


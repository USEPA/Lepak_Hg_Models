# Generate predictions from Isotope fingerprint model 

library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg")
# # The processes governing D199 and D201 are the same. The processes governing D200 and D204 are the same. So please use d202, D199 and D200. 
# Isos_Mod <- c("D199_Avg", "D200_Avg", "d202_Avg")

# Load imputed training data
Train_Dat <- read.csv("Formatted_Data/ISO_Imputed_Training_Data.csv") # 376
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100

# Standardize Isos in training set and remove original variables
Iso_stats <- data.frame(D199=c(NA, NA), D200=c(NA, NA), D202=c(NA, NA))
row.names(Iso_stats) <- c("Mean", "SD")

Iso_stats$D199 <- c(mean(Train_Dat$D199_Avg), sd(Train_Dat$D199_Avg))
Iso_stats$D200 <- c(mean(Train_Dat$D200_Avg), sd(Train_Dat$D200_Avg))
Iso_stats$D202 <- c(mean(Train_Dat$d202_Avg), sd(Train_Dat$d202_Avg))


# Read in all imputed data
All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
All_Dat$LOI_PERCENT <- All_Dat$LOI_PERCENT/100


# Standardize isos in test set using training set stats
All_Dat$D199 <- (All_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
All_Dat$D200 <- (All_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
All_Dat$D202 <- (All_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]


names(All_Dat)

# Read in reduced model
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))

preds <- rf.final$xvar.names

All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


# Predict all lakes
rf_predict_All <- predict(rf.final, newdata=All_Dat_sub)

# Predicted isos in SD
All_Dat_sub <- All_Dat_sub %>% mutate(Pred_D199_SD = rf_predict_All$regrOutput$D199$predicted, 
                                Pred_D200_SD = rf_predict_All$regrOutput$D200$predicted, 
                                Pred_D202_SD = rf_predict_All$regrOutput$D202$predicted)

# Obs isos in SD
All_Dat_sub$Obs_D199_SD <- All_Dat$D199
All_Dat_sub$Obs_D200_SD <- All_Dat$D200
All_Dat_sub$Obs_D202_SD <- All_Dat$D202

# Predicted isos in original units
All_Dat_sub$Pred_D199_origUnits <- ( All_Dat_sub$Pred_D199_SD * Iso_stats$D199[2] ) + Iso_stats$D199[1]
All_Dat_sub$Pred_D200_origUnits <- ( All_Dat_sub$Pred_D200_SD * Iso_stats$D200[2] ) + Iso_stats$D200[1]
All_Dat_sub$Pred_D202_origUnits <- ( All_Dat_sub$Pred_D202_SD * Iso_stats$D202[2] ) + Iso_stats$D202[1]

# Isos with _Avg are obs values in original units
# TrainTest == "Impute_Data" | "MissTHg" are lakes with iso predictions, no observed data

unique(All_Dat_sub$TrainTest)
names(All_Dat_sub)

Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83, HUC2, HUC8, Omernik_I, Omernik_II, Omernik_III)

All_Dat_sub <- left_join(All_Dat_sub, Lake_Geo)

write.csv(All_Dat_sub, paste0(output_dir, "Isotope_Predictions_All_Lakes.csv"), row.names = F)
          
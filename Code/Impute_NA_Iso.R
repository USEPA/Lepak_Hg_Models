library(readxl)
library(stringr)
library(missForest) # 1.5
library(doParallel)
library(tidyverse)



Data <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv")

Data[Data=="Not Assessed"] <- NA

# Recode Lakeshore_disturbance_cond_class as numerical (ordered factor)
Data$Lakeshore_disturbance_cond_class[Data$Lakeshore_disturbance_cond_class=="Poor"] <- 0
Data$Lakeshore_disturbance_cond_class[Data$Lakeshore_disturbance_cond_class=="Fair"] <- 1
Data$Lakeshore_disturbance_cond_class[Data$Lakeshore_disturbance_cond_class=="Good"] <- 2
Data$Lakeshore_disturbance_cond_class <- as.numeric(Data$Lakeshore_disturbance_cond_class)

# Recode Lakeshore_disturbance_cond_class as numerical (ordered factor)
Data$Lake_level_drawdown_exposure_cond[Data$Lake_level_drawdown_exposure_cond=="Small"] <- 0
Data$Lake_level_drawdown_exposure_cond[Data$Lake_level_drawdown_exposure_cond=="Medium"] <- 1
Data$Lake_level_drawdown_exposure_cond[Data$Lake_level_drawdown_exposure_cond=="Large"] <- 2
Data$Lake_level_drawdown_exposure_cond <- as.numeric(Data$Lake_level_drawdown_exposure_cond)

# Recode Trophic_state as numerical (ordered factor)
Data$Trophic_state[Data$Trophic_state=="Oligotrophic"] <- 0
Data$Trophic_state[Data$Trophic_state=="Mesotrophic"] <- 1
Data$Trophic_state[Data$Trophic_state=="Eutrophic"] <- 2
Data$Trophic_state[Data$Trophic_state=="Hypereutrophic"] <- 3
Data$Trophic_state <- as.numeric(Data$Trophic_state)

# Combine LAKE_ORIGIN12 categories into manmade and natural
table(Data$LAKE_ORIGIN12)
Data$LAKE_ORIGIN12[Data$LAKE_ORIGIN12=="MAN_MADE" | Data$LAKE_ORIGIN12=="MAN_MADE_AB" | Data$LAKE_ORIGIN12=="RESERVOIR"] <- 1
Data$LAKE_ORIGIN12[Data$LAKE_ORIGIN12=="NATURAL" | Data$LAKE_ORIGIN12=="NATURAL_ENH"] <- 0
Data$LAKE_ORIGIN12 <- as.numeric(Data$LAKE_ORIGIN12)

Data$log10THg <- log10(Data$STHG_ng_g)
Data$log10MeHg <- log10(Data$SMHG_ng_g)
Data$LOI_PERCENT <- Data$LOI_PERCENT/100


Final_THg_preds <- read.csv("Tables/Final_THg_preds_2023-01-10.csv")
names(Data)
Data %>% dplyr::select(Final_THg_preds$Variable)

# Iso preds are the same as THg & MeHg & LOI models except adding in those responses as predictors 
Iso_preds <- c(Final_THg_preds$Variable, "LOI_PERCENT", "log10THg", "log10MeHg")
#  *** May add in lat/longs but leave out for now. LAT_DD83, LON_DD83 ***
# Probably could have imputed preds previously using ecoregion or other spatial predictors?


Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg")

# Check categorical predictors for NA as character
# cat_preds <- Final_THg_preds %>% filter(Type=="character") %>% pull(Variable)
# unique(Data$LAKE_ORIGIN12)
# unique(Data$Lake_level_drawdown_exposure_cond) # Not Assessed should be NA
# unique(Data$Lakeshore_disturbance_cond_class) # Not Assessed should be NA
# unique(Data$Trophic_state) # Not Assessed should be NA



# NLA12_WY-R02 - MeHg exceeds THg
Data %>% filter(NLA12_ID=="NLA12_WY-R02") %>% dplyr::select(log10MeHg, log10THg)
hist(Data$log10MeHg)
hist(Data$log10THg)
# look <- Data %>% filter(NLA12_ID=="NLA12_WY-R02")
# sum(is.na(look))/453 # This one has 17% missing values


# Removed this one from MeHg analysis but kept in THg. Set MeHg to NA, extreme value likely inaccurate
Data$log10MeHg[Data$NLA12_ID=="NLA12_WY-R02"] <- NA
Data$SMHG_ng_g[Data$NLA12_ID=="NLA12_WY-R02"] <- NA


# Create 90-10 train-test split
sort(table(Data$Omernik_II))
# Sample 10% from each Omernik II greater than 10

# 36 lakes are missing THg or LOI (1 missing only LOI) - these could be used to help fill in predictor data though, and could predict values for these.

Lakes_wTHg_allVars <- Data %>% filter(!is.na(STHG_ng_g) &  !is.na(LOI_PERCENT)) # 1076 lakes
# Lakes_noTHg_allVars <- Data %>% filter(is.na(STHG_ng_g) | is.na(LOI_PERCENT)) # 36 (3.2% missing)

Lakes_wIso_allVars <- Data %>% filter(!is.na(d202_Avg)) # 410 
Lakes_THgNoIso_allVars <- Data %>% filter(!is.na(STHG_ng_g) &  !is.na(LOI_PERCENT) & is.na(d202_Avg)) # 666 lakes
Lakes_noTHg_allVars <- Data %>% filter(is.na(STHG_ng_g) | is.na(LOI_PERCENT)) # 36 (3.2% missing)



# Old test set selection for THg and MeHg
# sort(table(Lakes_wTHg_allVars$Omernik_II))
# ecoregions_origTrain <- unique(Lakes_wTHg_allVars$Omernik_II) # 19
# 
# set.seed(36)
# test.lakes.orig <- NA
# for (i in 1:length(ecoregions_origTrain)){
#   dat.i <- Lakes_wTHg_allVars %>% filter(Omernik_II==ecoregions_origTrain[i])
#   
#   if(floor(nrow(dat.i)/10)>0){
#     test.lakes.orig <- c(test.lakes.orig, sample(dat.i$NLA12_ID, size=floor(nrow(dat.i)/10), replace=F))
#   }
# }
# test.lakes.orig <- test.lakes.orig[-1] # 101



### New test set selection for Iso lakes
sort(table(Lakes_wIso_allVars$Omernik_II))
ecoregions <- unique(Lakes_wIso_allVars$Omernik_II) # 18

set.seed(36)
test.lakes <- NA
for (i in 1:length(ecoregions)){
  dat.i <- Lakes_wIso_allVars %>% filter(Omernik_II==ecoregions[i])
  
  if(floor(nrow(dat.i)/10)>0){
    test.lakes <- c(test.lakes, sample(dat.i$NLA12_ID, size=floor(nrow(dat.i)/10), replace=F))
  }
}
test.lakes <- test.lakes[-1] # 34

train.lakes.iso <- Lakes_wIso_allVars$NLA12_ID[!Lakes_wIso_allVars$NLA12_ID %in% test.lakes] # 376

# Denote Iso test and train for model, plus remaining data for imputing (divided by whether missing THg & LOI or not)
Data$TrainTest <- "Impute_Data"
Data$TrainTest[Data$NLA12_ID %in% test.lakes] <- "Iso_Test"
Data$TrainTest[Data$NLA12_ID %in% train.lakes.iso] <- "Iso_Train"
Data$TrainTest[is.na(Data$STHG_ng_g) | is.na(Data$LOI_PERCENT)] <- "MissTHg"

# Data$TrainTestOrig <- "Train"
# Data$TrainTestOrig[Data$NLA12_ID %in% test.lakes.orig.iso] <- "Iso_Test"
# Data$TrainTestOrig[is.na(Data$STHG_ng_g) | is.na(Data$LOI_PERCENT)] <- "MissTHg"



# Spatial distribution
ggplot(Data, aes(col=TrainTest, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()

# Similar catchment development
# ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumDevelopedLowCat)) + geom_histogram() + theme_minimal()
ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumDevelopedLowCat, after_stat(density))) + geom_freqpoly() + theme_minimal()
# ggplot(Data, aes(fill=TrainTest, col=TrainTest,x=SumForestCat)) + geom_histogram() + theme_minimal()
ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumForestCat, after_stat(density))) + geom_freqpoly() + theme_minimal()



##############################################################################
### Impute data using all data except iso test set ####
# Assess quality of categorical predictions - if poor remove
# Use imputed training set to fit models and do subset selection
# Assess performance on test set


# Impute data with all data except iso test set
Impute_dat <- Data %>% filter(TrainTest=="Iso_Train" | TrainTest=="Impute_Data" | TrainTest=="MissTHg") %>% dplyr::select(c("NLA12_ID",  Iso_preds, Isos, "TrainTest")) # 1078
Impute_dat_preds <- Impute_dat %>% dplyr::select(c(Iso_preds))
# Keep MissTHg - see below

# Impute data with Iso_Test
Impute_dat_wTEST <- Data %>% dplyr::select(c("NLA12_ID",  Iso_preds, Isos, "TrainTest")) # 1112
Impute_dat_preds_wTEST <- Impute_dat_wTEST %>% dplyr::select(c(Iso_preds))


# Look at iso train and test data to see how much missing and whether worth including lakes with missing THg data
# Test_dat <- Data %>% filter(TrainTest=="Iso_Test") %>% dplyr::select(c("NLA12_ID", Iso_preds, "TrainTest"))
# Test_dat_preds <- Test_dat %>% dplyr::select(c(Iso_preds))
Train_dat <- Data %>% filter(TrainTest=="Iso_Train") %>% dplyr::select(c("NLA12_ID",  Iso_preds, "TrainTest"))
Train_dat_preds <- Train_dat %>% dplyr::select(c(Iso_preds))

# Lakes with missing THg do have values of top missing predictors (see below Var_NA_iso) so will include them in imputation
# Miss_dat <- Data %>% filter(TrainTest=="MissTHg") %>% dplyr::select(c("NLA12_ID", Iso_preds, "TrainTest"))
# Miss_dat$Ave_CONDUCTIVITY_uS_cm    
# Miss_dat$MMI_BENT_NLA12
# Miss_dat$Ave_OXYGEN_mg_L    


# Need to create rules for removing lakes and predictors (remove if missing >10%?)

# Number NAs by variable
Var_NA <- data.frame(Variable=names(Impute_dat_preds))

for(i in 1:ncol(Impute_dat_preds)){
  Var_NA$NumNA[i] <- sum(is.na(Impute_dat_preds[,i]))
  Var_NA$Type[i] <- typeof(as.matrix(Impute_dat_preds[,i]))
  Var_NA$Sample_SD[i] <- sd(as.matrix(Impute_dat_preds[,i]), na.rm=T) # Use to standardize RMSE
}
Var_NA$PercentNA <- Var_NA$NumNA/nrow(Impute_dat_preds)
Var_NA %>% arrange(desc(PercentNA)) %>% head()
# Remove MMI_BENT_NLA12 (18%), Ave_CONDUCTIVITY_uS_cm (12%) ??
# Kept these before




# Number NAs by variable for just ISO training
Var_NA_iso <- data.frame(Variable=names(Train_dat_preds))
for(i in 1:ncol(Train_dat_preds)){
  Var_NA_iso$NumNA[i] <- sum(is.na(Train_dat_preds[,i]))
  Var_NA_iso$Type[i] <- typeof(as.matrix(Train_dat_preds[,i]))
  Var_NA_iso$Sample_SD[i] <- sd(as.matrix(Train_dat_preds[,i]), na.rm=T) # Use to standardize RMSE
}

Var_NA_iso$PercentNA <- Var_NA_iso$NumNA/nrow(Train_dat_preds)
Var_NA_iso %>% arrange(desc(PercentNA)) %>% head()
# Less missing data for isotope lakes
# Conductivity (11%), MMI_BENT_NLA12 (11%), Ave_OXYGEN_mg_L (4%)








# Actually preds are all numeric now, so unnecessary step
Impute_dat_preds[sapply(Impute_dat_preds, is.character)] <- lapply(Impute_dat_preds[sapply(Impute_dat_preds, is.character)],  as.factor)


# Don't include isotopes to impute predictors. Problematic for variable selection of best predictors.
registerDoParallel(cores=7)
set.seed(3)
Pred_Imp_Mod <- missForest(as.data.frame(Impute_dat_preds), verbose = TRUE, parallelize="variables", variablewise = TRUE, ntree=500, mtry = floor(ncol(Impute_dat_preds)/3))
saveRDS(Pred_Imp_Mod, "Saved_Models/ISO_missForest_TrainMiss_all127preds_500tr_mtryThird.rds")
# Pred_Imp_Mod <- readRDS("Saved_Models/ISO_missForest_TrainMiss_all127preds_500tr_mtryThird.rds")

names(Pred_Imp_Mod)

Imp_Errors <- data.frame(Variable=names(Impute_dat_preds), Error=Pred_Imp_Mod$OOBerror, Err_Type=names(Pred_Imp_Mod$OOBerror))


Imp_Errors <- left_join(Imp_Errors, Var_NA)

# Calculate NRMSE for continuous preds
Imp_Errors$NRMSE[Imp_Errors$Err_Type=="MSE"] <- sqrt(Imp_Errors$Error[Imp_Errors$Err_Type=="MSE"])/Imp_Errors$Sample_SD[Imp_Errors$Err_Type=="MSE"]

# Decent NRMSE for MMI_BENT_NLA12 (0.83) and Ave_CONDUCTIVITY_uS_cm (0.92), so will keep 


# Imputed training data
Impute_dat_preds_FILLED <- Pred_Imp_Mod$ximp
Impute_dat_preds_FILLED$NLA12_ID <- Impute_dat$NLA12_ID # Add ID back
Impute_dat_preds_FILLED$d202_Avg <- Impute_dat$d202_Avg # Add iso back
Impute_dat_preds_FILLED$D199_Avg <- Impute_dat$D199_Avg # Add iso back
Impute_dat_preds_FILLED$D200_Avg <- Impute_dat$D200_Avg # Add iso back
Impute_dat_preds_FILLED$D201_Avg <- Impute_dat$D201_Avg # Add iso back
Impute_dat_preds_FILLED$D204_Avg <- Impute_dat$D204_Avg # Add iso back
Impute_dat_preds_FILLED$TrainTest <- Impute_dat$TrainTest # Add Train ID back

unique(Impute_dat_preds_FILLED$TrainTest)


Impute_dat_preds_FILLED$Gas_Hg_Hg0Conc_ng_m3[1:20]
Impute_dat$Gas_Hg_Hg0Conc_ng_m3[1:20]


# 376 lakes for training
Imputed_Training_Set <- Impute_dat_preds_FILLED %>% filter(TrainTest=="Iso_Train") %>% dplyr::select(-TrainTest)

write.csv(Imputed_Training_Set, "Formatted_Data/ISO_Imputed_Training_Data.csv", row.names = F)

# Write which vars imputed
# Imputed_Vars <- Imp_Errors %>% filter(Error>0) %>% mutate(Impute="Y")
# Complete_Vars <- Imp_Errors %>% filter(Error==0) %>% mutate(Impute="N")
# write_Impute <- rbind(Imputed_Vars, Complete_Vars)
# write.csv(write_Impute, "Tables/List_Imputed_Training_Preds_THg_MHg.csv", row.names = F)





# Impute test data - have to run model again with test data
registerDoParallel(cores=7)
set.seed(3)
Pred_Imp_Mod_wTEST <- missForest(as.data.frame(Impute_dat_preds_wTEST), verbose = TRUE, parallelize="variables", variablewise = TRUE, ntree=500, mtry = floor(ncol(Impute_dat_preds_wTEST)/3))
saveRDS(Pred_Imp_Mod_wTEST, "Saved_Models/ISO_missForest_Test_all127preds_500tr_mtryThird.rds")
# Pred_Imp_Mod_wTEST <- readRDS("Saved_Models/ISO_missForest_Test_all127preds_500tr_mtryThird.rds")

# Imputed test data
Impute_dat_preds_TEST_FILLED <- Pred_Imp_Mod_wTEST$ximp
Impute_dat_preds_TEST_FILLED$NLA12_ID <- Impute_dat_wTEST$NLA12_ID # Add ID back
Impute_dat_preds_TEST_FILLED$d202_Avg <- Impute_dat_wTEST$d202_Avg # Add iso back
Impute_dat_preds_TEST_FILLED$D199_Avg <- Impute_dat_wTEST$D199_Avg # Add iso back
Impute_dat_preds_TEST_FILLED$D200_Avg <- Impute_dat_wTEST$D200_Avg # Add iso back
Impute_dat_preds_TEST_FILLED$D201_Avg <- Impute_dat_wTEST$D201_Avg # Add iso back
Impute_dat_preds_TEST_FILLED$D204_Avg <- Impute_dat_wTEST$D204_Avg # Add iso back
Impute_dat_preds_TEST_FILLED$TrainTest <- Impute_dat_wTEST$TrainTest # Add Train ID back


Impute_dat_preds_TEST_FILLED$Gas_Hg_Hg0Conc_ng_m3[1:20]
Impute_dat_wTEST$Gas_Hg_Hg0Conc_ng_m3[1:20]


# 101 lakes for testing
Imputed_Test_Set <- Impute_dat_preds_TEST_FILLED %>% filter(TrainTest=="Iso_Test") %>% dplyr::select(-TrainTest)
write.csv(Imputed_Test_Set, "Formatted_Data/ISO_Imputed_Test_Data.csv", row.names = F)



# # Write which vars imputed if different
# Imp_Errors_Test <- data.frame(Variable=names(Impute_dat_preds_wTEST), Error=Pred_Imp_Mod_wTEST$OOBerror, Err_Type=names(Pred_Imp_Mod_wTEST$OOBerror))
# 
# Imputed_Vars_Test <- Imp_Errors_Test %>% filter(Error>0) %>% mutate(Impute="Y")
# # Complete_Vars_Test <- Imp_Errors_Test %>% filter(Error==0) %>% mutate(Impute="N")
# # write_Impute <- rbind(Imputed_Vars, Complete_Vars)
# # write.csv(Imputed_Training_Set, "Tables/List_Imputed_Training_Preds_THg_MHg.csv", row.names = F)
# 
# # CHECK!!!
# Imputed_Vars_Test$Variable %in% Imputed_Vars$Variable
# Imputed_Vars$Variable %in% Imputed_Vars_Test$Variable
# # Same imputed vars - so no need to write again





# Remove lakes??


# hist(Lakes_wTHg$Ave_CONDUCTIVITY_uS_cm)
# 
# # Number NAs by lake
# Lake_NA <- data.frame(Lake=1:nrow(Lakes_wTHg))
# 
# for(i in 1:nrow(Lakes_wTHg)){
#   Lake_NA$NumNA_Of128[i] <- sum(is.na(Lakes_wTHg[i,]))
# }
# 
# sum(Lake_NA$NumNA_Of128>30)
# table(Lake_NA$NumNA_Of128)
# 
# Lake_NA$NumNA_Of128
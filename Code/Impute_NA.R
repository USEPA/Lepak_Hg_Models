library(readxl)
library(stringr)
library(missForest) # 1.5
library(doParallel)
library(tidyverse)


# o	Implement both random forest and elastic net analyses for THg and %MeHg
#   	Identify optimal predictor subsets and relative importance
#   	Assess predictive ability of reduced models
#   	Quantify/visualize predictor-response relationships and interactions
# o	Implement similar analysis for Hg isotopes
#   	Identify optimal predictor subsets for multivariate isotope response
#   	Understand influence/importance of predictors on each individual isotope


# R: randomForest, cforest, randomForestSRC for MVRF
# permimp 

# R: glmnet

# MV:	Identify which/how variable importance metrics will work with the multivariate approaches. Will likely want some aggregate measure of importance across all responses, as well as importance/influence of predictors on each individual response.
# Correlated predictors - functions for permuting groups of correlated preds, or conditional var impt (permimp)




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



Final_THg_preds <- read.csv("Tables/Final_THg_preds_2023-01-10.csv")
names(Data)
Data %>% dplyr::select(Final_THg_preds$Variable)


# Check categorical predictors for NA as character
# cat_preds <- Final_THg_preds %>% filter(Type=="character") %>% pull(Variable)
# 
# unique(THg_dat$LAKE_ORIGIN12)
# unique(THg_dat$Lake_level_drawdown_exposure_cond) # Not Assessed should be NA
# unique(THg_dat$Lakeshore_disturbance_cond_class) # Not Assessed should be NA
# unique(THg_dat$Trophic_state) # Not Assessed should be NA

# THg_dat$Lake_level_drawdown_exposure_cond[THg_dat$Lake_level_drawdown_exposure_cond=="Not Assessed"] <- NA
# THg_dat$Lakeshore_disturbance_cond_class[THg_dat$Lakeshore_disturbance_cond_class=="Not Assessed"] <- NA
# THg_dat$Trophic_state[THg_dat$Trophic_state=="Not Assessed"] <- NA




# Create 90-10 train-test split
sort(table(Data$Omernik_II))
# Sample 10% from each Omernik II greater than 5

# 36 lakes are missing THg or LOI (1 missing only LOI) - these could be used to help fill in predictor data though, and could predict values for these.

Lakes_wTHg_allVars <- Data %>% filter(!is.na(STHG_ng_g) &  !is.na(LOI_PERCENT)) # 1076 lakes
Lakes_noTHg_allVars <- Data %>% filter(is.na(STHG_ng_g) | is.na(LOI_PERCENT)) # 36 (3.2% missing)

sort(table(Lakes_wTHg_allVars$Omernik_II))
ecoregions <- unique(Lakes_wTHg_allVars$Omernik_II)

set.seed(36)
test.lakes <- NA
for (i in 1:length(ecoregions)){
  dat.i <- Lakes_wTHg_allVars %>% filter(Omernik_II==ecoregions[i])
  
  if(floor(nrow(dat.i)/10)>0){
  test.lakes <- c(test.lakes, sample(dat.i$NLA12_ID, size=floor(nrow(dat.i)/10), replace=F))
  }
}
test.lakes <- test.lakes[-1] # 101

# Test_Lake_Data <- Data %>% filter(NLA12_ID %in% test.lakes)
     
Data$TrainTest <- "Train"
Data$TrainTest[Data$NLA12_ID %in% test.lakes] <- "Test"
Data$TrainTest[is.na(Data$STHG_ng_g) | is.na(Data$LOI_PERCENT)] <- "MissTHg"


# Spatial distribution
ggplot(Data, aes(col=TrainTest, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()

# Similar catchment development
# ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumDevelopedLowCat)) + geom_histogram() + theme_minimal()
ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumDevelopedLowCat, after_stat(density))) + geom_freqpoly() + theme_minimal()
# ggplot(Data, aes(fill=TrainTest, col=TrainTest,x=SumForestCat)) + geom_histogram() + theme_minimal()
ggplot(Data, aes(fill=TrainTest, col=TrainTest, x=SumForestCat, after_stat(density))) + geom_freqpoly() + theme_minimal()



##############################################################################
### Impute data using just training data plus lakes without THg ####
# Assess quality of categorical predictions - if poor remove
# Use imputed training set to fit models and do subset selection
# Assess performance on test set


# Impute data with training set and lakes missing THg
Impute_dat <- Data %>% filter(TrainTest=="Train" | TrainTest=="MissTHg") %>% dplyr::select(c("NLA12_ID", "STHG_ng_g", "SMHG_ng_g", "LOI_PERCENT", Final_THg_preds$Variable, "TrainTest"))
Impute_dat_preds <- Impute_dat %>% dplyr::select(c(Final_THg_preds$Variable))

Impute_dat_wTEST <- Data %>% dplyr::select(c("NLA12_ID", "STHG_ng_g", "SMHG_ng_g", "LOI_PERCENT", Final_THg_preds$Variable, "TrainTest"))
Impute_dat_preds_wTEST <- Impute_dat_wTEST %>% dplyr::select(c(Final_THg_preds$Variable))

# Test_dat <- Data %>% filter(TrainTest=="Test") %>% dplyr::select(c("NLA12_ID", "STHG_ng_g", "SMHG_ng_g", Final_THg_preds$Variable))
# Test_dat_preds <- Test_dat %>% dplyr::select(c(Final_THg_preds$Variable))






# Need to create rules for removing lakes and predictors (remove if missing >10%?)

# Number NAs by variable
Var_NA <- data.frame(Variable=names(Impute_dat_preds))

for(i in 1:ncol(Impute_dat_preds)){
  Var_NA$NumNA_Of1011[i] <- sum(is.na(Impute_dat_preds[,i]))
  Var_NA$Type[i] <- typeof(as.matrix(Impute_dat_preds[,i]))
  Var_NA$Sample_SD[i] <- sd(as.matrix(Impute_dat_preds[,i]), na.rm=T) # Use to standardize RMSE
}

Var_NA$PercentNA <- Var_NA$NumNA_Of1011/1011
Var_NA %>% arrange(desc(PercentNA)) %>% head()
# Remove MMI_BENT_NLA12 (18%), Ave_CONDUCTIVITY_uS_cm (12%) ??
# Check accuracy first


# Actually preds are all numeric now, so unnecessary step
Impute_dat_preds[sapply(Impute_dat_preds, is.character)] <- lapply(Impute_dat_preds[sapply(Impute_dat_preds, is.character)],  as.factor)


# Don't use THg, MeHG, LOI to predict predictors. Seems problematic for variable selection of best predictors.
registerDoParallel(cores=8)
set.seed(3)
Pred_Imp_Mod <- missForest(as.data.frame(Impute_dat_preds), verbose = TRUE, parallelize="variables", variablewise = TRUE, ntree=500, mtry = floor(ncol(Impute_dat_preds)/3))
saveRDS(Pred_Imp_Mod, "Saved_Models/missForest_TrainMiss_all124preds_500tr_mtryThird.rds")
# Pred_Imp_Mod <- readRDS("Saved_Models/missForest_TrainMiss_all124preds_500tr_mtryThird.rds")

names(Pred_Imp_Mod)

Imp_Errors <- data.frame(Variable=names(Impute_dat_preds), Error=Pred_Imp_Mod$OOBerror, Err_Type=names(Pred_Imp_Mod$OOBerror))


Imp_Errors <- left_join(Imp_Errors, Var_NA)

# Calculate NRMSE for continuous preds
Imp_Errors$NRMSE[Imp_Errors$Err_Type=="MSE"] <- sqrt(Imp_Errors$Error[Imp_Errors$Err_Type=="MSE"])/Imp_Errors$Sample_SD[Imp_Errors$Err_Type=="MSE"]

# Decent NRMSE for MMI_BENT_NLA12 and Ave_CONDUCTIVITY_uS_cm, so will keep but re


# Imputed training data
Impute_dat_preds_FILLED <- Pred_Imp_Mod$ximp
Impute_dat_preds_FILLED$NLA12_ID <- Impute_dat$NLA12_ID # Add ID back
Impute_dat_preds_FILLED$STHG_ng_g <- Impute_dat$STHG_ng_g # Add THg back
Impute_dat_preds_FILLED$SMHG_ng_g <- Impute_dat$SMHG_ng_g # Add MHg back
Impute_dat_preds_FILLED$LOI_PERCENT <- Impute_dat$LOI_PERCENT # Add LOI back
Impute_dat_preds_FILLED$TrainTest <- Impute_dat$TrainTest # Add Train ID back

Impute_dat_preds_FILLED$Gas_Hg_Hg0Conc_ng_m3[1:20]
Impute_dat$Gas_Hg_Hg0Conc_ng_m3[1:20]


# 975 lakes for training
Imputed_Training_Set <- Impute_dat_preds_FILLED %>% filter(TrainTest=="Train") %>% dplyr::select(-TrainTest)

write.csv(Imputed_Training_Set, "Formatted_Data/THg_MHg_Imputed_Training_Data.csv", row.names = F)

# Write which vars imputed
Imputed_Vars <- Imp_Errors %>% filter(Error>0) %>% mutate(Impute="Y")
Complete_Vars <- Imp_Errors %>% filter(Error==0) %>% mutate(Impute="N")
write_Impute <- rbind(Imputed_Vars, Complete_Vars)
write.csv(write_Impute, "Tables/List_Imputed_Training_Preds_THg_MHg.csv", row.names = F)





# Impute test data - have to run model again with test data
registerDoParallel(cores=8)
set.seed(3)
Pred_Imp_Mod_wTEST <- missForest(as.data.frame(Impute_dat_preds_wTEST), verbose = TRUE, parallelize="variables", variablewise = TRUE, ntree=500, mtry = floor(ncol(Impute_dat_preds_wTEST)/3))
saveRDS(Pred_Imp_Mod_wTEST, "Saved_Models/missForest_Test_all124preds_500tr_mtryThird.rds")
# Pred_Imp_Mod_wTEST <- readRDS("Saved_Models/missForest_Test_all124preds_500tr_mtryThird.rds")

# Imputed test data
Impute_dat_preds_TEST_FILLED <- Pred_Imp_Mod_wTEST$ximp
Impute_dat_preds_TEST_FILLED$NLA12_ID <- Impute_dat_wTEST$NLA12_ID # Add ID back
Impute_dat_preds_TEST_FILLED$STHG_ng_g <- Impute_dat_wTEST$STHG_ng_g # Add THg back
Impute_dat_preds_TEST_FILLED$SMHG_ng_g <- Impute_dat_wTEST$SMHG_ng_g # Add MHg back
Impute_dat_preds_TEST_FILLED$LOI_PERCENT <- Impute_dat_wTEST$LOI_PERCENT # Add LOI back
Impute_dat_preds_TEST_FILLED$TrainTest <- Impute_dat_wTEST$TrainTest # Add Train ID back


Impute_dat_preds_TEST_FILLED$Gas_Hg_Hg0Conc_ng_m3[1:20]
Impute_dat_wTEST$Gas_Hg_Hg0Conc_ng_m3[1:20]


# 101 lakes for testing
Imputed_Test_Set <- Impute_dat_preds_TEST_FILLED %>% filter(TrainTest=="Test") %>% dplyr::select(-TrainTest)
write.csv(Imputed_Test_Set, "Formatted_Data/THg_MHg_Imputed_Test_Data.csv", row.names = F)



# Write which vars imputed if different
Imp_Errors_Test <- data.frame(Variable=names(Impute_dat_preds_wTEST), Error=Pred_Imp_Mod_wTEST$OOBerror, Err_Type=names(Pred_Imp_Mod_wTEST$OOBerror))

Imputed_Vars_Test <- Imp_Errors_Test %>% filter(Error>0) %>% mutate(Impute="Y")
# Complete_Vars_Test <- Imp_Errors_Test %>% filter(Error==0) %>% mutate(Impute="N")
# write_Impute <- rbind(Imputed_Vars, Complete_Vars)
# write.csv(Imputed_Training_Set, "Tables/List_Imputed_Training_Preds_THg_MHg.csv", row.names = F)

# CHECK!!!
Imputed_Vars_Test$Variable %in% Imputed_Vars$Variable
Imputed_Vars$Variable %in% Imputed_Vars_Test$Variable
# Same imputed vars - so no need to write again





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
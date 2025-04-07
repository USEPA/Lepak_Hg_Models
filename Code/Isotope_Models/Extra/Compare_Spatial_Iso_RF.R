# Compare THg model w/ LOI WITH and WITHOUT lat/long

library(randomForestSRC) # 3.2.1
library(colorspace)
# library(usmap)
# library(sp) # 1.6-0
library(tidyverse) # tidyverse_2.0.0


# Vignette here: https://www.randomforestsrc.org/articles/mvsplit.html
# Cite this vignette as
# H. Ishwaran, F. Tang, M. Lu, and U. B. Kogalur. 2021. “randomForestSRC: multivariate splitting rule vignette.” http://randomforestsrc.org/articles/mvsplit.html.

# library(remotes)
# install_version(
#   "randomForestSRC",
#   version = "3.2.1")

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(output_dir))
dir.create(paste0(model_dir))
dir.create(paste0(fig_dir))


# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/ISO_Imputed_Training_Data.csv") # 376
Test_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data.csv") # 34

# Lake spatial data
Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv")  %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83)


Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg")
Isos_Mod <- c("D199_Avg", "D200_Avg", "d202_Avg")

Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100

names(Train_Dat)


# Standardize Isos in training set and remove original variables
Iso_stats <- data.frame(D199=c(NA, NA), D200=c(NA, NA), D202=c(NA, NA))
row.names(Iso_stats) <- c("Mean", "SD")

Iso_stats$D199 <- c(mean(Train_Dat$D199_Avg), sd(Train_Dat$D199_Avg))
Iso_stats$D200 <- c(mean(Train_Dat$D200_Avg), sd(Train_Dat$D200_Avg))
Iso_stats$D202 <- c(mean(Train_Dat$d202_Avg), sd(Train_Dat$d202_Avg))

# Standardize isos in train set
Train_Dat$D199 <- (Train_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
Train_Dat$D200 <- (Train_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
Train_Dat$D202 <- (Train_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]

# Standardize isos in test set using training set stats
Test_Dat$D199 <- (Test_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
Test_Dat$D200 <- (Test_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
Test_Dat$D202 <- (Test_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]



Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -all_of(Isos)) 


set.seed(36)
p <- ncol(Train_Dat_run)-3 # 127 preds - includes LOI, THg, MeHg
mvrf.all <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_Dat_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=floor(p/3), ntree=5000, block.size=1, nodesize = 1)

# saveRDS(mvrf.all, paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36.rds"))
mvrf.all <- readRDS(paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36.rds"))

# Avg OOB RMSE
sqrt(mean(get.mv.error(mvrf.all)))
# 0.7258042
# OOB RMSE for each response
sqrt(get.mv.error(mvrf.all))
# D199      D200      D202 
# 0.5807113 0.8042107 0.7722661
# R2 each response
1- get.mv.error(mvrf.all)
# 0.6627744 0.3532451 0.4036051 

# Plot permutation importance for full model
v <- as.data.frame(get.mv.vimp(mvrf.all,  standardize = TRUE))
## compare standardized VIMP for top 25 variables
imp <- data.frame(Predictor=row.names(v), 
                  D199_Importance = 100*v$D199,
                  D200_Importance = 100*v$D200,
                  D202_Importance = 100*v$D202, 
                  Avg_Importance = 100*rowMeans(v))
row.names(imp) <- NULL
imp <- imp %>% arrange(desc(Avg_Importance)) 

# Change variable names for plotting
imp$Predictor_orig <- imp$Predictor

imp$Predictor[imp$Predictor=="SECCHI_m_use_depth_for_clear_to_bottom"] <- "SECCHI_m"
imp$Predictor[imp$Predictor=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
imp$Predictor[imp$Predictor=="WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s"] <- "WetLossLS"

imp$Predictor <- factor(imp$Predictor, levels=rev(imp$Predictor))

imp  %>% arrange(desc(Avg_Importance))  %>%
  slice(1:50) %>%
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>%
  ggplot(aes(x=Predictor, y=Avg_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() 

imp$D202_Rank <- rank(-imp$D202_Importance)
imp$D199_Rank <- rank(-imp$D199_Importance)
imp$D200_Rank <- rank(-imp$D200_Importance)
imp$Avg_Rank <- rank(-imp$Avg_Importance)

head(imp, 10)

# WetLossConv is subbed in for LAKE_ORIGIN12 in reduced model, other top 10 predictors are the same



#### Fit spatial version #######
Train_Dat_run_sp <- Train_Dat %>% left_join(Lake_Geo) %>% dplyr::select(-NLA12_ID, -all_of(Isos)) 

set.seed(36)
p_sp <- ncol(Train_Dat_run_sp)-3 # 129 preds 
mvrf.all.sp <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_Dat_run_sp, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=floor(p_sp/3), ntree=5000, block.size=1, nodesize = 1)

saveRDS(mvrf.all.sp, paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36_SP.rds"))
# mvrf.all.sp <- readRDS(paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36.rds"))


# Avg OOB RMSE
sqrt(mean(get.mv.error(mvrf.all.sp)))
# 0.7202933 (was 0.7258042)
# OOB RMSE for each response
sqrt(get.mv.error(mvrf.all.sp))
# D199      D200      D202 
# 0.5715525 0.8053346 0.7623851
# old
# 0.5807113 0.8042107 0.7722661
# R2 each response
1- get.mv.error(mvrf.all.sp)
# 0.6733277 0.3514361 0.4187690 
# Old
# 0.6627744 0.3532451 0.4036051 

# Plot permutation importance for full model
v <- as.data.frame(get.mv.vimp(mvrf.all.sp,  standardize = TRUE))
## compare standardized VIMP for top 25 variables
imp_sp <- data.frame(Predictor=row.names(v), 
                  D199_Importance = 100*v$D199,
                  D200_Importance = 100*v$D200,
                  D202_Importance = 100*v$D202, 
                  Avg_Importance = 100*rowMeans(v))
row.names(imp_sp) <- NULL
imp_sp <- imp_sp %>% arrange(desc(Avg_Importance)) 

# Change variable names for plotting
imp_sp$Predictor_orig <- imp_sp$Predictor

imp_sp$Predictor[imp_sp$Predictor=="SECCHI_m_use_depth_for_clear_to_bottom"] <- "SECCHI_m"
imp_sp$Predictor[imp_sp$Predictor=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
imp_sp$Predictor[imp_sp$Predictor=="WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s"] <- "WetLossLS"

imp_sp$Predictor <- factor(imp_sp$Predictor, levels=rev(imp_sp$Predictor))

imp_sp  %>% arrange(desc(Avg_Importance))  %>%
  slice(1:50) %>%
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>%
  ggplot(aes(x=Predictor, y=Avg_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() 

imp_sp$D202_Rank <- rank(-imp_sp$D202_Importance)
imp_sp$D199_Rank <- rank(-imp_sp$D199_Importance)
imp_sp$D200_Rank <- rank(-imp_sp$D200_Importance)
imp_sp$Avg_Rank <- rank(-imp_sp$Avg_Importance)

head(imp_sp, 10)

# WetLossConv is subbed in for LAKE_ORIGIN12 in reduced model, other top 10 predictors are the same

# Get OOB predictions
rf_predict_Test <- predict(mvrf.all.sp, newdata=Test_Dat)

# Add predictions to Test set and write out
Test_Dat <- Test_Dat %>% mutate(Pred_D199 = rf_predict_Test$regrOutput$D199$predicted, 
                                Pred_D200 = rf_predict_Test$regrOutput$D200$predicted, 
                                Pred_D202 = rf_predict_Test$regrOutput$D202$predicted)


Train_plot <- Train_Dat_run_sp

Train_plot$Residual_D199_sp <- Train_plot$D199 - mvrf.all.sp$regrOutput$D199$predicted.oob # CV pred
Train_plot$Residual_D200_sp <- Train_plot$D200 - mvrf.all.sp$regrOutput$D200$predicted.oob # CV pred
Train_plot$Residual_D202_sp <- Train_plot$D202 - mvrf.all.sp$regrOutput$D202$predicted.oob # CV pred

Train_plot$Residual_D199 <- Train_plot$D199 - mvrf.all$regrOutput$D199$predicted.oob # CV pred
Train_plot$Residual_D200 <- Train_plot$D200 - mvrf.all$regrOutput$D200$predicted.oob # CV pred
Train_plot$Residual_D202 <- Train_plot$D202 - mvrf.all$regrOutput$D202$predicted.oob # CV pred


library(vegan)

# D199
dists <- as.matrix(dist(cbind(Train_plot$LAT_DD83, Train_plot$LON_DD83)))
dist_res <- as.matrix(dist(Train_plot$Residual_D199_sp))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test # r=0.04814 , p=.03 There is slight spatial autocorrelation in residuals
# spatial: r: 0.04597, p=.01

# D200
dists <- as.matrix(dist(cbind(Train_plot$LAT_DD83, Train_plot$LON_DD83)))
dist_res <- as.matrix(dist(Train_plot$Residual_D200_sp))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test # r=-0.01869 , p=.82 No spatial autocorrelation in residuals
# spatial: -0.01444, p=0.74

# D202
dists <- as.matrix(dist(cbind(Train_plot$LAT_DD83, Train_plot$LON_DD83)))
dist_res <- as.matrix(dist(Train_plot$Residual_D202_sp))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test # r=0.12 , p=0.001 There is slight spatial autocorrelation in residuals
# spatial: r=.11, p=.001

# Including space did not reduce spatial autocorrelation in residuals

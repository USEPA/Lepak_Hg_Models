library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

# Predictions with spatial data and SKATER Clusters
Preds_with_Clusters <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
Preds_with_Clusters <- Preds_with_Clusters %>% rename(WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s=WetLossConv)
# This has LakeRole in it as well and can be joined


## *** Note that LOI was unnecessarily divided by 100 an extra time in isotope models, which is how it appears in Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv, and is how it should be input into models and for generating the PDPs
## But for visualizations it should be multiplied by 100 so that it is a proportion for better interpretation






# Read in imputed data for all 1112 lakes (created in Impute_NA_Iso.R)
# All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
# All_Dat$LOI_PERCENT <- All_Dat$LOI_PERCENT/100

# Subset lakes with iso data (combine original train+test sets) to train final-final model
# unique(Preds_with_Clusters$LakeRole)
Train_Dat <- Preds_with_Clusters %>% filter(LakeRole=="Iso_Lake_Trained_Model") # All 410 iso lakes

# Get original iso lake parameters for backtransforming from model
Iso_stats <- data.frame(D199=c(NA, NA), D200=c(NA, NA), D202=c(NA, NA))
row.names(Iso_stats) <- c("Mean", "SD")

Iso_stats$D199 <- c(mean(Train_Dat$Obs_D199_origUnits ), sd(Train_Dat$Obs_D199_origUnits ))
Iso_stats$D200 <- c(mean(Train_Dat$Obs_D200_origUnits), sd(Train_Dat$Obs_D200_origUnits))
Iso_stats$D202 <- c(mean(Train_Dat$Obs_D202_origUnits ), sd(Train_Dat$Obs_D202_origUnits ))
#             D199       D200       D202
# Mean -0.01110351 0.07362958 -0.8311328
# SD    0.21764874 0.04886975  0.2998654
# Nearly the same original training set

# Don't need these steps - this was for fitting model
# Standardize isos in train set
# Train_Dat$D199 <- (Train_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
# Train_Dat$D200 <- (Train_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
# Train_Dat$D202 <- (Train_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]
# 
# # Standardize isos in full set using training set stats
# All_Dat$D199 <- (All_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
# All_Dat$D200 <- (All_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
# All_Dat$D202 <- (All_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]



# Model trained on all lakes with iso data

# rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))

preds <- rf.final$xvar.names # predictors
# Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg") # original observed isos
# All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


pdp_dat <- Preds_with_Clusters %>% dplyr::select(NLA12_ID, all_of(preds))

# Here: Compute average prediction across all lakes at each x-value on an x-grid

# Use same limits as original figures?

# Try precip
y <- pdp_dat$Precip8110Cat



# Code from pdp
library(foreach)

pred.var <- "Precip8110Cat"

pred.val <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE),
                length = 40)

pred.grid <- expand.grid(pred.val, KEEP.OUT.ATTRS = FALSE) 
names(pred.grid) <- pred.var
pred.grid

# Serial - faster, likely due to overhead of creating clusters?
start.time <- Sys.time()

Iso_Mean_Preds <- NULL
for(i in seq_len(nrow(pred.grid))){
  temp <- pdp_dat
  
  # replace variable of interest with single value of interest
  temp[, pred.var] <- pred.grid[i, pred.var] 
  rf_predict_All <- predict(rf.final, newdata=temp)
  
  # Predicted isos in SD
  iso_preds_temp <- data.frame(Pred_D199_SD = mean(rf_predict_All$regrOutput$D199$predicted), 
                               Pred_D200_SD = mean(rf_predict_All$regrOutput$D200$predicted),
                               Pred_D202_SD = mean(rf_predict_All$regrOutput$D202$predicted))
  
  Iso_Mean_Preds <- rbind(Iso_Mean_Preds, iso_preds_temp)
}

end.time <- Sys.time()
end.time-start.time # 14.79553 secs


# Parallelizing likely only makes sense for bivariate PDPs

# library(foreach)
# library(doParallel)
# library(parallel)
# 
# cl <- makeCluster(10) 
# doParallel::registerDoParallel(cl)
# start.time <- Sys.time()
# Iso_Mean_Preds2 <- foreach(i = seq_len(nrow(pred.grid)), .combine = rbind) %dopar% {
#     temp <- pdp_dat
#   
#   # replace variable of interest with single value of interest
#   temp[, pred.var] <- pred.grid[i, pred.var] 
#   rf_predict_All <- predict(rf.final, newdata=temp)
#   
#   # Predicted isos in SD
#   iso_preds_temp <- data.frame(Pred_D199_SD = mean(rf_predict_All$regrOutput$D199$predicted), 
#                                Pred_D200_SD = mean(rf_predict_All$regrOutput$D200$predicted),
#                                Pred_D202_SD = mean(rf_predict_All$regrOutput$D202$predicted))
#   
#   # Iso_Mean_Preds <- rbind(Iso_Mean_Preds, iso_preds_temp)
#   iso_preds_temp
# }
# 
# 
# end.time <- Sys.time()
# end.time-start.time # 25.67474 secs
# 
# stopCluster(cl)

head(Iso_Mean_Preds2)
head(Iso_Mean_Preds)


# Same out to 14 decimal places
round(Iso_Mean_Preds2$Pred_D199_SD, 14) == round(Iso_Mean_Preds$Pred_D199_SD, 14)

# This should bind 3 yhat columns to pred.grid
res <- cbind(pred.grid, Iso_Mean_Preds)

# Predicted isos in original units
res$Pred_D199_origUnits <- ( res$Pred_D199_SD * Iso_stats$D199[2] ) + Iso_stats$D199[1]
res$Pred_D200_origUnits <- ( res$Pred_D200_SD * Iso_stats$D200[2] ) + Iso_stats$D200[1]
res$Pred_D202_origUnits <- ( res$Pred_D202_SD * Iso_stats$D202[2] ) + Iso_stats$D202[1]



# For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 

# if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT

par(mfrow=c(3,1))
plot(Pred_D199_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
plot(Pred_D200_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
plot(Pred_D202_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
# dev.off()


# , xlim = range(Preds_with_Clusters[pred.var]), ylim = range(Preds_with_Clusters$Pred_D202_origUnits)



########
final.preds <- rev(final.preds)
cat(final.preds, sep = "\n")

# Single variable partial dependence plots
for(i in 1:length(final.preds)){
  
  pred_dat <- Train_run %>% dplyr::select(final.preds[i])
  
  lim.i <- round(quantile(pred_dat[,1], probs=c(0.05, 0.95)), 4)
  
  grid.i <- data.frame(PlaceHold=seq(lim.i[1],lim.i[2], (lim.i[2]-lim.i[1])/20))
  names(grid.i) <- names(pred_dat)
  
  # partial1 <- partial(rf.final, pred.var=paste0(final.preds[i]), quantiles=T, probs=seq(0.05, 0.95, 0.05))
  partial1 <- partial(rf.final, pred.var=paste0(final.preds[i]),  pred.grid = grid.i)
  
  pred_lab <- final.preds[i]
  if(pred_lab == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred_lab <- "WetLossConv"
  if(pred_lab == "WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s") pred_lab <- "WetLossLS"
  
  saveRDS(partial1, paste0(output_dir, "PDP/", paste0(pred_lab), "_PDP.rds"))
  
  print(pred_lab)
  
  autoplot(partial1, size=1.2) + theme_minimal() + xlab(paste0(pred_lab)) + ylab("log10THg") +
    theme(text=element_text(size=20))  #+
  # scale_x_continuous(breaks=seq(-2,6,2)) #+
  ggsave(paste0(fig_dir, "PDP/PDP_", pred_lab, ".png"), width=7, height=5)
}
###############














# Predict all lakes
rf_predict_All <- predict(rf.final, newdata=All_Dat_sub)

# Predicted isos in SD
All_Dat_sub <- All_Dat_sub %>% mutate(Pred_D199_SD = rf_predict_All$regrOutput$D199$predicted, 
                                      Pred_D200_SD = rf_predict_All$regrOutput$D200$predicted, 
                                      Pred_D202_SD = rf_predict_All$regrOutput$D202$predicted)


# Predicted isos in original units
All_Dat_sub$Pred_D199_origUnits <- ( All_Dat_sub$Pred_D199_SD * Iso_stats$D199[2] ) + Iso_stats$D199[1]
All_Dat_sub$Pred_D200_origUnits <- ( All_Dat_sub$Pred_D200_SD * Iso_stats$D200[2] ) + Iso_stats$D200[1]
All_Dat_sub$Pred_D202_origUnits <- ( All_Dat_sub$Pred_D202_SD * Iso_stats$D202[2] ) + Iso_stats$D202[1]





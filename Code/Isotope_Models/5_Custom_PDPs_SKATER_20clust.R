library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "PDP_SKATER20/D202/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/D199/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/D200/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/All3/"), recursive=T, showWarnings = FALSE) 



# Predictions with spatial data and SKATER Clusters
Preds_with_Clusters <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
# Rename WetLossConv back to original name for prediction
Preds_with_Clusters <- Preds_with_Clusters %>% rename(WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s=WetLossConv)
# This has LakeRole in it as well and can be joined


## *** Note that LOI was unnecessarily divided by 100 an extra time in isotope models, which is how it appears in Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv, and is how it should be input into models ** AND FOR GENERATING THE PDPs ** ****
## But for visualizations it should be multiplied by 100 so that it is a proportion for better interpretation

names(Preds_with_Clusters)
# plot(Pred_D199_SD~Pred_D200_SD, data=Preds_with_Clusters)
# plot(Pred_D199_SD~Pred_D202_SD, data=Preds_with_Clusters)
# plot(Pred_D200_SD~Pred_D202_SD, data=Preds_with_Clusters)
# 
# plot(Obs_D199_SD~Obs_D200_SD, data=Preds_with_Clusters)
# plot(Obs_D199_SD~Obs_D202_SD, data=Preds_with_Clusters)
# plot(Obs_D200_SD~Obs_D202_SD, data=Preds_with_Clusters)


# Read in imputed data for all 1112 lakes (created in Impute_NA_Iso.R)
# All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
# All_Dat$LOI_PERCENT <- All_Dat$LOI_PERCENT/100

# Subset lakes with iso data (combine original train+test sets) used to train final-final model
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



# Model trained on all lakes with iso data

# rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))

preds <- rf.final$xvar.names # predictors
# Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg") # original observed isos
# All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


pdp_dat <- Preds_with_Clusters %>% dplyr::select(NLA12_ID, all_of(preds))



# Modifying code from pdp package
for(j in 1:length(preds)){
  print(j)
  
  pred.var <- preds[j] #"Precip8110Cat"
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  dir.create(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  
  x <- pdp_dat[, pred.var]
  
  # lim.j <- round(quantile(x, probs=c(0.05, 0.95)), 6)
  lim.j <- round(quantile(x, probs=c(0, 1)), 6)
  
  pred.val <- seq(lim.j[1],lim.j[2], (lim.j[2]-lim.j[1])/40)
  names(pred.val) <- names(pred.var)
  
  # pred.val <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                  # length = 40)
  
  pred.grid <- expand.grid(pred.val, KEEP.OUT.ATTRS = FALSE) 
  names(pred.grid) <- pred.var
  # pred.grid
  
  # start.time <- Sys.time()
  
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
  
  # end.time <- Sys.time()
  # end.time-start.time # 14.79553 secs
  
  
  # This should bind 3 yhat columns to pred.grid
  res <- cbind(pred.grid, Iso_Mean_Preds)
  
  # Predicted isos in original units
  res$Pred_D199_origUnits <- ( res$Pred_D199_SD * Iso_stats$D199[2] ) + Iso_stats$D199[1]
  res$Pred_D200_origUnits <- ( res$Pred_D200_SD * Iso_stats$D200[2] ) + Iso_stats$D200[1]
  res$Pred_D202_origUnits <- ( res$Pred_D202_SD * Iso_stats$D202[2] ) + Iso_stats$D202[1]
  
  
  
  # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
  if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
  

    
  # par(mfrow=c(3,1))
  # plot(Pred_D199_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
  # plot(Pred_D200_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
  # plot(Pred_D202_origUnits~Precip8110Cat, data=res, type="l", las=1, lwd=3)
  # 
  # plot(Pred_D199_origUnits~get(pred.var) , xlab=paste0(pred.var), data=res, type="l", las=1, lwd=3)
  # plot(Pred_D200_origUnits~get(pred.var), xlab=paste0(pred.var) , data=res, type="l", las=1, lwd=3)
  # plot(Pred_D202_origUnits~get(pred.var), xlab=paste0(pred.var) , data=res, type="l", las=1, lwd=3)
  

  res %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_line(size=2) + 
    xlab(pred.var.lab) +
    coord_cartesian(xlim = range(res[pred.var]), ylim = range(res$Pred_D199_origUnits)) 
  ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  
  res %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_line(size=2) + 
    xlab(pred.var.lab) +
    coord_cartesian(xlim = range(res[pred.var]), ylim = range(res$Pred_D200_origUnits)) 
  ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  
  res %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_line(size=2) + 
    # scale_shape_manual(values = c(4, 1))+ 
    # geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    # geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    # scale_linetype_manual(name="Type", 
                          # values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          # breaks=c("Artificial", "Natural")) +
    xlab(pred.var.lab) +
    coord_cartesian(xlim = range(res[pred.var]), ylim = range(res$Pred_D202_origUnits)) 
    # theme(legend.key.size = unit(2,"line"))
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  

  # All 3 together on SD scale
  res_long <- pivot_longer(res, cols ="Pred_D199_SD":"Pred_D202_origUnits", names_to = "Isotope", values_to = "Mean_Prediction")
  
  res_long %>% filter(Isotope %in% c("Pred_D199_SD", "Pred_D200_SD", "Pred_D202_SD")) %>% 
    ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_line(size=2) + 
    xlab(pred.var.lab) +
    coord_cartesian(xlim = range(res[pred.var]))
  # , ylim = range(res$Pred_D202_origUnits)
  # theme(legend.key.size = unit(2,"line"))
  ggsave(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab, "/PDP_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  # res_long %>% filter(Isotope %in% c("Pred_D199_origUnits", "Pred_D200_origUnits", "Pred_D202_origUnits")) %>% 
  #   ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
  #   theme_minimal() +
  #   theme(text=element_text(size=20)) +
  #   geom_line(size=2) + 
  #   xlab(pred.var.lab) +
  #   coord_cartesian(xlim = range(res[pred.var]))
  
}

# Use same limits as original figures? 
# Much narrower range than observed lakes

# , xlim = range(Preds_with_Clusters[pred.var]), ylim = range(Preds_with_Clusters$Pred_D202_origUnits)


# Serial - faster, likely due to overhead of creating clusters?

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

# head(Iso_Mean_Preds2)
# head(Iso_Mean_Preds)
# 
# 
# # Same out to 14 decimal places
# round(Iso_Mean_Preds2$Pred_D199_SD, 14) == round(Iso_Mean_Preds$Pred_D199_SD, 14)


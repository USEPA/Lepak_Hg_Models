library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0
library(colorspace)
# library(foreach)
# library(doParallel)
# library(parallel)
library(ggpubr)
library(kernelshap) # 0.7.0
library(shapviz) # 0.9.6
library(patchwork)


# library(showtext)
# library(sysfonts)
# font_add("Calibri", "C:/Windows/Fonts/calibri.ttf")
# showtext_auto()

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "SHAP"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(output_dir, "SHAP"), recursive=T, showWarnings = FALSE) 


# Predictions with spatial data and SKATER Clusters
Preds_with_Clusters <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
# Rename WetLossConv back to original name for prediction
Preds_with_Clusters <- Preds_with_Clusters %>% rename(WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s=WetLossConv)
# This has LakeRole in it as well and can be joined


## *** Note that LOI was unnecessarily divided by 100 an extra time in isotope models (in 3_Predict_ISO_MVRF_subset.R), which is how it appears in Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv, and is how it should be input into models AND FOR GENERATING THE PDPs ****
## But for visualizations it should be multiplied by 100 so that it is a proportion for better interpretation



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
# These are the same stats used to standardize training iso set for rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds 
# Only used to backtransform here

# Standardize isos in train set
Train_Dat$D199 <- (Train_Dat$Obs_D199_origUnits-Iso_stats$D199[1]) / Iso_stats$D199[2]
Train_Dat$D200 <- (Train_Dat$Obs_D200_origUnits-Iso_stats$D200[1]) / Iso_stats$D200[2]
Train_Dat$D202 <- (Train_Dat$Obs_D202_origUnits-Iso_stats$D202[1]) / Iso_stats$D202[2]

mtry_par <- 1/3

# Fit final-final model  
set.seed(73) 
# rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))

response_var <- c("D199", "D200", "D202")
preds <- rf.final$xvar.names # predictors
Train_run <- Train_Dat[, colnames(Train_Dat) %in% c(preds, response_var)]
nump <- ncol(Train_run)-3    # Number predictors (subtracting responses)

# Refit final model with only 2000 trees to reduce calculation time
set.seed(73) 
rf.final2000  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance=FALSE, splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=2000, block.size=1, nodesize = 1)





train_dat <- Train_Dat %>% dplyr::select(NLA12_ID, Maha20, all_of(preds))
all_dat <- Preds_with_Clusters %>% dplyr::select(NLA12_ID, Maha20, all_of(preds))



pred_fun <- function(m, x) {
  pred <- predict(m, newdata=x)$regrOutput
  sapply(pred, `[[`, "predicted")
}

pred_fun(rf.final2000, head(train_dat))


# mod1 <- rfsrc(Multivar(mpg, wt ) ~ ., data = mtcars, importance = TRUE)
# x <- c("cyl", "disp", "hp", "drat", "qsec", "vs", "am", "gear", "carb")

# Shap values for just isotope lakes
# set.seed(38) 
# shap_values <- kernelshap(
#   rf.final2000, 
#   X = train_dat[preds], 
#   bg_n=100,
#   # bg_X = train_dat,  # Usually small random sample
#   pred_fun = pred_fun
# )
# # saveRDS(shap_values, paste0(output_dir, "SHAP/shap_traindat_100bg_2000tr.rds"))
# shap_values_train <- readRDS(paste0(output_dir, "SHAP/shap_traindat_100bg_2000tr.rds"))

# Shap values for all lakes - use this one
set.seed(38) 
shap_values_all <- kernelshap(
  rf.final2000, 
  X = all_dat[preds], 
  bg_n=200,
  # bg_X = train_dat,  # Usually small random sample
  pred_fun = pred_fun
)
# saveRDS(shap_values_all, paste0(output_dir, "SHAP/shap_alldat_200bg_2000tr.rds"))
shap_values_all <- readRDS(paste0(output_dir, "SHAP/shap_alldat_200bg_2000tr.rds"))



# sv.train <- shapviz(shap_values_train)
# 
# # names(sv.train) <- response_var  # Not yet automatically
# colnames(sv.train$D199)[colnames(sv.train$D199)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
# colnames(sv.train$D200)[colnames(sv.train$D200)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
# colnames(sv.train$D202)[colnames(sv.train$D202)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
# 
# # Mean absolute shap values by predictor for each isotope and overall
# m199 <- colMeans(abs(sv.train$D199$S))
# m200 <- colMeans(abs(sv.train$D200$S))
# m202 <- colMeans(abs(sv.train$D202$S))
# 
# # tab_by_response <- bind_rows(
# #   tibble(predictor = names(m199), response = "D199", mean_abs_shap = as.numeric(m199)),
# #   tibble(predictor = names(m200), response = "D200", mean_abs_shap = as.numeric(m200)),
# #   tibble(predictor = names(m202), response = "D202", mean_abs_shap = as.numeric(m202))
# # )
# 
# mean_abs_across <- (m199 + m200 + m202) / 3
# mean_abs_across <- sort(mean_abs_across, decreasing = TRUE)
# mean_abs_across
# 
# 
# # Reorder for plotting - by average abs shap value across isotopes
# # col.order <- c("Tmean8110Cat", "Precip8110Cat", "RunoffCat", "CompStrgthCat", "LOI_PERCENT", "SumForestCat", "PctOwWs_Mean", "Evap_Inflow_ratio", "WetLossConv", "Hg0DryDep")
# 
# # Or could change this to order by D199 importance?
# col.order <- names(mean_abs_across)
# 
# sv.train$D199 <- sv.train$D199[, col.order]
# sv.train$D200 <- sv.train$D200[, col.order]
# sv.train$D202 <- sv.train$D202[, col.order]



## All lakes
sv.all <- shapviz(shap_values_all)

# names(sv.all) <- response_var  # Not yet automatically
colnames(sv.all$D199)[colnames(sv.all$D199)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
colnames(sv.all$D200)[colnames(sv.all$D200)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
colnames(sv.all$D202)[colnames(sv.all$D202)=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"


# Mean absolute shap values by predictor for each isotope and overall
m199all <- colMeans(abs(sv.all$D199$S))
m200all <- colMeans(abs(sv.all$D200$S))
m202all <- colMeans(abs(sv.all$D202$S))

# tab_by_response <- bind_rows(
#   tibble(predictor = names(m199), response = "D199", mean_abs_shap = as.numeric(m199)),
#   tibble(predictor = names(m200), response = "D200", mean_abs_shap = as.numeric(m200)),
#   tibble(predictor = names(m202), response = "D202", mean_abs_shap = as.numeric(m202))
# )

mean_abs_across_all <- (m199all + m200all + m202all) / 3
mean_abs_across_all <- sort(mean_abs_across_all, decreasing = TRUE)
mean_abs_across_all



# Reorder for plotting - by average abs shap value across isotopes
# col.order <- c("Tmean8110Cat", "Precip8110Cat", "RunoffCat", "CompStrgthCat", "LOI_PERCENT", "SumForestCat", "PctOwWs_Mean", "Evap_Inflow_ratio", "WetLossConv", "Hg0DryDep")

# Or could change this to order by D199 importance?
col.order.all <- names(mean_abs_across_all)

sv.all$D199 <- sv.all$D199[, col.order.all]
sv.all$D200 <- sv.all$D200[, col.order.all]
sv.all$D202 <- sv.all$D202[, col.order.all]

# # Possibly transform data for color scale to work better?
# hist(Train_Dat$Hg0DryDep)
# hist(Train_Dat$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s) # Transform
# hist(Train_Dat$Evap_Inflow_ratio, breaks=50) # Transform
# hist(Train_Dat$Precip8110Cat)
# hist(Train_Dat$Tmean8110Cat)
# hist(Train_Dat$CompStrgthCat)
# hist(Train_Dat$PctOwWs_Mean) # Transform
# hist(Train_Dat$SumForestCat)
# hist(Train_Dat$RunoffCat) # Transform
# hist(Train_Dat$LOI_PERCENT) # Transform

# min(Train_Dat$Hg0DryDep)
# min(Train_Dat$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s) 
# min(Train_Dat$Evap_Inflow_ratio) # 0, add .01
# min(Train_Dat$Precip8110Cat)
# min(Train_Dat$Tmean8110Cat)
# min(Train_Dat$CompStrgthCat)
# min(Train_Dat$PctOwWs_Mean) # 0, add 1
# min(Train_Dat$SumForestCat)
# min(Train_Dat$RunoffCat) # min 2
# min(Train_Dat$LOI_PERCENT) # *100, then log10

# sv.test <- sv
# sv.test$D199[["X"]]$Evap_Inflow_ratio <- log10(sv.test$D199[["X"]]$Evap_Inflow_ratio+.01)
# sv.test$D199[["X"]]$PctOwWs_Mean <- log10(sv.test$D199[["X"]]$PctOwWs_Mean+1)
# sv.test$D199[["X"]]$RunoffCat <- log10(sv.test$D199[["X"]]$RunoffCat)
# sv.test$D199[["X"]]$LOI_PERCENT <- log10(sv.test$D199[["X"]]$LOI_PERCENT*100)
# 
# sv.test$D200[["X"]]$Evap_Inflow_ratio <- log10(sv.test$D200[["X"]]$Evap_Inflow_ratio+.01)
# sv.test$D200[["X"]]$PctOwWs_Mean <- log10(sv.test$D200[["X"]]$PctOwWs_Mean+1)
# sv.test$D200[["X"]]$RunoffCat <- log10(sv.test$D200[["X"]]$RunoffCat)
# sv.test$D200[["X"]]$LOI_PERCENT <- log10(sv.test$D200[["X"]]$LOI_PERCENT*100)
# 
# sv.test$D202[["X"]]$Evap_Inflow_ratio <- log10(sv.test$D202[["X"]]$Evap_Inflow_ratio+.01)
# sv.test$D202[["X"]]$PctOwWs_Mean <- log10(sv.test$D202[["X"]]$PctOwWs_Mean+1)
# sv.test$D202[["X"]]$RunoffCat <- log10(sv.test$D202[["X"]]$RunoffCat)
# sv.test$D202[["X"]]$LOI_PERCENT <- log10(sv.test$D202[["X"]]$LOI_PERCENT*100)

# bee2 <- sv_importance(sv.test, kind="beeswarm", sort_features=F)
# bee2
# ggsave(paste0(fig_dir, "SHAP/Beeplot_log_Evap_OW_Runoff_LOI.png"), width=24, height=8)


# Plots for training lakes
# sv_importance(sv.train)
# 
# theme_set(theme_gray(base_size = 18))
# 
# # Bar plot of mean absolute shap value - training lakes
# meanshap.tr <- sv_importance(sv.train, sort_features=F) + # Already sorted
#   theme_bw(base_size = 20) +
#   scale_fill_manual(values = c("D199" = "black", "D200" = "gray60", "D202" = "gray80"),
#                     labels = c("D199" = expression(Delta^199 * Hg), 
#                                "D200" = expression(Delta^200 * Hg), 
#                                "D202" = expression(delta^202 * Hg))) +
#   scale_y_discrete(labels = c(
#     "RunoffCat" = "Runoff",
#     "Precip8110Cat" = "Precipitation",
#     "Hg0DryDep" = expression(paste("Hg"^"0", " dry dep.")),
#     "LOI_PERCENT" = "Sed. organic matter",
#     "PctOwWs_Mean" = "Percent open water",
#     "Evap_Inflow_ratio" = "Evap. loss:inflow",
#     "SumForestCat" = "Percent forest",
#     "CompStrgthCat" = "Compressive strength",
#     "Tmean8110Cat" = "Temperature",
#     "WetLossConv" = "Convect. precip. flux"
#   )) +
#   theme(text=element_text(size=28, family = "Calibri"),
#         legend.text = element_text(size=28, family = "Calibri"))
#         # legend.title = element_text(size=80, family = "Calibri", face = "bold"),
#         # legend.key.size = unit(3, "cm"),
#         # axis.title.y = element_text(margin = margin(r = -5), family = "Calibri")) 
# meanshap.tr
# ggsave(paste0(fig_dir, "SHAP/MeanShap_TrainLakes.png"), width=10, height=6)
# 
# 
# # meanshap.tr.highlow <- sv_importance(sv.train, sort_features=T)# +
# # meanshap.tr.highlow
# # ggsave(paste0(fig_dir, "SHAP/MeanShap_TrainLakes_HighLow.png"), width=10, height=6)
# 
# # Bee swarm plot
# bee.tr <- sv_importance(sv.train, kind="beeswarm", sort_features=F)# +
#   # theme(text=element_text(size=20))
# bee.tr
# ggsave(paste0(fig_dir, "SHAP/Beeplot_TrainLakes.png"), width=24, height=8)
# 
# both.tr <- sv_importance(sv.train, kind="both", sort_features=F)# +
# # theme(text=element_text(size=20))
# both.tr



# Bar plot of mean absolute shap value - all lakes
meanshap.all <- sv_importance(sv.all, sort_features=F)+ # Already sorted
  theme_bw(base_size = 20) +
  scale_fill_manual(values = c("D199" = "black", "D200" = "gray60", "D202" = "gray80"),
                    labels = c("D199" = expression(Delta^199 * Hg), 
                               "D200" = expression(Delta^200 * Hg), 
                               "D202" = expression(delta^202 * Hg))) +
  scale_y_discrete(labels = c(
    "RunoffCat" = "Runoff",
    "Precip8110Cat" = "Precipitation",
    "Hg0DryDep" = expression(paste("Hg"^"0", " dry dep.")),
    "LOI_PERCENT" = "Sed. organic matter",
    "PctOwWs_Mean" = "Percent open water",
    "Evap_Inflow_ratio" = "Evap. loss:inflow",
    "SumForestCat" = "Percent forest",
    "CompStrgthCat" = "Compressive strength",
    "Tmean8110Cat" = "Temperature",
    "WetLossConv" = "Convect. precip. flux"
  )) +
  theme(text=element_text(size=28, family = "Calibri"),
        legend.text = element_text(size=28, family = "Calibri"))
# legend.title = element_text(size=80, family = "Calibri", face = "bold"),
# legend.key.size = unit(3, "cm"),
# axis.title.y = element_text(margin = margin(r = -5), family = "Calibri")) 
meanshap.all
ggsave(paste0(fig_dir, "SHAP/MeanShap_AllLakes.png"), width=10, height=6) # Already sorted

# meanshap.all.highlow <- sv_importance(sv.all, sort_features=T)# +
# meanshap.all.highlow
# ggsave(paste0(fig_dir, "SHAP/MeanShap_AllLakes_HighLow.png"), width=10, height=6)


# Bee swarm plot
bee.all <- sv_importance(sv.all, kind="beeswarm", sort_features=F) 
bee.all
ggsave(paste0(fig_dir, "SHAP/Beeplot_AllLakes_default.png"), width=24, height=8)


# Do each plot separately so can give plot options
bee199 <- sv_importance(sv.all$D199, kind="beeswarm", sort_features=F) +
  theme_bw() +
  scale_y_discrete(labels = c(
    "RunoffCat" = "Runoff",
    "Precip8110Cat" = "Precipitation",
    "Hg0DryDep" = expression(paste("Hg"^"0", " dry dep.")),
    "LOI_PERCENT" = "Sed. organic matter",
    "PctOwWs_Mean" = "Percent open water",
    "Evap_Inflow_ratio" = "Evap. loss:inflow",
    "SumForestCat" = "Percent forest",
    "CompStrgthCat" = "Compressive strength",
    "Tmean8110Cat" = "Temperature",
    "WetLossConv" = "Convect. precip. flux"
  )) +
  theme(text=element_text(size=28)) +
  ggtitle(expression(Delta^199 * Hg))

bee199

bee200 <- sv_importance(sv.all$D200, kind="beeswarm", sort_features=F) +
  theme_bw() +
  scale_y_discrete(labels = c(
    "RunoffCat" = "Runoff",
    "Precip8110Cat" = "Precipitation",
    "Hg0DryDep" = expression(paste("Hg"^"0", " dry dep.")),
    "LOI_PERCENT" = "Sed. organic matter",
    "PctOwWs_Mean" = "Percent open water",
    "Evap_Inflow_ratio" = "Evap. loss:inflow",
    "SumForestCat" = "Percent forest",
    "CompStrgthCat" = "Compressive strength",
    "Tmean8110Cat" = "Temperature",
    "WetLossConv" = "Convect. precip. flux"
  )) +
  theme(text=element_text(size=28)) +
  ggtitle(expression(Delta^200 * Hg))


bee202 <- sv_importance(sv.all$D202, kind="beeswarm", sort_features=F) +
  theme_bw() +
  scale_y_discrete(labels = c(
    "RunoffCat" = "Runoff",
    "Precip8110Cat" = "Precipitation",
    "Hg0DryDep" = expression(paste("Hg"^"0", " dry dep.")),
    "LOI_PERCENT" = "Sed. organic matter",
    "PctOwWs_Mean" = "Percent open water",
    "Evap_Inflow_ratio" = "Evap. loss:inflow",
    "SumForestCat" = "Percent forest",
    "CompStrgthCat" = "Compressive strength",
    "Tmean8110Cat" = "Temperature",
    "WetLossConv" = "Convect. precip. flux"
  )) +
  theme(text=element_text(size=28)) +
  ggtitle(expression(delta^202 * Hg))


wrap.plot.bee <- wrap_plots(bee199, bee200, bee202, nrow=1, ncol=3, byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")

ggsave(plot=wrap.plot.bee, paste0(fig_dir, "SHAP/Beeplot_AllLakes_wrap.png"), width=24, height=8)

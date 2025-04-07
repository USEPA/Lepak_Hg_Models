# Compare THg model w/ LOI WITH and WITHOUT lat/long
# THg model including LOI as PREDICTOR

# library(party) # 1.3-11 on VM
# library(permimp) # permimp_1.0-2 
library(randomForest) # randomForest_4.7-1.1
# library(tuneRanger) # tuneRanger_0.5       
library(tidyverse) # tidyverse_1.3.2

# library(remotes)
# install_version(
#   "party",
#   version = "1.3-9")

output_dir <- "Model_Output/THg/"
model_dir <- "Saved_Models/THg/"
fig_dir <- "Figures/THg/"

# dir.create(paste0(output_dir))
# dir.create(paste0(model_dir))
# dir.create(paste0(fig_dir))

# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")


Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83)

# log-transform THg for all modeling
Train_Dat$log10THg <- log10(Train_Dat$STHG_ng_g)
Test_Dat$log10THg <- log10(Test_Dat$STHG_ng_g)

# Make LOI a percent (doesn't matter for this model, but need for THg/LOI model
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100


Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g) 
p <- ncol(Train_Dat_run)-1 # 125 preds - including LOI!!!


# Let's go with nodesize=1, mtry=.5*p
# Note without LOI, best was 1/3 preds



# First try classic random forest algorithm because all predictors are continuous (or almost all, some ordered categorical converted to numerical, one binary)
set.seed(36)
rf.all  <- randomForest(log10THg ~ ., data=Train_Dat_run, 
                        mtry=max(floor(p/2), 1), 
                        ntree=5000,
                        nodesize=1,
                        keep.forest=T,
                        keep.inbag = T,
                        importance=T)
# saveRDS(rf.all, paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36.rds"))
rf.all <- readRDS(paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36.rds"))

rf.all
# MSE: 0.06417612
# % Var explained: 56.33 (previously 46.34)

# Unconditional variable importance
rf_imp <- data.frame(Variable=names(rf.all$importance[,1]), Importance=rf.all$importance[,1])
row.names(rf_imp) <- NULL
rf_imp <- rf_imp %>% arrange(desc(Importance))
rf_imp$Variable <- factor(rf_imp$Variable, levels=rev(rf_imp$Variable))

head(rf_imp, 11)

ggplot(rf_imp, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_x_discrete(label=abbreviate)





#### Fit spatial version #######
Train_Dat_run_sp <- Train_Dat %>% left_join(Lake_Geo) %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g) 
p_sp <- ncol(Train_Dat_run_sp)-1 # 127 preds - including LOI and lat/long

set.seed(36)
rf.all.sp  <- randomForest(log10THg ~ ., data=Train_Dat_run_sp, 
                        mtry=max(floor(p_sp/2), 1), 
                        ntree=5000,
                        nodesize=1,
                        keep.forest=T,
                        keep.inbag = T,
                        importance=T)
saveRDS(rf.all.sp, paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36_SP.rds"))
# rf.all.sp <- readRDS(paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36_SP.rds"))


rf.all.sp
# MSE: 0.0634 (previously 0.06417612)
# % Var explained: 56.59 (previously 56.33 )

# Unconditional variable importance
rf_imp_sp <- data.frame(Variable=names(rf.all.sp$importance[,1]), Importance=rf.all.sp$importance[,1])
row.names(rf_imp_sp) <- NULL
rf_imp_sp <- rf_imp_sp %>% arrange(desc(Importance))
rf_imp_sp$Variable <- factor(rf_imp_sp$Variable, levels=rev(rf_imp_sp$Variable))

head(rf_imp_sp, 12)

ggplot(rf_imp_sp, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_x_discrete(label=abbreviate)


names(rf.all.sp)

Train_plot <- Train_Dat_run_sp
Train_plot$Residual <- Train_plot$log10THg - rf.all.sp$predicted

MainStates <- map_data("state")

# Spatial distribution of residuals
# ggplot(Test_Dat, aes(col=Residual, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()

library(colorspace)
library(vegan)
library(maps)

ggplot(Train_plot, aes(fill=Residual, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray50", shape=21) + 
  theme_void() +
  # geom_point(data=Train_plot, size=3.5, aes(fill=Residual, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-2,2,1), limits=c(-2.3,2.3))


CV_Resids <- Train_plot %>% dplyr::select(Residual, LON_DD83, LAT_DD83)

plot(Residual~LON_DD83, data=Train_plot)
plot(Residual~LAT_DD83, data=Train_plot)

dists <- as.matrix(dist(cbind(CV_Resids$LAT_DD83, CV_Resids$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids$Residual))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=0.1, p=.001 There is spatial autocorrelation in residuals
# Same as before - did not decrease spatial autocorrelation




#### Try spatial version with just top predictors

Train_Dat_run_sp2 <- Train_Dat %>% left_join(Lake_Geo) %>% dplyr::select(LOI_PERCENT,
                                                                         Ave_pH,
                                                                         WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s ,
                                                                         RunoffCat,
                                                                         Precip8110Cat ,
                                                                         Gas_Hg_Hg0Conc_ng_m3 ,
                                                                         Ave_CONDUCTIVITY_uS_cm,
                                                                         ClayCat ,
                                                                         pct_WetLossConv ,
                                                                         WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s,
                                                                         SandCat,
                                                                         Tmean8110Cat ,
                                                                         LON_DD83 ,
                                                                         LAT_DD83,
                                                                         log10THg) 
p_sp2 <- ncol(Train_Dat_run_sp2)-1 # 14 preds - including LOI and lat/long


set.seed(36)
rf.all.sp2  <- randomForest(log10THg ~ ., data=Train_Dat_run_sp2, 
                           mtry=max(floor(p_sp2/2), 1), 
                           ntree=5000,
                           nodesize=1,
                           keep.forest=T,
                           keep.inbag = T,
                           importance=T)
saveRDS(rf.all.sp, paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36_SP2.rds"))
# rf.all.sp <- readRDS(paste0(model_dir, "rf_full_mtryHalf_ntree5000_node1_sd36_SP2.rds"))


rf.all.sp2
# MSE: 0.0634 (previously 0.06417612)
# % Var explained: 56.59 (previously 56.33 )

# Unconditional variable importance
rf_imp_sp2 <- data.frame(Variable=names(rf.all.sp2$importance[,1]), Importance=rf.all.sp2$importance[,1])
row.names(rf_imp_sp2) <- NULL
rf_imp_sp2 <- rf_imp_sp2 %>% arrange(desc(Importance))
rf_imp_sp2$Variable <- factor(rf_imp_sp2$Variable, levels=rev(rf_imp_sp2$Variable))

head(rf_imp_sp2, 12)

ggplot(rf_imp_sp2, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_x_discrete(label=abbreviate)


names(rf.all.sp2)

Train_plot2 <- Train_Dat_run_sp2
Train_plot2$Residual <- Train_plot$log10THg - rf.all.sp2$predicted

MainStates <- map_data("state")

# Spatial distribution of residuals
# ggplot(Test_Dat, aes(col=Residual, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()


ggplot(Train_plot2, aes(fill=Residual, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray50", shape=21) + 
  theme_void() +
  # geom_point(data=Train_plot, size=3.5, aes(fill=Residual, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-2,2,1), limits=c(-2.3,2.3))


CV_Resids2 <- Train_plot2 %>% dplyr::select(Residual, LON_DD83, LAT_DD83)


dists <- as.matrix(dist(cbind(CV_Resids2$LAT_DD83, CV_Resids2$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids2$Residual))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=0.096, p=.001 There is spatial autocorrelation in residuals
# Same as before - did not decrease spatial autocorrelation

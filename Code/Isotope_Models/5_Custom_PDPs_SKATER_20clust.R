library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0
library(colorspace)
library(foreach)
library(doParallel)
library(parallel)
library(ggpubr)
library(patchwork)


output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "PDP_SKATER20/D202/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/D199/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/D200/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "PDP_SKATER20/All3/"), recursive=T, showWarnings = FALSE) 

dir.create(paste0(output_dir, "PDP/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(output_dir, "PDP/Bivariate/"), recursive=T, showWarnings = FALSE) 


# NEED TO THINK THROUGH HOW TO MODIFY CODE BELOW, WHICH USES ORIGINAL CLUSTER NUMBERS
# SHOULD KEEP ORIGINAL CLUSTER NUMBERS IN PDP DAT, ETC. BECAUSE CLUSTER NUMBER COULD CHANGE AGAIN AND DON'T WANT TO RERUN IF IT CHANGES


# Predictions with spatial data and SKATER Clusters
Preds_with_Clusters <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
# Rename WetLossConv back to original name for prediction
Preds_with_Clusters <- Preds_with_Clusters %>% rename(WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s=WetLossConv)
# This has LakeRole in it as well and can be joined

# Read in and join Ryan's cluster numbers and colors
New_ColorsNumbers <- read.csv("Tables/Maha-reassign.csv") %>% 
  dplyr::select(Maha20, New_Maha, Color_code) %>% 
  mutate(Color_code = paste0("#", Color_code)) %>% 
  mutate(Color_code = ifelse(Color_code=="#56608", "#056608", Color_code))#,
         # Maha20 = as.factor(Maha20),
         # New_Maha = as.factor(New_Maha))


Preds_with_Clusters <- left_join(Preds_with_Clusters, New_ColorsNumbers) %>% 
  dplyr::select(-c("Maha10", "Maha30")) 
# Maha20 is original cluster ID, New_Maha is new cluster ID for plotting
names(Preds_with_Clusters)

## *** Note that LOI was unnecessarily divided by 100 an extra time in isotope models (in 3_Predict_ISO_MVRF_subset.R), which is how it appears in Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv, and is how it should be input into models AND FOR GENERATING THE PDPs ****
## But for visualizations it should be multiplied by 100 so that it is a proportion for better interpretation

names(Preds_with_Clusters)



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

# Correlation among predictors - 4 moderate pairwise correlations (of 45 pairs)
pred_vals <- Preds_with_Clusters[,2:11]
cor_tab <- cor(pred_vals)
plot(Precip8110Cat~RunoffCat, data= pred_vals)
plot(WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s  ~Tmean8110Cat, data= pred_vals)
plot(Hg0DryDep  ~Precip8110Cat , data= pred_vals)
plot(Hg0DryDep  ~Tmean8110Cat , data= pred_vals)


pred_resps <- Train_Dat[, c("Pred_D199_SD", "Pred_D200_SD", "Pred_D202_SD")]
obs_resps <- Train_Dat[, c("Obs_D199_SD", "Obs_D200_SD", "Obs_D202_SD")]

# Similar correlation between predicted iso values and observed values
# Predicted iso values more highly correlated than obs
cor(pred_resps)
cor(obs_resps)

# Are errors correlated?
errors <- pred_resps-obs_resps
cor(errors)
plot(Pred_D199_SD ~ Pred_D200_SD , data=errors) # moderate correlation
plot(Pred_D199_SD ~ Pred_D202_SD , data=errors) # moderate correlation
plot(Pred_D200_SD ~ Pred_D202_SD , data=errors) # low correlation


# Model trained on all lakes with iso data

# rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))

preds <- rf.final$xvar.names # predictors
# Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg") # original observed isos
# All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


pdp_dat <- Preds_with_Clusters %>% dplyr::select(NLA12_ID, Maha20, New_Maha, all_of(preds))

table(pdp_dat$Maha20) # 15 have at least 10

names(Preds_with_Clusters)




#### Calculate mean predicted values for each isotope for all lakes together and by clusters ####

# Modified to save all lake-specific data for ICE curves



# Modified pdp code from pdp package
for(j in 1:length(preds)){

  start.time <- Sys.time()
  
  print(j)
  
  pred.var <- preds[j] #"Precip8110Cat"
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  
  # Obs values of predictor var
  x <- pdp_dat[, pred.var]
  
  # Set limits of predictor x-axis
  # lim.j <- round(quantile(x, probs=c(0.05, 0.95)), 6)
  lim.j <- round(quantile(x, probs=c(0, 1)), 6)
  
  # Resolution of predictor on x-axis
  res <- 40 # Each grid step is 2.5% of the range of the variable (could also do quantiles)
  pred.val <- seq(lim.j[1],lim.j[2], (lim.j[2]-lim.j[1])/res)
  names(pred.val) <- names(pred.var)
  
  # pred.val <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                  # length = res)
  
  pred.grid <- expand.grid(pred.val, KEEP.OUT.ATTRS = FALSE) 
  names(pred.grid) <- pred.var

  ### Calculate PDP/ICE data
  Lake_Preds_Save <- NULL # Data frame of all lake-specific predictions at different x-vals (ICE)
  Iso_Mean_Preds <- NULL # Data frame of mean predictions overall and by cluster (PDP)
  
  # i is index for x-value
  for(i in seq_len(nrow(pred.grid))){
    temp <- pdp_dat
    
    # replace variable of interest with single value of interest, leave rest of predictors as-is
    temp[, pred.var] <- pred.grid[i, pred.var] 
    rf_predict_All <- predict(rf.final, newdata=temp)
    
    Lake_Preds_i <- data.frame(TempName = pred.grid[i, pred.var], # Placeholder for x variable 
                               D199_pred = rf_predict_All$regrOutput$D199$predicted, 
                               D200_pred = rf_predict_All$regrOutput$D200$predicted,
                               D202_pred = rf_predict_All$regrOutput$D202$predicted,
                               Maha20 = pdp_dat$Maha20,
                               NLA12_ID = pdp_dat$NLA12_ID)
    names(Lake_Preds_i)[1] <- pred.var # Add x-var name
    
    # Bind all lake-specific predictions here - save after i loop
    Lake_Preds_Save <- rbind(Lake_Preds_Save, Lake_Preds_i)
    
    
    # Calculate mean predicted isos in SD at x-value i across all lakes
    iso_mean_preds_temp <- data.frame(Pred_D199_SD = mean(Lake_Preds_i$D199_pred), 
                                      Pred_D200_SD = mean(Lake_Preds_i$D200_pred),
                                      Pred_D202_SD = mean(Lake_Preds_i$D202_pred))
    
    
    # Mean predicted isos in SD at x-value i for each cluster; k is cluster index
    cl_iso_mean_preds_temp_jn <- list() # list to store mean predictions by cluster
    
    for(k in seq_len(length(unique(Lake_Preds_i$Maha20))) ){
      
      # Cluster k
      cl <- sort(unique(Lake_Preds_i$Maha20))[k]
      
      cluster_sub <- Lake_Preds_i %>% filter(Maha20==cl)
      
      # Calculate mean predicted isos in SD at x-value i in cluster k
      cl_iso_mean_preds_temp <- data.frame(Pred_D199_SD = mean(cluster_sub$D199_pred),
                                           Pred_D200_SD = mean(cluster_sub$D200_pred),
                                           Pred_D202_SD = mean(cluster_sub$D202_pred))
      
      names(cl_iso_mean_preds_temp) <- c(paste0('Pred_D199_SD_Cluster',cl),
                                         paste0('Pred_D200_SD_Cluster',cl),
                                         paste0('Pred_D202_SD_Cluster',cl))
      
      cl_iso_mean_preds_temp_jn[[k]] <- cl_iso_mean_preds_temp
    }
    
    cl_iso_mean_preds_temp_jn_df <- data.frame(do.call('cbind', cl_iso_mean_preds_temp_jn))
    
    # Join all-lake-mean-preds with cluster-level-mean-preds for i
    join_iso_mean_preds <- cbind(iso_mean_preds_temp, cl_iso_mean_preds_temp_jn_df)

    
    Iso_Mean_Preds <- rbind(Iso_Mean_Preds, join_iso_mean_preds)
  }
  # End loop through x-vals (pred.grid)
  
  end.time <- Sys.time()
  print(end.time-start.time) # 20.14093  secs for all lakes, 1 predictor
  
  # Write all lake-specific predictions for ICE curves
  saveRDS(Lake_Preds_Save, paste0(output_dir, "PDP/", pred.var.lab, "_ICE_dat.rds"))
  
  # Bind mean predictions with pred.grid for saving and plotting
  res <- cbind(pred.grid, Iso_Mean_Preds)
  
  saveRDS(res, paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
}
    
# Same after name change in line 180
# res2 <- readRDS(paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
# res1 <- readRDS(paste0(output_dir, "PDP/OLD/", pred.var.lab, "_PDP_dat.rds"))
# sum(round(res1-res2, 14))








#### Make figures using output from previous loop ####

# *** Note that figure colors and labels should correspond to New_Maha IDs, but underlying cluster data is coded for original Maha20 IDs ***



# Old Cluster colors 
# cols2 <- sequential_hcl(5, palette = "Light Grays")
# cols3 <- qualitative_hcl(17, palette = "Dark 3")
# set.seed(13) # 5
# cols <- sample(c(cols3, cols2[-c(4,5)]))
# 
# scale_color_KV <- function(...){
#   ggplot2:::manual_scale(
#     'color', 
#     values = setNames(cols, as.character(1:20))
#   )
# }

# New color function using Ryan's colors - matches 4_IsoPreds_Voronoi.R colors and map
scale_color_KV <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(New_ColorsNumbers$Color_code, New_ColorsNumbers$New_Maha)
  )
}

scale_color_KV_oldCl <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(New_ColorsNumbers$Color_code, New_ColorsNumbers$Maha20)
  )
}

pdp_dat$Cluster <- factor(pdp_dat$Maha20)

Cl_kp <- names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)>9]) # 15 clusters with at least 10 lakes
# "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "14" "15" "16" "19"

names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)<10]) # "12" "13" "17" "18" "20"




for(j in 1:length(preds)){
  print(j)
  
  pred.var <- preds[j] #"Precip8110Cat"
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  dir.create(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  
  
  # Read in PDP dat
  res <- readRDS(paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
  # Read in lake-specific predictions for ICE curves
  ICE_dat <- readRDS(paste0(output_dir, "PDP/", pred.var.lab, "_ICE_dat.rds"))
  
  
  # Predicted isos in original units
  res_origunit <- res
  ICE_origunit <- ICE_dat
  
  names(res_origunit)
  names(ICE_origunit)
  
  # Transform appropriate columns
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))

  colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
  
  
  # Transform appropriate columns
  ICE_origunit <- ICE_origunit %>%  mutate(across(starts_with("D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
  ICE_origunit <- ICE_origunit %>%  mutate(across(starts_with("D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
  ICE_origunit <- ICE_origunit %>%  mutate(across(starts_with("D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
  
  colnames(ICE_origunit)[2:4] <- c("Pred_D199_origUnits", "Pred_D200_origUnits", "Pred_D202_origUnits")
  

  
  # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
  if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
  if(pred.var == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
  
  if(pred.var == "LOI_PERCENT") ICE_dat$LOI_PERCENT <- 100*ICE_dat$LOI_PERCENT
  if(pred.var == "LOI_PERCENT") ICE_origunit$LOI_PERCENT <- 100*ICE_origunit$LOI_PERCENT
  
  
  # Calculate 95% interval of observed predictor values
  x_dat <- pdp_dat %>% dplyr::select(one_of(pred.var))  # subset by cluster here in cluster loop
  if(pred.var == "LOI_PERCENT") x_dat <- 100*x_dat
  x_95_int <- quantile(x_dat[,1], c(.025, .975))
  
  
  # Odd/even lakes for plotting ICE curves (split b/c so many)
  nlakes <- length(unique(ICE_origunit$NLA12_ID))
  odds <- seq_len(nlakes) %% 2 
  odd_lks <- unique(ICE_origunit$NLA12_ID)[odds == 1] 
  evn_lks <- unique(ICE_origunit$NLA12_ID)[odds == 0] 
  
  

  # D199 all lakes PDP - origUnit
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D199_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("Mean D199 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var]), 
                    ylim = range(res_origunit$Pred_D199_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) 
  ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  # Resave certain predictors for thresholds
  # res_origunit %>%
  #   ggplot(aes(x =  get(pred.var) , y = Pred_D199_origUnits)) +
  #   theme_minimal() +
  #   theme(text=element_text(size=20))+
  #   annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
  #   geom_line(linewidth=2) + 
  #   xlab(pred.var.lab) +
  #   ylab("Mean D199 prediction") +
  #   coord_cartesian(xlim = range(res_origunit[pred.var]), 
  #                   ylim = range(res_origunit$Pred_D199_origUnits))+
  #   # scale_x_continuous(breaks = seq(0, 20, by = 1)) # Hg0DryDep limits
  #   # scale_x_continuous(breaks = seq(0, 3000, by = 200)) +# Precip8110Cat limits
  #   # scale_y_continuous(breaks = seq(-.04, .04, by = .01)) # Precip8110Cat limits
  # 
  #   scale_x_continuous(breaks = seq(0, 3000, by = 200)) # RunoffCat limits
  # 
  # ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_ALL_fineAxis.png"), width=10, height=6)
  
  
  # D199 all lakes PDP WITH ICE - origUnit remove rectangle
  # Odd lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D199_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% odd_lks,], aes(x=get(pred.var), y = Pred_D199_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) + # , col="gray20"
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("D199 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
                    # ylim = range(res_origunit$Pred_D199_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) +
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_ALL_ICEcurvesOdd.png"), width=10, height=6)
  # Even lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D199_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% evn_lks,], aes(x=get(pred.var), y = Pred_D199_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) +
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("D199 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
    # ylim = range(res_origunit$Pred_D199_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)+
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_ALL_ICEcurvesEven.png"), width=10, height=6)
  
  
  # D200 all lakes - origUnit
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("Mean D200 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var]), 
                    ylim = range(res_origunit$Pred_D200_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)
  ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  # D200 all lakes PDP WITH ICE - origUnit remove rectangle
  # Odd lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D200_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% odd_lks,], aes(x=get(pred.var), y = Pred_D200_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) + # , col="gray20"
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("D200 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
    # ylim = range(res_origunit$Pred_D200_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) +
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_ALL_ICEcurvesOdd.png"), width=10, height=6)
  # Even lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D200_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% evn_lks,], aes(x=get(pred.var), y = Pred_D200_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) +
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("D200 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
    # ylim = range(res_origunit$Pred_D200_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)+
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_ALL_ICEcurvesEven.png"), width=10, height=6)
  
  
  
  # D202 all lakes - origUnit
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
    geom_line(linewidth=2) + 
    # scale_shape_manual(values = c(4, 1))+ 
    # geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    # geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    # scale_linetype_manual(name="Type", 
    # values = c("Artificial" = "dashed", "Natural" = "dotted"),
    # breaks=c("Artificial", "Natural")) +
    xlab(pred.var.lab) +
    ylab("Mean d202 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var]), 
                    ylim = range(res_origunit$Pred_D202_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  # D202 all lakes PDP WITH ICE - origUnit remove rectangle
  # Odd lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D202_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% odd_lks,], aes(x=get(pred.var), y = Pred_D202_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) + # , col="gray20"
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("d202 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
    # ylim = range(res_origunit$Pred_D202_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) +
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_ALL_ICEcurvesOdd.png"), width=10, height=6)
  # Even lakes
  res_origunit %>%
    ggplot(aes(x =  get(pred.var) , y = Pred_D202_origUnits)) +
    theme_minimal() +
    theme(text=element_text(size=20))+
    geom_line(data=ICE_origunit[ICE_origunit$NLA12_ID %in% evn_lks,], aes(x=get(pred.var), y = Pred_D202_origUnits, group=NLA12_ID, col=as.factor(Maha20)), alpha=0.2) +
    geom_line(linewidth=2) + 
    xlab(pred.var.lab) +
    ylab("d202 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var])) + #, 
    # ylim = range(res_origunit$Pred_D202_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)+
    scale_color_KV_oldCl() +
    theme(legend.position="none")
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_ALL_ICEcurvesEven.png"), width=10, height=6)
  
  
  
  # Plot all iso together - no ICE curves
  # Pivot PDP in SD to long format
  res_long <- pivot_longer(res, cols ="Pred_D199_SD":"Pred_D202_SD_Cluster20", names_to = "Isotope", values_to = "Mean_Prediction")
  
  
  # All isos all lakes - SD
  res_long %>% filter(Isotope %in% c("Pred_D199_SD", "Pred_D200_SD", "Pred_D202_SD")) %>% 
    mutate(Isotope=case_match(Isotope,
               "Pred_D199_SD" ~ "D199",
               "Pred_D200_SD" ~ "D200",
               "Pred_D202_SD" ~ "d202")) %>% 
    ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
    geom_line(linewidth=2, aes(linetype=Isotope)) + 
    scale_color_manual(values=c("black", "black", "black")) + # black, gray20, gray40
    scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
    xlab(pred.var.lab) +
    ylab("Mean prediction (z)") +
    coord_cartesian(xlim = range(res[pred.var]))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) +
    theme(legend.key.size = unit(4,"line"))  # , legend.position="bottom"
  ggsave(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab, "/PDP_", pred.var.lab, "_ALL.png"), width=12, height=6)
  # , ylim = range(res_long$Mean_Prediction) # Add back in for fixed axes
  
  
  
  
  # Loop for cluster plots - no ICE curves here
  
  for(i in 1:length(unique(pdp_dat$Maha20))){
    
    skater_i <- sort(unique(pdp_dat$Maha20))[i]
    newskater_i <- New_ColorsNumbers$New_Maha[which(New_ColorsNumbers$Maha20==skater_i)]
    color_i <- New_ColorsNumbers$Color_code[which(New_ColorsNumbers$Maha20==skater_i)]
    
    if(skater_i %in% Cl_kp){
      
      # Subset pdp data to cluster and predictor
      x_dat_cl <- pdp_dat %>% filter(Maha20 %in% skater_i) %>% dplyr::select(one_of(pred.var))
      if(pred.var == "LOI_PERCENT") x_dat_cl <- 100*x_dat_cl
      
      # Calculate 95% interval of observed predictor values
      x_95_int_cl <- quantile(x_dat_cl[,1], c(.025, .975))
      

      
      
      # D199 by cluster - origUnit
      annotations199 <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.05,
        ypos =  max(res_origunit[paste0("Pred_D199_origUnits_Cluster", skater_i)]),
        annotateText = c(paste0("Cluster ", newskater_i))) # Use new cluster number
      # -diff(range(res_origunit[paste0("Pred_D199_origUnits_Cluster", skater_i)]))*.01

        res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D199_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.3, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) + # Changed from cols[i]
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean D199 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D199_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations199,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_Cluster",  newskater_i, ".png"), width=10, height=6) # Save in directory for new cluster number
      
      

      
      
      # D200 by cluster - origUnit
      annotations200 <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.05,
        ypos =  max(res_origunit[paste0("Pred_D200_origUnits_Cluster", skater_i)]),
        annotateText = c(paste0("Cluster ", newskater_i)))
      
      res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D200_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.3, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) + # Changed from cols[i]
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean D200 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D200_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations200,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_Cluster",  newskater_i, ".png"), width=10, height=6)

      
      
      # D202 by cluster - origUnit
      annotations202 <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.05,
        ypos =  max(res_origunit[paste0("Pred_D202_origUnits_Cluster", skater_i)]),
        annotateText = c(paste0("Cluster ", newskater_i)))
      
      res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D202_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.3, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) + # Changed from cols[i]
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean d202 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D202_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations202,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_Cluster",  newskater_i, ".png"), width=10, height=6)
      

      
      
      # All isos by cluster - SD
      iso_dat_cl <- res_long %>% filter(Isotope %in% c(paste0("Pred_D199_SD_Cluster", skater_i), paste0("Pred_D200_SD_Cluster", skater_i), paste0("Pred_D202_SD_Cluster", skater_i)))
      
      annotations <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.06,
        ypos =   max(iso_dat_cl$Mean_Prediction),
        annotateText = c(paste0("Cluster ", newskater_i)))
      
      
      iso_dat_cl %>% 
        mutate(Isotope=case_match(Isotope,
                                  paste0("Pred_D199_SD_Cluster", skater_i) ~ "D199",
                                  paste0("Pred_D200_SD_Cluster", skater_i) ~ "D200",
                                  paste0("Pred_D202_SD_Cluster", skater_i) ~ "d202")) %>% 
        ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
        theme_minimal() +
        theme(text=element_text(size=20)) +
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.3, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) + # Changed from cols[i]
        geom_line(linewidth=2, aes(col=Isotope, linetype=Isotope)) + 
        scale_color_manual(values=c("black", "black", "black")) + # black, gray20, gray40
        scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
        xlab(pred.var.lab) +
        ylab("Mean prediction (z)") +
        coord_cartesian(xlim = range(res[pred.var]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        theme(legend.key.size = unit(4,"line"))  +
        geom_text(data=annotations,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      # , legend.position="bottom"
      ggsave(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab, "/PDP_", pred.var.lab, "_Cluster", newskater_i, ".png"), width=12, height=6)
      

    }
  }
  
}

# Much narrower range of mean predictions than individual lake predictions - likely because of interactions - see plots with ICE curves

# , xlim = range(Preds_with_Clusters[pred.var]), ylim = range(Preds_with_Clusters$Pred_D202_origUnits)
# theme(legend.key.size = unit(2,"line"))  
# , ylim = range(res_long$Mean_Prediction) # Add back in for fixed axes



#### Multi-panel figure for univariate PDPs ####
# Cluster colors - matches 4_IsoPreds_Voronoi.R colors and map
# cols2 <- sequential_hcl(5, palette = "Light Grays")
# cols3 <- qualitative_hcl(17, palette = "Dark 3")
# set.seed(13) # 5
# cols <- sample(c(cols3, cols2[-c(4,5)]))




pdp_dat$Cluster <- factor(pdp_dat$Maha20)

Cl_kp <- names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)>9]) # 15 clusters with at least 10 lakes
# "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "14" "15" "16" "19"

names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)<10]) # "12" "13" "17" "18" "20"
names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)<15]) # "12" "13" "17" "18" "20"

pred.order <- c("Precip8110Cat", "RunoffCat", "Hg0DryDep", "LOI_PERCENT", "Evap_Inflow_ratio", "Tmean8110Cat", "SumForestCat", "CompStrgthCat",   "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s",  "PctOwWs_Mean")


# Order of old cluster numbers - new cluster numbers go in order 1-20
cl.order <- New_ColorsNumbers %>% arrange(New_Maha) %>% dplyr::select(Maha20) %>% filter(Maha20 %in% Cl_kp) %>% pull(Maha20) %>% as.character()

# cl.order <- c("3", "8", "10", "5", "7", "1", "4", "15", "9", "16", "11", "14", "2", "19", "6")
# length(unique(cl.order))
# cl.order %in% Cl_kp


# j is column, i is row
# [j, 1]
# [j, i+1]

New_ColorsNumbers

# list.all <- NULL # Initialize list of plots
myplots <- vector('list', length(pred.order)*(length(cl.order)+1))




for(j in 1:length(pred.order)){
  
  print(j)
  

  myplots[[1 + (j-1)*(length(cl.order)+1)]] <- local({
  
    j <- j
    pred.var <- pred.order[j] 
    
    pred.var.lab <- pred.var
    if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
    
    
    # Read in PDP dat
    res <- readRDS(paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
  
    # Predicted isos in original units
    res_origunit <- res
    # Transform appropriate columns
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
    colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
    
    

    # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
    if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
    # if(pred.var == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
    
    
    
    # Calculate 95% interval of observed predictor values
    x_dat <- pdp_dat %>% dplyr::select(one_of(pred.var))  # subset by cluster here in cluster loop
    if(pred.var == "LOI_PERCENT") x_dat <- 100*x_dat
    x_95_int <- quantile(x_dat[,1], c(.025, .975))
    
   
    
    # Plot all iso together - no ICE curves
    # Pivot PDP in SD to long format
    res_long <- pivot_longer(res, cols ="Pred_D199_SD":"Pred_D202_SD_Cluster20", names_to = "Isotope", values_to = "Mean_Prediction")
    
    
    # All isos all lakes - SD
    all_plot <- res_long %>% filter(Isotope %in% c("Pred_D199_SD", "Pred_D200_SD", "Pred_D202_SD")) %>% 
      mutate(Isotope=case_match(Isotope,
                                "Pred_D199_SD" ~ "D199",
                                "Pred_D200_SD" ~ "D200",
                                "Pred_D202_SD" ~ "d202")) 
    
    if(j==1){
      p1 <- all_plot %>% 
        ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
        theme_classic() +
        theme(text=element_text(size=40)) + # 32
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        geom_line(linewidth=2.5, aes(linetype=Isotope)) + 
        scale_color_manual(values=c("black", "black", "black")) +
        scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
        xlab(pred.var.lab) +
        # ylab(paste0("All lakes: Isotope SD")) +
        # labs(y=expression(atop(bold("All lakes"),atop("Isotope SD")))) +
        # ylab(paste(c(paste0("All lakes"), paste0("Predicted z")), collapse = '\n')) +
        ylab("All lakes") +
        
        coord_cartesian(xlim = range(res[pred.var]))+
        geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
        theme(legend.key.size = unit(5,"line"),
              axis.title.x = element_blank(),
              legend.position="bottom",
              # legend.position="none",
              plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=32),
              legend.title = element_text(size=40))  +
        ggtitle(paste(pred.var.lab))+
        scale_y_continuous(breaks = round(range(all_plot$Mean_Prediction), 2))
      } else{  # no y-axis title
        p1 <- all_plot %>% 
          ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
          theme_classic() +
          theme(text=element_text(size=40)) +
          annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
          geom_line(linewidth=2.5, aes(linetype=Isotope)) + 
          scale_color_manual(values=c("black", "black", "black")) +
          scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
          xlab(pred.var.lab) +
          # ylab(paste0("All lakes: Isotope SD")) +
          # labs(y=expression(atop(bold("All lakes"),atop("Isotope SD")))) +
          coord_cartesian(xlim = range(res[pred.var]))+
          geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
          theme(legend.key.size = unit(5,"line"),
                axis.title.x = element_blank(),
                legend.position="bottom",
                # legend.position="none",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.text = element_text(size=32),
                legend.title = element_text(size=40)
                )  +
          ggtitle(paste(pred.var.lab))+
          scale_y_continuous(breaks = round(range(all_plot$Mean_Prediction), 2))
    
    }
    

  })

  # Loop for cluster plots 
  
  for(i in 1:length(cl.order)){
    
    myplots[[1 + (j-1)*(length(cl.order)+1) + i]] <- local({
    
      j <- j
      i <- i

      ###### Need to repeat from above loop again to get to store properly
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      
      # Read in PDP dat
      res <- readRDS(paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
      
      # Predicted isos in original units
      res_origunit <- res
      # Transform appropriate columns
      res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
      res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
      res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
      colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
      
      # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
      if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
      # if(pred.var == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT

      # Calculate 95% interval of observed predictor values
      x_dat <- pdp_dat %>% dplyr::select(one_of(pred.var))  # subset by cluster here in cluster loop
      if(pred.var == "LOI_PERCENT") x_dat <- 100*x_dat
      x_95_int <- quantile(x_dat[,1], c(.025, .975))
      
      # Plot all iso together - no ICE curves
      # Pivot PDP in SD to long format
      res_long <- pivot_longer(res, cols ="Pred_D199_SD":"Pred_D202_SD_Cluster20", names_to = "Isotope", values_to = "Mean_Prediction")
      
      ###### End repeat code from above
      
      
      
      
      
      skater_i <- cl.order[i]
      newskater_i <- New_ColorsNumbers$New_Maha[which(New_ColorsNumbers$Maha20==skater_i)]
      color_i <- New_ColorsNumbers$Color_code[which(New_ColorsNumbers$Maha20==skater_i)]
      
      # Subset pdp data to cluster and predictor
      x_dat_cl <- pdp_dat %>% filter(Maha20 %in% skater_i) %>% dplyr::select(one_of(pred.var))
      if(pred.var == "LOI_PERCENT") x_dat_cl <- 100*x_dat_cl
      
      # Calculate 95% interval of observed predictor values
      x_95_int_cl <- quantile(x_dat_cl[,1], c(.025, .975))
      
      # color.index <- as.numeric(skater_i)
      
      
      # All isos by cluster - SD units
      iso_dat_cl <- res_long %>% filter(Isotope %in% c(paste0("Pred_D199_SD_Cluster", skater_i), paste0("Pred_D200_SD_Cluster", skater_i), paste0("Pred_D202_SD_Cluster", skater_i)))
      
      annotations <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.06,
        ypos =   max(iso_dat_cl$Mean_Prediction),
        annotateText = c(paste0("Cluster ", newskater_i)))
      
      
      dat_plot <- iso_dat_cl %>% 
        mutate(Isotope=case_match(Isotope,
                                  paste0("Pred_D199_SD_Cluster", skater_i) ~ "D199",
                                  paste0("Pred_D200_SD_Cluster", skater_i) ~ "D200",
                                  paste0("Pred_D202_SD_Cluster", skater_i) ~ "d202")) 
     
      
      # j==1 & i==length(cl.order) # bottom left - needs y-title and x-title
      # j==1 & i<length(cl.order) # first column without corner - needs y-title and no x-title
      # j>1 & i==length(cl.order) # no y-title, needs x-title
      # j>1 & i<length(cl.order) # no y-title, no x-title
      
  
      
      # j==1 & i==length(cl.order) # bottom left - needs y-title and x-title
      if(j==1 & i==length(cl.order)){    
      p_i <- dat_plot %>% 
        ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
        theme_classic() +
        theme(text=element_text(size=40)) + # 32
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) +
        geom_line(linewidth=2.5, aes(col=Isotope, linetype=Isotope)) + 
        scale_color_manual(values=c("black", "black", "black")) +
        scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
        xlab(pred.var.lab) +
        # ylab(paste(c(paste0("Cluster ", newskater_i), paste0("Predicted z")), collapse = '\n')) +
        ylab(paste0("Cluster ", newskater_i)) +
        # labs(y=expression(atop(bold("All lakes"),atop("Predicted z"))))
        
        coord_cartesian(xlim = range(res[pred.var]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
        theme(legend.key.size = unit(5,"line"), 
              legend.position="bottom",
              legend.text = element_text(size=40), # 32
              legend.title = element_text(size=40)
              # legend.position="none"
              # axis.title.y = element_blank()
              )  +
        scale_y_continuous(breaks = round(range(dat_plot$Mean_Prediction), 2))
        } 
      
      # j==1 & i<length(cl.order) # first column without corner - needs y-title and no x-title
      # ** Could remove x-axis numbers **
      if(j==1 & i<length(cl.order)){    
        p_i <- dat_plot %>% 
          ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
          theme_classic() +
          theme(text=element_text(size=40)) +
          annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
          annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) +
          geom_line(linewidth=2.5, aes(col=Isotope, linetype=Isotope)) + 
          scale_color_manual(values=c("black", "black", "black")) +
          scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
          xlab(pred.var.lab) +
          # ylab(paste(c(paste0("Cluster ", newskater_i), paste0("Predicted z")), collapse = '\n')) +
          ylab(paste0("Cluster ", newskater_i)) +
          coord_cartesian(xlim = range(res[pred.var]))+
          geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
          theme(legend.key.size = unit(5,"line"), 
                legend.position="bottom",
                # legend.position="none",
                axis.title.x = element_blank(),
                legend.text = element_text(size=40),
                legend.title = element_text(size=40),
                axis.text.x=element_blank(), # Remove to keep all x-axes
                axis.ticks.x=element_blank() # Remove to keep all x-axes
          )  +
          scale_y_continuous(breaks = round(range(dat_plot$Mean_Prediction), 2))
      }     
      
      
      # j>1 & i==length(cl.order) # no y-title, needs x-title
      if(j>1 & i==length(cl.order)){   # Rest of bottom row
        p_i <- dat_plot %>% 
          ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
          theme_classic() +
          theme(text=element_text(size=40)) +
          annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
          annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) +
          geom_line(linewidth=2.5, aes(col=Isotope, linetype=Isotope)) + 
          scale_color_manual(values=c("black", "black", "black")) +
          scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
          xlab(pred.var.lab) +
          # ylab(paste0("Cluster ", newskater_i, ": Mean prediction (z)")) +
          coord_cartesian(xlim = range(res[pred.var]))+
          geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
          theme(legend.key.size = unit(5,"line"), 
                legend.position="bottom",
                # legend.position="none",
                axis.title.y = element_blank(),
                legend.text = element_text(size=32),
                legend.title = element_text(size=40)
          )  +
          scale_y_continuous(breaks = round(range(dat_plot$Mean_Prediction), 2))
      }
          
      # j>1 & i<length(cl.order) # no y-title, no x-title
      # ** Could remove x-axis numbers **
      if(j>1 & i<length(cl.order)){  
        p_i <- dat_plot %>% 
          ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
          theme_classic() +
          theme(text=element_text(size=40)) +
          annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
          annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.7, fill=color_i) +
          geom_line(linewidth=2.5, aes(col=Isotope, linetype=Isotope)) + 
          scale_color_manual(values=c("black", "black", "black")) +
          scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
          xlab(pred.var.lab) +
          coord_cartesian(xlim = range(res[pred.var]))+
          geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F, length = unit(0.05, "npc"), linewidth=.7) +
          theme(legend.key.size = unit(5,"line"), 
                legend.position="bottom",
                # legend.position="none",
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                legend.text = element_text(size=32),
                legend.title = element_text(size=40), 
                axis.text.x=element_blank(), # Remove to keep all x-axes
                axis.ticks.x=element_blank() # Remove to keep all x-axes
          )  +
          scale_y_continuous(breaks = round(range(dat_plot$Mean_Prediction), 2))
      }

      p_i
    })
  }
}

pred.order
# Temp
myplots[[1]]
myplots[[2]] # 3
myplots[[3]] # 8
# Precip
myplots[[17]]
myplots[[18]]
myplots[[19]]
#Runoff
myplots[[33]]
myplots[[34]]
myplots[[35]]


# grid.plot <- ggarrange(plotlist=myplots,
                       # nrow=16, ncol=10)

# Alignment doesn't work well
# grid.plot <- cowplot::plot_grid(plotlist=myplots, nrow=16, ncol=10, byrow=FALSE, align="hv")
# ggsave(plot=grid.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP_align.png"), width=60, height=48, limitsize=FALSE)

# Not aligned plot sizes
# grid.plot <- cowplot::plot_grid(plotlist=myplots, nrow=16, ncol=10, byrow=FALSE)
# ggsave(plot=grid.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP.png"), width=60, height=48, limitsize=FALSE)


# wrap.plot <- wrap_plots(myplots, nrow=16, ncol=10, byrow=FALSE)
# ggsave(plot=wrap.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP_wrap.png"), width=60, height=48, limitsize=FALSE)

wrap.plot <- wrap_plots(myplots, nrow=16, ncol=10, byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom",
        legend.text = element_text(size=48),
        legend.title = element_text(size=48),
        text=element_text(size=40))
ggsave(plot=wrap.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP_wrapLegendBottom_reduceXaxes.png"), width=60, height=48, limitsize=FALSE)
# ggsave(plot=wrap.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP_wrapLegendBottom.png"), width=60, height=48, limitsize=FALSE)

# wrap.plot <- wrap_plots(myplots, nrow=16, ncol=10, byrow=FALSE) +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = "right",
#         legend.text = element_text(size=40),
#         legend.title = element_text(size=40),
#         text=element_text(size=40))
# ggsave(plot=wrap.plot, filename=paste0(fig_dir, "PDP_SKATER20/All_Univariate_PDP_wrapLegendRight.png"), width=60, height=46, limitsize=FALSE)






# Bivariate PDPs


cl <- makeCluster(10)
doParallel::registerDoParallel(cl)

start.time <- Sys.time()


# for(j in 1:length(preds)){
foreach(j=1:length(preds)) %dopar% {
    
  
  print(j)
  
  pred.var <- preds[j] 
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  dir.create(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab)), recursive=T, showWarnings = FALSE) 
  
  other.vars <- preds[-j]
  
  for(w in 1:length(other.vars)){

    print(w)
    
    pred.var.w <- other.vars[w]
    
    pred.var.lab2 <- pred.var.w
    if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
    
    # Obs values of predictor vars
    x <- pdp_dat[, pred.var]
    y <- pdp_dat[, pred.var.w]
    
    # Resolution of predictor axes
    res <-  20 # Each grid step is 5% of the range of the variable (could also do quantiles)
    
    # Set limits of predictor x-axis - mid 95% range (note that I did 90% range for THg and MeHg plots)
    lim.j <- round(quantile(x, probs=c(0.025, 0.975)), 6)
    pred.val <- seq(lim.j[1],lim.j[2], (lim.j[2]-lim.j[1])/res)
    grid.j <- data.frame(PlaceHold=pred.val)
    names(grid.j) <- pred.var
    
    # Set limits of predictor y-axis - mid 95% range (note that I did 90% range for THg and MeHg plots)
    lim.w <- round(quantile(y, probs=c(0.025, 0.975)), 6)
    pred.val2 <- seq(lim.w[1],lim.w[2], (lim.w[2]-lim.w[1])/res)
    grid.w <- data.frame(PlaceHold=pred.val2)
    names(grid.w) <- pred.var.w
    
    pred.grid <- expand.grid(cbind(grid.j, grid.w), KEEP.OUT.ATTRS = FALSE) 

    
    
    ### Calculate PDP data
    Iso_Mean_Preds <- NULL # Data frame of mean predictions overall (PDP)
    
    # i is index for pred.grid row
    for(i in seq_len(nrow(pred.grid))){
      temp <- pdp_dat
      
      # replace variable of interest with two values of interest, leave rest of predictors as-is
      temp[, pred.var] <- pred.grid[i, pred.var] 
      temp[, pred.var.w] <- pred.grid[i, pred.var.w] 
      
      rf_predict_All <- predict(rf.final, newdata=temp)
      
      Lake_Preds_i <- data.frame(D199_pred = rf_predict_All$regrOutput$D199$predicted, 
                                 D200_pred = rf_predict_All$regrOutput$D200$predicted,
                                 D202_pred = rf_predict_All$regrOutput$D202$predicted,
                                 Maha20 = pdp_dat$Maha20,
                                 NLA12_ID = pdp_dat$NLA12_ID)
  
       
      
      # Calculate mean predicted isos in SD at x-value i across all lakes
      iso_mean_preds_temp <- data.frame(Pred_D199_SD = mean(Lake_Preds_i$D199_pred), 
                                        Pred_D200_SD = mean(Lake_Preds_i$D200_pred),
                                        Pred_D202_SD = mean(Lake_Preds_i$D202_pred))
      
      
      
      
      Iso_Mean_Preds <- rbind(Iso_Mean_Preds, iso_mean_preds_temp)
    }
  # End loop through x-vals and y-vals (pred.grid)
  
  
  # Bind mean predictions with pred.grid for saving and plotting
  res <- cbind(pred.grid, Iso_Mean_Preds)
  
  # saveRDS(res, paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
  saveRDS(res, paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
  }
  
}

stopCluster(cl)

end.time <- Sys.time()
print(end.time-start.time) # 2.6 min for one combo (x90 = 234 min = 3.9 hr serial)
# Took 3.12 hours in parallel





  
#### Make bivariate figures using output from above code - variable scale
  
for(j in 1:length(preds)){
  print(j)
  
  pred.var <- preds[j] 
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  dir.create(paste0(fig_dir, "PDP_SKATER20/D199/BIVARIATE_PLOTS/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D200/BIVARIATE_PLOTS/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D202/BIVARIATE_PLOTS/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/All3/BIVARIATE_PLOTS/", pred.var.lab), recursive=T, showWarnings = FALSE) 

  other.vars <- preds[-j]
  
  for(w in 1:length(other.vars)){
    
    print(w)
    
    pred.var.w <- other.vars[w]
    
    pred.var.lab2 <- pred.var.w
    if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
    
  # Read in PDP dat
  res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
  
  # Predicted isos in original units
  res_origunit <- res
  
  # Transform appropriate columns
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
  
  colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
  
  
  # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
  if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
  if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
  
  
  # Observed values for 2 vars of interest
  obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
  if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT

  
  
  # D199 all lakes PDP - origUnit
  # Center D199 at 0 - others will be centered at the mean
  # Also use coord_cartesian here because cuts off outer row otherwise
  p199 <- res_origunit %>%
    ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
    theme_minimal() +
    coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
    # xlim(range(res_origunit[pred.var]))+
    # ylim(range(res_origunit[pred.var.w])) +
    geom_tile(aes(fill=Pred_D199_origUnits)) +
    geom_contour(color = "white") +
    scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F) +
    theme(text=element_text(size=20)) +
    xlab(paste0(pred.var.lab)) +
    ylab(paste0(pred.var.lab2)) +
    geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
    # geom_point(aes(x =  get(pred.var), y = get(pred.var.w))) # Look to see where the data points are relative to tiles
  p199
  ggsave(paste0(fig_dir, "PDP_SKATER20/D199/BIVARIATE_PLOTS/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)

  # D200 all lakes PDP - origUnit
  p200 <- res_origunit %>%
    ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
    theme_minimal() +
    coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
    # xlim(range(res_origunit[pred.var]))+
    # ylim(range(res_origunit[pred.var.w])) +
    geom_tile(aes(fill=Pred_D200_origUnits)) +
    geom_contour(color = "white") +
    scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean(range(res_origunit$Pred_D200_origUnits)), alpha=1, rev=F) +
    theme(text=element_text(size=20)) +
    xlab(paste0(pred.var.lab)) +
    ylab(paste0(pred.var.lab2)) +
    geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
  # geom_point(aes(x =  get(pred.var), y = get(pred.var.w))) # Look to see where the data points are relative to tiles
  p200
  ggsave(paste0(fig_dir, "PDP_SKATER20/D200/BIVARIATE_PLOTS/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)
  
  # D202 all lakes PDP - origUnit
  p202 <- res_origunit %>%
    ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
    theme_minimal() +
    coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
    # xlim(range(res_origunit[pred.var]))+
    # ylim(range(res_origunit[pred.var.w])) +
    geom_tile(aes(fill=Pred_D202_origUnits)) +
    geom_contour(color = "white") +
    scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean(range(res_origunit$Pred_D202_origUnits)), alpha=1, rev=F) +
    theme(text=element_text(size=20)) +
    xlab(paste0(pred.var.lab)) +
    ylab(paste0(pred.var.lab2)) +
    geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
  # geom_point(aes(x =  get(pred.var), y = get(pred.var.w))) # Look to see where the data points are relative to tiles
  p202
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/BIVARIATE_PLOTS/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)
  
  # All isos PDP together
  ggarrange(p199, p200, p202, 
            # labels = c("A", "B", "C"),
            ncol = 3, nrow = 1)
  ggsave(paste0(fig_dir, "PDP_SKATER20/All3/BIVARIATE_PLOTS/", pred.var.lab, "/PDP_", pred.var.lab, "_", pred.var.lab2, ".png"), width=21, height=5)
  }
}



#### Make bivariate figures with fixed color scale ####

#### Get max and min predicted values over all predictor pairs ####
minD199 <- NA
maxD199 <- NA
minD200 <- NA
maxD200 <- NA    
minD202 <- NA
maxD202 <- NA   
meanD200 <- NA
meanD202 <- NA

for(j in 1:length(preds)){
  print(j)
  
  pred.var <- preds[j] 
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  other.vars <- preds[-j]
  
  for(w in 1:length(other.vars)){
    
    print(w)
    
    pred.var.w <- other.vars[w]
    
    pred.var.lab2 <- pred.var.w
    if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
    
    # Read in PDP dat
    res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
    
    
    # Predicted isos in original units
    res_origunit <- res
    
    # Transform appropriate columns
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
    
    colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
    
    minD199 <- c(minD199, min(res_origunit$Pred_D199_origUnits))
    maxD199 <- c(maxD199, max(res_origunit$Pred_D199_origUnits))
    
    minD200 <- c(minD200, min(res_origunit$Pred_D200_origUnits))
    maxD200 <- c(maxD200, max(res_origunit$Pred_D200_origUnits))  
    meanD200 <- c(meanD200, mean(res_origunit$Pred_D200_origUnits))
    
    minD202 <- c(minD202, min(res_origunit$Pred_D202_origUnits))
    maxD202 <- c(maxD202, max(res_origunit$Pred_D202_origUnits))   
    meanD202 <- c(meanD202, mean(res_origunit$Pred_D202_origUnits))
    
  }
}

min(minD199, na.rm = T)

range199 <- c( min(minD199, na.rm = T), max(maxD199, na.rm=T))
range200 <- c( min(minD200, na.rm = T), max(maxD200, na.rm=T))
range202 <- c( min(minD202, na.rm = T), max(maxD202, na.rm=T))
mean200 <- mean(meanD200, na.rm = T)
mean202 <- mean(meanD202, na.rm = T)

# Loop for bivariate plots with fixed scale
for(j in 1:length(preds)){
  print(j)
  
  pred.var <- preds[j] 
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
  
  dir.create(paste0(fig_dir, "PDP_SKATER20/D199/BIVARIATE_PLOTS_FIXED/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D200/BIVARIATE_PLOTS_FIXED/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/D202/BIVARIATE_PLOTS_FIXED/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  dir.create(paste0(fig_dir, "PDP_SKATER20/All3/BIVARIATE_PLOTS_FIXED/", pred.var.lab), recursive=T, showWarnings = FALSE) 
  
  
  
  other.vars <- preds[-j]
  
  for(w in 1:length(other.vars)){
    
    print(w)
    
    pred.var.w <- other.vars[w]
    
    pred.var.lab2 <- pred.var.w
    if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
    
    
    # Read in PDP dat
    res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
    
    # Predicted isos in original units
    res_origunit <- res
    
    # Transform appropriate columns
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
    res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
    
    colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
    
    
    
    # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
    if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
    if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
    
    
    
    # Observed values for 2 vars of interest
    obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
    if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT

    
    
    # D199 all lakes PDP - origUnit
    # Center D199 at 0 - others will be centered at the mean
    # Also use coord_cartesian here because cuts off outer row otherwise
    p199 <- res_origunit %>%
      ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
      theme_minimal() +
      coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
      # xlim(range(res_origunit[pred.var]))+
      # ylim(range(res_origunit[pred.var.w])) +
      geom_tile(aes(fill=Pred_D199_origUnits)) +
      geom_contour(color = "white") +
      scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
      theme(text=element_text(size=20)) +
      xlab(paste0(pred.var.lab)) +
      ylab(paste0(pred.var.lab2)) +
      geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
    p199
    ggsave(paste0(fig_dir, "PDP_SKATER20/D199/BIVARIATE_PLOTS_FIXED/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)
    
    # D200 all lakes PDP - origUnit
    p200 <- res_origunit %>%
      ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
      theme_minimal() +
      coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
      # xlim(range(res_origunit[pred.var]))+
      # ylim(range(res_origunit[pred.var.w])) +
      geom_tile(aes(fill=Pred_D200_origUnits)) +
      geom_contour(color = "white") +
      scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
      theme(text=element_text(size=20)) +
      xlab(paste0(pred.var.lab)) +
      ylab(paste0(pred.var.lab2)) +
      geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
    p200
    ggsave(paste0(fig_dir, "PDP_SKATER20/D200/BIVARIATE_PLOTS_FIXED/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)
    
    # D202 all lakes PDP - origUnit
    p202 <- res_origunit %>%
      ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
      theme_minimal() +
      coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
      # xlim(range(res_origunit[pred.var]))+
      # ylim(range(res_origunit[pred.var.w])) +
      geom_tile(aes(fill=Pred_D202_origUnits)) +
      geom_contour(color = "white") +
      scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
      theme(text=element_text(size=20)) +
      xlab(paste0(pred.var.lab)) +
      ylab(paste0(pred.var.lab2)) +
      geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
    p202
    ggsave(paste0(fig_dir, "PDP_SKATER20/D202/BIVARIATE_PLOTS_FIXED/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_", pred.var.lab2, ".png"), width=7, height=5)
    
    # All isos PDP together
    ggarrange(p199, p200, p202, 
              # labels = c("A", "B", "C"),
              ncol = 3, nrow = 1)
    ggsave(paste0(fig_dir, "PDP_SKATER20/All3/BIVARIATE_PLOTS_FIXED/", pred.var.lab, "/PDP_", pred.var.lab, "_", pred.var.lab2, ".png"), width=21, height=5)
  }
}


col.order <- c("Tmean8110Cat", "Precip8110Cat", "RunoffCat", "CompStrgthCat", "LOI_PERCENT", "SumForestCat", "PctOwWs_Mean", "Evap_Inflow_ratio", "WetLossConv", "Hg0DryDep")


#### Multi-panel figure for bivariate PDPs - each isotope separate ####
# Make sure run loop above to get ranges for each isotope
# range199 <- c( min(minD199, na.rm = T), max(maxD199, na.rm=T))
# range200 <- c( min(minD200, na.rm = T), max(maxD200, na.rm=T))
# range202 <- c( min(minD202, na.rm = T), max(maxD202, na.rm=T))
# mean200 <- mean(meanD200, na.rm = T)
# mean202 <- mean(meanD202, na.rm = T)

# pred.order <- c("Tmean8110Cat", "Precip8110Cat", "RunoffCat", "CompStrgthCat", "LOI_PERCENT", "SumForestCat", "PctOwWs_Mean", "Evap_Inflow_ratio", "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s", "Hg0DryDep")

pred.order <- c("Precip8110Cat", "RunoffCat", "Hg0DryDep", "LOI_PERCENT", "Evap_Inflow_ratio", "Tmean8110Cat", "SumForestCat", "CompStrgthCat",   "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s",  "PctOwWs_Mean")


# D199 
biplots199 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots199[[(j-1)*(length(pred.order)) + w]] <- local({
   
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      
      pred.var.w <- pred.order[w]
      
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      
      if(pred.var==pred.var.w){
        # if(w==1) p199 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
        # if(w>1 & w<length(pred.order)) p199 <- ggplot() + theme_void() 
        # if(w==length(pred.order)) p199 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
        p199 <- ggplot() + 
          annotate("text", x = 4, y = 25, size=16, label = paste(pred.var.lab)) + 
          theme_void()
          
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D199 all lakes PDP - origUnit
        # Center D199 at 0 - others will be centered at the mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<10){
         # y-axis label
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }

        if(j==1 & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # both labels
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm")) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # x-axis label
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }

        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% 10 != 0)){
          # no axis labels
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        # All axis labels
        # p199 <- res_origunit %>%
        #   ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
        #   theme_minimal() +
        #   coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
        #   geom_tile(aes(fill=Pred_D199_origUnits)) +
        #   geom_contour(color = "white") +
        #   scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
        #   theme(text=element_text(size=40),
        #         legend.text = element_text(size=40),
        #         legend.title = element_text(size=48),
        #         legend.key.size = unit(2, "cm")) +
        #   xlab(paste0(pred.var.lab)) +
        #   ylab(paste0(pred.var.lab2)) +
        #   geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        
        
        p199
        }
    })
  }
}

biplots199[[2]]

wrap.plot199 <- wrap_plots(biplots199, nrow=10, ncol=10, byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
ggsave(plot=wrap.plot199, filename=paste0(fig_dir, "PDP_SKATER20/D199_All_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)




# D200
biplots200 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots200[[(j-1)*(length(pred.order)) + w]] <- local({
      
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      
      pred.var.w <- pred.order[w]
      
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      
      if(pred.var==pred.var.w){
        p200 <- ggplot() + 
          annotate("text", x = 4, y = 25, size=16, label = paste(pred.var.lab)) + 
          theme_void()
        
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D200 all lakes PDP - origUnit
        # Center D200 at mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<10){
          # y-axis label
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if(j==1 & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # both labels
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm")) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # x-axis label
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% 10 != 0)){
          # no axis labels
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
          
        p200
      }
    })
  }
}

biplots200[[2]]

wrap.plot200 <- wrap_plots(biplots200, nrow=10, ncol=10, byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
ggsave(plot=wrap.plot200, filename=paste0(fig_dir, "PDP_SKATER20/D200_All_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)





# D202
biplots202 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots202[[(j-1)*(length(pred.order)) + w]] <- local({
      
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      
      pred.var.w <- pred.order[w]
      
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      
      if(pred.var==pred.var.w){
        p202 <- ggplot() + 
          annotate("text", x = 4, y = 25, size=16, label = paste(pred.var.lab)) + 
          theme_void()
        
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D202 all lakes PDP - origUnit
        # Center D202 at mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<10){
          # y-axis label
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if(j==1 & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # both labels
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm")) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% 10 == 0){
          # x-axis label
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% 10 != 0)){
          # no axis labels
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=40),
                  legend.text = element_text(size=40),
                  legend.title = element_text(size=48),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(paste0(pred.var.lab)) +
            ylab(paste0(pred.var.lab2)) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.09, shape=16, size=1)
        }
        
        p202
      }
    })
  }
}

biplots202[[2]]

wrap.plot202 <- wrap_plots(biplots202, nrow=10, ncol=10, byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
ggsave(plot=wrap.plot202, filename=paste0(fig_dir, "PDP_SKATER20/D202_All_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)






#### REDUCED multi-panel figure for bivariate PDPs - each isotope separate ####
pred.order <- c("Precip8110Cat", "RunoffCat", "Hg0DryDep", "LOI_PERCENT")

defined.pred.labs <- c("Mean precip. (mm)",
  "Mean runoff (mm)",
  # "Hg0 dry deposition (ng m^-3)",
  expression(paste("Hg"^"0", " dry dep. (ng ", "m"^"-3", ")")),
  "Sed. carbon (fraction)")

pred.labs.line1 <- c("Mean precipitation",
                     "Mean runoff",
                     expression(paste("Hg"^"0", " dry deposition")),
                     "Sediment carbon")
pred.labs.line2 <- c("(mm)",
                     "(mm)",
                     expression(paste("(ng ", "m"^"-3", ")")),
                     "(fraction)")

# bquote(atop("first line",
#             "second line" ~ x ^ 2))





# D199 
biplots199 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots199[[(j-1)*(length(pred.order)) + w]] <- local({
      
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      pred.var.lab.def <- defined.pred.labs[j]
      
      pred.var.lab.mid1 <- pred.labs.line1[j]
      pred.var.lab.mid2 <- pred.labs.line2[j]
      
      
      pred.var.w <- pred.order[w]
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      pred.var.lab2.def <- defined.pred.labs[w]
      
      
      
      if(pred.var==pred.var.w){
        # if(w==1) p199 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
        # if(w>1 & w<length(pred.order)) p199 <- ggplot() + theme_void() 
        # if(w==length(pred.order)) p199 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
        p199 <- ggplot() + 
          ylim(0,1) +
          # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
          theme_void()
        
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D199 all lakes PDP - origUnit
        # Center D199 at 0 - others will be centered at the mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<length(pred.order)){
          # y-axis label
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # both labels
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm")) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # x-axis label
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
          # no axis labels
          p199 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D199_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
       
        p199
      }
    })
  }
}

# biplots199[[2]]

wrap.plot199 <- wrap_plots(biplots199, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
# ggsave(plot=wrap.plot199, filename=paste0(fig_dir, "PDP_SKATER20/D199_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
ggsave(plot=wrap.plot199, filename=paste0(fig_dir, "PDP_SKATER20/D199_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)








# D200 
biplots200 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots200[[(j-1)*(length(pred.order)) + w]] <- local({
      
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      pred.var.lab.def <- defined.pred.labs[j]
      
      pred.var.lab.mid1 <- pred.labs.line1[j]
      pred.var.lab.mid2 <- pred.labs.line2[j]
      
      
      pred.var.w <- pred.order[w]
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      pred.var.lab2.def <- defined.pred.labs[w]
      
      
      
      if(pred.var==pred.var.w){
        # if(w==1) p200 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
        # if(w>1 & w<length(pred.order)) p200 <- ggplot() + theme_void() 
        # if(w==length(pred.order)) p200 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
        p200 <- ggplot() + 
          ylim(0,1) +
          # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
          theme_void()
        
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D200 all lakes PDP - origUnit
        # Center D200 at 0 - others will be centered at the mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<length(pred.order)){
          # y-axis label
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # both labels
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm")) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # x-axis label
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
          # no axis labels
          p200 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D200_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        p200
      }
    })
  }
}

# biplots200[[2]]

wrap.plot200 <- wrap_plots(biplots200, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
# ggsave(plot=wrap.plot200, filename=paste0(fig_dir, "PDP_SKATER20/D200_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
ggsave(plot=wrap.plot200, filename=paste0(fig_dir, "PDP_SKATER20/D200_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)





# D202 
biplots202 <- vector('list', length(pred.order)*length(pred.order))

for(j in 1:length(pred.order)){
  print(j)
  
  for(w in 1:length(pred.order)){
    
    print(w)
    
    biplots202[[(j-1)*(length(pred.order)) + w]] <- local({
      
      j <- j
      w <- w
      pred.var <- pred.order[j] 
      
      pred.var.lab <- pred.var
      if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
      pred.var.lab.def <- defined.pred.labs[j]
      
      pred.var.lab.mid1 <- pred.labs.line1[j]
      pred.var.lab.mid2 <- pred.labs.line2[j]
      
      
      pred.var.w <- pred.order[w]
      pred.var.lab2 <- pred.var.w
      if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
      
      pred.var.lab2.def <- defined.pred.labs[w]
      
      
      
      if(pred.var==pred.var.w){
        # if(w==1) p202 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
        # if(w>1 & w<length(pred.order)) p202 <- ggplot() + theme_void() 
        # if(w==length(pred.order)) p202 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
        p202 <- ggplot() + 
          ylim(0,1) +
          # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
          annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
          theme_void()
        
      } else{
        
        # Read in PDP dat
        res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
        
        # Predicted isos in original units
        res_origunit <- res
        
        # Transform appropriate columns
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
        res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
        
        colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
        
        
        
        # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
        if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
        
        # Observed values for 2 vars of interest
        obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
        if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
        
        
        
        # D202 all lakes PDP - origUnit
        # Center D202 at 0 - others will be centered at the mean
        # Also use coord_cartesian here because cuts off outer row otherwise
        
        if(j==1 & w<length(pred.order)){
          # y-axis label
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # both labels
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm")) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
          # x-axis label
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
          # no axis labels
          p202 <- res_origunit %>%
            ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
            theme_minimal() +
            coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
            geom_tile(aes(fill=Pred_D202_origUnits)) +
            geom_contour(color = "white") +
            scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
            theme(text=element_text(size=48),
                  legend.text = element_text(size=48),
                  legend.title = element_text(size=56),
                  legend.key.size = unit(2, "cm"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            xlab(pred.var.lab.def) +
            ylab(pred.var.lab2.def) +
            geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.2, shape=16, size=1)
        }
        
        p202
      }
    })
  }
}

# biplots202[[2]]

wrap.plot202 <- wrap_plots(biplots202, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "right")
# ggsave(plot=wrap.plot202, filename=paste0(fig_dir, "PDP_SKATER20/D202_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
ggsave(plot=wrap.plot202, filename=paste0(fig_dir, "PDP_SKATER20/D202_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)

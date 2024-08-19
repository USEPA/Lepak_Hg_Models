library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0
library(colorspace)

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





# Model trained on all lakes with iso data

# rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES.rds"))

preds <- rf.final$xvar.names # predictors
# Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg") # original observed isos
# All_Dat_sub <- All_Dat %>% dplyr::select(NLA12_ID, all_of(preds), all_of(Isos), TrainTest)


pdp_dat <- Preds_with_Clusters %>% dplyr::select(NLA12_ID, Maha20, all_of(preds))

table(pdp_dat$Maha20) # 15 have at least 10





#### Calculate mean predicted values for each isotope for all lakes together and by clusters ####

# Modified pdp code from pdp package
for(j in 1:length(preds)){

  start.time <- Sys.time()
  
  print(j)
  
  pred.var <- preds[j] #"Precip8110Cat"
  
  pred.var.lab <- pred.var
  if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
  
 
  x <- pdp_dat[, pred.var]
  
  # Set limits of predictor x-axis
  # lim.j <- round(quantile(x, probs=c(0.05, 0.95)), 6)
  lim.j <- round(quantile(x, probs=c(0, 1)), 6)
  
  # Resolution of predictor on x-axis
  pred.val <- seq(lim.j[1],lim.j[2], (lim.j[2]-lim.j[1])/40)
  names(pred.val) <- names(pred.var)
  
  # pred.val <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                  # length = 40)
  
  pred.grid <- expand.grid(pred.val, KEEP.OUT.ATTRS = FALSE) 
  names(pred.grid) <- pred.var

  
  # i is index for x-value
  Iso_Mean_Preds <- NULL
  for(i in seq_len(nrow(pred.grid))){
    temp <- pdp_dat
    
    # replace variable of interest with single value of interest, leave rest of predictors as-is
    temp[, pred.var] <- pred.grid[i, pred.var] 
    rf_predict_All <- predict(rf.final, newdata=temp)
    
    Lake_Preds_i <- data.frame(D199_pred = rf_predict_All$regrOutput$D199$predicted, 
                               D200_pred = rf_predict_All$regrOutput$D200$predicted,
                               D202_pred = rf_predict_All$regrOutput$D202$predicted,
                               Maha20 = pdp_dat$Maha20,
                               NLA12_ID = pdp_dat$NLA12_ID)
    
    # Mean predicted isos in SD at x-value
    iso_mean_preds_temp <- data.frame(Pred_D199_SD = mean(Lake_Preds_i$D199_pred), 
                                      Pred_D200_SD = mean(Lake_Preds_i$D200_pred),
                                      Pred_D202_SD = mean(Lake_Preds_i$D202_pred))
    
    cl_iso_mean_preds_temp_jn <- list()
    
    # Mean predicted isos in SD at x-value for each cluster
    for(k in seq_len(length(unique(Lake_Preds_i$Maha20))) ){
      
      cl <- sort(unique(Lake_Preds_i$Maha20))[k]
      
      cluster_sub <- Lake_Preds_i %>% filter(Maha20==cl)
      
      cl_iso_mean_preds_temp <- data.frame(Pred_D199_SD = mean(cluster_sub$D199_pred),
                                           Pred_D200_SD = mean(cluster_sub$D200_pred),
                                           Pred_D202_SD = mean(cluster_sub$D202_pred))
      
      names(cl_iso_mean_preds_temp) <- c(paste0('Pred_D199_SD_Cluster',cl),
                                         paste0('Pred_D200_SD_Cluster',cl),
                                         paste0('Pred_D202_SD_Cluster',cl))
      
      cl_iso_mean_preds_temp_jn[[k]] <- cl_iso_mean_preds_temp
    }
    
    cl_iso_mean_preds_temp_jn <- data.frame(do.call('cbind', cl_iso_mean_preds_temp_jn))
    
    # Join all-lake-mean-preds with cluster-level-mean-preds
    join_iso_mean_preds <- cbind(iso_mean_preds_temp, cl_iso_mean_preds_temp_jn)

    
    Iso_Mean_Preds <- rbind(Iso_Mean_Preds, join_iso_mean_preds)
  }
  
  end.time <- Sys.time()
  print(end.time-start.time) # 14.79553 secs for all lakes, 1 predictor
  
  # Bind mean predictions back to pred.grid for plotting
  res <- cbind(pred.grid, Iso_Mean_Preds)
  
  saveRDS(res, paste0(output_dir, "PDP/", pred.var.lab, "_PDP_dat.rds"))
}
    








#### Make figures using output from previous loop ####

# Cluster colors - matches 4_IsoPreds_Voronoi.R colors and map
cols2 <- sequential_hcl(5, palette = "Light Grays")
cols3 <- qualitative_hcl(17, palette = "Dark 3")
set.seed(13) # 5
cols <- sample(c(cols3, cols2[-c(4,5)]))


pdp_dat$Cluster <- factor(pdp_dat$Maha20)

Cl_kp <- names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)>9]) # 15 clusters with at least 10 lakes
# "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "14" "15" "16" "19"

names(table(pdp_dat$Cluster)[table(pdp_dat$Cluster)<10]) # "12" "13" "17" "18" "20"


# For cluster figs, keep all-lake reference lines?
# Make y-range be across across all cluster mean predictions for each isotope-predictor?


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
  
  # Predicted isos in original units
  res_origunit <- res
  
  # Transform appropriate columns
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
  res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))

  colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
  

  
  # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
  if(pred.var == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
  if(pred.var == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
  
  
  
  # Calculate 95% interval of observed predictor values
  x_dat <- pdp_dat %>% dplyr::select(one_of(pred.var))  # subset by cluster here in cluster loop
  if(pred.var == "LOI_PERCENT") x_dat <- 100*x_dat
  x_95_int <- quantile(x_dat[,1], c(.025, .975))
  
  

  # D199 all lakes - origUnit
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
    ylab("Mean D202 prediction") +
    coord_cartesian(xlim = range(res_origunit[pred.var]), 
                    ylim = range(res_origunit$Pred_D202_origUnits))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F)
  ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  
  # Pivot PDP in SD to long format
  res_long <- pivot_longer(res, cols ="Pred_D199_SD":"Pred_D202_SD_Cluster20", names_to = "Isotope", values_to = "Mean_Prediction")
  
  
  # All isos all lakes - SD
  res_long %>% filter(Isotope %in% c("Pred_D199_SD", "Pred_D200_SD", "Pred_D202_SD")) %>% 
    mutate(Isotope=case_match(Isotope,
               "Pred_D199_SD" ~ "D199",
               "Pred_D200_SD" ~ "D200",
               "Pred_D202_SD" ~ "D202")) %>% 
    ggplot(aes(x =  get(pred.var) , y = Mean_Prediction, col=Isotope)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
    geom_line(linewidth=2, aes(linetype=Isotope)) + 
    scale_color_manual(values=c("black", "gray20", "gray40")) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
    xlab(pred.var.lab) +
    ylab("Mean prediction (SD)") +
    coord_cartesian(xlim = range(res[pred.var]))+
    geom_rug(data=x_dat, aes(x=get(pred.var)), inherit.aes = F) +
    theme(legend.key.size = unit(3,"line"))  
  # , ylim = range(res_long$Mean_Prediction) # Add back in for fixed axes
  ggsave(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab, "/PDP_", pred.var.lab, "_ALL.png"), width=10, height=6)
  
  
  # Loop for cluster plots
  
  for(i in 1:length(unique(pdp_dat$Maha20))){
    
    skater_i <- sort(unique(pdp_dat$Maha20))[i]
    
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
        annotateText = c(paste0("Cluster ", skater_i)))
      # -diff(range(res_origunit[paste0("Pred_D199_origUnits_Cluster", skater_i)]))*.01

            res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D199_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.65, fill=cols[i]) +
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean D199 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D199_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations199,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D199/", pred.var.lab, "/PDP_D199_", pred.var.lab, "_Cluster",  skater_i, ".png"), width=10, height=6)
      
      

      
      
      # D200 by cluster - origUnit
      annotations200 <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.05,
        ypos =  max(res_origunit[paste0("Pred_D200_origUnits_Cluster", skater_i)]),
        annotateText = c(paste0("Cluster ", skater_i)))
      
      res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D200_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.65, fill=cols[i]) +
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean D200 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D200_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations200,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D200/", pred.var.lab, "/PDP_D200_", pred.var.lab, "_Cluster",  skater_i, ".png"), width=10, height=6)

      
      
      # D202 by cluster - origUnit
      annotations202 <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.05,
        ypos =  max(res_origunit[paste0("Pred_D202_origUnits_Cluster", skater_i)]),
        annotateText = c(paste0("Cluster ", skater_i)))
      
      res_origunit %>%
        ggplot(aes(x =  get(pred.var) , y = get(paste0("Pred_D202_origUnits_Cluster", skater_i)))) +
        theme_minimal() +
        theme(text=element_text(size=20))+
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.65, fill=cols[i]) +
        geom_line(linewidth=2) + 
        xlab(pred.var.lab) +
        ylab("Mean D202 prediction") +
        coord_cartesian(xlim = range(res_origunit[pred.var]), 
                        ylim = range(res_origunit[paste0("Pred_D202_origUnits_Cluster", skater_i)]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        geom_text(data=annotations202,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      ggsave(paste0(fig_dir, "PDP_SKATER20/D202/", pred.var.lab, "/PDP_D202_", pred.var.lab, "_Cluster",  skater_i, ".png"), width=10, height=6)
      

      
      
      # All isos by cluster - SD
      iso_dat_cl <- res_long %>% filter(Isotope %in% c(paste0("Pred_D199_SD_Cluster", skater_i), paste0("Pred_D200_SD_Cluster", skater_i), paste0("Pred_D202_SD_Cluster", skater_i)))
      
      annotations <- data.frame(
        xpos = min(res_origunit[pred.var]) + diff(range(res_origunit[pred.var]))*.06,
        ypos =   max(iso_dat_cl$Mean_Prediction),
        annotateText = c(paste0("Cluster ", skater_i)))
      
      
      
      iso_dat_cl %>% 
        mutate(Isotope=case_match(Isotope,
                                  paste0("Pred_D199_SD_Cluster", skater_i) ~ "D199",
                                  paste0("Pred_D200_SD_Cluster", skater_i) ~ "D200",
                                  paste0("Pred_D202_SD_Cluster", skater_i) ~ "D202")) %>% 
        ggplot(aes(x =  get(pred.var) , y = Mean_Prediction)) + 
        theme_minimal() +
        theme(text=element_text(size=20)) +
        annotate("rect", xmin=x_95_int[1], xmax=x_95_int[2], ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray60") +
        annotate("rect", xmin=x_95_int_cl[1], xmax=x_95_int_cl[2], ymin=-Inf, ymax=Inf, alpha=0.65, fill=cols[i]) +
        geom_line(linewidth=2, aes(col=Isotope, linetype=Isotope)) + 
        scale_color_manual(values=c("black", "gray20", "gray40")) +
        scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
        xlab(pred.var.lab) +
        ylab("Mean prediction (SD)") +
        coord_cartesian(xlim = range(res[pred.var]))+
        geom_rug(data=x_dat_cl, aes(x=get(pred.var)), inherit.aes = F) +
        theme(legend.key.size = unit(4,"line"))  +
        geom_text(data=annotations,aes(x=xpos,y=ypos,label=annotateText), size = 18/.pt)
      
      # , ylim = range(res_long$Mean_Prediction) # Add back in for fixed axes
      ggsave(paste0(fig_dir, "PDP_SKATER20/All3/", pred.var.lab, "/PDP_", pred.var.lab, "_Cluster", skater_i, ".png"), width=10, height=6)
      

    }
  }
  
}



# Much narrower range of mean predictions than individual lake predictions

# , xlim = range(Preds_with_Clusters[pred.var]), ylim = range(Preds_with_Clusters$Pred_D202_origUnits)
# theme(legend.key.size = unit(2,"line"))  












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


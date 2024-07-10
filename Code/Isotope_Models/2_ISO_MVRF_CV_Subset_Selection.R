# This script estimates CV error for each RFE iteration, selects best model using 1-SE rule, refits final model to all training data, evaluates performance on holdout test set

library(randomForestSRC) # 3.2.1
library(tidyverse) # tidyverse_2.0.0
library(foreach)
library(doParallel)
library(pdp) # 0.8.1
library(colorspace)
library(vegan)
library(maps)

# library(remotes)
# install_version( "tidyverse",version = "2.0.0")
# install_version("randomForestSRC", version = "3.2.1")


output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(output_dir, "CV"), showWarnings = F)
dir.create(paste0(output_dir, "PDP"), showWarnings = F)
dir.create(paste0(fig_dir, "PDP"), showWarnings = F)
dir.create(paste0(fig_dir, "PDP/Bivariate"), showWarnings = F)


# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/ISO_Imputed_Training_Data.csv") # 376
Test_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data.csv") # 34

Isos <- c("d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg")
# The processes governing D199 and D201 are the same. The processes governing D200 and D204 are the same. So please use d202, D199 and D200. 
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




## *****************  Change this for different models  ******************* ####
response_var <- c("D199", "D200", "D202")
mtry_par <- 1/3





##### Do 10-fold CV for all models #####

# Do stratified partitioning by space

Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83, HUC2, HUC8, Omernik_I, Omernik_II, Omernik_III)

sort(table(Lake_Geo$Omernik_II))
ecoregions <- unique(Lake_Geo$Omernik_II)

Train_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Train_Dat$NLA12_ID)
Test_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Test_Dat$NLA12_ID)

Train_ecoregions <- unique(Train_Geo$Omernik_II) # Has 18 ecoregions (missing one)
Test_ecoregions <- unique(Test_Geo$Omernik_II)   # Has 13 ecoregions - Does not have 5 ecoregions with <10 lakes

# Number lakes in each ecoregion
ecoregion_n <- Train_Geo %>% group_by(Omernik_II) %>% summarize(n=n()) %>% arrange(desc(n))
# Max number lakes in each ecoregion is 62

min_n <- floor(nrow(Train_Geo)/10) # min number lakes in each fold
# 10-fold CV will result in 4 folds with 37 lakes, 6 folds with 38 lakes

# Randomly put lakes in folds, attempting to allocate lakes in ecoregions evenly across folds
set.seed(73) 
Lake_folds <- NULL
fold_breakdown <- rep(1,10) # initiate

for(i in 1:length(ecoregion_n$Omernik_II)){
  
  # Lakes in ecoregion i
  lakes.i <-  Train_Geo %>% filter(Omernik_II==ecoregion_n$Omernik_II[i])
  
  # Sort lakes by Omernik_III, then state (state is in NLA12_ID), and then randomly distribute in order to attempt to evenly distribute ecoregions and nearby lakes across folds
  lakes.i <- lakes.i %>% arrange(Omernik_III, NLA12_ID)
  
  # Create vector of random permutations of fold numbers for assignment
  # Note that lake IDs are grouped spatially by state, so makes sense
  random.folds <- as.vector(replicate(n = ceiling(nrow(lakes.i)/10), expr = {sample(1:10, 10)}))

  if(fold_breakdown[1]>min_n) random.folds <- random.folds[!random.folds==1]
  if(fold_breakdown[2]>min_n) random.folds <- random.folds[!random.folds==2]
  if(fold_breakdown[3]>min_n) random.folds <- random.folds[!random.folds==3]
  if(fold_breakdown[4]>min_n) random.folds <- random.folds[!random.folds==4]
  if(fold_breakdown[5]>min_n) random.folds <- random.folds[!random.folds==5]
  if(fold_breakdown[6]>min_n) random.folds <- random.folds[!random.folds==6]
  if(fold_breakdown[7]>min_n) random.folds <- random.folds[!random.folds==7]
  if(fold_breakdown[8]>min_n) random.folds <- random.folds[!random.folds==8]
  if(fold_breakdown[9]>min_n) random.folds <- random.folds[!random.folds==9]
  if(fold_breakdown[10]>min_n) random.folds <- random.folds[!random.folds==10]
  
  lakes.i$folds <- random.folds[1:nrow(lakes.i)]
  
  Lake_folds <- rbind(Lake_folds, lakes.i)
  fold_breakdown <- table(Lake_folds$folds)
  
}

table(Lake_folds$folds) # Goal: 4 37s, 6 38s
sum(table(Lake_folds$folds)) # Should be 376

# Randomly reassign NLA12_IDs to folds with <min_n chems
while(sum(table(Lake_folds$folds)<min_n)>0){
  fold.assign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)<min_n])[1]
  
  folds.reassign <- as.numeric(names(table(Lake_folds$folds))[table(Lake_folds$folds)>min_n])
  lake.reassign <- Lake_folds %>% filter(folds %in% folds.reassign) %>% sample_n(1) %>% pull(NLA12_ID)
  Lake_folds$folds[Lake_folds$NLA12_ID==lake.reassign] <- fold.assign
}

table(Lake_folds$folds) # Goal: 4 37s, 6 38s
sum(table(Lake_folds$folds)) # Should be 376





### Do 10-fold CV to estimate error ####


## *****************  Change RESPONSE in loop below!!!!!!!!!! **************

# Vars to remove
RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)


# numCores <- detectCores()
# numCores <- detectCores()-1
# registerDoParallel(numCores)

registerDoParallel(20) # 2.3 hr on server

start.time <- Sys.time()

foreach(i = (1:nrow(RFE_info)), .packages=c('dplyr', 'randomForestSRC')) %dopar% {
  All_dat_preds_loop <- Train_Dat[, colnames(Train_Dat) %in% c(RFE_info$Worst_Var[i:nrow(RFE_info)], response_var, "NLA12_ID")]
  
  nump <- ncol(All_dat_preds_loop)-4    # Number predictors (subtracting responses, NLA12_ID)
  
  All_dat_Pred <- NULL
  
  for(j in 1:max(Lake_folds$folds)){
    print(paste("Iteration:", i, "out of", nrow(RFE_info), "... Fold", j))
    
    TestLakes <- Lake_folds$NLA12_ID[which(Lake_folds$folds==j)]
    
    TrainData <- All_dat_preds_loop %>% filter(!NLA12_ID %in% TestLakes) %>% dplyr::select(-NLA12_ID)
    
    TestData <- All_dat_preds_loop %>% filter(NLA12_ID %in% TestLakes)
    
    set.seed(73) 
    rf <- rfsrc(Multivar(D199, D200, D202) ~., data = TrainData, samptype="swr", importance=FALSE, splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=NULL, nodesize = 1)
    
    rf_pred_j <- predict(rf, newdata=TestData)
    
    TestData <- TestData %>% mutate(Pred_D199 = rf_pred_j$regrOutput$D199$predicted, 
                                    Pred_D200 = rf_pred_j$regrOutput$D200$predicted, 
                                    Pred_D202 = rf_pred_j$regrOutput$D202$predicted, 
                                    Fold=j)
    
    All_dat_Pred <- rbind(All_dat_Pred, TestData)
    
  }
  
  write.csv(All_dat_Pred, paste0(output_dir, "CV/CV_preds_Iter",i,".csv"), row.names = F)
  
}

stopImplicitCluster()  

end.time <- Sys.time()
end.time-start.time

# 5.36 hr on server



#### CV error estimates for RFE #####

## ******* NEED TO CHANGE RESPONSE VARIABLE IN LOOP BELOW FOR OTHER RESPONSES!!!!! **********

RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)

RFE_info$Avg_MeanCV_rmse <- RFE_info$Avg_MeanCV_mse <- RFE_info$Avg_MeanCV_mae <- RFE_info$Avg_MeanCV_bias <- NA
RFE_info$Avg_SE_CV_rmse <- RFE_info$Avg_SE_CV_mse <- RFE_info$Avg_SE_CV_mae <- RFE_info$Avg_SE_CV_bias <- NA

RFE_info$D199_MeanCV_rmse <- RFE_info$D199_MeanCV_mse <- RFE_info$D199_MeanCV_mae <- RFE_info$D199_MeanCV_bias <- NA
RFE_info$D199_SE_CV_rmse <- RFE_info$D199_SE_CV_mse <- RFE_info$D199_SE_CV_mae <- RFE_info$D199_SE_CV_bias <- NA

RFE_info$D200_MeanCV_rmse <- RFE_info$D200_MeanCV_mse <- RFE_info$D200_MeanCV_mae <- RFE_info$D200_MeanCV_bias <- NA
RFE_info$D200_SE_CV_rmse <- RFE_info$D200_SE_CV_mse <- RFE_info$D200_SE_CV_mae <- RFE_info$D200_SE_CV_bias <- NA

RFE_info$D202_MeanCV_rmse <- RFE_info$D202_MeanCV_mse <- RFE_info$D202_MeanCV_mae <- RFE_info$D202_MeanCV_bias <- NA
RFE_info$D202_SE_CV_rmse <- RFE_info$D202_SE_CV_mse <- RFE_info$D202_SE_CV_mae <- RFE_info$D202_SE_CV_bias <- NA


for(i in 1:nrow(RFE_info)){
  
  All_dat_Pred <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))
  
  CV_Stats_D199 <- All_dat_Pred %>% group_by(Fold) %>% summarize(
    MAE=mean(abs(D199-Pred_D199)), 
    RMSE=sqrt(mean((D199-Pred_D199)^2)), 
    MSE=mean((D199-Pred_D199)^2),
    Bias=mean(Pred_D199-D199)) 
  
  CV_Stats_D200 <- All_dat_Pred %>% group_by(Fold) %>% summarize(
    MAE=mean(abs(D200-Pred_D200)), 
    RMSE=sqrt(mean((D200-Pred_D200)^2)), 
    MSE=mean((D200-Pred_D200)^2),
    Bias=mean(Pred_D200-D200)) 

  CV_Stats_D202 <- All_dat_Pred %>% group_by(Fold) %>% summarize(
    MAE=mean(abs(D202-Pred_D202)), 
    RMSE=sqrt(mean((D202-Pred_D202)^2)), 
    MSE=mean((D202-Pred_D202)^2),
    Bias=mean(Pred_D202-D202)) 
  
  CV_Stats_Avg <- data.frame(Fold=CV_Stats_D202$Fold)
  CV_Stats_Avg$MAE <- rowMeans(cbind( CV_Stats_D199$MAE, CV_Stats_D200$MAE,  CV_Stats_D202$MAE ))
  CV_Stats_Avg$MSE <- rowMeans(cbind( CV_Stats_D199$MSE, CV_Stats_D200$MSE,  CV_Stats_D202$MSE ))
  CV_Stats_Avg$RMSE <- sqrt(CV_Stats_Avg$MSE)
  CV_Stats_Avg$Bias <- rowMeans(cbind( CV_Stats_D199$Bias, CV_Stats_D200$Bias,  CV_Stats_D202$Bias ))

  # Avg CV error and SE
  RFE_info$Avg_MeanCV_rmse[i] <- mean(CV_Stats_Avg$RMSE)
  RFE_info$Avg_MeanCV_mse[i] <- mean(CV_Stats_Avg$MSE)
  RFE_info$Avg_MeanCV_mae[i] <- mean(CV_Stats_Avg$MAE)
  RFE_info$Avg_MeanCV_bias[i] <- mean(CV_Stats_Avg$Bias)
  
  RFE_info$Avg_SE_CV_rmse[i] <- sd(CV_Stats_Avg$RMSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$Avg_SE_CV_mse[i] <- sd(CV_Stats_Avg$MSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$Avg_SE_CV_mae[i] <- sd(CV_Stats_Avg$MAE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$Avg_SE_CV_bias[i] <- sd(CV_Stats_Avg$Bias)/sqrt(max(All_dat_Pred$Fold))
  
  # D199 CV error and SE
  RFE_info$D199_MeanCV_rmse[i] <- mean(CV_Stats_D199$RMSE)
  RFE_info$D199_MeanCV_mse[i] <- mean(CV_Stats_D199$MSE)
  RFE_info$D199_MeanCV_mae[i] <- mean(CV_Stats_D199$MAE)
  RFE_info$D199_MeanCV_bias[i] <- mean(CV_Stats_D199$Bias)
  
  RFE_info$D199_SE_CV_rmse[i] <- sd(CV_Stats_D199$RMSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D199_SE_CV_mse[i] <- sd(CV_Stats_D199$MSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D199_SE_CV_mae[i] <- sd(CV_Stats_D199$MAE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D199_SE_CV_bias[i] <- sd(CV_Stats_D199$Bias)/sqrt(max(All_dat_Pred$Fold))
  
  # D200 CV error and SE
  RFE_info$D200_MeanCV_rmse[i] <- mean(CV_Stats_D200$RMSE)
  RFE_info$D200_MeanCV_mse[i] <- mean(CV_Stats_D200$MSE)
  RFE_info$D200_MeanCV_mae[i] <- mean(CV_Stats_D200$MAE)
  RFE_info$D200_MeanCV_bias[i] <- mean(CV_Stats_D200$Bias)
  
  RFE_info$D200_SE_CV_rmse[i] <- sd(CV_Stats_D200$RMSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D200_SE_CV_mse[i] <- sd(CV_Stats_D200$MSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D200_SE_CV_mae[i] <- sd(CV_Stats_D200$MAE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D200_SE_CV_bias[i] <- sd(CV_Stats_D200$Bias)/sqrt(max(All_dat_Pred$Fold))
  
  # D202 CV error and SE
  RFE_info$D202_MeanCV_rmse[i] <- mean(CV_Stats_D202$RMSE)
  RFE_info$D202_MeanCV_mse[i] <- mean(CV_Stats_D202$MSE)
  RFE_info$D202_MeanCV_mae[i] <- mean(CV_Stats_D202$MAE)
  RFE_info$D202_MeanCV_bias[i] <- mean(CV_Stats_D202$Bias)
  
  RFE_info$D202_SE_CV_rmse[i] <- sd(CV_Stats_D202$RMSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D202_SE_CV_mse[i] <- sd(CV_Stats_D202$MSE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D202_SE_CV_mae[i] <- sd(CV_Stats_D202$MAE)/sqrt(max(All_dat_Pred$Fold))
  RFE_info$D202_SE_CV_bias[i] <- sd(CV_Stats_D202$Bias)/sqrt(max(All_dat_Pred$Fold))
  
}


# write.csv(RFE_info, paste0(output_dir, "rf_RFE_info_wCVerrors.csv"), row.names = F)


RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info_wCVerrors.csv"))


# MAE iteration with least variables that has error within 1 SE of min error
min_it_mae <- which(RFE_info$Avg_MeanCV_mae==min(RFE_info$Avg_MeanCV_mae))
best_it_mae <- max(which(RFE_info$Avg_MeanCV_mae <= RFE_info$Avg_MeanCV_mae[min_it_mae] + RFE_info$Avg_SE_CV_mae[min_it_mae]))
# Remaining variables
RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)]

cat(rev(RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)]), sep = "\n")

RFE_info$Predictor <- RFE_info$Worst_Var
RFE_info$Predictor[RFE_info$Predictor=="WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s"] <- "WetLossConv"
RFE_info$Predictor[RFE_info$Predictor=="WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s"] <- "WetLossLS"


RFE_info$Subset_MAE <- "Out"
RFE_info$Subset_MAE[best_it_mae:nrow(RFE_info)] <- "In"
RFE_info$Point_Col <- "Black"
RFE_info$Point_Col[best_it_mae:nrow(RFE_info)] <- "firebrick3"

RFE_info  %>% filter(NumVars %in% 1:50) %>% 
  ggplot(aes(x=NumVars, y=Avg_MeanCV_mae, label=Predictor)) + 
  geom_point(size=3, aes(col=Subset_MAE)) + 
  geom_line(linewidth=1.2) +
  coord_cartesian(ylim=c(.5,1.1)) +
  geom_hline(yintercept=min(RFE_info$Avg_MeanCV_mae), size=1) +
  geom_hline(yintercept=RFE_info$Avg_MeanCV_mae[min_it_mae] + RFE_info$Avg_SE_CV_mae[min_it_mae], lty=2, size=1) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  annotate(geom = "text", x=rev(1:50), y=1.1, label=RFE_info$Predictor[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4, col=RFE_info$Point_Col[RFE_info$NumVars %in% 1:50]) +
  ylab("CV MAE") + xlab("Number variables") + 
  theme(legend.position="none") +
  scale_color_manual(values = c("firebrick3", "Black"))
ggsave(paste0(fig_dir, "/Avg_RFE_CV_MAE.png"), width=10, height=6)


iso_colors <- c("D199" = "darkgreen", "D200" = "darkorange", "D202" = "blue", "Avg" = "black")

# Add Iso lines
RFE_info  %>% filter(NumVars %in% 1:50) %>% 
  ggplot(aes(x=NumVars, y=Avg_MeanCV_mae, label=Predictor)) + 
  geom_point(size=3, col=RFE_info$Point_Col[RFE_info$NumVars %in% 1:50]) + 
  geom_line(linewidth=1.2, aes(col="Avg")) +

  geom_point(aes(y=D199_MeanCV_mae, col="D199")) +
  geom_line(aes(y=D199_MeanCV_mae, col="D199")) +
  geom_point(aes(y=D200_MeanCV_mae, col="D200")) +
  geom_line(aes(y=D200_MeanCV_mae, col="D200")) +
  geom_point(aes(y=D202_MeanCV_mae, col="D202")) +
  geom_line(aes(y=D202_MeanCV_mae, col="D202")) +
  labs( color = "Isotope") +
  scale_color_manual(values = iso_colors) +
  
  coord_cartesian(ylim=c(.4,1.2)) +
  geom_hline(yintercept=min(RFE_info$Avg_MeanCV_mae), size=1) +
  geom_hline(yintercept=RFE_info$Avg_MeanCV_mae[min_it_mae] + RFE_info$Avg_SE_CV_mae[min_it_mae], lty=2, size=1) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  annotate(geom = "text", x=rev(1:50), y=1.2, label=RFE_info$Predictor[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4, col=RFE_info$Point_Col[RFE_info$NumVars %in% 1:50]) +
  ylab("CV MAE") + xlab("Number variables") #+ 
  # theme(legend.position="none") +
  # scale_color_manual(values = c("firebrick3", "Black"))
ggsave(paste0(fig_dir, "/Avg_RFE_CV_MAE_wIso.png"), width=12, height=6)





# Visualize errors of best MAE model 
i <- best_it_mae
Top_MAE_Mod <- read.csv(paste0(output_dir, "CV/CV_preds_Iter",i,".csv"))

range(Top_MAE_Mod$D199)
range(Top_MAE_Mod$Pred_D199)

iso_colors <- c("D199" = "darkgreen", "D200" = "darkorange", "D202" = "blue", "Avg" = "black")

Top_MAE_Mod %>% ggplot(aes(x=D199, y=Pred_D199)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2, col="darkgreen") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D199") + ylab("Predicted D199")
ggsave(paste0(fig_dir, "/D199_Best_MAE_CVPred_vs_Obs.png"), width=8, height=6)

Top_MAE_Mod %>% ggplot(aes(x=D200, y=Pred_D200)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2, col="darkorange") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D200") + ylab("Predicted D200")
ggsave(paste0(fig_dir, "/D200_Best_MAE_CVPred_vs_Obs.png"), width=8, height=6)

Top_MAE_Mod %>% ggplot(aes(x=D202, y=Pred_D202)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2, col="blue") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D202") + ylab("Predicted D202")
ggsave(paste0(fig_dir, "/D202_Best_MAE_CVPred_vs_Obs.png"), width=8, height=6)







# Then REFIT FINAL MODEL to all training data, test on holdout test set, do PDPs ####

# Do  with best MAE model
# Using MAE instead of RMSE because more conservative in retaining predictors than RMSE and tracks with initial visual assessment

# MAE iteration with least variables that has error within 1 SE of min error
min_it_mae <- which(RFE_info$Avg_MeanCV_mae==min(RFE_info$Avg_MeanCV_mae))
best_it_mae <- max(which(RFE_info$Avg_MeanCV_mae <= RFE_info$Avg_MeanCV_mae[min_it_mae] + RFE_info$Avg_SE_CV_mae[min_it_mae]))

# Remaining variables
RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)]
final.preds <- c(RFE_info$Worst_Var[best_it_mae:nrow(RFE_info)])

Train_run <- Train_Dat[, colnames(Train_Dat) %in% c(final.preds, response_var)]

nump <- ncol(Train_run)-3    # Number predictors (subtracting responses)

# Fit final model  
set.seed(73) 
rf.final  <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(mtry_par*nump), 1), ntree=5000, block.size=1, nodesize = 1)
# saveRDS(rf.final, paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))
rf.final <- readRDS(paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))


# Predict test set
rf_predict_Test <- predict(rf.final, newdata=Test_Dat)

# Add predictions to Test set and write out
Test_Dat <- Test_Dat %>% mutate(Pred_D199 = rf_predict_Test$regrOutput$D199$predicted, 
                                    Pred_D200 = rf_predict_Test$regrOutput$D200$predicted, 
                                    Pred_D202 = rf_predict_Test$regrOutput$D202$predicted)

write.csv(Test_Dat, paste0(output_dir, "Test_Final_Mod_Preds.csv"), row.names = F)





### Compute Error tables ########

# Compute errors 
Test_Errors_D199 <- Test_Dat  %>% summarize(
  MAE=mean(abs(D199-Pred_D199)), 
  RMSE=sqrt(mean((D199-Pred_D199)^2)), 
  MSE=mean((D199-Pred_D199)^2),
  Bias=mean(Pred_D199-D199)) 
# MAE       RMSE       MSE       Bias
# 0.5139595 0.640327 0.4100186 -0.0214378

Test_Errors_D200 <- Test_Dat %>% summarize(
  MAE=mean(abs(D200-Pred_D200)), 
  RMSE=sqrt(mean((D200-Pred_D200)^2)), 
  MSE=mean((D200-Pred_D200)^2),
  Bias=mean(Pred_D200-D200)) 
# MAE       RMSE       MSE       Bias
# 0.7100451 0.9632848 0.9279177 -0.1219637

Test_Errors_D202 <- Test_Dat %>% summarize(
  MAE=mean(abs(D202-Pred_D202)), 
  RMSE=sqrt(mean((D202-Pred_D202)^2)), 
  MSE=mean((D202-Pred_D202)^2),
  Bias=mean(Pred_D202-D202)) 
# MAE       RMSE       MSE       Bias
# 0.7189243 0.8810962 0.7763305 0.0204517

Test_Errors_Avg <- data.frame(MAE=rowMeans(cbind( Test_Errors_D199$MAE, Test_Errors_D200$MAE,  Test_Errors_D202$MAE )))
Test_Errors_Avg$MSE <- rowMeans(cbind( Test_Errors_D199$MSE, Test_Errors_D200$MSE,  Test_Errors_D202$MSE ))
Test_Errors_Avg$RMSE <- sqrt(Test_Errors_Avg$MSE)
Test_Errors_Avg$Bias <- rowMeans(cbind( Test_Errors_D199$Bias, Test_Errors_D200$Bias,  Test_Errors_D202$Bias ))
# MAE       RMSE       MSE       Bias
# 0.647643 0.7047556 0.8394972 -0.04098325

Test_Errors_Avg_bind <- Test_Errors_Avg %>% dplyr::select(-MSE)


names(RFE_info[best_it_mae,])

# Compare to Avg CV error
CV_Errors_Avg <- RFE_info[best_it_mae,] %>% dplyr::select(Avg_MeanCV_mae, Avg_MeanCV_rmse, Avg_MeanCV_bias) %>% rename(MAE=Avg_MeanCV_mae, RMSE=Avg_MeanCV_rmse, Bias=Avg_MeanCV_bias)
OOB_Errors_Avg <- RFE_info[best_it_mae,] %>% dplyr::select(OOB_mae_Avg, OOB_rmse_Avg   , OOB_bias_Avg ) %>% rename(MAE=OOB_mae_Avg, RMSE=OOB_rmse_Avg, Bias=OOB_bias_Avg)

Errors <- rbind(Test_Errors_Avg_bind, CV_Errors_Avg, OOB_Errors_Avg)
Errors$Dataset <- c("Test_Set_34", "Train_CV_Select", "Train_Set_OOB")
Errors <- Errors %>% relocate(Dataset)
write.csv(Errors, paste0(output_dir, "Avg_Error_Table.csv"), row.names = F)


# Save errors by isotope

names(RFE_info[best_it_mae,])


CV_Errors_D199 <- RFE_info[best_it_mae,] %>% dplyr::select(D199_MeanCV_mae, D199_MeanCV_rmse,  D199_MeanCV_bias) %>% rename(MAE=D199_MeanCV_mae, RMSE=D199_MeanCV_rmse, Bias=D199_MeanCV_bias)
CV_Errors_D200 <- RFE_info[best_it_mae,] %>% dplyr::select(D200_MeanCV_mae, D200_MeanCV_rmse,  D200_MeanCV_bias) %>% rename(MAE=D200_MeanCV_mae, RMSE=D200_MeanCV_rmse, Bias=D200_MeanCV_bias)
CV_Errors_D202 <- RFE_info[best_it_mae,] %>% dplyr::select(D202_MeanCV_mae, D202_MeanCV_rmse,  D202_MeanCV_bias) %>% rename(MAE=D202_MeanCV_mae, RMSE=D202_MeanCV_rmse, Bias=D202_MeanCV_bias)

Iso_Errors <- rbind(Test_Errors_D199[,-3], CV_Errors_D199, Test_Errors_D200[,-3], CV_Errors_D200, Test_Errors_D202[,-3], CV_Errors_D202) 
Iso_Errors$Dataset <- c("D199_test", "D199_CV", "D200_test", "D200_CV", "D202_test", "D202_CV")
Iso_Errors <- Iso_Errors %>% relocate(Dataset)
write.csv(Iso_Errors, paste0(output_dir, "Iso_Error_Table.csv"), row.names = F)


Iso_Errors_Units <- Iso_Errors

Iso_Errors_Units[1:2,2:4] <- (Iso_Errors_Units[1:2,2:4])*(Iso_stats$D199[2])
Iso_Errors_Units[3:4,2:4] <- (Iso_Errors_Units[3:4,2:4])*(Iso_stats$D200[2])
Iso_Errors_Units[5:6,2:4] <- (Iso_Errors_Units[5:6,2:4])*(Iso_stats$D202[2])

write.csv(Iso_Errors_Units, paste0(output_dir, "Iso_Error_Table_Orig_Units.csv"), row.names = F)


# Compute relative errors

# Naive test errors
naive_pred <- 0

Naive_Test_Errors_D199 <- Test_Dat  %>% summarize(
  Naive_MAE=mean(abs(D199-naive_pred)), 
  Naive_RMSE=sqrt(mean((D199-naive_pred)^2)), 
  Naive_MSE=mean((D199-naive_pred)^2),
  Naive_Bias=mean(naive_pred-D199)) 

Naive_Test_Errors_D200 <- Test_Dat  %>% summarize(
  Naive_MAE=mean(abs(D200-naive_pred)), 
  Naive_RMSE=sqrt(mean((D200-naive_pred)^2)), 
  Naive_MSE=mean((D200-naive_pred)^2),
  Naive_Bias=mean(naive_pred-D200)) 

Naive_Test_Errors_D202 <- Test_Dat  %>% summarize(
  Naive_MAE=mean(abs(D202-naive_pred)), 
  Naive_RMSE=sqrt(mean((D202-naive_pred)^2)), 
  Naive_MSE=mean((D202-naive_pred)^2),
  Naive_Bias=mean(naive_pred-D202)) 

Naive_Test_Errors_Avg <- data.frame(Naive_MAE=rowMeans(cbind( Naive_Test_Errors_D199$Naive_MAE, Naive_Test_Errors_D200$Naive_MAE,  Naive_Test_Errors_D202$Naive_MAE )))

Naive_Test_Errors_Avg$Naive_MSE <- rowMeans(cbind( Naive_Test_Errors_D199$Naive_MSE, Naive_Test_Errors_D200$Naive_MSE,  Naive_Test_Errors_D202$Naive_MSE ))

Naive_Test_Errors_Avg$Naive_RMSE <- sqrt(Naive_Test_Errors_Avg$Naive_MSE)

Naive_Test_Errors_Avg$Naive_Bias <- rowMeans(cbind( Naive_Test_Errors_D199$Naive_Bias, Naive_Test_Errors_D200$Naive_Bias,  Naive_Test_Errors_D202$Naive_Bias ))

Relative_Test_Errors_Avg <- data.frame(RAE=Test_Errors_Avg$MAE/Naive_Test_Errors_Avg$Naive_MAE,
                                   RRSE=Test_Errors_Avg$RMSE/Naive_Test_Errors_Avg$Naive_RMSE)


# Naive CV errors
All_dat_Pred_Naive <- NULL
for(j in 1:max(Lake_folds$folds)){
  print(paste("Iteration:", i, "out of", nrow(RFE_info), "... Fold", j))
  TestLakes <- Lake_folds$NLA12_ID[which(Lake_folds$folds==j)]
  TrainData <- Train_Dat %>% filter(!NLA12_ID %in% TestLakes) %>% dplyr::select(-NLA12_ID)
  TestData <- Train_Dat %>% filter(NLA12_ID %in% TestLakes)
  
  TestData <- TestData %>% mutate(Pred_D199 = mean(TrainData$D199), 
                                  Pred_D200 = mean(TrainData$D200),
                                  Pred_D202 = mean(TrainData$D202),
                                  Fold=j)
  All_dat_Pred_Naive <- rbind(All_dat_Pred_Naive, TestData)

}


NaiveCV_Stats_D199 <- All_dat_Pred_Naive %>% group_by(Fold) %>% summarize(
  MAE=mean(abs(D199-Pred_D199)), 
  RMSE=sqrt(mean((D199-Pred_D199)^2)), 
  MSE=mean((D199-Pred_D199)^2),
  Bias=mean(Pred_D199-D199)) 

NaiveCV_Stats_D200 <- All_dat_Pred_Naive %>% group_by(Fold) %>% summarize(
  MAE=mean(abs(D200-Pred_D200)), 
  RMSE=sqrt(mean((D200-Pred_D200)^2)), 
  MSE=mean((D200-Pred_D200)^2),
  Bias=mean(Pred_D200-D200)) 

NaiveCV_Stats_D202 <- All_dat_Pred_Naive %>% group_by(Fold) %>% summarize(
  MAE=mean(abs(D202-Pred_D202)), 
  RMSE=sqrt(mean((D202-Pred_D202)^2)), 
  MSE=mean((D202-Pred_D202)^2),
  Bias=mean(Pred_D202-D202)) 

NaiveCV_Stats_Avg <- data.frame(Fold=NaiveCV_Stats_D202$Fold)
NaiveCV_Stats_Avg$MAE <- rowMeans(cbind( NaiveCV_Stats_D199$MAE, NaiveCV_Stats_D200$MAE,  NaiveCV_Stats_D202$MAE ))
NaiveCV_Stats_Avg$MSE <- rowMeans(cbind( NaiveCV_Stats_D199$MSE, NaiveCV_Stats_D200$MSE,  NaiveCV_Stats_D202$MSE ))
NaiveCV_Stats_Avg$RMSE <- sqrt(NaiveCV_Stats_Avg$MSE)
NaiveCV_Stats_Avg$Bias <- rowMeans(cbind( NaiveCV_Stats_D199$Bias, NaiveCV_Stats_D200$Bias,  NaiveCV_Stats_D202$Bias ))




Naive_MeanCV_mae_Avg <- mean(NaiveCV_Stats_Avg$MAE)
Naive_MeanCV_rmse_Avg <- mean(NaiveCV_Stats_Avg$RMSE)

Relative_CV_Errors_Avg <- data.frame(RAE=CV_Errors_Avg$MAE/Naive_MeanCV_mae_Avg,
                                 RRSE=CV_Errors_Avg$RMSE/Naive_MeanCV_rmse_Avg)


Relative_Errors <- rbind(Relative_Test_Errors_Avg, Relative_CV_Errors_Avg)
Relative_Errors$Dataset <- c("Test_Set_34", "Train_CV_Select")
Relative_Errors <- Relative_Errors %>% relocate(Dataset)
write.csv(Relative_Errors, paste0(output_dir, "Avg_Relative_Error_Table.csv"), row.names = F)



####################




# Visualize TEST errors 


Test_Dat %>% ggplot(aes(x=D199, y=Pred_D199)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2.5, col="darkgreen") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D199") + ylab("Predicted D199")
ggsave(paste0(fig_dir, "/D199_Best_MAE_TEST_vs_Obs.png"), width=8, height=6)

Test_Dat %>% ggplot(aes(x=D200, y=Pred_D200)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2.5, col="darkorange") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D200") + ylab("Predicted D200")
ggsave(paste0(fig_dir, "/D200_Best_MAE_TEST_vs_Obs.png"), width=8, height=6)

Test_Dat %>% ggplot(aes(x=D202, y=Pred_D202)) +
  geom_abline(intercept=0, slope=1, color="black", size=1.1) +
  geom_abline(intercept=1, slope=1, color="black", linetype="dashed", size=1.1) +
  geom_abline(intercept=-1, slope=1, color="black", linetype="dashed", size=1.1) +
  # geom_abline(intercept=log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +
  # geom_abline(intercept=-log10(5), slope=1, color="gray50", linetype="dashed", size=.8) +  
  geom_point(size=2.5, col="blue") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))+
  theme(text=element_text(size=20)) +
  xlab("Observed D202") + ylab("Predicted D202")
ggsave(paste0(fig_dir, "/D202_Best_MAE_TEST_vs_Obs.png"), width=8, height=6)





# Plot residuals spatially
Test_Dat$Residual_D199 <- Test_Dat$D199 - Test_Dat$Pred_D199
Test_Dat$Residual_D200 <- Test_Dat$D200 - Test_Dat$Pred_D200
Test_Dat$Residual_D202 <- Test_Dat$D202 - Test_Dat$Pred_D202
Top_MAE_Mod$Residual_D199 <- Top_MAE_Mod$D199 - Top_MAE_Mod$Pred_D199 # CV pred
Top_MAE_Mod$Residual_D200 <- Top_MAE_Mod$D200 - Top_MAE_Mod$Pred_D200 # CV pred
Top_MAE_Mod$Residual_D202 <- Top_MAE_Mod$D202 - Top_MAE_Mod$Pred_D202 # CV pred


Train_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Train_Dat$NLA12_ID)
Test_Geo <- Lake_Geo %>% filter(NLA12_ID %in% Test_Dat$NLA12_ID)


Test_Dat <- left_join(Test_Dat, Test_Geo)
Top_MAE_Mod <- left_join(Top_MAE_Mod, Train_Geo)

MainStates <- map_data("state")


# Spatial distribution of residuals
# ggplot(Test_Dat, aes(fill=Residual_D199, x=LON_DD83, y=LAT_DD83)) + geom_point(size=2) + theme_minimal()+
#   scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-3,3,1), limits=c(-3,3))

ggplot(Top_MAE_Mod, aes(fill=Residual_D199, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray70", shape=21) + 
  theme_void() +
  geom_point(data=Test_Dat, size=4, aes(fill=Residual_D199, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-3,3,1), limits=c(-3,3))
ggsave(paste0(fig_dir, "/Best_MAE_Residuals_Space_D199.png"), width=10, height=6)

ggplot(Top_MAE_Mod, aes(fill=Residual_D200, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray70", shape=21) + 
  theme_void() +
  geom_point(data=Test_Dat, size=4, aes(fill=Residual_D200, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-3,3,1), limits=c(-3,3))
ggsave(paste0(fig_dir, "/Best_MAE_Residuals_Space_D200.png"), width=10, height=6)

ggplot(Top_MAE_Mod, aes(fill=Residual_D202, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=3, col="gray70", shape=21) + 
  theme_void() +
  geom_point(data=Test_Dat, size=4, aes(fill=Residual_D202, x=LON_DD83, y=LAT_DD83),  col="black", shape=22) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-3,3,1), limits=c(-3,3))
ggsave(paste0(fig_dir, "/Best_MAE_Residuals_Space_D202.png"), width=10, height=6)



# ArmyRose, Earth, Fall, Geyser, TealRose, Temps, Tropic, PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu,  Spectral, Zissou 1, Cividis, Roma




### Test spatial autocorrelation for final model
CV_Resids <- Top_MAE_Mod %>% dplyr::select(Residual_D199, Residual_D200, Residual_D202, LON_DD83, LAT_DD83)

# D199
dists <- as.matrix(dist(cbind(CV_Resids$LAT_DD83, CV_Resids$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids$Residual_D199))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=0.04814 , p=.03 There is slight spatial autocorrelation in residuals
# D200
dists <- as.matrix(dist(cbind(CV_Resids$LAT_DD83, CV_Resids$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids$Residual_D200))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=-0.01869 , p=.82 No spatial autocorrelation in residuals
# D202
dists <- as.matrix(dist(cbind(CV_Resids$LAT_DD83, CV_Resids$LON_DD83)))
dist_res <- as.matrix(dist(CV_Resids$Residual_D202))
mantel_test <- mantel(dists, dist_res, permutations=1000)
mantel_test$signif # r=0.12 , p=0.001 There is slight spatial autocorrelation in residuals
















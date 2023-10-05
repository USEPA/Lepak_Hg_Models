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
Lake_Geo <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv") %>% dplyr::select(NLA12_ID, LAT_DD83, LON_DD83, HUC2, HUC8, Omernik_I, Omernik_II, Omernik_III)


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

# hist(Train_Dat$D199)
# hist(Train_Dat$D200)
# hist(Train_Dat$D202)


# Standardize isos in test set using training set stats
Test_Dat$D199 <- (Test_Dat$D199_Avg-Iso_stats$D199[1]) / Iso_stats$D199[2]
Test_Dat$D200 <- (Test_Dat$D200_Avg-Iso_stats$D200[1]) / Iso_stats$D200[2]
Test_Dat$D202 <- (Test_Dat$d202_Avg-Iso_stats$D202[1]) / Iso_stats$D202[2]

# hist(Test_Dat$D199, breaks=20)
# hist(Test_Dat$D200, breaks=20)
# hist(Test_Dat$D202, breaks=20)


# Remove NLA12_ID and original Isos from predictors

Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -all_of(Isos)) 
p <- ncol(Train_Dat_run)-3 # 127 preds - includes LOI, THg, MeHg




# Tune mtry across isos

mtrys <- c(floor(.1*p), floor(.15*p), floor(.2*p), floor(p/3), floor(.5*p), floor(2*p/3), floor(3*p/4), floor(.9*p))
# nodesizes <- c(1,5) - don't tune just set to 1

grid_search <- data.frame(mtry=mtrys)
grid_search$Avg_OOB_error <- NA
grid_search$D199_OOB_error <- NA
grid_search$D200_OOB_error <- NA
grid_search$D202_OOB_error <- NA

set.seed(3)
for(i in 1:nrow(grid_search)){

  # For this part, don't need importance
  
  mvrf.i <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_Dat_run, samptype="swr", importance="none", splitrule = "mahalanobis", mtry=grid_search$mtry[i], ntree=2000, block.size=NULL, nodesize = 1)
  # Doing ntree=2000 to reduce computation time for computing - errors level off after 50-150 trees
  # Set blocksize=NULL here to speed up - not looking at cumulative error or importance here
  
  grid_search$Avg_OOB_error[i] <- mean(get.mv.error(mvrf.i))
  grid_search$D199_OOB_error[i] <- get.mv.error(mvrf.i)[1]
  grid_search$D200_OOB_error[i] <- get.mv.error(mvrf.i)[2]
  grid_search$D202_OOB_error[i] <- get.mv.error(mvrf.i)[3]
  
  rm(mvrf.i)
}

# write.csv(grid_search, "Tables/Grid_Search/Iso_grid_srch.csv", row.names = F)
grid_search <- read.csv("Tables/Grid_Search/Iso_grid_srch.csv")

ggplot(grid_search, aes(x=mtry, y=Avg_OOB_error))+geom_point() + geom_line() # (5th one?)
ggplot(grid_search, aes(x=mtry, y=D199_OOB_error))+geom_point() + geom_line() # Definitely 4th
ggplot(grid_search, aes(x=mtry, y=D200_OOB_error))+geom_point() + geom_line() # keeps declining. 5th?
ggplot(grid_search, aes(x=mtry, y=D202_OOB_error))+geom_point() + geom_line() # 4h, but lowest at max (.9)

ggplot(grid_search, aes(x=mtry, y=Avg_OOB_error))+geom_point() + geom_line() +
  geom_point(aes(y=D199_OOB_error), col="blue") +
  geom_line(aes(y=D199_OOB_error), col="blue") +
  geom_point(aes(y=D200_OOB_error), col="orange") +
  geom_line(aes(y=D200_OOB_error), col="orange") +
  geom_point(aes(y=D202_OOB_error), col="green") +
  geom_line(aes(y=D202_OOB_error), col="green") 
  

# Try again with nodesize=5
mtrys5 <- c(floor(.1*p),  floor(.2*p), floor(p/3), floor(.5*p), floor(2*p/3), floor(3*p/4), floor(.9*p))

grid_search5 <- data.frame(mtry=mtrys5)
grid_search5$Avg_OOB_error <- NA
grid_search5$D199_OOB_error <- NA
grid_search5$D200_OOB_error <- NA
grid_search5$D202_OOB_error <- NA

set.seed(3)
for(i in 1:nrow(grid_search5)){
  
  # For this part, don't need importance
  
  mvrf.i <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_Dat_run, samptype="swr", importance="none", splitrule = "mahalanobis", mtry=grid_search5$mtry[i], ntree=2000, block.size=NULL, nodesize = 5)
  # Doing ntree=2000 to reduce computation time for computing - errors level off after 50-150 trees
  # Set blocksize=NULL here to speed up - not looking at cumulative error or importance here
  
  grid_search5$Avg_OOB_error[i] <- mean(get.mv.error(mvrf.i))
  grid_search5$D199_OOB_error[i] <- get.mv.error(mvrf.i)[1]
  grid_search5$D200_OOB_error[i] <- get.mv.error(mvrf.i)[2]
  grid_search5$D202_OOB_error[i] <- get.mv.error(mvrf.i)[3]
  
  rm(mvrf.i)
}

# write.csv(grid_search5, "Tables/Grid_Search/Iso_grid_srch_node5.csv", row.names = F)
grid_search5 <- read.csv("Tables/Grid_Search/Iso_grid_srch_node5.csv")

grid_search$node <- as.factor("1")
grid_search5$node <- as.factor("5")

grd <- rbind(grid_search, grid_search5)

ggplot(grd, aes(x=mtry, y=Avg_OOB_error, col=node))+geom_point() + geom_line() # (5th one?)
ggplot(grd, aes(x=mtry, y=D199_OOB_error, col=node))+geom_point() + geom_line() # node1, Definitely 4th
ggplot(grd, aes(x=mtry, y=D200_OOB_error, col=node))+geom_point() + geom_line() # node1. 5th?
ggplot(grd, aes(x=mtry, y=D202_OOB_error, col=node))+geom_point() + geom_line() # node1, 4h, but lowest at max (.9)

ggplot(grd, aes(x=mtry, y=Avg_OOB_error))+geom_point() + geom_line() +
  geom_point(aes(y=D199_OOB_error), col="blue") +
  geom_line(aes(y=D199_OOB_error), col="blue") +
  geom_point(aes(y=D200_OOB_error), col="orange") +
  geom_line(aes(y=D200_OOB_error), col="orange") +
  geom_point(aes(y=D202_OOB_error), col="green") +
  geom_line(aes(y=D202_OOB_error), col="green") 

##### Let's go with node=1, i=4, which is floor(p/3)




#### Fit and save full model ####
set.seed(36)
p <- ncol(Train_Dat_run)-3 # 127 preds - includes LOI, THg, MeHg
mvrf.all <- rfsrc(Multivar(D199, D200, D202) ~., data = Train_Dat_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=floor(p/3), ntree=5000, block.size=1, nodesize = 1)

# saveRDS(mvrf.all, paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36.rds"))
mvrf.all <- readRDS(paste0(model_dir, "Iso_full_mtryThird_ntree5000_node1_sd36.rds"))

mvrf.all
# These are just D199 performance metrics - not average
# (OOB) R squared: 0.6627744
# (OOB) Requested performance error: 0.3372256 
# R2 is 1- oob_mse/var(y)

# Avg OOB RMSE
sqrt(mean(get.mv.error(mvrf.all)))
# 0.7258042
# OOB RMSE for each response
sqrt(get.mv.error(mvrf.all))
# D199      D200      D202 
# 0.5807113 0.8042107 0.7722661
# R2 each response
1- get.mv.error(mvrf.all)

# Avg OOB MSE
mean(get.mv.error(mvrf.all))
# 0.5267918
# Avg R2
1-mean(get.mv.error(mvrf.all))
# 0.4732082

# splitrule  "mv.mse". Assums independence, doesn't take into account correlation among responses
# samptype="swr" is sampling with replacement
#  save.memory="TRUE",
# blocksize=1 gives importance metric comparable to usual permutation importance
# When block.size=1, VIMP is calculated for each tree. This is what was used in the traditional Breiman-Cutler VIMP and we will refer to this setting as tree VIMP. When block.size="ntree", VIMP is calculated for the entire forest by comparing the perturbed OOB forest ensemble (using all trees) to the unperturbed OOB forest ensemble (using all trees). This yields ensemble VIMP, which does not measure the tree average effect of a variable, but rather its overall forest effect.

# For multivariate families, predicted values (and other performance values such as VIMP and error rates) are stored in the lists regrOutput and clasOutput which can be extracted using functions get.mv.error, get.mv.predicted and get.mv.vimp.
# get.mv.error(mvrf.all) # OOB mse
# get.mv.error(mvrf.all, standardize=TRUE)  # Same as above because standardized responses
# get.mv.predicted(mvrf.all) # OOB MSE
# get.mv.predicted(mvrf.all, oob=TRUE) # OOB 
# mean((mvrf.all$regrOutput$D199$predicted.oob - Train_Dat_run$D199)^2)

# Cumulative errors to see where stabilizes - Need small block size to look at this - checked
# plot(mvrf.all$regrOutput$D199$err.rate) # flat after 75 trees
# plot(mvrf.all$regrOutput$D200$err.rate) # flat after 150 trees
# plot(mvrf.all$regrOutput$D202$err.rate) # flat after 100 trees

# names(mvrf.i$regrOutput$D199)
# # [1] "predicted"     "predicted.oob" "cse.num"       "cse.den"       "quantile"      "quantile.oob"  "err.rate"
# # [8] "csv.num"       "csv.den"   




# Plot permutation importance for full model
v <- as.data.frame(get.mv.vimp(mvrf.all,  standardize = TRUE))
## compare standardized VIMP for top 25 variables
imp <- data.frame(Predictor=row.names(v), 
                  D199_Importance = 100*v$D199,
                  D200_Importance = 100*v$D200,
                  D202_Importance = 100*v$D202, 
                  Avg_Importance = 100*rowMeans(v))
# default     = rowMeans(get.mv.vimp(obj2, standardize = TRUE)))
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
ggsave(paste0(fig_dir, "/VarImp_Avg_top50.png"), width=10, height=10)

imp  %>% arrange(desc(D202_Importance))  %>%
  slice(1:50) %>%
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>%
  ggplot(aes(x=Predictor, y=D202_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip()
ggsave(paste0(fig_dir, "/VarImp_d202_top50.png"), width=10, height=10)

imp  %>% arrange(desc(D199_Importance))  %>%
  slice(1:50) %>%
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>%
  ggplot(aes(x=Predictor, y=D199_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() 
ggsave(paste0(fig_dir, "/VarImp_D199_top50.png"), width=10, height=10)

imp  %>% arrange(desc(D200_Importance))  %>%
  slice(1:50) %>%
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>%
  ggplot(aes(x=Predictor, y=D200_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() 
ggsave(paste0(fig_dir, "/VarImp_D200_top50.png"), width=10, height=10)

imp$D202_Rank <- rank(-imp$D202_Importance)
imp$D199_Rank <- rank(-imp$D199_Importance)
imp$D200_Rank <- rank(-imp$D200_Importance)
imp$Avg_Rank <- rank(-imp$Avg_Importance)
# write.csv(imp, paste0(output_dir, "/Iso_Pred_Importance_withoutRFE.csv"), row.names = FALSE)










########### RFE #############


All_dat_preds_loop <- Train_Dat_run 

p <- ncol(Train_Dat_run)-3 # 127 preds - includes LOI, THg, MeHg

# Save OOB error, variable importance
VI.list <- NULL
var.list <- rep(NA,p)

OOB.rmse.D199 <- rep(NA,p)
OOB.mae.D199 <- rep(NA,p)
OOB.bias.D199 <- rep(NA,p)
train.rmse.D199 <- rep(NA,p)
train.mae.D199 <- rep(NA,p)
train.bias.D199 <- rep(NA,p)

OOB.rmse.D200 <- rep(NA,p)
OOB.mae.D200 <- rep(NA,p)
OOB.bias.D200 <- rep(NA,p)
train.rmse.D200 <- rep(NA,p)
train.mae.D200 <- rep(NA,p)
train.bias.D200 <- rep(NA,p)

OOB.rmse.D202 <- rep(NA,p)
OOB.mae.D202 <- rep(NA,p)
OOB.bias.D202 <- rep(NA,p)
train.rmse.D202 <- rep(NA,p)
train.mae.D202 <- rep(NA,p)
train.bias.D202 <- rep(NA,p)

OOB.rmse.Avg <- rep(NA,p)
OOB.mae.Avg <- rep(NA,p)
OOB.bias.Avg <- rep(NA,p)
train.rmse.Avg <- rep(NA,p)
train.mae.Avg <- rep(NA,p)
train.bias.Avg <- rep(NA,p)



start.time <- Sys.time()

for(i in 1:p){
  print(i)
  
  nump <- ncol(All_dat_preds_loop)-3 # Number predictors
  
  set.seed(13) 
  rf <- rfsrc(Multivar(D199, D200, D202) ~., data = All_dat_preds_loop, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=max(floor(nump/3), 1), ntree=5000, block.size=1, nodesize = 1)
  

  # OOB error - D199
  OOB.rmse.D199[i] <- sqrt(mean((Train_Dat_run$D199-rf$regrOutput$D199$predicted.oob)^2)) # OOB rmse
  OOB.mae.D199[i] <- mean(abs(Train_Dat_run$D199-rf$regrOutput$D199$predicted.oob)) # oob mae
  OOB.bias.D199[i] <- mean(rf$regrOutput$D199$predicted.oob-Train_Dat_run$D199) # oob bias
  # Training error - D199
  train.rmse.D199[i] <- sqrt(mean((Train_Dat_run$D199-rf$regrOutput$D199$predicted)^2)) # training rmse
  train.mae.D199[i] <- mean(abs(Train_Dat_run$D199-rf$regrOutput$D199$predicted)) #  training mae
  train.bias.D199[i] <- mean(rf$regrOutput$D199$predicted-Train_Dat_run$D199) # training bias
  
  # OOB error - D200
  OOB.rmse.D200[i] <- sqrt(mean((Train_Dat_run$D200-rf$regrOutput$D200$predicted.oob)^2)) # OOB rmse
  OOB.mae.D200[i] <- mean(abs(Train_Dat_run$D200-rf$regrOutput$D200$predicted.oob)) # oob mae
  OOB.bias.D200[i] <- mean(rf$regrOutput$D200$predicted.oob-Train_Dat_run$D200) # oob bias
  # Training error - D200
  train.rmse.D200[i] <- sqrt(mean((Train_Dat_run$D200-rf$regrOutput$D200$predicted)^2)) # training rmse
  train.mae.D200[i] <- mean(abs(Train_Dat_run$D200-rf$regrOutput$D200$predicted)) #  training mae
  train.bias.D200[i] <- mean(rf$regrOutput$D200$predicted-Train_Dat_run$D200) # training bias
 
  # OOB error - D202
  OOB.rmse.D202[i] <- sqrt(mean((Train_Dat_run$D202-rf$regrOutput$D202$predicted.oob)^2)) # OOB rmse
  OOB.mae.D202[i] <- mean(abs(Train_Dat_run$D202-rf$regrOutput$D202$predicted.oob)) # oob mae
  OOB.bias.D202[i] <- mean(rf$regrOutput$D202$predicted.oob-Train_Dat_run$D202) # oob bias
  # Training error - D202
  train.rmse.D202[i] <- sqrt(mean((Train_Dat_run$D202-rf$regrOutput$D202$predicted)^2)) # training rmse
  train.mae.D202[i] <- mean(abs(Train_Dat_run$D202-rf$regrOutput$D202$predicted)) #  training mae
  train.bias.D202[i] <- mean(rf$regrOutput$D202$predicted-Train_Dat_run$D202) # training bias
  
  # OOB error - Avg
  OOB.rmse.Avg[i] <- sqrt(mean(c( OOB.rmse.D199[i]^2, OOB.rmse.D200[i]^2,  OOB.rmse.D202[i]^2 )))
  OOB.mae.Avg[i] <- mean(c( OOB.mae.D199[i], OOB.mae.D200[i],  OOB.mae.D202[i] ))
  OOB.bias.Avg[i] <- mean(c( OOB.bias.D199[i], OOB.bias.D200[i],  OOB.bias.D202[i] ))
  # Training error - Avg
  train.rmse.Avg[i] <- sqrt(mean(c( train.rmse.D199[i]^2, train.rmse.D200[i]^2,  train.rmse.D202[i]^2 )))
  train.mae.Avg[i] <- mean(c( train.mae.D199[i], train.mae.D200[i],  train.mae.D202[i] ))
  train.bias.Avg[i] <- mean(c( train.bias.D199[i], train.bias.D200[i],  train.bias.D202[i] ))
  
  
  v <- as.data.frame(get.mv.vimp(rf,  standardize = TRUE))
  
  rf_imp <- data.frame(Variable=row.names(v), 
                    D199_Importance = 100*v$D199,
                    D200_Importance = 100*v$D200,
                    D202_Importance = 100*v$D202, 
                    Avg_Importance = 100*rowMeans(v))
  row.names(rf_imp) <- NULL
  rf_imp <- rf_imp %>% arrange(Avg_Importance) 



  VI.list[[i]] <- rf_imp
  var.list[i] <- rf_imp$Variable[1] # Least informative variable to remove
  

  All_dat_preds_loop <- All_dat_preds_loop[,!colnames(All_dat_preds_loop)==var.list[i]] # remove variable
  
  rm(rf)
}

# Save output 
saveRDS(VI.list, paste0(output_dir, "rf_RFE_VI_at_iter.rds")) # R object (list) with the variable importances at each iteration of the algorithm

# RFE info needs to be saved before do the CV errors!
RFE_info <- data.frame(Worst_Var=var.list, 
                       OOB_rmse_D199=OOB.rmse.D199, 
                       OOB_mae_D199=OOB.mae.D199, 
                       OOB_bias_D199=OOB.bias.D199, 
                       Train_rmse_D199=train.rmse.D199, 
                       Train_mae_D199=train.mae.D199, 
                       Train_bias_D199=train.bias.D199,
                       
                       OOB_rmse_D200=OOB.rmse.D200, 
                       OOB_mae_D200=OOB.mae.D200, 
                       OOB_bias_D200=OOB.bias.D200, 
                       Train_rmse_D200=train.rmse.D200, 
                       Train_mae_D200=train.mae.D200, 
                       Train_bias_D200=train.bias.D200,
                       
                       OOB_rmse_D202=OOB.rmse.D202, 
                       OOB_mae_D202=OOB.mae.D202, 
                       OOB_bias_D202=OOB.bias.D202, 
                       Train_rmse_D202=train.rmse.D202, 
                       Train_mae_D202=train.mae.D202, 
                       Train_bias_D202=train.bias.D202,
                       
                       OOB_rmse_Avg=OOB.rmse.Avg, 
                       OOB_mae_Avg=OOB.mae.Avg, 
                       OOB_bias_Avg=OOB.bias.Avg, 
                       Train_rmse_Avg=train.rmse.Avg, 
                       Train_mae_Avg=train.mae.Avg, 
                       Train_bias_Avg=train.bias.Avg)


write.csv(RFE_info, paste0(output_dir, "/rf_RFE_info.csv"), row.names = FALSE)

end.time <- Sys.time()
end.time-start.time

# 2.78 hr on compute server



###############



RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
VI.list <- readRDS(paste0(output_dir, "rf_RFE_VI_at_iter.rds")) # R object (list) with the variable importances at each iteration of the algorithm

RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)
RFE_info[(nrow(RFE_info)-9):nrow(RFE_info),]

names(RFE_info)

ggplot(RFE_info)+
  geom_line(aes(x=1:length(OOB_bias_Avg), y=OOB_bias_Avg), col="red") +
  geom_point(aes(x=1:length(OOB_bias_Avg), y=OOB_bias_Avg), col="red")+
  geom_hline(yintercept=0, col="red")


iso_colors <- c("D199" = "darkgreen", "D200" = "darkorange", "D202" = "blue", "Avg" = "black")

# Plot OOB error
RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_mae_Avg, label=Worst_Var)) +
  geom_point(size=3, aes(col="Avg")) +
  geom_line(size=1.2, aes(col="Avg")) +
  
  geom_point(aes(y=OOB_mae_D199, col="D199")) +
  geom_line(aes(y=OOB_mae_D199, col="D199")) +
  geom_point(aes(y=OOB_mae_D200, col="D200")) +
  geom_line(aes(y=OOB_mae_D200, col="D200")) +
  geom_point(aes(y=OOB_mae_D202, col="D202")) +
  geom_line(aes(y=OOB_mae_D202, col="D202")) +
  labs( color = "Isotope") +
  scale_color_manual(values = iso_colors) +

  coord_cartesian(ylim=c(.4,1.2)) +
  geom_hline(yintercept=min(RFE_info$OOB_mae_Avg)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=rev(1:50), y=1.2, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB MAE") + xlab("Number variables")
ggsave(paste0(fig_dir, "/RFE_OOB_MAE.png"), width=12, height=6)


RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse_Avg, label=Worst_Var)) +
  geom_point(size=3, aes(col="Avg")) +
  geom_line(size=1.2, aes(col="Avg")) +

  geom_point(aes(y=OOB_rmse_D199, col="D199")) +
  geom_line(aes(y=OOB_rmse_D199, col="D199")) +
  geom_point(aes(y=OOB_rmse_D200, col="D200")) +
  geom_line(aes(y=OOB_rmse_D200, col="D200")) +
  geom_point(aes(y=OOB_rmse_D202, col="D202")) +
  geom_line(aes(y=OOB_rmse_D202, col="D202")) +
  labs( color = "Isotope") +
  scale_color_manual(values = iso_colors) +
  
  coord_cartesian(ylim=c(.5,1.5)) +
  geom_hline(yintercept=min(RFE_info$OOB_rmse_Avg)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=rev(1:50), y=1.5, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")
ggsave(paste0(fig_dir, "/RFE_OOB_RMSE.png"), width=12, height=6)


#
RFE_info_write <- RFE_info %>% arrange(NumVars)
write.csv(RFE_info_write, paste0(output_dir, "Iso_rf_RFE_info.csv"), row.names = FALSE) # R object (list) with the variable importances at each iteration of the algorithm







##### Do CV for subset selection - see ISO_MVRF_CV_Subset_Selection.R #####

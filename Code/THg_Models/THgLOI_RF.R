# THg/LOI model

library(party) # 1.3-11 on VM
library(permimp) # permimp_1.0-2 
library(randomForest) # randomForest_4.7-1.1
# library(tuneRanger) # tuneRanger_0.5       
library(tidyverse) # tidyverse_1.3.2

# library(remotes)
# install_version(
#   "party",
#   version = "1.3-9")



output_dir <- "Model_Output/THgLOI/"
model_dir <- "Saved_Models/THgLOI/"
fig_dir <- "Figures/THgLOI/"

dir.create(paste0(output_dir))
dir.create(paste0(model_dir))
dir.create(paste0(fig_dir))

# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")

# Which predictors were imputed
Imputed_Preds <- read.csv("Tables/List_Imputed_Training_Preds_THg_MHg.csv")


hist(Train_Dat$STHG_ng_g)
hist(log10(Train_Dat$STHG_ng_g))

sum(c(Train_Dat$STHG_ng_g, Test_Dat$STHG_ng_g)==0)
names(Train_Dat)
range(Train_Dat$LOI_PERCENT)

# Make LOI a percent (doesn't matter for this model, but need for THg/LOI model
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100

# log-transform THg for all modeling
Train_Dat$log10THgLOI <- log10(Train_Dat$STHG_ng_g/Train_Dat$LOI_PERCENT)
Test_Dat$log10THgLOI <- log10(Test_Dat$STHG_ng_g/Test_Dat$LOI_PERCENT)




Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g, -LOI_PERCENT) 
p <- ncol(Train_Dat_run)-1 # 124 preds 


# Try tuning mtry and nodesize first at 5000 trees

# Try 1 vs 5 nodesize and mtry=
# floor(.1*p) # 12
# floor(.2*p) # 25
# floor(p/3) # 41
# floor(.5*p) # 62
# floor(2*p/3) # 83
# floor(3*p/4) # 93
# floor(.9*p) # 112

mtrys <- c(floor(.1*p), floor(.2*p), floor(p/3), floor(.5*p), floor(2*p/3), floor(3*p/4), floor(.9*p))
nodesizes <- c(1,5)

grid_search <- data.frame(mtry=rep(mtrys,2), nodesize=rep(nodesizes, times=c(7,7)))
grid_search$OOB_error <- NA
grid_search$rsq <- NA

set.seed(3)
for(i in 1:nrow(grid_search)){
  
  rf.i  <- randomForest(log10THgLOI ~ ., data=Train_Dat_run, 
                        mtry=grid_search$mtry[i], 
                        ntree=5000,
                        nodesize=grid_search$nodesize[i],
                        keep.forest=F,
                        keep.inbag = F,
                        importance=F,
                        replace=T)
  grid_search$OOB_error[i] <- rf.i$mse[5000] # final OOB mse
  grid_search$rsq[i] <- rf.i$rsq[5000] 
  rm(rf.i)
}

# write.csv(grid_search, "Tables/Grid_Search/THgLOI_grid_srch.csv", row.names = F)
grid_search <- read.csv("Tables/Grid_Search/THgLOI_grid_srch.csv")

ggplot(grid_search, aes(x=mtry, y=OOB_error, col=nodesize))+geom_point()
# nodesize=1 generally better but essentially same at mtry=p/3


mtrys2 <- c(floor(.05*p))
nodesizes <- c(1,5)

grid_search2 <- data.frame(mtry=rep(mtrys2,2), nodesize=rep(nodesizes, times=c(1,1)))
grid_search2$OOB_error <- NA
grid_search2$rsq <- NA

set.seed(3)
for(i in 1:nrow(grid_search2)){
  
  rf.i  <- randomForest(log10THgLOI ~ ., data=Train_Dat_run, 
                        mtry=grid_search2$mtry[i], 
                        ntree=5000,
                        nodesize=grid_search2$nodesize[i],
                        keep.forest=F,
                        keep.inbag = F,
                        importance=F,
                        replace=T)
  grid_search2$OOB_error[i] <- rf.i$mse[5000] # final OOB mse
  grid_search2$rsq[i] <- rf.i$rsq[5000] 
  rm(rf.i)
}

# write.csv(grid_search2, "Tables/Grid_Search/THgLOI_grid_srch2.csv", row.names = F)
grid_search2 <- read.csv("Tables/Grid_Search/THgLOI_grid_srch2.csv")

grid_search <- rbind(grid_search, grid_search2)
ggplot(grid_search, aes(x=mtry, y=OOB_error, col=nodesize))+geom_point()


mtrys3 <- c(floor(.15*p))
nodesizes <- c(1,5)

grid_search3 <- data.frame(mtry=rep(mtrys3,2), nodesize=rep(nodesizes, times=c(1,1)))
grid_search3$OOB_error <- NA
grid_search3$rsq <- NA

set.seed(13)
for(i in 1:nrow(grid_search3)){
  
  rf.i  <- randomForest(log10THgLOI ~ ., data=Train_Dat_run, 
                        mtry=grid_search3$mtry[i], 
                        ntree=5000,
                        nodesize=grid_search3$nodesize[i],
                        keep.forest=F,
                        keep.inbag = F,
                        importance=F,
                        replace=T)
  grid_search3$OOB_error[i] <- rf.i$mse[5000] # final OOB mse
  grid_search3$rsq[i] <- rf.i$rsq[5000] 
  rm(rf.i)
}

# write.csv(grid_search3, "Tables/Grid_Search/THgLOI_grid_srch3.csv", row.names = F)
grid_search3 <- read.csv("Tables/Grid_Search/THgLOI_grid_srch3.csv")

grid_search <- rbind(grid_search, grid_search3)
ggplot(grid_search, aes(x=mtry, y=OOB_error, col=nodesize))+geom_point()


# Let's go with nodesize=1, mtry=.15*p
# Not going with nodesize=1, mtry=.1*p because kind of a weird jumpy outlier
# And like that nodesize doesn't matter at mtry=0.15
# Note with LOI, mtry=.5*p, probably to capture LOI!

# Old:
# Best is 1/3 predictors and essentially equivalent at nodesize=1 and nodesize=5
# Pick nodesize=5 and see if fixes var importance issue




# Try tuneRF
# set.seed(36)
# rf.tun <- tuneRF(Train_Dat_run[,-128], Train_Dat_run[,128], stepFactor=1.1, ntreeTry=5000, nodesize=1, improve=0.05)
# saveRDS(rf.tun, "Saved_Models/THg/THg_rf_tune_ntree5000_node1_sd36_improve05.rds")

# plot(rf.tun[,1], rf.tun[,2])
# Best is mtry=42 Increasing to 46 decreased OOB error by 0.03%





# First try classic random forest algorithm because all predictors are continuous (or almost all, some ordered categorical converted to numerical, one binary)
set.seed(36)
rf.all  <- randomForest(log10THgLOI ~ ., data=Train_Dat_run, 
                        mtry=max(floor(.15*p), 1), 
                        ntree=5000,
                        nodesize=1,
                        keep.forest=T,
                        keep.inbag = T,
                        importance=T)
saveRDS(rf.all, paste0(model_dir, "rf_full_mtry15_ntree5000_node1_sd36.rds"))
# rf.all <- readRDS(paste0(model_dir, "rf_full_mtry15_ntree5000_node1_sd36.rds"))



rf.all
# MSE: 0.07363133
# % Var explained: 28.97 

# mean((rf.all$predicted - Train_Dat_run$log10THg)^2) # OOB error
# rf.all$
# rf.all$mse[5000] # final OOB mse

plot(rf.all$mse, type='l')

names(Train_Dat)

str(rf.all$importance)

# Unconditional variable importance
rf_imp <- data.frame(Variable=names(rf.all$importance[,1]), Importance=rf.all$importance[,1])
row.names(rf_imp) <- NULL
rf_imp <- rf_imp %>% arrange(desc(Importance))
rf_imp$Variable <- factor(rf_imp$Variable, levels=rev(rf_imp$Variable))

head(rf_imp, 10)

ggplot(rf_imp, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_x_discrete(label=abbreviate)

# Importance at nodesize=1 is nearly identical to nodesize=5
# rf_imp1 <- data.frame(Variable=names(rf.all1$importance[,1]), Importance=rf.all1$importance[,1])
# row.names(rf_imp1) <- NULL
# rf_imp1 <- rf_imp1 %>% arrange(desc(Importance))
# rf_imp1$Variable <- factor(rf_imp1$Variable, levels=rev(rf_imp1$Variable))



# Conditional permutation importance - still not working
# Conditional variable importance (can try values between 0.8-1)
# set.seed(3)
# start.time <- Sys.time()
# rf.cpi9999 <- permimp(rf.all, conditional=TRUE, threshold=0.9999, thresholdDiagnostics = TRUE)
# end.time <- Sys.time()
# end.time-start.time # 5.7
# # saveRDS(rf.cpi9999, paste0(output_dir, "CPI9999_rf_full_mtryHalf_ntree5000_node1_sd36.rds"))
# rf.cpi9999 <- readRDS(paste0(output_dir, "CPI9999_rf_full_mtryHalf_ntree5000_node1_sd36.rds")) #
# 


# as.matrix(sort(rf.cpi9999$values, decreasing=T)) # CF, conditional
# plot(rf.cpi9999, type="bar", las=1)
# 
# rf_impC <- tibble(Variable=names(rf.cpi9999$values), Importance=rf.cpi9999$values) %>% arrange(desc(Importance))
# rf_impC$Variable <- factor(rf_impC$Variable, levels=rev(rf_impC$Variable))
# 
# head(rf_impC, 10)

# This must have happened because correlated with many predictors and couldn't permute within grid

# Warning: ‘permimp’ Unable to permute conditionally for 78 variable(s) in 50 percent of the cases.
# Increasing the threshold may help. 
# The variables for which conditionally permuting (often) was impossible are: (showing only six) 
# - Gas_Hg_Hg0Conc_ng_m3
# - Ionic_Hg2Conc_ng_m3
# - Particle_Hg_HgPConc_ng_m3
# - Hg0DryDep
# - Hg2DryDep
# - HgPDryDep





########### START HERE WITH RFE #############



# RFE with unconditional permutation importance

# Pull out response and predictors
All_dat_preds_loop <- Train_Dat_run 

p <- ncol(Train_Dat_run)-1 # 

# Save OOB error, variable importance
VI.list <- NULL
var.list <- rep(NA,p)

OOB.rmse <- rep(NA,p)
OOB.mae <- rep(NA,p)
OOB.bias <- rep(NA,p)

train.rmse <- rep(NA,p)
train.mae <- rep(NA,p)
train.bias <- rep(NA,p)



start.time <- Sys.time()

for(i in 1:p){
  print(i)
  
  nump <- ncol(All_dat_preds_loop)-1 # Number predictors
  
  set.seed(13) 
  rf  <- randomForest(log10THgLOI ~ ., data=All_dat_preds_loop, 
                      mtry=max(floor(.15*nump), 1), 
                      ntree=5000,
                      nodesize=1,
                      #keep.forest=T,
                      # keep.inbag = F,
                      importance=T)
  
  rf.pred <- rf$predicted # OOB
  rf.pred.tr <- predict(rf, newdata=Train_Dat_run) # training
  
  # OOB error
  OOB.rmse[i] <- sqrt(mean((Train_Dat_run$log10THg-rf.pred)^2)) # OOB rmse
  OOB.mae[i] <- mean(abs(Train_Dat_run$log10THg-rf.pred)) # oob mae
  OOB.bias[i] <- mean(rf.pred-Train_Dat_run$log10THg) # oob bias
  
  # Training error
  train.rmse[i] <- sqrt(mean((Train_Dat_run$log10THg-rf.pred.tr)^2)) # training rmse
  train.mae[i] <- mean(abs(Train_Dat_run$log10THg-rf.pred.tr)) #  training mae
  train.bias[i] <- mean(rf.pred.tr-Train_Dat_run$log10THg) # training bias
  
  # Conditional permutation importance
  # set.seed(3) 
  # cf.cpi <- permimp(rf, conditional=TRUE, threshold=0.9) # set threshold to 1 for unconditional
  
  
  rf_imp <- data.frame(Variable=row.names(rf$importance), Importance=rf$importance[,1])
  row.names(rf_imp) <- NULL
  rf_imp <- rf_imp %>% arrange(Importance)
  # rf_imp$Variable <- factor(rf_imp$Variable, levels=rf_imp$Variable)
  
  # plot(rf$importance[,1], rf$importance[,2])
  
  VI.list[[i]] <- rf_imp
  var.list[i] <- rf_imp$Variable[1] # Least informative variable to remove
  
  # If least informative variable is species, go to next variable
  # if(var.list[i]=="Species") var.list[i] <- row.names(VI.list[[i]])[2]
  
  All_dat_preds_loop <- All_dat_preds_loop[,!colnames(All_dat_preds_loop)==var.list[i]] # remove variable
  
  rm(rf)
}

# Save output 
saveRDS(VI.list, paste0(output_dir, "rf_RFE_VI_at_iter.rds")) # R object (list) with the variable importances at each iteration of the algorithm


# RFE info needs to be saved before do the CV errors!
RFE_info <- data.frame(Worst_Var=var.list, OOB_rmse=OOB.rmse, OOB_mae=OOB.mae, OOB_bias=OOB.bias, Train_rmse=train.rmse, Train_mae=train.mae, Train_bias=train.bias)
write.csv(RFE_info, paste0(output_dir, "rf_RFE_info.csv"), row.names = FALSE)

end.time <- Sys.time()
end.time-start.time


RFE_info <- read.csv(paste0(output_dir, "rf_RFE_info.csv"))
VI.list <- readRDS(paste0(output_dir, "rf_RFE_VI_at_iter.rds")) # R object (list) with the variable importances at each iteration of the algorithm

RFE_info$Iteration <- 1:nrow(RFE_info)
RFE_info$NumVars <- rev(RFE_info$Iteration)
RFE_info[(nrow(RFE_info)-9):nrow(RFE_info),]

ggplot(RFE_info, aes(x=1:length(OOB_rmse), y=OOB_rmse)) + geom_line() + geom_point() + 
  geom_hline(yintercept=min(RFE_info$OOB_rmse)) +
  geom_line(aes(x=1:length(OOB_mae), y=OOB_mae), col="blue") +
  geom_point(aes(x=1:length(OOB_mae), y=OOB_mae), col="blue") + 
  geom_hline(yintercept=min(RFE_info$OOB_mae), col="blue")

ggplot(RFE_info)+
  geom_line(aes(x=1:length(OOB_mae), y=OOB_bias), col="red") +
  geom_point(aes(x=1:length(OOB_mae), y=OOB_bias), col="red")+ 
  geom_hline(yintercept=0, col="red")

# ggplot(RFE_info, aes(x=1:length(OOB_mae), y=OOB_mae)) + geom_line() + geom_point()
ggplot(RFE_info, aes(x=1:length(OOB_mae), y=OOB_bias)) + geom_line() + geom_point()



which(RFE_info$OOB_rmse==min(RFE_info$OOB_rmse)) # 79
which(RFE_info$OOB_mae==min(RFE_info$OOB_mae)) # 68

min(RFE_info$OOB_rmse) # 0.2679942
min(RFE_info$OOB_mae) # 0.1733658


RFE_info[which(RFE_info$OOB_rmse==min(RFE_info$OOB_rmse)) : nrow(RFE_info),]


tail(VI.list[[1]], 10)
tail(RFE_info, 10)

# Plot OOB error
RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_mae, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.15,.4)) +
  geom_hline(yintercept=min(RFE_info$OOB_mae)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=rev(1:50), y=.40, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB MAE") + xlab("Number variables")
ggsave(paste0(fig_dir, "/RFE_OOB_MAE.png"), width=10, height=6)


RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.25,.5)) +
  geom_hline(yintercept=min(RFE_info$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=rev(1:50), y=.50, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")
ggsave(paste0(fig_dir, "/RFE_OOB_RMSE.png"), width=10, height=6)


#
RFE_info_write <- RFE_info %>% arrange(NumVars)
write.csv(RFE_info_write, paste0(output_dir, "THgLOI_rf_RFE_info.csv"), row.names = FALSE)



# Do CV at each iteration to get SE

# Do stratified partitioning by space though to get 
# Rerun with THg/LOI% ??????????




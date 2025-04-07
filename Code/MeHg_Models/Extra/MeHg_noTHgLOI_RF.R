# MeHg model without THg or LOI

# library(party) # 1.3-11 on VM
# library(permimp) # permimp_1.0-2 
library(randomForest) # randomForest_4.7-1.1
# library(tuneRanger) # tuneRanger_0.5       
library(tidyverse) # tidyverse_1.3.2

# library(remotes)
# install_version(
#   "party",
#   version = "1.3-9")

output_dir <- "Model_Output/Extra/MeHg_noTHgLOI/"
model_dir <- "Saved_Models/Extra/MeHg_noTHgLOI/"
fig_dir <- "Figures/Extra/MeHg_noTHgLOI/"

dir.create(paste0(output_dir))
dir.create(paste0(output_dir,"/Temp_Files/"))
dir.create(paste0(model_dir))
dir.create(paste0(fig_dir))

# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")

# Which predictors were imputed
Imputed_Preds <- read.csv("Tables/List_Imputed_Training_Preds_THg_MHg.csv")


hist(Train_Dat$SMHG_ng_g)
hist(log10(Train_Dat$SMHG_ng_g))

sum(c(Train_Dat$SMHG_ng_g, Test_Dat$SMHG_ng_g)==0)
names(Train_Dat)

# log-transform THg for all modeling
Train_Dat$log10THg <- log10(Train_Dat$STHG_ng_g)
Test_Dat$log10THg <- log10(Test_Dat$STHG_ng_g)

# Make LOI a percent doesn't matter for this model
Train_Dat$LOI_PERCENT <- Train_Dat$LOI_PERCENT/100
Test_Dat$LOI_PERCENT <- Test_Dat$LOI_PERCENT/100

# log-transform MeHg for all modeling
Train_Dat$log10MeHg <- log10(Train_Dat$SMHG_ng_g)
Test_Dat$log10MeHg <- log10(Test_Dat$SMHG_ng_g)

# Toss lakes where MeHg > THg
Train_Dat %>% filter(STHG_ng_g <SMHG_ng_g) %>% dplyr::select(STHG_ng_g, SMHG_ng_g,  NLA12_ID) # NLA12_WY-R02
Test_Dat %>% filter(STHG_ng_g <SMHG_ng_g) %>% dplyr::select(STHG_ng_g, SMHG_ng_g,  NLA12_ID) # No test lakes have this issue

Train_Dat <- Train_Dat %>% filter(!(STHG_ng_g <SMHG_ng_g)) # Now 974


# Remove THg and LOI
Train_Dat_run <- Train_Dat %>% dplyr::select(-NLA12_ID, -SMHG_ng_g, -STHG_ng_g, -LOI_PERCENT, -log10THg) 
p <- ncol(Train_Dat_run)-1 # 124 preds 


# Try tuning mtry and nodesize first at 5000 trees
mtrys <- c(floor(.1*p), floor(.15*p), floor(.2*p), floor(p/3), floor(.5*p), floor(2*p/3), floor(3*p/4)) 
nodesizes <- c(1, 5)

length(mtrys)

grid_search <- data.frame(mtry=rep(mtrys,2), nodesize=rep(nodesizes, times=c(length(mtrys),length(mtrys))))
grid_search$OOB_error <- NA
grid_search$rsq <- NA

set.seed(7)
for(i in 1:nrow(grid_search)){
  print(i)
  rf.i  <- randomForest(log10MeHg ~ ., data=Train_Dat_run, 
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

write.csv(grid_search, "Tables/Grid_Search/MeHg_noTHgLOI_grid_srch.csv", row.names = F)
# grid_search <- read.csv("Tables/Grid_Search/MeHg_noTHgLOI_grid_srch.csv")

ggplot(grid_search, aes(x=mtry, y=OOB_error, col=nodesize))+geom_point()

grid_search %>% arrange(OOB_error)
floor(p/3)
# Let's go with nodesize=5, mtry=p/3 where it levels off






# First try classic random forest algorithm because all predictors are continuous (or almost all, some ordered categorical converted to numerical, one binary)
set.seed(36)
rf.all  <- randomForest(log10MeHg ~ ., data=Train_Dat_run, 
                        mtry=max(floor(p/3), 1), 
                        ntree=5000,
                        nodesize=5,
                        keep.forest=T,
                        keep.inbag = T,
                        importance=T)
saveRDS(rf.all, paste0(model_dir, "rf_full_mtryThird_ntree5000_node5_sd36.rds"))
# rf.all <- readRDS(paste0(model_dir, "rf_full_mtryThird_ntree5000_node5_sd36.rds"))

rf.all
# MSE: 0.1271845
# % Var explained: 32.27 

# mean((rf.all$predicted - Train_Dat_run$log10LOI)^2) # OOB error
# rf.all$
# rf.all$mse[5000] # final OOB mse

plot(rf.all$mse, type='l')


# Unconditional variable importance
rf_imp <- data.frame(Variable=names(rf.all$importance[,1]), Importance=rf.all$importance[,1])
row.names(rf_imp) <- NULL
rf_imp <- rf_imp %>% arrange(desc(Importance))
rf_imp$Variable <- factor(rf_imp$Variable, levels=rev(rf_imp$Variable))

head(rf_imp, 20)


ggplot(rf_imp, aes(x=Variable, y=Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  scale_x_discrete(label=abbreviate)

rf_imp$Variable[1:20]




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

########## Run if needed 
VI.list <- readRDS(paste0(output_dir, "/Temp_Files/VI_list.rds"))
var.list <- readRDS(paste0(output_dir, "/Temp_Files/var_list.rds"))
OOB.rmse <- readRDS(paste0(output_dir, "/Temp_Files/OOB_rmse.rds"))
OOB.mae <- readRDS(paste0(output_dir, "/Temp_Files/OOB_mae.rds"))
OOB.bias <- readRDS(paste0(output_dir, "/Temp_Files/OOB_bias.rds"))
train.rmse <- readRDS(paste0(output_dir, "/Temp_Files/train_rmse.rds"))
train.mae <- readRDS(paste0(output_dir, "/Temp_Files/train_mae.rds"))
train.bias <- readRDS(paste0(output_dir, "/Temp_Files/train_bias.rds"))

All_dat_preds_loop <- All_dat_preds_loop[,!colnames(All_dat_preds_loop) %in% var.list] # remove variables
###############

## ***** Note: Need to change response in model fit and error calculations below, plus mtry ******
start.time <- Sys.time()

start_i <- sum(!is.na(OOB.rmse)) + 1
# start_i <- 1
for(i in start_i:p){
  print(i)
  
  nump <- ncol(All_dat_preds_loop)-1 # Number predictors
  
  set.seed(13) 
  rf  <- randomForest(log10MeHg ~ ., data=All_dat_preds_loop, 
                      mtry=max(floor(p/3), 1), 
                      ntree=5000,
                      nodesize=5,
                      #keep.forest=T,
                      # keep.inbag = F,
                      importance=T)
  
  rf.pred <- rf$predicted # OOB
  rf.pred.tr <- predict(rf, newdata=Train_Dat_run) # training
  
  # OOB error
  OOB.rmse[i] <- sqrt(mean((Train_Dat_run$log10MeHg-rf.pred)^2)) # OOB rmse
  OOB.mae[i] <- mean(abs(Train_Dat_run$log10MeHg-rf.pred)) # oob mae
  OOB.bias[i] <- mean(rf.pred-Train_Dat_run$log10MeHg) # oob bias
  
  # Training error
  train.rmse[i] <- sqrt(mean((Train_Dat_run$log10MeHg-rf.pred.tr)^2)) # training rmse
  train.mae[i] <- mean(abs(Train_Dat_run$log10MeHg-rf.pred.tr)) #  training mae
  train.bias[i] <- mean(rf.pred.tr-Train_Dat_run$log10MeHg) # training bias
  
  
  rf_imp <- data.frame(Variable=row.names(rf$importance), Importance=rf$importance[,1])
  row.names(rf_imp) <- NULL
  rf_imp <- rf_imp %>% arrange(Importance)
  
  
  VI.list[[i]] <- rf_imp
  var.list[i] <- rf_imp$Variable[1] # Least informative variable to remove
  
  All_dat_preds_loop <- All_dat_preds_loop[,!colnames(All_dat_preds_loop)==var.list[i]] # remove variable
  
  rm(rf)
  
  # Save results periodically in case it fails
  if(i %% 10 ==0){ 
    saveRDS(VI.list, paste0(output_dir, "/Temp_Files/VI_list.rds")) 
    saveRDS(var.list, paste0(output_dir, "/Temp_Files/var_list.rds")) 
    saveRDS(OOB.rmse, paste0(output_dir, "/Temp_Files/OOB_rmse.rds")) 
    saveRDS(OOB.mae, paste0(output_dir, "/Temp_Files/OOB_mae.rds")) 
    saveRDS(OOB.bias, paste0(output_dir, "/Temp_Files/OOB_bias.rds")) 
    saveRDS(train.rmse, paste0(output_dir, "/Temp_Files/train_rmse.rds")) 
    saveRDS(train.mae, paste0(output_dir, "/Temp_Files/train_mae.rds")) 
    saveRDS(train.bias, paste0(output_dir, "/Temp_Files/train_bias.rds")) 
    
    }
  
}

# Save output 
saveRDS(VI.list, paste0(output_dir, "rf_RFE_VI_at_iter.rds")) # R object (list) with the variable importances at each iteration of the algorithm


# RFE info needs to be saved before do the CV errors!
RFE_info <- data.frame(Worst_Var=var.list, OOB_rmse=OOB.rmse, OOB_mae=OOB.mae, OOB_bias=OOB.bias, Train_rmse=train.rmse, Train_mae=train.mae, Train_bias=train.bias)
write.csv(RFE_info, paste0(output_dir, "rf_RFE_info.csv"), row.names = FALSE)

end.time <- Sys.time()
end.time-start.time
# 9.661366    hr




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



which(RFE_info$OOB_rmse==min(RFE_info$OOB_rmse)) # 89
which(RFE_info$OOB_mae==min(RFE_info$OOB_mae)) # 44

min(RFE_info$OOB_rmse) # 0.3541396
min(RFE_info$OOB_mae) # 0.2724213


RFE_info[which(RFE_info$OOB_rmse==min(RFE_info$OOB_rmse)) : nrow(RFE_info),]


tail(VI.list[[1]], 10)
tail(RFE_info, 10)

# Plot OOB error
RFE_info %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_mae, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.27,.4)) +
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
  coord_cartesian(ylim=c(.36,.5)) +
  geom_hline(yintercept=min(RFE_info$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=rev(1:50), y=.50, label=RFE_info$Worst_Var[RFE_info$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")
ggsave(paste0(fig_dir, "/RFE_OOB_RMSE.png"), width=10, height=6)


#
RFE_info_write <- RFE_info %>% arrange(NumVars)
write.csv(RFE_info_write, paste0(output_dir, "MeHg_noTHgLOI_rf_RFE_info.csv"), row.names = FALSE)



# Do CV at each iteration to get SE

# Do stratified partitioning by space though to get 




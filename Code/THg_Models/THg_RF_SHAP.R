library(randomForest) # randomForest_4.7-1.1
library(tidyverse) # tidyverse_2.0.0
library(colorspace)
library(ggpubr)
library(kernelshap) # 0.7.0
library(shapviz) # 0.9.6
library(patchwork)
library(doParallel)

output_dir <- "Model_Output/THg/"
model_dir <- "Saved_Models/THg/"
fig_dir <- "Figures/THg/"

dir.create(paste0(fig_dir, "SHAP"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(output_dir, "SHAP"), recursive=T, showWarnings = FALSE) 


# Load imputed training and test data - does not contain ecoregion
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv")
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv")

All_Dat <- rbind(Train_Dat, Test_Dat)

# log-transform THg for all modeling
All_Dat$log10THg <- log10(All_Dat$STHG_ng_g)

# Transform LOI (doesn't matter for this model, but need for THg/LOI model
All_Dat$LOI_PERCENT <- All_Dat$LOI_PERCENT/100

# Read in original model 
# rf.final.orig  <- randomForest(log10THg ~ ., data=Train_run, 
#                                mtry=max(floor(mtry_par*nump), 1), 
#                                ntree=5000,
#                                nodesize=1,
#                                keep.forest=T,
#                                keep.inbag = F,
#                                importance=T)

# saveRDS(rf.final, paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))
rf.final.orig <- readRDS(paste0(model_dir, "rf_sd73_FINAL_SUBSET.rds"))

# Get predictors
final.preds <- attr(rf.final.orig$terms, "term.labels")

response_var <- "log10THg"

mtry_par <- 0.5

# Subset to predictors and response for final model
All_run <- All_Dat[, colnames(All_Dat) %in% c(final.preds, response_var)]

nump <- ncol(All_run)-1    # Number predictors 


# Fit final model using all data but with only 2000 trees to reduce calculation time
# set.seed(73) 
# rf.final2000  <- randomForest(log10THg ~ ., data=All_run, 
#                           mtry=max(floor(mtry_par*nump), 1), 
#                           ntree=2000,
#                           nodesize=1,
#                           keep.forest=T,
#                           keep.inbag = F,
#                           importance=F)

# saveRDS(rf.final2000, paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES_2000.rds"))
rf.final2000 <- readRDS(paste0(model_dir, "rf_sd73_FINALFINAL_SUBSET_TRAIN_ALL_LAKES_2000.rds"))


pred_fun <- function(object, X) { predict(object, newdata = X) }

# pred_fun(rf.final2000, head(All_Dat))


# Shap values for all lakes

# n_cores <- parallel::detectCores()
# # plan(multisession, workers = n_cores - 4)
# 
# cl <- makeCluster(n_cores-4)
# registerDoParallel(cl)
# 
# parallel::clusterExport(cl, varlist = c("rf.final2000", "All_run", "final.preds"))

## NOT WORKING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

set.seed(38) 

shap_values_all <- kernelshap(
  object = rf.final2000, 
  X = All_run[final.preds], 
  bg_n=200,
  parallel = TRUE,
  parallel_args = list(packages = "randomForest"),
  pred_fun = function(object, X) { predict(object, newdata = X) }
)
# stopCluster(cl)

saveRDS(shap_values_all, paste0(output_dir, "SHAP/shap_alldat_200bg_2000tr.rds"))
shap_values_all <- readRDS(paste0(output_dir, "SHAP/shap_alldat_200bg_2000tr.rds"))

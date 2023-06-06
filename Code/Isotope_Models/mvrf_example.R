library(randomForestSRC)

# Multivariate forests can be specified in two ways:
#   
# rfsrc(Multivar(y1, y2, ..., yd) ~ . , my.data, ...)
# 
# rfsrc(cbind(y1, y2, ..., yd) ~ . , my.data, ...)
# 
# By default, a multivariate normalized composite splitting rule is used to split nodes (for multivariate regression, users have the option to use Mahalanobis splitting).
# 
# The nature of the outcomes informs the code as to what type of multivariate forest is grown; i.e. whether it is real-valued, categorical, or a combination of both (mixed). Performance measures (when requested) are returned for all outcomes.
# 
# Helper functions get.mv.formula, get.mv.predicted, get.mv.error can be used for defining the multivariate forest formula and extracting predicted values (all outcomes) and VIMP (all variables, all outcomes; assuming importance was requested in the call). The latter two functions also work for univariate (regular) forests. Both functions return standardized values (dividing by the variance for regression, or multiplying by 100, otherwise) using option standardize="TRUE".

## ------------------------------------------------------------
## case-specific joint vimp for multivariate regression
## returns joint VIMP for each case, for each outcome
## ------------------------------------------------------------
mtcars <- mtcars
o <- rfsrc(Multivar(mpg, cyl) ~., data = mtcars, samptype="swr", importance="permute")

vcase <- vimp(o, joint = TRUE, csv = TRUE, importance="permute")
csvimpcase <- get.mv.csvimp(vcase, standardize=TRUE)
print(csvimpcase)
# 9 predictors, two responses

# joint=TRUE returns joint importance for group of variables when whole group is permuted simultaneously
# csv=TRUE returns case-specific importance (variable importance for each case)

v <- vimp(o,  importance="permute")
# imp <- get.mv.csvimp(v, standardize=TRUE)
v$importance
v
print(vimp(o)$importance)

get.mv.error(v, standardize=TRUE)
get.mv.error(v)

# subsampling() calculates CIs for VIMP - how does correct for smaller sample size?



# ----------
# Load imputed training and test data
Train_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Training_Data.csv") # imputed separately, 975 lakes for training
Test_Dat <- read.csv("Formatted_Data/THg_MHg_Imputed_Test_Data.csv") # imputed with training data combined, 101 lakes for testing

# All Data
Data <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv")

# Attach isotope data to Train and Test sets
names(Data)
Iso_Dat <- Data %>% dplyr::select(NLA12_ID, d202_Avg, D199_Avg, D200_Avg, D201_Avg, D204_Avg) %>% filter(!is.na(d202_Avg))
# 410 isotope lakes - possibly removed 3 that didn't have all predictors?
# This approach of joining is fine if want to keep predictors the same, with same imputed values, and don't want to resplit train and test sets

Train_Iso <- left_join(Train_Dat, Iso_Dat) %>% filter(!is.na(d202_Avg)) # 377
Test_Iso <- left_join(Test_Dat, Iso_Dat) %>% filter(!is.na(d202_Avg)) # 33

33/410 # 8% test data

# Keep LOI, THg, MeHg, and GEOS-Chem
Train_Dat_run <- Train_Iso %>% dplyr::select(-NLA12_ID, -D201_Avg, -D204_Avg) 
p <- ncol(Train_Dat_run)-3 # 125 preds - including LOI!!!
p # 127
ceiling(p/3)
# 5 isotope response with LOI and GEOS-Chem preds

# Use mtry=ceiling(p/3) for prelim run
set.seed(3)
o <- rfsrc(Multivar(d202_Avg, D199_Avg, D200_Avg) ~., data = Train_Dat_run, samptype="swr", importance="permute", splitrule = "mahalanobis", mtry=43, ntree=5000)
# saveRDS(o, paste0("Saved_Models/mvrf/mvrf_full_mtryThird_ntree5000_mahal_sd3.rds"))

v <- as.data.frame(get.mv.vimp(o,  standardize = TRUE))
## compare standardized VIMP for top 25 variables
imp <- data.frame(Predictor=row.names(v), 
                  d202_Importance = 100*v$d202_Avg, 
                  D199_Importance = 100*v$D199_Avg,
                  D200_Importance = 100*v$D200_Avg,
                  Avg_Importance = 100*rowMeans(v))
                  # default     = rowMeans(get.mv.vimp(obj2, standardize = TRUE)))
row.names(imp) <- NULL
imp %>% arrange(desc(Avg_Importance)) 

imp  %>% arrange(desc(Avg_Importance))  %>% 
  slice(1:50) %>% 
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>% 
  ggplot(aes(x=Predictor, y=Avg_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip()
ggsave("Figures/mvrf/VarImp_Avg_top50.png", width=15, height=10)

imp  %>% arrange(desc(d202_Importance))  %>% 
  slice(1:50) %>% 
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>% 
  ggplot(aes(x=Predictor, y=d202_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip()
ggsave("Figures/mvrf/VarImp_d202_top50.png", width=15, height=10)

imp  %>% arrange(desc(D199_Importance))  %>% 
  slice(1:50) %>% 
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>% 
  ggplot(aes(x=Predictor, y=D199_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip()
ggsave("Figures/mvrf/VarImp_D199_top50.png", width=15, height=10)

imp  %>% arrange(desc(D200_Importance))  %>% 
  slice(1:50) %>% 
  mutate(Predictor=factor(Predictor, levels=rev(Predictor))) %>% 
  ggplot(aes(x=Predictor, y=D200_Importance)) +
  geom_bar(stat="identity" ) +
  theme_minimal(base_size = 18) +
  coord_flip()
ggsave("Figures/mvrf/VarImp_D200_top50.png", width=15, height=10)

imp <- imp %>% arrange(desc(Avg_Importance)) 
imp$d202_Rank <- rank(-imp$d202_Importance)
imp$D199_Rank <- rank(-imp$D199_Importance)
imp$D200_Rank <- rank(-imp$D200_Importance)
imp$Avg_Rank <- rank(-imp$Avg_Importance)

write.csv(imp, "Tables/mvrf/Prelim_Isotope_Predictor_Importance_withoutRFE.csv", row.names = F)

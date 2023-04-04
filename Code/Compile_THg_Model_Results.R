#' ---
#' title: "Summary of THg and LOI model results"
#' output:
#'   html_document:
#'     df_print: paged
#' ---

#+ echo=FALSE, message=FALSE, warning=FALSE
library(tidyverse)
require(knitr)
library(rprojroot)
library(stringr)
library(DT)

#+ echo=FALSE
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#' THg model without LOI
#' ===

#+ echo=FALSE
THg_withoutLOI_RFE <- read.csv("Model_Output/THg_noLOI/rf_RFE_info.csv")
THg_withoutLOI_RFE$Iteration <- 1:nrow(THg_withoutLOI_RFE)
THg_withoutLOI_RFE$NumVars <- rev(THg_withoutLOI_RFE$Iteration)
THg_withoutLOI_RFE <- THg_withoutLOI_RFE %>% arrange(NumVars)
#' * Harvard’s GEOS-Chem WetLoss variables dropped in relative importance compared to the first iteration of this model after a couple redundant predictors were removed and missing data were re-imputed.
#' * Top predictors are pH, GEOS-Chem predictors, catchment characteristics (temp, precip, clay/sand, runoff, elevation, etc.), conductivity, etc..
#' * ~25 predictors
#' * Minimum RMSE: `r round(min(THg_withoutLOI_RFE$OOB_rmse), 3)`
#' * Minimum MAE:  `r round(min(THg_withoutLOI_RFE$OOB_mae), 3)`
#+ echo=FALSE, warning=FALSE, fig.width=10
THg_withoutLOI_RFE %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.28,.5)) +
  geom_hline(yintercept=min(THg_withoutLOI_RFE$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=(1:50), y=.50, label=THg_withoutLOI_RFE$Worst_Var[THg_withoutLOI_RFE$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")


#+ echo=FALSE
THg_RFE <- read.csv("Model_Output/THg/rf_RFE_info.csv")
THg_RFE$Iteration <- 1:nrow(THg_RFE)
THg_RFE$NumVars <- rev(THg_RFE$Iteration)
THg_RFE <- THg_RFE %>% arrange(NumVars)
#' THg model with LOI included
#' ===
#' * LOI is now top predictor, then pH, and then the story is pretty similar to the situation above, with some reordering of top predictors. Some of these predictors are highly correlated, and we may not be able to tease out exact rankings relative to one another.
#' * This THg model with LOI included has better predictive accuracy than without LOI and requires fewer predictors.
#' * **Results and interpretation seem cleaner for this model relative to THg/LOI below because we're accounting for LOI, but predictors for LOI are not mixed in with predictors for THg**
#' * ~14 vars
#' * Minimum RMSE: `r round(min(THg_RFE$OOB_rmse), 3)`
#' * Minimum MAE:  `r round(min(THg_RFE$OOB_mae), 3)`
#+ echo=FALSE, fig.width=10
THg_RFE %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.25,.5)) +
  geom_hline(yintercept=min(THg_RFE$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=(1:50), y=.50, label=THg_RFE$Worst_Var[THg_RFE$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")

#+ echo=FALSE
THgLOI_RFE <- read.csv("Model_Output/THgLOI/rf_RFE_info.csv")
THgLOI_RFE$Iteration <- 1:nrow(THgLOI_RFE)
THgLOI_RFE$NumVars <- rev(THgLOI_RFE$Iteration)
THgLOI_RFE <- THgLOI_RFE %>% arrange(NumVars)
#' THg/LOI model 
#' ===
#' * Interesting non-monotonic pattern in RMSE as predictors are removed - likely because some predictors are better for predicting THg vs. LOI.
#' * RMSE seems to start to decline again when predictors associated with LOI but not THg are removed (around 8-11 preds): SN, NH4, InorgNWetDep, NO3. 
#' * Hg0DryDep is top predictor for THg/LOI - Hg0DryDep and HgPDryDep rose in importance relative to the WetLoss predictors
#' * Elevation also rose in relative importance
#' * Takes more variables to maintain best predictive performance, likely because we are predicting both THg and LOI, and LOI models show that more predictors are needed for LOI relative to THg.
#' * ~ 29 vars
#' * Minimum RMSE: `r round(min(THgLOI_RFE$OOB_rmse), 3)`
#' * Minimum MAE:  `r round(min(THgLOI_RFE$OOB_mae), 3)`
#+ echo=FALSE, fig.width=10
THgLOI_RFE %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.27,.4)) +
  geom_hline(yintercept=min(THgLOI_RFE$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=(1:50), y=.4, label=THgLOI_RFE$Worst_Var[THgLOI_RFE$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")


#+ echo=FALSE
LOI_RFE <- read.csv("Model_Output/LOI/rf_RFE_info.csv")
LOI_RFE$Iteration <- 1:nrow(LOI_RFE)
LOI_RFE$NumVars <- rev(LOI_RFE$Iteration)
LOI_RFE <- LOI_RFE %>% arrange(NumVars)
#' LOI model with GEOS-Chem predictors
#' ===
#' * Top 6 predictors are non-GEOS-Chem predictors
#' * More predictors are generally retained for LOI than THg, probably because the GEOS-Chem variables are strong predictors of THg and less so for LOI.
#' * Note that iterative removal of GEOS-Chem predictors doesn't usually strongly impact predictive accuracy.
#' * ~24 vars
#' * Minimum RMSE: `r round(min(LOI_RFE$OOB_rmse), 3)`
#' * Minimum MAE:  `r round(min(LOI_RFE$OOB_mae), 3)`
#+ echo=FALSE, fig.width=10
LOI_RFE %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.2,.5)) +
  geom_hline(yintercept=min(LOI_RFE$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=(1:50), y=.5, label=LOI_RFE$Worst_Var[LOI_RFE$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")


#+ echo=FALSE
LOInoHg_RFE <- read.csv("Model_Output/LOInoHgPreds/rf_RFE_info.csv")
LOInoHg_RFE$Iteration <- 1:nrow(LOInoHg_RFE)
LOInoHg_RFE$NumVars <- rev(LOInoHg_RFE$Iteration)
LOInoHg_RFE <- LOInoHg_RFE %>% arrange(NumVars)
#' LOI w/ GEOS-Chem removed
#' ===
#' * Top 6 predictors are the exact same as above (all non-GEOS-Chem predictors), and the rest of the order is very similar if the GEOS-Chem predictors are ignored.
#' * Note that removal of GEOS-Chem predictors doesn't impact best predictive accuracy for LOI.
#' * ~23 vars
#' * Minimum RMSE: `r round(min(LOInoHg_RFE$OOB_rmse), 3)`
#' * Minimum MAE:  `r round(min(LOInoHg_RFE$OOB_mae), 3)`
#+ echo=FALSE, fig.width=10
LOInoHg_RFE %>% filter(NumVars %in% 1:50) %>% ggplot(aes(x=NumVars, y=OOB_rmse, label=Worst_Var)) + 
  geom_point(size=3) + 
  geom_line(size=1.2) +
  # geom_hline(yintercept=.699, lty=2, col="green3", size=1.2) +
  # geom_hline(yintercept=1, lty=2, col="yellow3", size=1.2) +
  coord_cartesian(ylim=c(.2,.5)) +
  geom_hline(yintercept=min(LOInoHg_RFE$OOB_rmse)) +
  theme_minimal(base_size = 19) +
  scale_x_continuous(breaks=seq(2,50,2)) +
  # geom_text(angle=90, nudge_y=.12) +
  annotate(geom = "text", x=(1:50), y=.5, label=LOInoHg_RFE$Worst_Var[LOInoHg_RFE$NumVars %in% 1:50], angle=90, hjust=1, size=4) +
  ylab("OOB RMSE") + xlab("Number variables")



#' Table of ranked predictors for each model
#' ===
#' 

#+ echo=FALSE
Pred_Table <- data.frame(THg_noLOI=THg_withoutLOI_RFE$Worst_Var[1:50], 
                         THg_withLOI=THg_RFE$Worst_Var[1:50],
                         THgLOI_ratio=THgLOI_RFE$Worst_Var[1:50],
                         LOI=LOI_RFE$Worst_Var[1:50],
                         LOI_noGEOSChem=LOInoHg_RFE$Worst_Var[1:50])
write.csv(Pred_Table, "Tables/Top50preds_THg_LOI_Mods.csv", row.names = FALSE)

Print_Table <- data.frame(Rank=1:50,
                          THg_noLOI=str_trunc(THg_withoutLOI_RFE$Worst_Var[1:50], 20), 
                          THg_withLOI=str_trunc(THg_RFE$Worst_Var[1:50], 20),
                          THgLOI_ratio=str_trunc(THgLOI_RFE$Worst_Var[1:50], 20),
                          LOI=str_trunc(LOI_RFE$Worst_Var[1:50], 20),
                          LOI_noGEOSChem=str_trunc(LOInoHg_RFE$Worst_Var[1:50], 20))
# kable(Print_Table, format="html")# %>% row_spec(seq(2,20,2), background= 'gray80')
datatable(Print_Table)
# library(knitr); rmarkdown::render("Code/Compile_THg_Model_Results.R")
                         
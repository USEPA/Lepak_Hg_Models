library(tidyverse)
library(readxl)
library(stringr)


# Read in new isotope data
New_iso <- read_excel("Data/032521 NLA Seds.xlsx") # 36
head(New_iso)

New_iso <- New_iso %>% select(-position) %>% rename(NLA12_ID=Site, USGS_ID=`USGS MRL ID`)
names(New_iso) <- str_replace_all(names(New_iso)," ", "_")


# All NARS lakes: in-lake water quality
NARS_all <- read_excel("Data/NARS_Hg_isotopes_060622.xlsx") # 1127
# NARS_all <- read_excel("Data/NARS_Hg_isotopes_031321.xlsx") # 1127 - Ryan corrected errors in this version to get above version
# IDs: UID (####), SITE_ID (NLA12_State-###), USGS ID (MSC###@)

names(NARS_all) <- str_replace(names(NARS_all),"\\...*", "") # replace first instance
names(NARS_all) <- str_replace_all(names(NARS_all)," - ", "_")
names(NARS_all) <- str_replace_all(names(NARS_all)," ", "_")
names(NARS_all) <- str_replace_all(names(NARS_all),"/", "_")
names(NARS_all) <- str_replace_all(names(NARS_all),"\\(", "")
names(NARS_all) <- str_replace_all(names(NARS_all),"\\)", "")
names(NARS_all) <- str_replace_all(names(NARS_all),"\\^", "")
names(NARS_all) <- str_replace_all(names(NARS_all),"\\-", "_")
names(NARS_all) <- str_replace_all(names(NARS_all),",", "")
# Shorten names
names(NARS_all) <- str_replace_all(names(NARS_all),"condition", "cond")
names(NARS_all) <- str_replace_all(names(NARS_all),"Condition", "Cond")
names(NARS_all) <- str_replace_all(names(NARS_all),"recreational", "rec")
names(NARS_all) <- str_replace_all(names(NARS_all),"oxygen", "O2")
names(NARS_all) <- str_replace_all(names(NARS_all),"chlorophyll_a", "chla")
names(NARS_all) <- str_replace_all(names(NARS_all),"Chlorophyll_a", "Chla")
names(NARS_all) <- str_replace_all(names(NARS_all),"cyanobacteria", "cyano")
names(NARS_all) <- str_replace_all(names(NARS_all),"_the_", "_")
names(NARS_all) <- str_replace_all(names(NARS_all),"TotalNitrogen", "TN")
names(NARS_all) <- str_replace_all(names(NARS_all),"total_nitrogen", "TN")
names(NARS_all) <- str_replace_all(names(NARS_all),"total_phosphorus", "TP")
names(NARS_all) <- str_replace_all(names(NARS_all),"totalP", "TP")
names(NARS_all) <- str_replace_all(names(NARS_all),"_Oligotrophic_Mesotrophic_Eutrophic_Hypereutrophic", "")
names(NARS_all) <- str_replace_all(names(NARS_all),"total_mercury", "THg")
names(NARS_all) <- str_replace_all(names(NARS_all),"methylmercury", "MHg")
names(NARS_all) <- str_replace_all(names(NARS_all),"X_site", "Site")

# Rename SITE_ID as NLA12_ID to match LakeCat
names(NARS_all)[names(NARS_all)=="SITE_ID"] <- c("NLA12_ID")
names(NARS_all)[duplicated(names(NARS_all))]

# Remove empty rows
NARS_all <- NARS_all[!is.na(NARS_all$NLA12_ID),] # 1124

test <- NARS_all %>% filter(is.na(NLA12_ID))
length(unique(NARS_all$NLA12_ID))

# Remove duplicate columns (keep first instance, fewer NAs) - actually are none
NARS_all <- NARS_all[, !duplicated(colnames(NARS_all))] 

str(NARS_all)

# Deal with non-numeric columns
NARS_all$SITE_DEPTH_m[NARS_all$SITE_DEPTH_m=="NA"] <- NA
NARS_all$SITE_DEPTH_m <- as.numeric(NARS_all$SITE_DEPTH_m)

NARS_all$COLOR_RESULT_APHA_Pt_Co[NARS_all$COLOR_RESULT_APHA_Pt_Co=="NA"] <- NA
NARS_all$COLOR_RESULT_APHA_Pt_Co <- as.numeric(NARS_all$COLOR_RESULT_APHA_Pt_Co)

# Replace "Clear to bottom" secchi depth with depth
NARS_all$SECCHI_m_use_depth_for_clear_to_bottom[which(NARS_all$SECCHI_m_use_depth_for_clear_to_bottom=="Clear to bottom")] <- NARS_all$SITE_DEPTH_m[which(NARS_all$SECCHI_m_use_depth_for_clear_to_bottom=="Clear to bottom")]
NARS_all$SECCHI_m_use_depth_for_clear_to_bottom <- as.numeric(NARS_all$SECCHI_m_use_depth_for_clear_to_bottom)

NARS_all$MMI_BENT_NLA12[NARS_all$MMI_BENT_NLA12=="NA"] <- NA
NARS_all$MMI_BENT_NLA12 <- as.numeric(NARS_all$MMI_BENT_NLA12)

NARS_all$MMI_ZOOP_NLA6[NARS_all$MMI_ZOOP_NLA6=="NA"] <- NA
NARS_all$MMI_ZOOP_NLA6 <- as.numeric(NARS_all$MMI_ZOOP_NLA6)

NARS_all$Aquatic_macrophytes_index[NARS_all$Aquatic_macrophytes_index=="NA"] <- NA
NARS_all$Aquatic_macrophytes_index <- as.numeric(NARS_all$Aquatic_macrophytes_index)

NARS_all$ATRAZINE_RESULT_ppb[NARS_all$ATRAZINE_RESULT_ppb=="NA"] <- NA
NARS_all$ATRAZINE_RESULT_ppb <- as.numeric(NARS_all$ATRAZINE_RESULT_ppb)


# Add in new isotope data
New_iso$NLA12_ID %in% NARS_all$NLA12_ID

new.ind <- match(New_iso$NLA12_ID, NARS_all$NLA12_ID)
NARS_all$d202_Avg[new.ind] <- New_iso$d202_Avg
NARS_all$D199_Avg[new.ind] <- New_iso$D199_Avg
NARS_all$D200_Avg[new.ind] <- New_iso$D200_Avg
NARS_all$D201_Avg[new.ind] <- New_iso$D201_Avg
NARS_all$D204_Avg[new.ind] <- New_iso$D204_Avg


# Add lake status
NARS_all$Status <- "Not Done"
# # NARS_all$Status[NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"
NARS_all$Status[!is.na(NARS_all$d202_Avg)] <- "Done"

# NARS_all$Status2 <- NARS_all$Status
# NARS_all$Status2[NARS_all$Status2=="Rest" | NARS_all$Status2=="Ready"] <- "Not Done"

# Matches intended dataset
# NARS_done <- NARS_all %>% filter(Status=="Done")
# NARS2 <- read_csv("Data/DonePlusNew_Lakes_NARSdata.csv")
# sum(NARS2$NLA12_ID %in% NARS_done$NLA12_ID)



## All LakeCat lakes
LakeCat_all <- read_excel("Data/LakeCat_NLA_Hg_isotopes_020421.xlsx", skip=1) # 1256 (1082 have Hg data)

LakeCat_all <- LakeCat_all %>% rename(USGS_ID=`Row Labels`)
# IDs: NLA12_ID (NLA12_State-### - SITE_ID), UID (####), Row Labels (MSC###@ = USGS ID)
names(LakeCat_all)
names(LakeCat_all) <- str_replace_all(names(LakeCat_all)," - ", "_")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all)," ", "_")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),"/", "_")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),"\\(", "")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),"\\)", "")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),"\\^", "")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),"\\-", "_")
names(LakeCat_all) <- str_replace_all(names(LakeCat_all),",", "")

str(LakeCat_all)

length(unique(LakeCat_all$NLA12_ID))


# Add in new isotope data
sum(New_iso$NLA12_ID %in% LakeCat_all$NLA12_ID)

new.ind <- match(New_iso$NLA12_ID, LakeCat_all$NLA12_ID)
LakeCat_all$d202_Avg[new.ind] <- New_iso$d202_Avg
LakeCat_all$D199_Avg[new.ind] <- New_iso$D199_Avg
LakeCat_all$D200_Avg[new.ind] <- New_iso$D200_Avg
LakeCat_all$D201_Avg[new.ind] <- New_iso$D201_Avg
LakeCat_all$D204_Avg[new.ind] <- New_iso$D204_Avg


# Round isotope and pct_WetLossConv data in both to 10 decimal places for matching
LakeCat_all <- LakeCat_all %>% mutate(across(c(d202_Avg, D199_Avg, D200_Avg, D201_Avg, D204_Avg, pct_WetLossConv), round, 10))
NARS_all <- NARS_all %>% mutate(across(c(d202_Avg, D199_Avg, D200_Avg, D201_Avg, D204_Avg, pct_WetLossConv), round, 10))

# Add lake status
LakeCat_all$Status <- "Not Done"
# # LakeCat_all$Status[LakeCat_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"
LakeCat_all$Status[!is.na(LakeCat_all$d202_Avg)] <- "Done"

# LakeCat_all$Status2 <- LakeCat_all$Status
# LakeCat_all$Status2[LakeCat_all$Status2=="Rest" | LakeCat_all$Status2=="Ready"] <- "Not Done"

# Matches intended dataset
# LakeCat_done <- LakeCat_all %>% filter(Status=="Done")
# LakeCat2 <- read_csv("Data/DonePlusNew_Lakes_LakeCatdata.csv")
# sum(LakeCat2$NLA12_ID %in% LakeCat_done$NLA12_ID)




# Read in corrected data (based on tables output in Format_Data1.R)

corrected_data <- read.csv("Weird_Data/Mismatch_Data_between_NARS_LakeCat_RYAN_CORRECTED.csv")

# Replace percentage values >100 or <0 with NA
corrected_data$DRY_WEIGHT_PERCENT_correct[corrected_data$DRY_WEIGHT_PERCENT_correct>100 | corrected_data$DRY_WEIGHT_PERCENT_correct<0] <- NA

# Replace values in LakeCat
LakeCat_all %>% filter(NLA12_ID %in% corrected_data$NLA12_ID)
lakecat.ind <- match(corrected_data$NLA12_ID, LakeCat_all$NLA12_ID)

LakeCat_all$DRY_WEIGHT_PERCENT[lakecat.ind] <- corrected_data$DRY_WEIGHT_PERCENT_correct
LakeCat_all$LOI_PERCENT[lakecat.ind] <- corrected_data$LOI_PERCENT_correct
LakeCat_all$SMHG[lakecat.ind] <- corrected_data$SMHG_correctg_g_correct
LakeCat_all$STHG[lakecat.ind] <- corrected_data$STHG_correctg_g_correct


# Replace values in NARS
NARS_all %>% filter(NLA12_ID %in% corrected_data$NLA12_ID)
nars.ind <- match(corrected_data$NLA12_ID, NARS_all$NLA12_ID)

NARS_all$DRY_WEIGHT_PERCENT[nars.ind] <- corrected_data$DRY_WEIGHT_PERCENT_correct
NARS_all$LOI_PERCENT[nars.ind] <- corrected_data$LOI_PERCENT_correct
NARS_all$SMHG_ng_g[nars.ind] <- corrected_data$SMHG_correctg_g_correct
NARS_all$STHG_ng_g[nars.ind] <- corrected_data$STHG_correctg_g_correct


# Replace missing USGS IDs in NARS with LakeCat ID
Mismatch_ID <- read_csv("Weird_Data/Mismatch_ID_between_NARS_LakeCat.csv")

NARS_all %>% filter(NLA12_ID %in% Mismatch_ID$NLA12_ID)
nars.ind2 <- match(Mismatch_ID$NLA12_ID, NARS_all$NLA12_ID)

NARS_all$USGS_ID[nars.ind2] <- Mismatch_ID$USGS_ID_L
NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis[nars.ind2] <- Mismatch_ID$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L


# Replace LOI% and Dry weight values >100 with NA in both datasets
NARS_all$LOI_PERCENT[NARS_all$LOI_PERCENT>100] <- NA
LakeCat_all$LOI_PERCENT[LakeCat_all$LOI_PERCENT>100] <- NA



# LakeCat_overlap <- LakeCat_all %>% filter(NLA12_ID %in% NARS_all$NLA12_ID) %>% arrange(NLA12_ID) # 1112

# Two datasets have different DRY_WEIGHT_PERCENT, LOI_PERCENT, SMHG, STHG for a few lakes
# Rename all common variables
LakeCat_join <- LakeCat_all %>% rename(SMHG_ng_g=SMHG, STHG_ng_g=STHG)
LakeCat_join$LakeCat <- "Yes"

NARS_join <- NARS_all #%>% rename(SMHG_ng_g_N=SMHG_ng_g, STHG_ng_g_N=STHG_ng_g)
NARS_join$NARS <- "Yes"

names(NARS_join)[names(NARS_join) %in% names(LakeCat_join)]

# This way no longer needed - got cols to match up by rounding to 10 decimals 
# Data_overlap <- inner_join(NARS_join, LakeCat_join, by="NLA12_ID", suffix=c("_N", "_L")) %>% arrange(NLA12_ID) # 1112

# Data_overlap <- inner_join(NARS_join, LakeCat_join, by=c("NLA12_ID", "UID", "USGS_ID", "USGS_ID_did_not_match_OR_weighed_ready_for_analysis", "DRY_WEIGHT_PERCENT", "LOI_PERCENT", "d202_Avg", "D199_Avg", "D200_Avg", "D201_Avg", "D204_Avg", "Gas_Hg_Hg0Conc_ng_m3", "Ionic_Hg2Conc_ng_m3", "Particle_Hg_HgPConc_ng_m3", "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s", "WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s", "pct_WetLossConv"), suffix=c("_N", "_L")) %>% arrange(NLA12_ID) # 1112

Data_overlap <- inner_join(NARS_join, LakeCat_join) %>% arrange(NLA12_ID) # 1112
Data_ALL <- full_join(NARS_join, LakeCat_join) %>% arrange(NLA12_ID) # 1268

sum(NARS_all$NLA12_ID %in% LakeCat_all$NLA12_ID) # 1112 in common
sum(!NARS_all$NLA12_ID %in% LakeCat_all$NLA12_ID) # 12 NARS not in LakeCat
sum(!LakeCat_all$NLA12_ID %in% NARS_all$NLA12_ID) # 144 LakeCat not in NARS
1112+12+144
# Adds up!


# These checks won't work if commented because they have _N and _L suffixes, but these all check out and these variables are all used in the join now.
# Investigate variable diffs - everything matches now!
# Data_overlap %>% filter(!DRY_WEIGHT_PERCENT_N==DRY_WEIGHT_PERCENT_L) %>% select(NLA12_ID, DRY_WEIGHT_PERCENT_N, DRY_WEIGHT_PERCENT_L)
# Data_overlap %>% filter(!LOI_PERCENT_N==LOI_PERCENT_L) %>% select(NLA12_ID, LOI_PERCENT_N, LOI_PERCENT_L)
# Data_overlap %>% filter(!SMHG_ng_g_N==SMHG_L) %>% select(NLA12_ID, SMHG_ng_g_N, SMHG_L)
# Data_overlap %>% filter(!STHG_ng_g_N==STHG_L) %>% select(NLA12_ID, STHG_ng_g_N, STHG_L)
# 
# sum(!is.na(Data_overlap$LOI_PERCENT_N) ==is.na(Data_overlap$LOI_PERCENT_L)) # 0
# sum(!is.na(Data_overlap$DRY_WEIGHT_PERCENT_N) == is.na(Data_overlap$DRY_WEIGHT_PERCENT_L)) # 0
# sum(!is.na(Data_overlap$SMHG_ng_g_N) == is.na(Data_overlap$SMHG_L)) # 0
# sum(!is.na(Data_overlap$STHG_ng_g_N) == is.na(Data_overlap$STHG_L)) # 0
# 
# # Other data do match!
# test <- Data_overlap %>% filter(!d202_Avg_N==d202_Avg_L) %>% select(NLA12_ID, d202_Avg_N, d202_Avg_L)
# sum(abs(test$d202_Avg_N - test$d202_Avg_L))
# 
# sum(abs(Data_overlap$d202_Avg_N - Data_overlap$d202_Avg_L), na.rm=T)
# sum(abs(Data_overlap$D199_Avg_N - Data_overlap$D199_Avg_L), na.rm=T)
# sum(abs(Data_overlap$D200_Avg_N - Data_overlap$D200_Avg_L), na.rm=T)
# sum(abs(Data_overlap$D201_Avg_N - Data_overlap$D201_Avg_L), na.rm=T)
# sum(abs(Data_overlap$D204_Avg_N - Data_overlap$D204_Avg_L), na.rm=T)
# 
# sum(!is.na(Data_overlap$d202_Avg_N) == is.na(Data_overlap$d202_Avg_L))
# sum(!is.na(Data_overlap$D199_Avg_N) == is.na(Data_overlap$D199_Avg_L))
# sum(!is.na(Data_overlap$D200_Avg_N) == is.na(Data_overlap$D200_Avg_L))
# sum(!is.na(Data_overlap$D201_Avg_N) == is.na(Data_overlap$D201_Avg_L))
# sum(!is.na(Data_overlap$D204_Avg_N) == is.na(Data_overlap$D204_Avg_L))
# 
# # USGS_ID_did_not_match_OR_weighed_ready_for_analysis
# Data_overlap %>% filter(!(USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N==USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L) | !(is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N) == is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L))) %>% select(NLA12_ID, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L)
# 
# # UID matches
# Data_overlap %>% filter(!(UID_N==UID_L) | !(is.na(Data_overlap$UID_N) == is.na(Data_overlap$UID_L))) %>% select(NLA12_ID, UID_N, UID_L)
# 
# # USGS_ID has 6 mismatches (NAs in NARS)
# Data_overlap %>% filter(!(USGS_ID_N==USGS_ID_L) | !(is.na(Data_overlap$USGS_ID_N) == is.na(Data_overlap$USGS_ID_L))) %>% select(NLA12_ID, USGS_ID_N, USGS_ID_L)
# 
# # Hg data match
# sum(abs(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_N - Data_overlap$Gas_Hg_Hg0Conc_ng_m3_L), na.rm=T)
# sum(!is.na(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_N) == is.na(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_L))
# 
# sum(abs(Data_overlap$Ionic_Hg2Conc_ng_m3_N - Data_overlap$Ionic_Hg2Conc_ng_m3_L), na.rm=T)
# sum(!is.na(Data_overlap$Ionic_Hg2Conc_ng_m3_N) == is.na(Data_overlap$Ionic_Hg2Conc_ng_m3_L))
# 
# sum(abs(Data_overlap$Particle_Hg_HgPConc_ng_m3_N - Data_overlap$Particle_Hg_HgPConc_ng_m3_L), na.rm=T)
# sum(!is.na(Data_overlap$Particle_Hg_HgPConc_ng_m3_N) == is.na(Data_overlap$Particle_Hg_HgPConc_ng_m3_L))
# 
# sum(abs(Data_overlap$Hg0DryDep_N - Data_overlap$Hg0DryDep_L), na.rm=T)
# sum(!is.na(Data_overlap$Hg0DryDep_N) == is.na(Data_overlap$Hg0DryDep_L))
# 
# sum(abs(Data_overlap$Hg2DryDep_N - Data_overlap$Hg2DryDep_L), na.rm=T)
# sum(!is.na(Data_overlap$Hg2DryDep_N) == is.na(Data_overlap$Hg2DryDep_L))
# 
# 
# sum(abs(Data_overlap$HgPDryDep_N - Data_overlap$HgPDryDep_L), na.rm=T)
# sum(!is.na(Data_overlap$HgPDryDep_N) == is.na(Data_overlap$HgPDryDep_L))
# 
# sum(abs(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_N - Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_L), na.rm=T)
# sum(!is.na(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_N) == is.na(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_L))
# 
# sum(abs(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_N - Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L), na.rm=T)
# sum(!is.na(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_N) == is.na(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L))
# 
# 
# sum(abs(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_N - Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L), na.rm=T)
# 
# 
# sum(abs(Data_overlap$pct_WetLossConv_N - Data_overlap$pct_WetLossConv_L), na.rm=T)
# 
# sum(!is.na(Data_overlap$pct_WetLossConv_N) == is.na(Data_overlap$pct_WetLossConv_L))
# 
# 
# d_dat <- Data_ALL %>% filter(!d202_Avg_N==d202_Avg_L | 
#                           !D199_Avg_N==D199_Avg_L | 
#                           !D200_Avg_N==D200_Avg_L | 
#                           !D201_Avg_N==D201_Avg_L | 
#                           !D204_Avg_N==D204_Avg_L | 
#                           !is.na(Data_overlap$d202_Avg_N) == is.na(Data_overlap$d202_Avg_L) | 
#                           !is.na(Data_overlap$D199_Avg_N) == is.na(Data_overlap$D199_Avg_L) | 
#                           !is.na(Data_overlap$D200_Avg_N) == is.na(Data_overlap$D200_Avg_L) | 
#                           !is.na(Data_overlap$D201_Avg_N) == is.na(Data_overlap$D201_Avg_L) |
#                           !is.na(Data_overlap$D204_Avg_N) == is.na(Data_overlap$D204_Avg_L)) %>% 
#   select(NLA12_ID, d202_Avg_N, d202_Avg_L, D199_Avg_N, D199_Avg_L, D200_Avg_N, D200_Avg_L, D201_Avg_N, D201_Avg_L, D204_Avg_N, D204_Avg_L)
# 
# 
# 
# sum(is.na(d_dat)) # None of the non-matching data are because isotopes are NA in one and not another
# # Must be rounding issue, so can get rid of one column


# Final checks of variables
sum(!is.na(Data_ALL$LOI_PERCENT )) # 1092
ks.test(x=Data_ALL$LOI_PERCENT[Data_ALL$Status=="Done"], y=Data_ALL$LOI_PERCENT[!Data_ALL$Status=="Done"]) # 0.2491

sum(!is.na(Data_ALL$Lake_Area_HA )) # 1124
ks.test(x=Data_ALL$Lake_Area_HA[Data_ALL$Status=="Done"], y=Data_ALL$Lake_Area_HA[!Data_ALL$Status=="Done"]) # 0.7704

sum(!is.na(Data_ALL$ELEVATION )) # 1124
ks.test(x=Data_ALL$ELEVATION[Data_ALL$Status=="Done"], y=Data_ALL$ELEVATION[!Data_ALL$Status=="Done"]) # 0.06445

sum(!is.na(Data_ALL$Site_chla_ug_L )) # 1123
ks.test(x=Data_ALL$Site_chla_ug_L[Data_ALL$Status=="Done"], y=Data_ALL$Site_chla_ug_L[!Data_ALL$Status=="Done"]) # 0.2852

sum(!is.na(Data_ALL$CHLORIDE_RESULT_mg_L )) # 1034
ks.test(x=Data_ALL$CHLORIDE_RESULT_mg_L[Data_ALL$Status=="Done"], y=Data_ALL$CHLORIDE_RESULT_mg_L[!Data_ALL$Status=="Done"]) # 0.08987

sum(!is.na(Data_ALL$Min_OXYGEN_mg_L )) # 1087
ks.test(x=Data_ALL$Min_OXYGEN_mg_L[Data_ALL$Status=="Done"], y=Data_ALL$Min_OXYGEN_mg_L[!Data_ALL$Status=="Done"]) # 0.5021

sum(!is.na(Data_ALL$Particle_Hg_HgPConc_ng_m3 )) # 1040
ks.test(x=Data_ALL$Particle_Hg_HgPConc_ng_m3[Data_ALL$Status=="Done"], y=Data_ALL$Particle_Hg_HgPConc_ng_m3[!Data_ALL$Status=="Done"]) # 0.1629

sum(!is.na(Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s )) # 1040
ks.test(x=Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[Data_ALL$Status=="Done"], y=Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[!Data_ALL$Status=="Done"]) # 0.05999

sum(!is.na(Data_ALL$Omernik_II )) # 1124
chisq.test(table(Data_ALL$Omernik_II, Data_ALL$Status)) # 0.06656

sum(!is.na(Data_ALL$Fe2O3Ws )) # 1254
ks.test(x=LakeCat_all$Fe2O3Ws[LakeCat_all$Status=="Done"], y=LakeCat_all$Fe2O3Ws[!LakeCat_all$Status=="Done"]) # 0.6705



# Write out data

# All NARS data - 1124
write_csv(NARS_all, paste0("Formatted_Data/NARS_final_", Sys.Date() ,".csv"))

names(NARS_all)
names(LakeCat_all)

# All LakeCat data - 1256
write_csv(LakeCat_all, paste0("Formatted_Data/LakeCat_final_", Sys.Date() ,".csv"))

# All lakes and data combined - 1268
write_csv(Data_ALL, paste0("Formatted_Data/AllLakes_AllVariables_final_", Sys.Date() ,".csv"))


# All lakes in both sets - 1112
write_csv(Data_overlap, paste0("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_", Sys.Date() ,".csv"))


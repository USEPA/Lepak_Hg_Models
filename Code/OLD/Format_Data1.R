library(tidyverse)
library(readxl)
library(stringr)


# Read in new isotope data
New_iso <- read_excel("Data/032521 NLA Seds.xlsx") # 36
head(New_iso)

New_iso <- New_iso %>% select(-position) %>% rename(NLA12_ID=Site, USGS_ID=`USGS MRL ID`)
names(New_iso) <- str_replace_all(names(New_iso)," ", "_")


# All NARS lakes: in-lake water quality
NARS_all <- read_excel("Data/NARS_Hg_isotopes_031321.xlsx") # 1127
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
# NARS_all$Status <- "Not Done"
# # NARS_all$Status[NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"
# NARS_all$Status[!is.na(NARS_all$d202_Avg)] <- "Done"

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


# Add lake status
# LakeCat_all$Status <- "Not Done"
# # LakeCat_all$Status[LakeCat_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"
# LakeCat_all$Status[!is.na(LakeCat_all$d202_Avg)] <- "Done"

# LakeCat_all$Status2 <- LakeCat_all$Status
# LakeCat_all$Status2[LakeCat_all$Status2=="Rest" | LakeCat_all$Status2=="Ready"] <- "Not Done"

# Matches intended dataset
# LakeCat_done <- LakeCat_all %>% filter(Status=="Done")
# LakeCat2 <- read_csv("Data/DonePlusNew_Lakes_LakeCatdata.csv")
# sum(LakeCat2$NLA12_ID %in% LakeCat_done$NLA12_ID)




# LakeCat_overlap <- LakeCat_all %>% filter(NLA12_ID %in% NARS_all$NLA12_ID) %>% arrange(NLA12_ID) # 1112

# Two datasets have different DRY_WEIGHT_PERCENT, LOI_PERCENT, SMHG, STHG for a few lakes
# Rename all common variables
LakeCat_join <- LakeCat_all %>% rename(SMHG_L=SMHG, STHG_L=STHG)
LakeCat_join$LakeCat <- "Yes"
# , DRY_WEIGHT_PERCENT_L=DRY_WEIGHT_PERCENT, LOI_PERCENT_L=LOI_PERCENT, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L=USGS_ID_did_not_match_OR_weighed_ready_for_analysis, d202_Avg_L=d202_Avg, D199_Avg_L=D199_Avg, D200_Avg_L=D200_Avg, D201_Avg_L=D201_Avg, D204_Avg_L=D204_Avg, Gas_Hg_Hg0Conc_ng_m3_L=Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3_L=Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3_L=Particle_Hg_HgPConc_ng_m3, Hg0DryDep_L=Hg0DryDep, Hg2DryDep_L=Hg2DryDep, HHgPDryDep_L=HgPDryDep,  WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_L=WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s, WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L=WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s, pct_WetLossConv_L=pct_WetLossConv, UID_L=UID, USGS_ID_L=USGS_ID

NARS_join <- NARS_all %>% rename(SMHG_ng_g_N=SMHG_ng_g, STHG_ng_g_N=STHG_ng_g)
NARS_join$NARS <- "Yes"

names(NARS_join)[names(NARS_join) %in% names(LakeCat_join)]

# Can also do this way - adds _N or _L to common columns
Data_overlap <- inner_join(NARS_join, LakeCat_join, by="NLA12_ID", suffix=c("_N", "_L")) %>% arrange(NLA12_ID) # 1112

Data_ALL <- full_join(NARS_join, LakeCat_join, by="NLA12_ID", suffix=c("_N", "_L")) %>% arrange(NLA12_ID) # 1268

sum(NARS_all$NLA12_ID %in% LakeCat_all$NLA12_ID) # 1112 in common
sum(!NARS_all$NLA12_ID %in% LakeCat_all$NLA12_ID) # 12 NARS not in LakeCat
sum(!LakeCat_all$NLA12_ID %in% NARS_all$NLA12_ID) # 144 LakeCat not in NARS
1112+12+144
# Adds up!

# Investigate variable diffs
Data_overlap %>% filter(!DRY_WEIGHT_PERCENT_N==DRY_WEIGHT_PERCENT_L) %>% select(NLA12_ID, DRY_WEIGHT_PERCENT_N, DRY_WEIGHT_PERCENT_L)

Data_overlap %>% filter(!LOI_PERCENT_N==LOI_PERCENT_L) %>% select(NLA12_ID, LOI_PERCENT_N, LOI_PERCENT_L)

Data_overlap %>% filter(!SMHG_ng_g_N==SMHG_L) %>% select(NLA12_ID, SMHG_ng_g_N, SMHG_L)

Data_overlap %>% filter(!STHG_ng_g_N==STHG_L) %>% select(NLA12_ID, STHG_ng_g_N, STHG_L)


sum(!is.na(Data_overlap$LOI_PERCENT_N) ==is.na(Data_overlap$LOI_PERCENT_L)) # 6
sum(!is.na(Data_overlap$DRY_WEIGHT_PERCENT_N) == is.na(Data_overlap$DRY_WEIGHT_PERCENT_L)) # 9
sum(!is.na(Data_overlap$SMHG_ng_g_N) == is.na(Data_overlap$SMHG_L)) # 6
sum(!is.na(Data_overlap$STHG_ng_g_N) == is.na(Data_overlap$STHG_L)) # 6






# Other data do match!
test <- Data_overlap %>% filter(!d202_Avg_N==d202_Avg_L) %>% select(NLA12_ID, d202_Avg_N, d202_Avg_L)
sum(abs(test$d202_Avg_N - test$d202_Avg_L))

sum(abs(Data_overlap$d202_Avg_N - Data_overlap$d202_Avg_L), na.rm=T)
sum(abs(Data_overlap$D199_Avg_N - Data_overlap$D199_Avg_L), na.rm=T)
sum(abs(Data_overlap$D200_Avg_N - Data_overlap$D200_Avg_L), na.rm=T)
sum(abs(Data_overlap$D201_Avg_N - Data_overlap$D201_Avg_L), na.rm=T)
sum(abs(Data_overlap$D204_Avg_N - Data_overlap$D204_Avg_L), na.rm=T)

sum(!is.na(Data_overlap$d202_Avg_N) == is.na(Data_overlap$d202_Avg_L))
sum(!is.na(Data_overlap$D199_Avg_N) == is.na(Data_overlap$D199_Avg_L))
sum(!is.na(Data_overlap$D200_Avg_N) == is.na(Data_overlap$D200_Avg_L))
sum(!is.na(Data_overlap$D201_Avg_N) == is.na(Data_overlap$D201_Avg_L))
sum(!is.na(Data_overlap$D204_Avg_N) == is.na(Data_overlap$D204_Avg_L))

# USGS_ID_did_not_match_OR_weighed_ready_for_analysis has 1 mismatch (NA in NARS)
Data_overlap %>% filter(!(USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N==USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L) | !(is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N) == is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L))) %>% select(NLA12_ID, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L)

# UID matches
Data_overlap %>% filter(!(UID_N==UID_L) | !(is.na(Data_overlap$UID_N) == is.na(Data_overlap$UID_L))) %>% select(NLA12_ID, UID_N, UID_L)

# USGS_ID has 6 mismatches (NAs in NARS)
Data_overlap %>% filter(!(USGS_ID_N==USGS_ID_L) | !(is.na(Data_overlap$USGS_ID_N) == is.na(Data_overlap$USGS_ID_L))) %>% select(NLA12_ID, USGS_ID_N, USGS_ID_L)


# Other Hg data match
sum(abs(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_N - Data_overlap$Gas_Hg_Hg0Conc_ng_m3_L), na.rm=T)
sum(!is.na(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_N) == is.na(Data_overlap$Gas_Hg_Hg0Conc_ng_m3_L))

sum(abs(Data_overlap$Ionic_Hg2Conc_ng_m3_N - Data_overlap$Ionic_Hg2Conc_ng_m3_L), na.rm=T)
sum(!is.na(Data_overlap$Ionic_Hg2Conc_ng_m3_N) == is.na(Data_overlap$Ionic_Hg2Conc_ng_m3_L))

sum(abs(Data_overlap$Particle_Hg_HgPConc_ng_m3_N - Data_overlap$Particle_Hg_HgPConc_ng_m3_L), na.rm=T)
sum(!is.na(Data_overlap$Particle_Hg_HgPConc_ng_m3_N) == is.na(Data_overlap$Particle_Hg_HgPConc_ng_m3_L))

sum(abs(Data_overlap$Hg0DryDep_N - Data_overlap$Hg0DryDep_L), na.rm=T)
sum(!is.na(Data_overlap$Hg0DryDep_N) == is.na(Data_overlap$Hg0DryDep_L))

sum(abs(Data_overlap$Hg2DryDep_N - Data_overlap$Hg2DryDep_L), na.rm=T)
sum(!is.na(Data_overlap$Hg2DryDep_N) == is.na(Data_overlap$Hg2DryDep_L))

sum(abs(Data_overlap$HgPDryDep_N - Data_overlap$HgPDryDep_L), na.rm=T)
sum(!is.na(Data_overlap$HgPDryDep_N) == is.na(Data_overlap$HgPDryDep_L))

sum(abs(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_N - Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_L), na.rm=T)
sum(!is.na(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_N) == is.na(Data_overlap$WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s_L))

sum(abs(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_N - Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L), na.rm=T)
sum(!is.na(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_N) == is.na(Data_overlap$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s_L))




Mismatch_Hg <- Data_overlap %>% filter(!LOI_PERCENT_N==LOI_PERCENT_L | !DRY_WEIGHT_PERCENT_N==DRY_WEIGHT_PERCENT_L | !SMHG_ng_g_N==SMHG_L | !STHG_ng_g_N==STHG_L | !is.na(Data_overlap$LOI_PERCENT_N) == is.na(Data_overlap$LOI_PERCENT_L) | !is.na(Data_overlap$DRY_WEIGHT_PERCENT_N) == is.na(Data_overlap$DRY_WEIGHT_PERCENT_L) | !is.na(Data_overlap$SMHG_ng_g_N) == is.na(Data_overlap$SMHG_L) | !is.na(Data_overlap$STHG_ng_g_N) == is.na(Data_overlap$STHG_L)) %>% 
  select(NLA12_ID, LOI_PERCENT_N, LOI_PERCENT_L, DRY_WEIGHT_PERCENT_N, DRY_WEIGHT_PERCENT_L, SMHG_ng_g_N, SMHG_L, STHG_ng_g_N, STHG_L)
Mismatch_Hg
write_csv(Mismatch_Hg, "Weird_Data/Mismatch_Data_between_NARS_LakeCat.csv")


Mismatch_ID <- Data_overlap %>% filter(!(USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N==USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L) | !(is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N) == is.na(Data_overlap$USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L)) | !(USGS_ID_N==USGS_ID_L) | !(is.na(Data_overlap$USGS_ID_N) == is.na(Data_overlap$USGS_ID_L))) %>% 
  select(NLA12_ID, USGS_ID_N, USGS_ID_L, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_N, USGS_ID_did_not_match_OR_weighed_ready_for_analysis_L)
Mismatch_ID
write_csv(Mismatch_ID, "Weird_Data/Mismatch_ID_between_NARS_LakeCat.csv")




# Write out data

# All NARS data - 1124

# All LakeCat data - 1256

# All data combined - 1268

# All lakes in both sets - 1112
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)

## All NARS and LakeCat lakes
NARS_all <- read_excel("Data/1 - Combined NARS 090319.xlsx", skip=2) # 1147 (1072 have Hg data)
names(NARS_all)[801:815]
NARS_all <- NARS_all[,1:807] # Remove extra 'Average' columns. Note to ask about the misalignment
# IDs: UID (####), SITE_ID (NLA12_State-###), USGS ID (MSC###@)

names(NARS_all) <- str_replace(names(NARS_all),"\\...*", "")
names(NARS_all) <- str_replace(names(NARS_all)," ", "_")
names(NARS_all) <- str_replace(names(NARS_all),"/", "_")
names(NARS_all) <- str_replace(names(NARS_all),"\\(", "")
names(NARS_all) <- str_replace(names(NARS_all),"\\)", "")

# Rename first instance of SITE_ID as NLA12_ID to match LakeCat and second instance of SITE_ID as Date
names(NARS_all)[names(NARS_all)=="SITE_ID"] <- c("NLA12_ID", "Date")
names(NARS_all)[duplicated(names(NARS_all))]

# Remove duplicate columns (keep first instance)
NARS_all <- NARS_all[, !duplicated(colnames(NARS_all))] 

NARS_all$CHLX_RESULT <- as.numeric(NARS_all$CHLX_RESULT)
NARS_all[NARS_all==-99999] <- NA


LakeCat_all <- read.csv("Data/1 - nla_2012_lakecat_data.csv") # 1256 (1082 have Hg data)
LakeCat_all <- LakeCat_all %>% rename(USGS_ID=Row.Labels)
# IDs: NLA12_ID (NLA12_State-### - SITE_ID), Row Labels (MSC###@ = USGS ID)
LakeCat_all[LakeCat_all==""] <- NA
names(LakeCat_all)

# Lakes pre-selected for Hg isotope analysis. Some are analyzed
NARS_iso <- read_excel("Data/2 - Combined NARS 090319.xlsx", skip=2) # 673
names(NARS_iso)[801:815]
NARS_iso <- NARS_iso[,1:807] # Remove extra 'Average' columns

names(NARS_iso) <- str_replace(names(NARS_iso),"\\...*", "")
names(NARS_iso) <- str_replace(names(NARS_iso)," ", "_")
names(NARS_iso) <- str_replace(names(NARS_iso),"/", "_")
names(NARS_iso) <- str_replace(names(NARS_iso),"\\(", "")
names(NARS_iso) <- str_replace(names(NARS_iso),"\\)", "")

# Rename first instance of SITE_ID as NLA12_ID to match LakeCat and second instance of SITE_ID as Date
names(NARS_iso)[names(NARS_iso)=="SITE_ID"] <- c("NLA12_ID")

# Remove duplicate columns (keep first instance)
NARS_iso <- NARS_iso[, !duplicated(colnames(NARS_iso))] 

NARS_iso$CHLX_RESULT <- as.numeric(NARS_iso$CHLX_RESULT)
NARS_iso[NARS_iso==-99999] <- NA


LakeCat_iso <- read.csv("Data/2 - nla_2012_lakecat_data.csv") # 685 (extra rows)
LakeCat_iso <- LakeCat_iso %>% filter(!NLA12_ID=="") # filter out empty rows
LakeCat_iso[LakeCat_iso==""] <- NA
names(LakeCat_iso)



# Lakes that have completed Hg isotope analysis (some need reanalysis)
NARS_run <- read_excel("Data/3 - NLA_WaterQuality_wIsotopes_012420 pairs w NARS.xlsx") # 391
names(NARS_run)
# IDs: Site (NLA12_State-###, same as SITE_ID in _all), ID (MSC###@ - same as USGS ID in _all). No UID.
NARS_run <- NARS_run %>% rename(NLA12_ID=Site, USGS_ID=ID)
NARS_run[NARS_run==""] <- NA
NARS_run[NARS_run==-99999] <- NA



LakeCat_run <- read_excel("Data/3 - NLA_Catchment_wIsotopes_GEOSChem_081120 pairs with lakecat.xlsx", skip=1) # 402
names(LakeCat_run)
# IDs: SITE_ID (NLA12_State-###, same as NLA12_ID in _all), MRL ID (MSC###@, same as Row.Labels/USGS_ID in _all)
LakeCat_run <- LakeCat_run %>% rename(NLA12_ID=SITE_ID, USGS_ID='MRL ID')
LakeCat_run[LakeCat_run==""] <- NA

# USGS ID starts 'MSC'. NLA12 ID starts as such.

LakeCat_all$USGS_ID # Some missing USGS ID? - not missing in other datasets
NARS_all$USGS_ID # Some missing USGS ID? - not missing in other datasets

sum(is.na(NARS_all$NLA12_ID))
sum(is.na(NARS_all$USGS_ID)) # 75
sum(is.na(LakeCat_all$NLA12_ID))
sum(is.na(LakeCat_all$USGS_ID)) # 174

# Use NLA12_ID because none are NA




# Step 1) Take 'run' and analyze those labeled “OK” under status versus 'all'  for bias. Next, do all of 'run' versus 'all'. If bias exists in both, then do 'iso' versus 'all'. 

# Step 2) Samples in 'run' labeled “re-run” can easily be used to reduce bias between Hg isotope data and the broader NLA in 'all'. If step one analyses of all of 'run' versus 'all' show that’ll be inadequate to reduce bias, 'iso' samples can be selectively chosen, prepared and analyzed to reduce bias further. 

# Step 3) if samples from 'all' need novel selection to eliminate bias, this can be done but at the cost of much greater/slower work. 
# Kelsey, provide guidance once data is produced on additional samples to add to reduce bias. We’d like to do as few as possible but will do what’s necessary to make this robust. 


# In NARS analyze on columns: LOI_%, THg, AREA_HA, ELEVATION, CHLX_RESUL, CHLORIDE_R, MICX_RESUL, EPA_REG – Note that the prefix one file contains -9999 values for no data entered. We needed this for database consumption. 

# In LakeCat analyze on columns: CatAreaSqK, Fe2O3Cat, Fe2O3Ws


# Create data subsets of interest. Binding subsets together from different files because 'all' doesn't have all IDs

# LakeCat_run
LakeCat_runOK <- LakeCat_run %>% filter(Status=="OK" | Status=="ok" | Status=="Ok") # 295/402
unique(LakeCat_run$Status)

LakeCat_all_COMPrun <- LakeCat_all %>% filter(!NLA12_ID %in% LakeCat_run$NLA12_ID) # 854
LakeCat_all_COMPrunOK <- LakeCat_all %>% filter(!NLA12_ID %in% LakeCat_runOK$NLA12_ID) # 961
# LakeCat_all is 1256 - these add up

# NARS_run
NARS_runOK <- NARS_run %>% filter(Status=="OK" | Status=="ok") # 283/391
unique(NARS_run$Status)
NARS_all_COMPrun <- NARS_all %>% filter(!NLA12_ID %in% NARS_run$NLA12_ID) # 764
NARS_all_COMPrunOK <- NARS_all %>% filter(!NLA12_ID %in% NARS_runOK$NLA12_ID) # 872
# NARS_all is 1147 - Don't add up because 8 extra in NARS_run not in NARS_all (or NARS_iso)

NARS_runOK$NLA12_ID[!NARS_runOK$NLA12_ID %in% NARS_all$NLA12_ID] # 8 OK NLA ID not in full dataset
NARS_run$NLA12_ID[!NARS_run$NLA12_ID %in% NARS_all$NLA12_ID] # 8 run NLA ID not in full dataset


# LakeCat_iso
LakeCat_all_COMPiso <- LakeCat_all %>% filter(!NLA12_ID %in% LakeCat_iso$NLA12_ID) # 571

# NARS_iso
NARS_all_COMPiso <- NARS_all %>% filter(!NLA12_ID %in% NARS_iso$NLA12_ID) # 474



# *** Alternatively, could just filter all file to those in/out of OK or run using NLA ID. Did this way because there are 8 extra in run files


# NARS - compare just OK status to full data
NARS_runOK_bind <- NARS_runOK %>% dplyr::select(NLA12_ID, `LOI_%`, THg, AREA_HA, ELEVATION,  CHLX_RESUL, CHLORIDE_R,  MICX_RESUL, EPA_REG)
NARS_runOK_bind$Status <- "OK"
NARS_all_COMPrunOK_bind <- NARS_all_COMPrunOK %>% dplyr::select(NLA12_ID, LOI_PERCENT, STHG_ng_g, AREA_HA, ELEVATION,  CHLX_RESULT, CHLORIDE_RESULT,  MICX_RESULT, EPA_REG) # lakes in 'all' without OK status in 'run'
NARS_all_COMPrunOK_bind$Status <- "OK-Complement"
names(NARS_all_COMPrunOK_bind) <- names(NARS_runOK_bind)
NARS_runOK_bindBoth <- rbind(NARS_runOK_bind, NARS_all_COMPrunOK_bind) # 1155 - all OK + complement in all (8 extra in OK)

# NARS - compare all run to full data
NARS_run_bind <- NARS_run %>% dplyr::select(NLA12_ID, `LOI_%`, THg, AREA_HA, ELEVATION,  CHLX_RESUL, CHLORIDE_R,  MICX_RESUL, EPA_REG)
NARS_run_bind$Status <- "Run"
NARS_all_COMPrun_bind <- NARS_all_COMPrun %>% dplyr::select(NLA12_ID, LOI_PERCENT, STHG_ng_g, AREA_HA, ELEVATION,  CHLX_RESULT, CHLORIDE_RESULT,  MICX_RESULT, EPA_REG) # lakes in 'all' not in in 'run'
NARS_all_COMPrun_bind$Status <- "Run-Complement"
names(NARS_all_COMPrun_bind) <- names(NARS_run_bind)
NARS_run_bindBoth <- rbind(NARS_run_bind, NARS_all_COMPrun_bind) # 1155 - all run + complement in all (8 extra in run)

# NARS - compare iso to full data
NARS_iso_bind <- NARS_iso %>% dplyr::select(NLA12_ID, LOI_PERCENT, STHG, AREA_HA, ELEVATION,  CHLX_RESULT, CHLORIDE_RESULT,  MICX_RESULT, EPA_REG) 
NARS_iso_bind$Status <- "Selected"
names(NARS_iso_bind) <- names(NARS_run_bind)
NARS_all_COMPiso_bind <- NARS_all_COMPiso %>% dplyr::select(NLA12_ID, LOI_PERCENT, STHG_ng_g, AREA_HA, ELEVATION,  CHLX_RESULT, CHLORIDE_RESULT,  MICX_RESULT, EPA_REG) # lakes in 'all' not in 'iso'
NARS_all_COMPiso_bind$Status <- "Selected-Complement"
names(NARS_all_COMPiso_bind) <- names(NARS_run_bind)
NARS_iso_bindBoth <- rbind(NARS_iso_bind, NARS_all_COMPiso_bind) # 1147 - all iso + complement in all (no extras, matches)



# LakeCat - compare just OK status to full data
LakeCat_runOK_bind <- LakeCat_runOK %>% dplyr::select(NLA12_ID, CatAreaSqK, Fe2O3Cat, Fe2O3Ws)
LakeCat_runOK_bind$Status <- "OK"
LakeCat_all_COMPrunOK_bind <- LakeCat_all_COMPrunOK %>% dplyr::select(NLA12_ID, CatAreaSqKm, Fe2O3Cat, Fe2O3Ws) # lakes in 'all' without OK status in 'run'
LakeCat_all_COMPrunOK_bind$Status <- "OK-Complement"
names(LakeCat_all_COMPrunOK_bind) <- names(LakeCat_runOK_bind)
LakeCat_runOK_bindBoth <- rbind(LakeCat_runOK_bind, LakeCat_all_COMPrunOK_bind) # 1256 - all OK + complement in all

# LakeCat - compare all run to full data
LakeCat_run_bind <- LakeCat_run %>% dplyr::select(NLA12_ID, CatAreaSqK, Fe2O3Cat, Fe2O3Ws)
LakeCat_run_bind$Status <- "Run"
LakeCat_all_COMPrun_bind <- LakeCat_all_COMPrun %>% dplyr::select(NLA12_ID, CatAreaSqKm, Fe2O3Cat, Fe2O3Ws) # lakes in 'all' with either OK status or need rerun in 'run'
LakeCat_all_COMPrun_bind$Status <- "Run-Complement"
names(LakeCat_all_COMPrun_bind) <- names(LakeCat_run_bind)
LakeCat_run_bindBoth <- rbind(LakeCat_run_bind, LakeCat_all_COMPrun_bind) # 1256 - all run + complement in all 

# LakeCat - compare iso to full data
LakeCat_iso_bind <- LakeCat_iso %>% dplyr::select(NLA12_ID, CatAreaSqKm, Fe2O3Cat, Fe2O3Ws)
LakeCat_iso_bind$Status <- "Selected"
names(LakeCat_iso_bind) <- names(LakeCat_run_bind)
LakeCat_all_COMPiso_bind <- LakeCat_all_COMPiso %>% dplyr::select(NLA12_ID, CatAreaSqKm, Fe2O3Cat, Fe2O3Ws) # lakes in 'all' not in 'iso'
LakeCat_all_COMPiso_bind$Status <- "Selected-Complement"
names(LakeCat_all_COMPiso_bind) <- names(LakeCat_run_bind)
LakeCat_iso_bindBoth <- rbind(LakeCat_iso_bind, LakeCat_all_COMPiso_bind) # 1256 - all iso + complement in all 

  
# 11 variables. Choose alpha=.004545 using Bonferroni correction

####  LOI  ####
range(NARS_runOK_bindBoth$`LOI_%`, na.rm = TRUE)
range(NARS_run_bindBoth$`LOI_%`, na.rm = TRUE)
range(NARS_iso_bindBoth$`LOI_%`, na.rm = TRUE)

NARS_runOK_bindBoth$`LOI_%`[NARS_runOK_bindBoth$`LOI_%`>100] <- 100 # Change % over 100 to 100
NARS_run_bindBoth$`LOI_%`[NARS_run_bindBoth$`LOI_%`>100] <- 100 # Change % over 100 to 100
NARS_iso_bindBoth$`LOI_%`[NARS_iso_bindBoth$`LOI_%`>100] <- 100 # Change % over 100 to 100


# LOI: OK vs. rest
# Removed outlier >100 (same conclusions if kept in)
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=`LOI_%`)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$`LOI_%`[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$`LOI_%`[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=.0014. # unequal CDF
# LOI: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=`LOI_%`)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$`LOI_%`[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$`LOI_%`[NARS_run_bindBoth$Status=="Run-Complement"]) # p=.014
### LOI % diff for OK, not for run

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=`LOI_%`)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$`LOI_%`[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$`LOI_%`[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=.013


####  THg  ####
range(NARS_runOK_bindBoth$THg, na.rm = TRUE)
range(NARS_run_bindBoth$THg, na.rm = TRUE)
range(NARS_iso_bindBoth$THg, na.rm = TRUE)
NARS_runOK_bindBoth %>% filter(THg>2000)

# THg: OK vs. rest
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$THg[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$THg[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=2.2e-16 # unequal CDF
# THg: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$THg[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$THg[NARS_run_bindBoth$Status=="Run-Complement"]) # p=2.2e-16
# THg diff CDF for both - same results and statistics if log

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=2.2e-16

median((NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected"]), na.rm = T)
median((NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected-Complement"]), na.rm = T)
mean((NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected"]), na.rm = T)
mean((NARS_iso_bindBoth$THg[NARS_iso_bindBoth$Status=="Selected-Complement"]), na.rm = T)


#### Try removing extreme values
NARS_runOK_bindBoth_THg <- NARS_runOK_bindBoth %>% filter(THg<2000)
NARS_run_bindBoth_THg <- NARS_run_bindBoth %>% filter(THg<2000)
NARS_iso_bindBoth_THg <- NARS_iso_bindBoth %>% filter(THg<2000)

# THg: OK vs. rest
NARS_runOK_bindBoth_THg  %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth_THg$THg[NARS_runOK_bindBoth_THg$Status=="OK"], y=NARS_runOK_bindBoth_THg$THg[NARS_runOK_bindBoth_THg$Status=="OK-Complement"]) # p=2.2e-16 # unequal CDF
# THg: Selected vs. rest
NARS_run_bindBoth_THg %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth_THg$THg[NARS_run_bindBoth_THg$Status=="Run"], y=NARS_run_bindBoth_THg$THg[NARS_run_bindBoth_THg$Status=="Run-Complement"]) # p=2.2e-16
### THg diff CDF for both - same results and statistics if log
### THg different!!

# iso
NARS_iso_bindBoth_THg %>% ggplot(aes(x=Status, y=log(THg))) + geom_violin(aes(fill=Status)) 
NARS_iso_bindBoth_THg %>% ggplot(aes(x=Status, y=THg)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth_THg$THg[NARS_iso_bindBoth_THg$Status=="Selected"], y=NARS_iso_bindBoth_THg$THg[NARS_iso_bindBoth_THg$Status=="Selected-Complement"]) # p=2.2e-16


####  AREA_HA  ####
range(NARS_runOK_bindBoth$AREA_HA, na.rm = TRUE)
range(NARS_run_bindBoth$AREA_HA, na.rm = TRUE)
range(NARS_iso_bindBoth$AREA_HA, na.rm = TRUE)

# AREA_HA: OK vs. rest
# Removed outlier >100 (same conclusions if kept in)
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(AREA_HA))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$AREA_HA[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$AREA_HA[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=0.7553 # equal CDF
# AREA_HA: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=log(AREA_HA))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$AREA_HA[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$AREA_HA[NARS_run_bindBoth$Status=="Run-Complement"]) # p=0.7878 equal CDF
# AREA_HA same CDF for both

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=log(AREA_HA))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$AREA_HA[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$AREA_HA[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=0.7995


####  ELEVATION  ####
range(NARS_runOK_bindBoth$ELEVATION, na.rm = TRUE)
range(NARS_run_bindBoth$ELEVATION, na.rm = TRUE)
range(NARS_iso_bindBoth$ELEVATION, na.rm = TRUE)

# ELEVATION: OK vs. rest
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=ELEVATION)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$ELEVATION[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$ELEVATION[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=0.005112 # equal CDF
# ELEVATION: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=ELEVATION)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$ELEVATION[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$ELEVATION[NARS_run_bindBoth$Status=="Run-Complement"]) # p=0.1255 equal CDF
# ELEVATION same CDF for both

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=ELEVATION)) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$ELEVATION[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$ELEVATION[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=0.0009551


####  CHLX_RESUL  ####
range(NARS_runOK_bindBoth$CHLX_RESUL, na.rm = TRUE)
range(NARS_run_bindBoth$CHLX_RESUL, na.rm = TRUE)
range(NARS_iso_bindBoth$CHLX_RESUL, na.rm = TRUE)

# CHLX_RESUL: OK vs. rest
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(CHLX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$CHLX_RESUL[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$CHLX_RESUL[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=0.5856 # equal CDF
# CHLX_RESUL: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=log(CHLX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$CHLX_RESUL[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$CHLX_RESUL[NARS_run_bindBoth$Status=="Run-Complement"]) # p=0.3616 equal CDF
# CHLX_RESUL same CDF for both

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=log(CHLX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$CHLX_RESUL[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$CHLX_RESUL[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=0.8325


####  CHLORIDE_R  ####
range(NARS_runOK_bindBoth$CHLORIDE_R, na.rm = TRUE)
range(NARS_run_bindBoth$CHLORIDE_R, na.rm = TRUE)
range(NARS_iso_bindBoth$CHLORIDE_R, na.rm = TRUE)

# CHLORIDE_R: OK vs. rest
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(CHLORIDE_R))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$CHLORIDE_R[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$CHLORIDE_R[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=0.5856 # equal CDF
# CHLORIDE_R: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=log(CHLORIDE_R))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$CHLORIDE_R[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$CHLORIDE_R[NARS_run_bindBoth$Status=="Run-Complement"]) # p=0.3616 equal CDF
# CHLORIDE_R same CDF for both

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=log(CHLORIDE_R))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$CHLORIDE_R[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$CHLORIDE_R[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=0.1797


####  MICX_RESULT  ####
range(NARS_runOK_bindBoth$MICX_RESUL, na.rm = TRUE)
range(NARS_run_bindBoth$MICX_RESUL, na.rm = TRUE)
range(NARS_iso_bindBoth$MICX_RESUL, na.rm = TRUE)

# MICX_RESUL: OK vs. rest
NARS_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(MICX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_runOK_bindBoth$MICX_RESUL[NARS_runOK_bindBoth$Status=="OK"], y=NARS_runOK_bindBoth$MICX_RESUL[NARS_runOK_bindBoth$Status=="OK-Complement"]) # p=0.02134 # equal CDF
# MICX_RESUL: Selected vs. rest
NARS_run_bindBoth %>% ggplot(aes(x=Status, y=log(MICX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_run_bindBoth$MICX_RESUL[NARS_run_bindBoth$Status=="Run"], y=NARS_run_bindBoth$MICX_RESUL[NARS_run_bindBoth$Status=="Run-Complement"]) # p=0.2341 equal CDF
# MICX_RESUL same CDF for both

# iso
NARS_iso_bindBoth %>% ggplot(aes(x=Status, y=log(MICX_RESUL))) + geom_violin(aes(fill=Status)) 
ks.test(x=NARS_iso_bindBoth$MICX_RESUL[NARS_iso_bindBoth$Status=="Selected"], y=NARS_iso_bindBoth$MICX_RESUL[NARS_iso_bindBoth$Status=="Selected-Complement"]) # p=0.7976


####  EPA_REG  ####
# EPA_REG: OK vs. rest
NARS_runOK_bindBoth %>% filter(!is.na(EPA_REG)) %>% ggplot(aes(Status)) + geom_bar(aes(fill=EPA_REG), position="fill") 
# Compare proportions in each ecoregion between OK and OK-Complement (chi square test of independence)
chisq.test(table(NARS_runOK_bindBoth$EPA_REG, NARS_runOK_bindBoth$Status)) # p-value = 6.212e-08 diff proportions
# EPA_REG: Selected vs. rest
NARS_run_bindBoth %>% filter(!is.na(EPA_REG)) %>% ggplot(aes(Status)) + geom_bar(aes(fill=EPA_REG), position="fill") 
chisq.test(table(NARS_run_bindBoth$EPA_REG, NARS_run_bindBoth$Status)) # p-value = 0.0003342 diff proportions
# EPA_REG diff for both

# iso
NARS_iso_bindBoth %>% filter(!is.na(EPA_REG)) %>% ggplot(aes(Status)) + geom_bar(aes(fill=EPA_REG), position="fill") 
chisq.test(table(NARS_iso_bindBoth$EPA_REG, NARS_iso_bindBoth$Status)) # p-value = 0.09412 same proportions


####  CatAreaSqK  ####
range(LakeCat_runOK_bindBoth$CatAreaSqK, na.rm = TRUE)
range(LakeCat_run_bindBoth$CatAreaSqK, na.rm = TRUE)
range(LakeCat_iso_bindBoth$CatAreaSqK, na.rm = TRUE)

# CatAreaSqK: OK vs. rest
LakeCat_runOK_bindBoth  %>% ggplot(aes(x=Status, y=log(CatAreaSqK))) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_runOK_bindBoth$CatAreaSqK[LakeCat_runOK_bindBoth$Status=="OK"], y=LakeCat_runOK_bindBoth$CatAreaSqK[LakeCat_runOK_bindBoth$Status=="OK-Complement"]) # p=0.2278 # equal CDF
# CatAreaSqK: Selected vs. rest
LakeCat_run_bindBoth %>% ggplot(aes(x=Status, y=log(CatAreaSqK))) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_run_bindBoth$CatAreaSqK[LakeCat_run_bindBoth$Status=="Run"], y=LakeCat_run_bindBoth$CatAreaSqK[LakeCat_run_bindBoth$Status=="Run-Complement"]) # p=0.2493 equal CDF
# CatAreaSqK same CDF for both

# iso
LakeCat_iso_bindBoth %>% ggplot(aes(x=Status, y=log(CatAreaSqK))) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_iso_bindBoth$CatAreaSqK[LakeCat_iso_bindBoth$Status=="Selected"], y=LakeCat_iso_bindBoth$CatAreaSqK[LakeCat_iso_bindBoth$Status=="Selected-Complement"]) # p=0.03988


####  Fe2O3Cat  ####
range(LakeCat_runOK_bindBoth$Fe2O3Cat, na.rm = TRUE)
range(LakeCat_run_bindBoth$Fe2O3Cat, na.rm = TRUE)
range(LakeCat_iso_bindBoth$Fe2O3Cat, na.rm = TRUE)

# Fe2O3Cat: OK vs. rest
LakeCat_runOK_bindBoth  %>% ggplot(aes(x=Status, y=Fe2O3Cat)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_runOK_bindBoth$Fe2O3Cat[LakeCat_runOK_bindBoth$Status=="OK"], y=LakeCat_runOK_bindBoth$Fe2O3Cat[LakeCat_runOK_bindBoth$Status=="OK-Complement"]) # p=0.06069 # equal CDF
# Fe2O3Cat: Selected vs. rest
LakeCat_run_bindBoth %>% ggplot(aes(x=Status, y=Fe2O3Cat)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_run_bindBoth$Fe2O3Cat[LakeCat_run_bindBoth$Status=="Run"], y=LakeCat_run_bindBoth$Fe2O3Cat[LakeCat_run_bindBoth$Status=="Run-Complement"]) # p=0.1158 equal CDF
# Fe2O3Cat same CDF for both

# iso
LakeCat_iso_bindBoth %>% ggplot(aes(x=Status, y=Fe2O3Cat)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_iso_bindBoth$Fe2O3Cat[LakeCat_iso_bindBoth$Status=="Selected"], y=LakeCat_iso_bindBoth$Fe2O3Cat[LakeCat_iso_bindBoth$Status=="Selected-Complement"]) # p=0.01291


####  Fe2O3Ws  ####
range(LakeCat_runOK_bindBoth$Fe2O3Ws, na.rm = TRUE)
range(LakeCat_run_bindBoth$Fe2O3Ws, na.rm = TRUE)
range(LakeCat_iso_bindBoth$Fe2O3Ws, na.rm = TRUE)

# Fe2O3Ws: OK vs. rest
LakeCat_runOK_bindBoth  %>% ggplot(aes(x=Status, y=Fe2O3Ws)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_runOK_bindBoth$Fe2O3Ws[LakeCat_runOK_bindBoth$Status=="OK"], y=LakeCat_runOK_bindBoth$Fe2O3Ws[LakeCat_runOK_bindBoth$Status=="OK-Complement"]) # p=0.02742 # equal CDF
# Fe2O3Ws: Selected vs. rest
LakeCat_run_bindBoth %>% ggplot(aes(x=Status, y=Fe2O3Ws)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_run_bindBoth$Fe2O3Ws[LakeCat_run_bindBoth$Status=="Run"], y=LakeCat_run_bindBoth$Fe2O3Ws[LakeCat_run_bindBoth$Status=="Run-Complement"]) # p=0.3957 equal CDF
# Fe2O3Ws same CDF for both

# iso
LakeCat_iso_bindBoth %>% ggplot(aes(x=Status, y=Fe2O3Ws)) + geom_violin(aes(fill=Status)) 
ks.test(x=LakeCat_iso_bindBoth$Fe2O3Ws[LakeCat_iso_bindBoth$Status=="Selected"], y=LakeCat_iso_bindBoth$Fe2O3Ws[LakeCat_iso_bindBoth$Status=="Selected-Complement"]) # p=0.05652

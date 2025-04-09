library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)

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

# Rename SITE_ID as NLA12_ID to match LakeCat
names(NARS_all)[names(NARS_all)=="SITE_ID"] <- c("NLA12_ID")
names(NARS_all)[duplicated(names(NARS_all))]

NARS_all <- NARS_all[!is.na(NARS_all$NLA12_ID),] # 1124

# Remove duplicate columns (keep first instance, fewer NAs)
NARS_all <- NARS_all[, !duplicated(colnames(NARS_all))] 

NARS_all$SITE_DEPTH_m[NARS_all$SITE_DEPTH_m=="NA"] <- NA
NARS_all$SITE_DEPTH_m <- as.numeric(NARS_all$SITE_DEPTH_m)

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



# Add lake status
NARS_all$Status <- "Rest"
NARS_all$Status[!is.na(NARS_all$d202_Avg)] <- "Done"
NARS_all$Status[NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"

NARS_all$Status2 <- NARS_all$Status
NARS_all$Status2[NARS_all$Status2=="Done" | NARS_all$Status2=="Ready"] <- "Done/Ready"

NARS_all$Status3 <- NARS_all$Status
NARS_all$Status3[NARS_all$Status3=="Rest" | NARS_all$Status3=="Ready"] <- "Ready/Rest"


#sum(!is.na(NARS_all$d202_Avg) & NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis", na.rm = T)
unique(NARS_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis)




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

str(LakeCat_all)

# Add lake status
LakeCat_all$Status <- "Rest"
LakeCat_all$Status[!is.na(LakeCat_all$d202_Avg)] <- "Done"
LakeCat_all$Status[LakeCat_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis"] <- "Ready"

LakeCat_all$Status2 <- LakeCat_all$Status
LakeCat_all$Status2[LakeCat_all$Status2=="Done" | LakeCat_all$Status2=="Ready"] <- "Done/Ready"

LakeCat_all$Status3 <- LakeCat_all$Status
LakeCat_all$Status3[LakeCat_all$Status3=="Rest" | LakeCat_all$Status3=="Ready"] <- "Ready/Rest"

#sum(!is.na(LakeCat_all$d202_Avg) & LakeCat_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis=="weighed ready for analysis", na.rm = T)
unique(LakeCat_all$USGS_ID_did_not_match_OR_weighed_ready_for_analysis)



# 
sum(NARS_all$Status=="Done")           # 377 (previously 391) run
sum(NARS_all$Status2=="Done/Ready")    # 465 (previously 673) iso

sum(LakeCat_all$Status=="Done")        # 374 (previously 402) run
sum(LakeCat_all$Status2=="Done/Ready") # 462 (previously 685) iso

## For now, subset lakes in LakeCat_all to those in NARS_all

LakeCat_all <- LakeCat_all %>% filter(NLA12_ID %in% NARS_all$NLA12_ID) # 1112

# Step 1) Analyze those labeled “Done” under status versus 'Rest'  for bias.

# Step 2) If step one analysis of 'Done' versus 'Rest' show bias, 'Ready' samples can be selectively analyzed to reduce bias further. 

# Step 3) If samples from 'Rest' need novel selection to eliminate bias, this can be done but at the cost of much greater/slower work. 


# Previous NARS variables: LOI_%, THg, AREA_HA, ELEVATION, CHLX_RESUL, CHLORIDE_R, MICX_RESUL, EPA_REG 
# In LakeCat analyze on columns: CatAreaSqK, Fe2O3Cat, Fe2O3Ws

# Current requests:
# LOI_PERCENT, STHG_ng_g, Lake_Area_HA, ELEVATION, X_site_chlorophyll_a_ug_L, CHLORIDE_RESULT_mg_L, MICX_RESULT_ppb, , EPA_REG
# NEW: DRY_WEIGHT_PERCENT,  SMHG_ng_g,  Hg0DryDep, Hg2DryDep, HgPDryDep,  Trophic_state_Oligotrophic_Mesotrophic_Eutrophic_Hypereutrophic,   Min_OXYGEN_mg_L, SECCHI_m_use_depth_for_clear_to_bottom
# Plus one of each:
# Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3
# WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s, WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s


NARS_all_vars <- NARS_all %>% dplyr::select(NLA12_ID, USGS_ID, Status, Status2, Status3, DRY_WEIGHT_PERCENT, LOI_PERCENT, SMHG_ng_g, STHG_ng_g, Hg0DryDep, Hg2DryDep, HgPDryDep, Lake_Area_HA, ELEVATION, X_site_chlorophyll_a_ug_L, Trophic_state_Oligotrophic_Mesotrophic_Eutrophic_Hypereutrophic, CHLORIDE_RESULT_mg_L, MICX_RESULT_ppb, Min_OXYGEN_mg_L, SECCHI_m_use_depth_for_clear_to_bottom, EPA_REG, Omernik_I, Omernik_II, HUC2, Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3, WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s, WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s)

NARS_all_vars <- NARS_all_vars %>% rename(chla_ug_L=X_site_chlorophyll_a_ug_L, Trophic_state=Trophic_state_Oligotrophic_Mesotrophic_Eutrophic_Hypereutrophic, SECCHI_m=SECCHI_m_use_depth_for_clear_to_bottom, WetLossConv_kg_s=WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s, WetLossLS_kg_s=WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s)

# log certain variables
NARS_all_vars <- NARS_all_vars %>%  mutate(logChla=log(chla_ug_L+.01), logSecchi=log(SECCHI_m+.01), logSMHG=log(SMHG_ng_g+.01), logSTHG=log(STHG_ng_g+.01), logArea=log(Lake_Area_HA), logChloride=log(CHLORIDE_RESULT_mg_L+.01), logMICX=log(MICX_RESULT_ppb+.01), logMinOXYGEN=log(Min_OXYGEN_mg_L+.01))


hist(NARS_all_vars$logMinOXYGEN)

NARS_all_vars_Cont <- NARS_all_vars %>% dplyr::select(DRY_WEIGHT_PERCENT, LOI_PERCENT, logSMHG, logSTHG, Hg0DryDep, Hg2DryDep, HgPDryDep, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, logSecchi, Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3, WetLossConv_kg_s, WetLossLS_kg_s)


NARS_all_vars_Cont_cor <- NARS_all_vars_Cont[complete.cases(NARS_all_vars_Cont),]

Cont_cor <- cor(NARS_all_vars_Cont_cor)
abs(Cont_cor)>.5
# DRY_WEIGHT_PERCENT, LOI_PERCENT    - remove DRY_WEIGHT_PERCENT or LOI_PERCENT?
# LOI_PERCENT more correlated with SMHG/STHG and removing those because know STHG different, and SMHG correlated with STHG
# So choose LOI_PERCENT


NARS_all_vars_Cont2 <- NARS_all_vars %>% dplyr::select(LOI_PERCENT, Hg0DryDep, Hg2DryDep, HgPDryDep, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, logSecchi, Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3, WetLossConv_kg_s, WetLossLS_kg_s)

NARS_all_vars_Cont2_cor <- NARS_all_vars_Cont2[complete.cases(NARS_all_vars_Cont2),]

Cont_cor2 <- cor(NARS_all_vars_Cont2_cor)
abs(Cont_cor2)>.5

# Hg0DryDep, ELEVATION                    - remove Hg0DryDep
# Hg0DryDep, Gas_Hg_Hg0Conc_ng_m3  
# Hg2DryDep, ELEVATION                   - remove Hg2DryDep
# Hg2DryDep, Gas_Hg_Hg0Conc_ng_m3
# Hg2DryDep, Ionic_Hg2Conc_ng_m3
# HgPDryDep, Particle_Hg_HgPConc_ng_m3  - remove HgPDryDep
# Gas_Hg_Hg0Conc_ng_m3, ELEVATION       - remove Gas_Hg_Hg0Conc_ng_m3       
# logChla, logSecchi                    - remove Secchi


NARS_all_vars_Cont3 <- NARS_all_vars %>% dplyr::select(LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN,  Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3, WetLossConv_kg_s, WetLossLS_kg_s)

NARS_all_vars_Cont3_cor <- NARS_all_vars_Cont3[complete.cases(NARS_all_vars_Cont3),]
Cont_cor3 <- cor(NARS_all_vars_Cont3_cor)

mean(abs(Cont_cor3[,8])) #  ionic
mean(abs(Cont_cor3[,9])) #  particle, choose this one

mean(abs(Cont_cor3[,10])) #  WetLossConv_kg_s # 0.23
mean(abs(Cont_cor3[,11])) #  WetLossLS_kg_s # 0.22 - choose this one


NARS_all_vars_Cont4 <- NARS_all_vars %>% dplyr::select(LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s)


NARS_all_vars_Cont4_cor <- NARS_all_vars_Cont4[complete.cases(NARS_all_vars_Cont4),]
Cont_cor4 <- cor(NARS_all_vars_Cont4_cor)


# NOT INCLUDING TROPHIC STATE BECAUSE TOO SIMILAR TO logChla!!

boxplot(logChla~Trophic_state, data=NARS_all_vars)

# FINAL 10 VARIABLES FROM NARS_all
# LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s, EPA_REG



# Previous LakeCat variables: CatAreaSqK, Fe2O3Cat, Fe2O3Ws
# Current requests: Fe2O3Cat, Fe2O3Ws, Precip08Cat, Precip09Cat, PopDen2010Ws

LakeCat_all_vars <- LakeCat_all %>% dplyr::select(NLA12_ID, USGS_ID, Status, Status2, Status3, Fe2O3Cat, Fe2O3Ws, Precip08Cat, Precip09Cat, PopDen2010Ws)

# log certain variables
LakeCat_all_vars <- LakeCat_all_vars %>%  mutate(logPopDen2010Ws=log(PopDen2010Ws+.01))

LakeCat_all_vars_Cont <- LakeCat_all_vars %>% dplyr::select(Fe2O3Cat, Fe2O3Ws, Precip08Cat, Precip09Cat, logPopDen2010Ws)

LakeCat_all_vars_Cont_cor <- LakeCat_all_vars_Cont[complete.cases(LakeCat_all_vars_Cont),]

Cont_cor <- cor(LakeCat_all_vars_Cont_cor)
abs(Cont_cor)>.5

plot(LakeCat_all_vars$Fe2O3Cat, log(LakeCat_all_vars$PopDen2010Ws))

# Final: Fe2O3Ws (least correlated), Precip09Cat (least correlated with Fe2O3Ws), logPopDen2010Ws


LakeCat_all_vars_Cont_Final <- LakeCat_all_vars %>% dplyr::select(NLA12_ID, Fe2O3Ws, Precip09Cat, logPopDen2010Ws)

NARS_all_vars_Cont_Final <- NARS_all_vars %>% dplyr::select(NLA12_ID, LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s)

Both_all_vars_Cont_Final <- inner_join(NARS_all_vars_Cont_Final, LakeCat_all_vars_Cont_Final)
Both_all_vars_Cont_cor <- Both_all_vars_Cont_Final[complete.cases(Both_all_vars_Cont_Final),]

Cont_cor_both <- cor(Both_all_vars_Cont_cor[,-1])
abs(Cont_cor_both)>.5

# logPopDen2010Ws, ELEVATION  - remove logPopDen2010Ws
# Precip09Cat, WetLossLS_kg_s - remove Precip09Cat

plot(Both_all_vars_Cont_Final$logChla, Both_all_vars_Cont_Final$logMICX)


# FINAL VARIABLES:
# NARS: LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMICX, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s
# LakeCat: Fe2O3Ws


# After talking with Ryan, remove MICX, keep EPA_REG. So 10 final continuous variables if just remove MICX

LakeCat_all_vars_Final <- LakeCat_all_vars %>% dplyr::select(NLA12_ID, Status, Status2, Status3, Fe2O3Ws)

NARS_all_vars_Final <- NARS_all_vars %>% dplyr::select(NLA12_ID, Status, Status2, Status3, LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s, EPA_REG, Omernik_I, Omernik_II, HUC2)



NARS_all_vars_Final_complete <- NARS_all_vars_Final[complete.cases(NARS_all_vars_Final),] # 969 have all

NARS_all_vars_Final_incomplete  <- NARS_all_vars_Final[!complete.cases(NARS_all_vars_Final),]

length(unique(NARS_all_vars_Final$NLA12_ID)) # 1124


Both_all_vars_Final <- left_join(NARS_all_vars_Final, LakeCat_all_vars_Final)

Both_all_vars_Final <- Both_all_vars_Final[complete.cases(Both_all_vars_Final),] # 964 have all





# 10 variables. Choose alpha=.005 using Bonferroni correction

####  LOI  ####
range(NARS_all_vars_Final$LOI_PERCENT, na.rm = TRUE)
NARS_all_vars_Final$LOI_PERCENT[NARS_all_vars_Final$LOI_PERCENT>100] <- 100 # Change % over 100 to 100
sum(!is.na(NARS_all_vars_Final$LOI_PERCENT)) # 1082/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(LOI_PERCENT))) # 42 missing are in rest

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=LOI_PERCENT)) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=LOI_PERCENT)) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$LOI_PERCENT[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$LOI_PERCENT[NARS_all_vars_Final$Status3=="Ready/Rest"]) # p=.01. # equal CDF

# LOI % same 



####  AREA_HA  ####
range(NARS_all_vars_Final$logArea, na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$logArea)) # 1124/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(logArea))) # none missing

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=logArea)) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=logArea)) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$logArea[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$logArea[NARS_all_vars_Final$Status3=="Ready/Rest"]) # p=0.8746. # equal CDF

# Area same



####  ELEVATION  ####
range(NARS_all_vars_Final$ELEVATION, na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$ELEVATION)) # 1124/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(ELEVATION))) # none missing

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=ELEVATION)) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=ELEVATION)) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$ELEVATION[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$ELEVATION[NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.01105

# ELEVATION different



####  CHLX_RESUL  ####
range(NARS_all_vars_Final$logChla , na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$logChla )) # 1123/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(logChla))) # 1 missing in done

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=logChla )) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=logChla )) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$logChla[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$logChla [NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.109

# logChla same



####  CHLORIDE_R  ####
range(NARS_all_vars_Final$logChloride , na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$logChloride )) # 1034/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(logChloride))) # 1 missing in done, 89 missing in rest

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=logChloride )) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=logChloride )) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$logChloride[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$logChloride[NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.1227

# logChloride same



####  logMinOXYGEN   ####
range(NARS_all_vars_Final$logMinOXYGEN  , na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$logMinOXYGEN  )) # 1098/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(logMinOXYGEN))) # 12 missing in done, 3 missing in ready, 11 missing in rest


# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=logMinOXYGEN  )) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=logMinOXYGEN  )) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$logMinOXYGEN[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$logMinOXYGEN[NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.1191

# logMinOXYGEN same


####  Particle_Hg_HgPConc_ng_m3   ####
range(NARS_all_vars_Final$Particle_Hg_HgPConc_ng_m3, na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$Particle_Hg_HgPConc_ng_m3  )) # 1035/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(Particle_Hg_HgPConc_ng_m3))) # 89 missing in rest

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=Particle_Hg_HgPConc_ng_m3  )) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=Particle_Hg_HgPConc_ng_m3  )) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$Particle_Hg_HgPConc_ng_m3[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$Particle_Hg_HgPConc_ng_m3[NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.03535

# Particle_Hg_HgPConc_ng_m3 different


####  WetLossLS_kg_s   ####
range(NARS_all_vars_Final$WetLossLS_kg_s, na.rm = TRUE)
sum(!is.na(NARS_all_vars_Final$WetLossLS_kg_s  )) # 1035/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(WetLossLS_kg_s))) # 89 missing in rest

# Done vs. rest
NARS_all_vars_Final  %>% ggplot(aes(x=Status3, y=WetLossLS_kg_s  )) + geom_violin(aes(fill=Status3)) 
NARS_all_vars_Final  %>% ggplot(aes(x=Status, y=WetLossLS_kg_s  )) + geom_violin(aes(fill=Status)) 

ks.test(x=NARS_all_vars_Final$WetLossLS_kg_s[NARS_all_vars_Final$Status3=="Done"], y=NARS_all_vars_Final$WetLossLS_kg_s[NARS_all_vars_Final$Status3=="Ready/Rest"]) # 0.0101

# WetLossLS_kg_s different



####  Omernik_II  ####
sum(!is.na(NARS_all_vars_Final$Omernik_II  )) # 0/1124

NARS_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(Omernik_II))) # 0 missing

# Done vs. rest
NARS_all_vars_Final %>% filter(!is.na(Omernik_II)) %>% ggplot(aes(Status3)) + geom_bar(aes(fill=Omernik_II), position="fill") 
NARS_all_vars_Final %>% filter(!is.na(Omernik_II)) %>% ggplot(aes(Status)) + geom_bar(aes(fill=Omernik_II), position="fill") 

# Compare proportions in each ecoregion between Done vs. Rest (chi square test of independence)
chisq.test(table(NARS_all_vars_Final$Omernik_II, NARS_all_vars_Final$Status3)) # p-value = 2.063e-07 diff proportions

# Omernik_II different





####  Fe2O3Ws  ####
range(LakeCat_all_vars_Final$Fe2O3Ws, na.rm = TRUE)
sum(!is.na(LakeCat_all_vars_Final$Fe2O3Ws  )) # 1110/1112

LakeCat_all_vars_Final %>% group_by(Status) %>% summarize(sum(is.na(Fe2O3Ws))) # 1 missing done, 1 missing rest

# Done vs. rest
LakeCat_all_vars_Final  %>% ggplot(aes(x=Status3, y=Fe2O3Ws  )) + geom_violin(aes(fill=Status3)) 
LakeCat_all_vars_Final  %>% ggplot(aes(x=Status, y=Fe2O3Ws  )) + geom_violin(aes(fill=Status)) 

ks.test(x=LakeCat_all_vars_Final$Fe2O3Ws[LakeCat_all_vars_Final$Status3=="Done"], y=LakeCat_all_vars_Final$Fe2O3Ws[LakeCat_all_vars_Final$Status3=="Ready/Rest"]) # 0.1361

# Fe2O3Ws same


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

LakeCat_all <- LakeCat_all %>% filter(NLA12_ID %in% NARS_all$NLA12_ID) # 1112


###################

# ECOREGION

#NARS_all %>% filter(!is.na(Omernik_I)) %>% ggplot(aes(Status3)) + geom_bar(aes(fill=Omernik_I), position="fill") 
NARS_all %>% filter(!is.na(Omernik_II)) %>% ggplot(aes(Status3)) + geom_bar(aes(fill=Omernik_II), position="fill") 
#NARS_all %>% filter(!is.na(HUC2)) %>% ggplot(aes(Status3)) + geom_bar(aes(fill=as.factor(HUC2)), position="fill") 

# Compare proportions in each ecoregion between Done vs. Rest (chi square test of independence)
# chisq.test(table(as.factor(NARS_all$HUC2), NARS_all$Status3)) # p-value = 0.0007608 diff proportions
# chisq.test(table(NARS_all$Omernik_I, NARS_all$Status3)) # p-value = 1.948e-08 diff proportions
chisq.test(table(NARS_all$Omernik_II, NARS_all$Status3)) # p-value = 2.063e-07 diff proportions


eco_table_all <- data.frame(table(NARS_all$Omernik_II)) %>% rename(Region=Var1) %>% mutate(Prop=Freq/sum(Freq))

#eco_table <- as.data.frame.matrix(table(NARS_all$Omernik_II, NARS_all$Status3))

eco_table <- as.data.frame.matrix(table(NARS_all$Omernik_II, NARS_all$Status))

eco_table <- cbind(eco_table, eco_table_all) %>% relocate(Region)
row.names(eco_table) <- NULL

eco_table <- eco_table %>% rename(Pop_Prop=Prop) %>% mutate(Current_Prop=Done/sum(Done)) %>% mutate(Diff_Prop=Pop_Prop - Current_Prop) %>% mutate(Add= Diff_Prop>0) %>% arrange(desc(Diff_Prop)) 


samp_eco <- function(NumAdd=20, eco_table=eco_table){
  eco_table <- eco_table %>% mutate(Target=floor((sum(Done) + NumAdd) * Pop_Prop))
  eco_table <- eco_table %>% mutate(Target_Diff=Target-Done)
  
  # Weight the number allocated by the number needed
  eco1 <- eco_table %>% filter(Add==TRUE) %>% mutate(CanAdd=round((Target_Diff/sum(Target_Diff))*NumAdd))
  eco2 <- eco_table %>% filter(Add==FALSE) %>% mutate(CanAdd=0)
  new_eco <- rbind(eco1, eco2)
  
  new_eco <- new_eco %>% mutate(ReadyRest=Ready+Rest-CanAdd, NewDone = Done+CanAdd)
  
  test_eco <- new_eco %>% dplyr::select(NewDone, ReadyRest)
  
  list(new_eco=new_eco, test_eco=test_eco)
  
}

# Add samples until p>.05
tune_NumAdd <- samp_eco(NumAdd=35, eco_table=eco_table)
chisq.test(tune_NumAdd$test_eco)

Target_dist <- tune_NumAdd$new_eco
sum(Target_dist$Add) # actually adds 36 lakes

sum(Target_dist$NewDone) # 413 lakes

Target_dist <- Target_dist %>% filter(CanAdd>0) %>% dplyr::select(Region, Done, CanAdd, Ready, Rest) %>% rename(Add=CanAdd)

# Target_dist <- Target_dist %>% mutate(SampleReady=apply(Target_dist[, c("Add", "Ready")], 1, min))
# Target_dist <- Target_dist %>% mutate(SampleRest=Add-SampleReady)


###### Sample lakes for ecoregion ###########

set.seed(3)
NARS_all$NewStatus <- NARS_all$Status

new.IDs <- ready.IDs <- rest.IDs <- NULL

for(i in 1:nrow(Target_dist)){
  print(i)
  Reg.i <- Target_dist$Region[i]
  
  Ready.i <- NARS_all %>% filter(Status=="Ready", Omernik_II==Reg.i, !is.na(USGS_ID), STHG_ng_g>30, !is.na(Particle_Hg_HgPConc_ng_m3)) 
  Ready.IDs.i <- Ready.i %>% summarize(IDs=sample(NLA12_ID, min(c(Target_dist$Add[i], nrow(Ready.i))) ))
  
  left.i <- Target_dist$Add[i]-nrow(Ready.IDs.i)
                                       
  Rest.IDs.i <- NARS_all %>% filter(Status=="Rest", Omernik_II==Reg.i, !is.na(USGS_ID), STHG_ng_g>30, !is.na(Particle_Hg_HgPConc_ng_m3)) %>% summarize(IDs=sample(NLA12_ID, left.i))
  
  new.IDs <- rbind(new.IDs, Ready.IDs.i, Rest.IDs.i)
  ready.IDs <- rbind(ready.IDs, Ready.IDs.i)
  rest.IDs <- rbind(rest.IDs, Rest.IDs.i)
  
  }

nrow(ready.IDs) # 28 ready
nrow(rest.IDs) # 8 rest


NARS_all$NewStatus[NARS_all$NLA12_ID %in% new.IDs$IDs] <- "Done"

sum(!new.IDs$IDs %in% LakeCat_all$NLA12_ID) # 0

LakeCat_all$NewStatus <- LakeCat_all$Status
LakeCat_all$NewStatus[LakeCat_all$NLA12_ID %in% new.IDs$IDs] <- "Done"


ready.IDs$Status <- "Ready"
rest.IDs$Status <- "Rest"

write.lakes <- rbind(ready.IDs, rest.IDs)
write.csv(write.lakes, "Data/Lakes_To_Add.csv", row.names = F)

write.lakes.data <- NARS_all %>% filter(NLA12_ID %in% write.lakes$IDs)
write.csv(write.lakes.data, "Data/Lakes_To_Add_NARSdata.csv", row.names = F)

write.lakes.data_LakeCat <- LakeCat_all %>% filter(NLA12_ID %in% write.lakes$IDs)
write.csv(write.lakes.data_LakeCat, "Data/Lakes_To_Add_LakeCatdata.csv", row.names = F)

write.lakes.data.NewDone <- NARS_all %>% filter(NewStatus=="Done")
write.csv(write.lakes.data.NewDone, "Data/DonePlusNew_Lakes_NARSdata.csv", row.names = F)

write.lakes.data.NewDone_LakeCat <- LakeCat_all %>% filter(NewStatus=="Done")
write.csv(write.lakes.data.NewDone_LakeCat, "Data/DonePlusNew_Lakes_LakeCatdata.csv", row.names = F)


ks.test(x=NARS_all$LOI_PERCENT[NARS_all$NewStatus=="Done"], y=NARS_all$LOI_PERCENT[!NARS_all$NewStatus=="Done"]) # 0.2202

ks.test(x=NARS_all$Lake_Area_HA[NARS_all$NewStatus=="Done"], y=NARS_all$Lake_Area_HA[!NARS_all$NewStatus=="Done"]) # 0.7704

ks.test(x=NARS_all$ELEVATION[NARS_all$NewStatus=="Done"], y=NARS_all$ELEVATION[!NARS_all$NewStatus=="Done"]) # 0.06445

ks.test(x=NARS_all$X_site_chlorophyll_a_ug_L[NARS_all$NewStatus=="Done"], y=NARS_all$X_site_chlorophyll_a_ug_L[!NARS_all$NewStatus=="Done"]) # 0.2852

ks.test(x=NARS_all$CHLORIDE_RESULT_mg_L[NARS_all$NewStatus=="Done"], y=NARS_all$CHLORIDE_RESULT_mg_L[!NARS_all$NewStatus=="Done"]) # 0.08987

ks.test(x=NARS_all$Min_OXYGEN_mg_L[NARS_all$NewStatus=="Done"], y=NARS_all$Min_OXYGEN_mg_L[!NARS_all$NewStatus=="Done"]) # 0.473

ks.test(x=NARS_all$Particle_Hg_HgPConc_ng_m3[NARS_all$NewStatus=="Done"], y=NARS_all$Particle_Hg_HgPConc_ng_m3[!NARS_all$NewStatus=="Done"]) # 0.1764

ks.test(x=NARS_all$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[NARS_all$NewStatus=="Done"], y=NARS_all$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[!NARS_all$NewStatus=="Done"]) # 0.04829

NARS_all$NewStatus2 <- "Ready/Rest";  NARS_all$NewStatus2[NARS_all$NewStatus=="Done"] <- "Done"
chisq.test(table(NARS_all$Omernik_II, NARS_all$NewStatus2)) # 0.06656

ks.test(x=LakeCat_all$Fe2O3Ws[LakeCat_all$NewStatus=="Done"], y=LakeCat_all$Fe2O3Ws[!LakeCat_all$NewStatus=="Done"]) # 0.5973

LakeCat_all$NewStatus2 <- "Ready/Rest";  LakeCat_all$NewStatus2[LakeCat_all$NewStatus=="Done"] <- "Done"


NARS_all  %>% ggplot(aes(x=NewStatus2, y=LOI_PERCENT)) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=log(Lake_Area_HA))) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=ELEVATION)) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=log(X_site_chlorophyll_a_ug_L+1))) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=log(CHLORIDE_RESULT_mg_L+1))) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=Min_OXYGEN_mg_L)) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=Particle_Hg_HgPConc_ng_m3)) + geom_violin(aes(fill=NewStatus2)) 
NARS_all  %>% ggplot(aes(x=NewStatus2, y=WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s)) + geom_violin(aes(fill=NewStatus2)) 

LakeCat_all  %>% ggplot(aes(x=NewStatus2, y=Fe2O3Ws)) + geom_violin(aes(fill=NewStatus2)) 

NARS_all %>% filter(!is.na(Omernik_II)) %>% ggplot(aes(NewStatus2)) + geom_bar(aes(fill=Omernik_II), position="fill") 


######################

# NARS_all <- NARS_all %>% mutate(logArea=log(Lake_Area_HA))
# 
# round(quantile(NARS_all$logArea[NARS_all$Status3=="Done"], probs=seq(0,1,.1)), 2)
# 
# 
# # Area_deciles6 <- log(c(4, 10, 20, 50, 100))
# # Area_num6 <- c(sum(NARS_all$logArea<=Area_deciles6[1]),
# #                     sum(NARS_all$logArea>Area_deciles6[1] & NARS_all$logArea<=Area_deciles6[2]),
# #                     sum(NARS_all$logArea>Area_deciles6[2] & NARS_all$logArea<=Area_deciles6[3]),
# #                     sum(NARS_all$logArea>Area_deciles6[3] & NARS_all$logArea<=Area_deciles6[4]),
# #                     sum(NARS_all$logArea>Area_deciles6[4] & NARS_all$logArea<=Area_deciles6[5]),
# #                     sum(NARS_all$logArea>Area_deciles6[5]))
# 
# 
# # Area deciles in whole population
# Area_deciles <- round(quantile(NARS_all$logArea, probs=seq(0,1,.1)), 2)
# 
# NARS_all %>% filter(is.na(logArea))
# 
# sum(is.na(NARS_all$logArea))
# 
# round(exp(Area_deciles), 2)
# # Number originally done in each category 
# Area_num_Done <- c(sum(NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[2]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[2] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[3]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[3] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[4]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[4] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[5]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[5] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[6]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[6] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[7]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[7] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[8]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[8] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[9]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[9] & NARS_all$logArea[NARS_all$Status=="Done"]<=Area_deciles[10]),
#                    sum(NARS_all$logArea[NARS_all$Status=="Done"]>Area_deciles[10]))
# 
# 
# 
# 
# Area_num_Ready <- c(sum(NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[2]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[2] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[3]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[3] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[4]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[4] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[5]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[5] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[6]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[6] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[7]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[7] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[8]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[8] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[9]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[9] & NARS_all$logArea[NARS_all$Status=="Ready"]<=Area_deciles[10]),
#                     sum(NARS_all$logArea[NARS_all$Status=="Ready"]>Area_deciles[10]))
# 
# 
# Area_num_Done
# Area_num_Ready
# sum(NARS_all$Status=="Ready") # Only 88 Ready lakes
# 
# sum(Area_num_Done[1:5]); sum(Area_num_Done[6:10])
# 
# 
# # Try 30 in each category
# 
# # Add Ready lakes first
# #num <- 40 # Number lakes in each decile (goal is to have equal number lakes in each decile of original dist)
# 
# SampleLakes <- function(NARS_all=NARS_all, Area_num_Done=Area_num_Done, Area_num_Ready=Area_num_Ready, Area_deciles=Area_deciles, num=40, RemoveLakes=FALSE, numCap=50, seed=3){
#   
#   set.seed(seed)
#   NARS_all$NewStatus <- NARS_all$Status
#   
#   Sample1 <- NARS_all %>% filter(Status=="Ready", logArea<=Area_deciles[2]) %>% summarize(IDs=sample(NLA12_ID, min(c(Area_num_Ready[1], max(c(0,num-Area_num_Done[1]))))))
#   Sample2 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[2], logArea<=Area_deciles[3]) %>% summarize(IDs=sample(NLA12_ID,  min(c(Area_num_Ready[2], max(c(0,num-Area_num_Done[2]))))))
#   Sample3 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[3], logArea<=Area_deciles[4]) %>% summarize(IDs=sample(NLA12_ID,  min(c(Area_num_Ready[3], max(c(0,num-Area_num_Done[3]))))))
#   Sample4 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[4], logArea<=Area_deciles[5]) %>% summarize(IDs=sample(NLA12_ID,  min(c(Area_num_Ready[4], max(c(0,num-Area_num_Done[4]))))))
#   Sample5 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[5], logArea<=Area_deciles[6]) %>% summarize(IDs=sample(NLA12_ID,  min(c(Area_num_Ready[5], max(c(0,num-Area_num_Done[5]))))))
#   Sample6 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[6], logArea<=Area_deciles[7]) %>% summarize(IDs=sample(NLA12_ID,   min(c(Area_num_Ready[6], max(c(0,num-Area_num_Done[6]))))))
#   Sample7 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[7], logArea<=Area_deciles[8]) %>% summarize(IDs=sample(NLA12_ID,  min(c(Area_num_Ready[7], max(c(0,num-Area_num_Done[7]))))))
#   Sample8 <- NARS_all %>% filter(Status=="Ready", logArea>Area_deciles[8], logArea<=Area_deciles[9]) %>% summarize(IDs=sample(NLA12_ID,   min(c(Area_num_Ready[8], max(c(0,num-Area_num_Done[8]))))))
#   ReadySamps <- rbind(Sample1, Sample2, Sample3, Sample4, Sample5, Sample6, Sample7, Sample8)
#   
#   NARS_all$NewStatus[NARS_all$NLA12_ID %in% ReadySamps$IDs] <- "Done"
#   
#   #sum(NARS_all$NLA12_ID %in% ReadySamps$IDs)
#   # Areas still different with just ready lakes p-value < 2.2e-16
#   
#   # Number done in each category 
#   Area_num_NewDone <- c(sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[2]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[2] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[3]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[3] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[4]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[4] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[5]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[5] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[6]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[6] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[7]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[7] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[8]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[8] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[9]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[9] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[10]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[10]))
#   
#   
#   # Randomly select lakes from Rest to fill in
#   # Try 30 in first 3
#   #set.seed(3333)
#   # Filter to those with data? , !is.na(SMHG_ng_g) 
#   Sample1 <- NARS_all %>% filter(NewStatus=="Rest", logArea<=Area_deciles[2]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[1]))))
#   Sample2 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[2], logArea<=Area_deciles[3]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[2]))))
#   Sample3 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[3], logArea<=Area_deciles[4]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[3]))))
#   Sample4 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[4], logArea<=Area_deciles[5]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[4]))))
#   Sample5 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[5], logArea<=Area_deciles[6]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[5]))))
#   Sample6 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[6], logArea<=Area_deciles[7]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[6]))))
#   Sample7 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[7], logArea<=Area_deciles[8]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[7]))))
#   Sample8 <- NARS_all %>% filter(NewStatus=="Rest", logArea>Area_deciles[8], logArea<=Area_deciles[9]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,num-Area_num_NewDone[8]))))
#   Samps <- rbind(Sample1, Sample2, Sample3, Sample4, Sample5, Sample6, Sample7, Sample8)
#   
#   NARS_all$NewStatus[NARS_all$NLA12_ID %in% Samps$IDs] <- "Done"
#   
#   if(RemoveLakes==TRUE){
#     # Remove some Dones
#     Sample7 <- NARS_all %>% filter(NewStatus=="Done", logArea>Area_deciles[7], logArea<=Area_deciles[8]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,Area_num_NewDone[7]-numCap))))
#     Sample8 <- NARS_all %>% filter(NewStatus=="Done", logArea>Area_deciles[8], logArea<=Area_deciles[9]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,Area_num_NewDone[8]-numCap))))
#     Sample9 <- NARS_all %>% filter(NewStatus=="Done", logArea>Area_deciles[9], logArea<=Area_deciles[10]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,Area_num_NewDone[9]-numCap))))
#     Sample10 <- NARS_all %>% filter(NewStatus=="Done", logArea>Area_deciles[10], logArea<=Area_deciles[11]) %>% summarize(IDs=sample(NLA12_ID, max(c(0,Area_num_NewDone[10]-numCap))))
#     SampsRemove <- rbind(Sample7, Sample8, Sample9, Sample10)
#     
#     NARS_all$NewStatus[NARS_all$NLA12_ID %in% SampsRemove$IDs] <- "Rest"
#   }
#   
#   # Number done in each category 
#   Area_num_NewDone <- c(sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[2]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[2] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[3]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[3] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[4]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[4] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[5]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[5] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[6]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[6] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[7]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[7] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[8]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[8] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[9]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[9] & NARS_all$logArea[NARS_all$NewStatus=="Done"]<=Area_deciles[10]),
#                         sum(NARS_all$logArea[NARS_all$NewStatus=="Done"]>Area_deciles[10]))
#   
#   if(RemoveLakes==TRUE){list(NARS_all=NARS_all, ReadySamps=ReadySamps, Samps=Samps, SampsRemove=SampsRemove, Area_num_NewDone=Area_num_NewDone)}else{
#     list(NARS_all=NARS_all, ReadySamps=ReadySamps, Samps=Samps, Area_num_NewDone=Area_num_NewDone)}
#   
# }
# 
# 
# ################################
# 
# newdata.test.Area <- function(data=NewData$NARS_all){
#   ks.test(x=data$logArea[data$NewStatus=="Done"], y=data$logArea[!data$NewStatus=="Done"]) 
# }
# 
# NewData$Area_num_NewDone
# 
# NewData <- SampleLakes(NARS_all=NARS_all, Area_num_Done=Area_num_Done, Area_num_Ready=Area_num_Ready, Area_deciles=Area_deciles, num=40, RemoveLakes=T, numCap=50, seed=3)
# newdata.test.Area(data=NewData$NARS_all)
# 
# round(exp(Area_deciles), 2)
# 
# nrow(NewData$ReadySamps); nrow(NewData$Samps); nrow(NewData$SampsRemove); sum(NewData$NARS_all$NewStatus=="Done")
# # 30 in each: 13 Ready, 34  New, p=1.266e-14 ; 0.04395 if cap at 38 (remove 92)
# # 35 in each: 24 Ready, 53  New, p=5.709e-10 ; 0.02865 if cap at 39 (remove 66)
# # 40 in each: 29 Ready, 78  New, p= 8.441e-07; 0.02621 if cap at 50 (remove 45)
# # 45 in each: 29 Ready, 108 New, p= 7.502e-05; 0.03281 if cap at 59 (remove 26)
# # 50 in each; 30 Ready, 138 New, p= 0.002282;  0.02636 if cap at 67 (remove 10)
# # 55 in each; 39 Ready, 168 New, p= 0.02543;   0.9984 if cap at 55 (remove 34)
# 
# # So, have to add 168 to get same distribution without removing lakes
# 
# 
# newdata.test.Elev <- function(data=NewData$NARS_all){
#   ks.test(x=data$ELEVATION[data$NewStatus=="Done"], y=data$ELEVATION[!data$NewStatus=="Done"]) 
# }
# newdata.test.Elev(data=NewData$NARS_all)
# 
# 
# ####  AREA_HA  ####
# sum(is.na(NARS_all$logArea)) # 0/2664
# 
# # Done vs. rest
# NARS_all$StatusPlot <- NARS_all$NewStatus
# NARS_all$StatusPlot[NARS_all$StatusPlot=="Ready" | NARS_all$StatusPlot=="Rest"] <- "Ready/Rest"
# NARS_all  %>% ggplot(aes(x=StatusPlot, y=logArea)) + geom_violin(aes(fill=StatusPlot)) 
# NARS_all  %>% ggplot(aes(x=Status3, y=logArea)) + geom_violin(aes(fill=Status3)) 
# 
# ks.test(x=NARS_all$logArea[NARS_all$NewStatus=="Done"], y=NARS_all$logArea[!NARS_all$NewStatus=="Done"]) 
# mean(NARS_all$logArea[NARS_all$NewStatus=="Done"]); mean(NARS_all$logArea[!NARS_all$NewStatus=="Done"])
# median(NARS_all$logArea[NARS_all$NewStatus=="Done"]); median(NARS_all$logArea[!NARS_all$NewStatus=="Done"])
# median(NARS_all$Lake_Area_HA[NARS_all$NewStatus=="Done"]); median(NARS_all$Lake_Area_HA[!NARS_all$NewStatus=="Done"])
# 
# 
# # Original data
# ks.test(x=NARS_all$logArea[NARS_all$Status=="Done"], y=NARS_all$logArea[!NARS_all$Status=="Done"]) 
# 
# # Area different
# 
# range(exp(NARS_all$logArea[NARS_all$NewStatus=="Done"]))
# 
# 
# ####  ELEVATION  ####
# sum(is.na(NARS_all$ELEVATION)) # 0/2664
# 
# 
# # Done vs. rest
# NARS_all$StatusPlot <- NARS_all$NewStatus
# NARS_all$StatusPlot[NARS_all$StatusPlot=="Ready" | NARS_all$StatusPlot=="Rest"] <- "Ready/Rest"
# NARS_all  %>% ggplot(aes(x=StatusPlot, y=ELEVATION)) + geom_violin(aes(fill=StatusPlot)) 
# #NARS_all  %>% ggplot(aes(x=Status3, y=ELEVATION)) + geom_violin(aes(fill=Status3)) 
# 
# ks.test(x=NARS_all$ELEVATION[NARS_all$NewStatus=="Done"], y=NARS_all$ELEVATION[!NARS_all$NewStatus=="Done"]) 
# mean(NARS_all$ELEVATION[NARS_all$NewStatus=="Done"]); mean(NARS_all$ELEVATION[!NARS_all$NewStatus=="Done"])
# median(NARS_all$ELEVATION[NARS_all$NewStatus=="Done"]); median(NARS_all$ELEVATION[!NARS_all$NewStatus=="Done"])
# 
# # Original data
# ks.test(x=NARS_all$ELEVATION[NARS_all$Status=="Done"], y=NARS_all$ELEVATION[!NARS_all$Status=="Done"]) 
# 
# # ELEVATION different
# 
# # p-value = 0.01105




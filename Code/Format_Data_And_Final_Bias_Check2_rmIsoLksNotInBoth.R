# Check if distributions still non-significantly different after removing 3 lakes that don't have both NARS and LakeCat data from isotope set

# All lakes summary - 1268 lakes
Data_ALL <- read.csv("Formatted_Data/AllLakes_AllVariables_final_2022-09-28.csv")
# Data_ALL <- read.csv("Formatted_Data/AllLakes_AllVariables_final_2021-05-19.csv")

Data_LksInBoth <- read.csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_2022-09-28.csv")
# Data_LksInBoth <- read.csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_2021-05-19.csv")
# 1112 lakes

### Lakes with isotopes summary
Data_iso <- Data_ALL %>% filter(!is.na(d202_Avg)) # 413
Data_iso_missing <- Data_iso %>% filter(!NLA12_ID %in% Data_LksInBoth$NLA12_ID)


# Remove 3 lakes that aren't in both and see if tests still hold
# Data_ALL <- Data_ALL %>% filter(!NLA12_ID %in% Data_iso_missing$NLA12_ID)
# Don't remove just change their status
Data_ALL$Status[Data_ALL$NLA12_ID %in% Data_iso_missing$NLA12_ID] <- "Not Done"

# Final checks of variables
sum(!is.na(Data_ALL$LOI_PERCENT )) # 1092
ks.test(x=Data_ALL$LOI_PERCENT[Data_ALL$Status=="Done"], y=Data_ALL$LOI_PERCENT[!Data_ALL$Status=="Done"]) # 0.1771

sum(!is.na(Data_ALL$Lake_Area_HA )) # 1124
ks.test(x=Data_ALL$Lake_Area_HA[Data_ALL$Status=="Done"], y=Data_ALL$Lake_Area_HA[!Data_ALL$Status=="Done"]) # 0.7709

sum(!is.na(Data_ALL$ELEVATION )) # 1124
ks.test(x=Data_ALL$ELEVATION[Data_ALL$Status=="Done"], y=Data_ALL$ELEVATION[!Data_ALL$Status=="Done"]) # 0.09791

sum(!is.na(Data_ALL$Site_chla_ug_L )) # 1123
ks.test(x=Data_ALL$Site_chla_ug_L[Data_ALL$Status=="Done"], y=Data_ALL$Site_chla_ug_L[!Data_ALL$Status=="Done"]) # 0.2517

sum(!is.na(Data_ALL$CHLORIDE_RESULT_mg_L )) # 1034
ks.test(x=Data_ALL$CHLORIDE_RESULT_mg_L[Data_ALL$Status=="Done"], y=Data_ALL$CHLORIDE_RESULT_mg_L[!Data_ALL$Status=="Done"]) # 0.1126

sum(!is.na(Data_ALL$Min_OXYGEN_mg_L )) # 1087
ks.test(x=Data_ALL$Min_OXYGEN_mg_L[Data_ALL$Status=="Done"], y=Data_ALL$Min_OXYGEN_mg_L[!Data_ALL$Status=="Done"]) # 0.5154

sum(!is.na(Data_ALL$Particle_Hg_HgPConc_ng_m3 )) # 1040
ks.test(x=Data_ALL$Particle_Hg_HgPConc_ng_m3[Data_ALL$Status=="Done"], y=Data_ALL$Particle_Hg_HgPConc_ng_m3[!Data_ALL$Status=="Done"]) # 0.1415

sum(!is.na(Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s )) # 1040
ks.test(x=Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[Data_ALL$Status=="Done"], y=Data_ALL$WetLossLS_Loss_of_soluble_species_in_large_scale_precipitation_kg_s[!Data_ALL$Status=="Done"]) # 0.04373

sum(!is.na(Data_ALL$Omernik_II )) # 1124
chisq.test(table(Data_ALL$Omernik_II, Data_ALL$Status)) # 0.08243

sum(!is.na(Data_ALL$Fe2O3Ws )) # 1254
ks.test(x=Data_ALL$Fe2O3Ws[Data_ALL$Status=="Done"], y=Data_ALL$Fe2O3Ws[!Data_ALL$Status=="Done"]) # 0.6705


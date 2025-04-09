library(tidyverse)

dir.create("Tables", showWarnings = F)

# Plot Dry wt, LOI, MHg, THg, %MHg
# Plot together to understand relationships 
# ID drivers of each

# All lakes summary - 1268 lakes
Data_ALL <- read.csv("Formatted_Data/AllLakes_AllVariables_final_2021-05-19.csv")
str(Data_ALL)
# Calculate THg/LOI and Percent SMHG
Data_ALL$Percent_SMHG <- Data_ALL$SMHG_ng_g/Data_ALL$STHG_ng_g
Data_ALL$STHG_LOI_ratio <- Data_ALL$STHG_ng_g/Data_ALL$LOI_PERCENT

NARS_Dat <- read.csv("Formatted_Data/NARS_final_2021-05-19.csv")

LakeCat_Dat <- read.csv("Formatted_Data/LakeCat_final_2021-05-19.csv")

names(Data_ALL) %in% colnames(NARS_Dat)

Var_df <- data.frame(Variable=names(Data_ALL))
Var_df$LakeCat <- Var_df$Variable %in% colnames(LakeCat_Dat)
Var_df$NARS <- Var_df$Variable %in% colnames(NARS_Dat)

Var_df$Source <- NA
Var_df$Source[Var_df$LakeCat==TRUE & Var_df$NARS==FALSE] <- "LakeCat"
Var_df$Source[Var_df$LakeCat==FALSE & Var_df$NARS==TRUE] <- "NARS"



Var_df$Type <- NA
Var_df$Domain <- NA
Var_df$NumNA_Of1268 <- NA
Var_df$Num0_Of1268 <- NA
Var_df$NumMostFreq_Of1268 <- NA

Var_df$SD_Nuniq <- NA


for(i in 5:nrow(Var_df)){
 Var_df$Type[i] <- typeof(Data_ALL[,i])

 if(typeof(Data_ALL[,i])=="character"){
   vect.i <- Data_ALL[,i]
   Vals <- c(unique(vect.i[!is.na(vect.i)]),NA,NA,NA,NA)[1:5]
   Var_df$Domain[i] <- paste0(Vals, collapse=",")
   Var_df$SD_Nuniq[i] <- length(unique(vect.i[!is.na(vect.i)]))
 } else{
   Vals <- c(min(Data_ALL[,i], na.rm = T), median(Data_ALL[,i], na.rm = T), max(Data_ALL[,i], na.rm = T))
   Vals <- round(quantile(Data_ALL[,i], na.rm=T, probs = seq(0, 1, 0.25)),2)
   Var_df$Domain[i] <- paste0(Vals, collapse=",")
   Var_df$SD_Nuniq[i] <- sd(Data_ALL[,i], na.rm = T)
   
 }
 
 Var_df$NumNA_Of1268[i] <- sum(is.na(Data_ALL[,i]))
 Var_df$Num0_Of1268[i] <- sum(Data_ALL[,i]==0, na.rm = T)
 Var_df$NumMostFreq_Of1268[i] <- max(table(Data_ALL[,i]))

}

Var_df$Percent_NA <- round(Var_df$NumNA_Of1268/1268,4)
Var_df$Percent_0 <- round(Var_df$Num0_Of1268/1268,4)
Var_df$Percent_MostFreq <- round(Var_df$NumMostFreq_Of1268/1268,4)

Var_df$Combine_Code <- ""
Var_df$Remove <- ""
Var_df$THG_predictor <- ""

Var_df <- Var_df %>% dplyr::select(-LakeCat, -NARS) %>% relocate(Percent_NA, .after = NumNA_Of1268) %>% relocate(Percent_0, .after = Num0_Of1268) %>% relocate(Percent_MostFreq, .after = NumMostFreq_Of1268)

write.csv(Var_df, "Tables/Variable_summary_AllLakes_AllVariables.csv", row.names = F)



### Lakes in both NARS and LakeCat summary
Data_LksInBoth <- read.csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_2021-05-19.csv")
# 1112 lakes

# Calculate THg/LOI and Percent SMHG
Data_LksInBoth$Percent_SMHG <- Data_LksInBoth$SMHG_ng_g/Data_LksInBoth$STHG_ng_g
Data_LksInBoth$STHG_LOI_ratio <- Data_LksInBoth$STHG_ng_g/Data_LksInBoth$LOI_PERCENT


Var_df_Both <- data.frame(Variable=names(Data_LksInBoth))
Var_df_Both$LakeCat <- Var_df_Both$Variable %in% colnames(LakeCat_Dat)
Var_df_Both$NARS <- Var_df_Both$Variable %in% colnames(NARS_Dat)

Var_df_Both$Source <- NA
Var_df_Both$Source[Var_df_Both$LakeCat==TRUE & Var_df_Both$NARS==FALSE] <- "LakeCat"
Var_df_Both$Source[Var_df_Both$LakeCat==FALSE & Var_df_Both$NARS==TRUE] <- "NARS"



Var_df_Both$Type <- NA
Var_df_Both$Domain <- NA
Var_df_Both$NumNA_Of1112 <- NA
Var_df_Both$Num0_Of1112 <- NA
Var_df_Both$NumMostFreq_Of1112 <- NA
Var_df_Both$SD_Nuniq <- NA

for(i in 5:nrow(Var_df_Both)){
  Var_df_Both$Type[i] <- typeof(Data_LksInBoth[,i])
  
  if(typeof(Data_LksInBoth[,i])=="character"){
    vect.i <- Data_LksInBoth[,i]
    Vals <- c(unique(vect.i[!is.na(vect.i)]),NA,NA,NA,NA)[1:5]
    Var_df_Both$Domain[i] <- paste0(Vals, collapse=",")
    Var_df_Both$SD_Nuniq[i] <- length(unique(vect.i[!is.na(vect.i)]))
  } else{
    Vals <- c(min(Data_LksInBoth[,i], na.rm = T), median(Data_LksInBoth[,i], na.rm = T), max(Data_LksInBoth[,i], na.rm = T))
    Vals <- round(quantile(Data_LksInBoth[,i], na.rm=T, probs = seq(0, 1, 0.25)),2)
    Var_df_Both$Domain[i] <- paste0(Vals, collapse=",")
    Var_df_Both$SD_Nuniq[i] <- sd(Data_LksInBoth[,i], na.rm = T)
  }
  
  Var_df_Both$NumNA_Of1112[i] <- sum(is.na(Data_LksInBoth[,i]))
  Var_df_Both$Num0_Of1112[i] <- sum(Data_LksInBoth[,i]==0, na.rm = T)
  Var_df_Both$NumMostFreq_Of1112[i] <- max(table(Data_LksInBoth[,i]))
  
}

Var_df_Both$Percent_NA <- round(Var_df_Both$NumNA_Of1112/1112,4)
Var_df_Both$Percent_0 <- round(Var_df_Both$Num0_Of1112/1112,4)
Var_df_Both$Percent_MostFreq <- round(Var_df_Both$NumMostFreq_Of1112/1112,4)

Var_df_Both$Combine_Code <- ""
Var_df_Both$Remove <- ""
Var_df_Both$THG_predictor <- ""

Var_df_Both <- Var_df_Both %>% dplyr::select(-LakeCat, -NARS) %>% relocate(Percent_NA, .after = NumNA_Of1112) %>% relocate(Percent_0, .after = Num0_Of1112) %>% relocate(Percent_MostFreq, .after = NumMostFreq_Of1112)

write.csv(Var_df_Both, "Tables/Variable_summary_LakesInBoth_AllVariables.csv", row.names = F)








### Lakes with isotopes summary
Data_iso <- Data_ALL %>% filter(!is.na(d202_Avg)) # 413
sum(!Data_iso$NLA12_ID %in% Data_LksInBoth$NLA12_ID)
Data_iso$Status

sum(is.na(Data_iso$STHG_LOI_ratio))
sum(is.na(Data_iso$SMHG_ng_g))

Data_iso_missing <- Data_iso %>% filter(!NLA12_ID %in% Data_LksInBoth$NLA12_ID)
# Note: 3 lakes with isotope data are not in both NARS and LakeCat

# Should remove these from iso set if want to use both datasets. Note that distribution alikeness was checked again in Format_Data_And_Final_Bias_Check2_rmIsoLksNotInBoth.R and still non-significantly different




## Lakes in both NARS and LakeCat summary and HAVE LOI, MHg, or THg
Data_miss_response <- Data_LksInBoth %>% filter(is.na(STHG_ng_g) | is.na(SMHG_ng_g) | is.na(LOI_PERCENT)) # 413

Data_LksInBoth_wResp <- Data_LksInBoth %>% filter(!NLA12_ID %in% Data_miss_response$NLA12_ID)

Var_df_Both_Resp <- data.frame(Variable=names(Data_LksInBoth_wResp))
Var_df_Both_Resp$LakeCat <- Var_df_Both_Resp$Variable %in% colnames(LakeCat_Dat)
Var_df_Both_Resp$NARS <- Var_df_Both_Resp$Variable %in% colnames(NARS_Dat)

Var_df_Both_Resp$Source <- NA
Var_df_Both_Resp$Source[Var_df_Both_Resp$LakeCat==TRUE & Var_df_Both_Resp$NARS==FALSE] <- "LakeCat"
Var_df_Both_Resp$Source[Var_df_Both_Resp$LakeCat==FALSE & Var_df_Both_Resp$NARS==TRUE] <- "NARS"



Var_df_Both_Resp$Type <- NA
Var_df_Both_Resp$Domain <- NA
Var_df_Both_Resp$NumNA_Of1076 <- NA
Var_df_Both_Resp$Num0_Of1076 <- NA
Var_df_Both_Resp$NumMostFreq_Of1076 <- NA
Var_df_Both_Resp$SD_Nuniq <- NA

for(i in 5:nrow(Var_df_Both_Resp)){
  Var_df_Both_Resp$Type[i] <- typeof(Data_LksInBoth_wResp[,i])
  
  if(typeof(Data_LksInBoth_wResp[,i])=="character"){
    vect.i <- Data_LksInBoth_wResp[,i]
    Vals <- c(unique(vect.i[!is.na(vect.i)]),NA,NA,NA,NA)[1:5]
    Var_df_Both_Resp$Domain[i] <- paste0(Vals, collapse=",")
    Var_df_Both_Resp$SD_Nuniq[i] <- length(unique(vect.i[!is.na(vect.i)]))
  } else{
    Vals <- c(min(Data_LksInBoth_wResp[,i], na.rm = T), median(Data_LksInBoth_wResp[,i], na.rm = T), max(Data_LksInBoth_wResp[,i], na.rm = T))
    Vals <- round(quantile(Data_LksInBoth_wResp[,i], na.rm=T, probs = seq(0, 1, 0.25)),2)
    Var_df_Both_Resp$Domain[i] <- paste0(Vals, collapse=",")
    Var_df_Both_Resp$SD_Nuniq[i] <- sd(Data_LksInBoth_wResp[,i], na.rm = T)
  }
  
  Var_df_Both_Resp$NumNA_Of1076[i] <- sum(is.na(Data_LksInBoth_wResp[,i]))
  Var_df_Both_Resp$Num0_Of1076[i] <- sum(Data_LksInBoth_wResp[,i]==0, na.rm = T)
  Var_df_Both_Resp$NumMostFreq_Of1076[i] <- max(table(Data_LksInBoth_wResp[,i]))
  
}

Var_df_Both_Resp$Percent_NA <- round(Var_df_Both_Resp$NumNA_Of1076/1076,4)
Var_df_Both_Resp$Percent_0 <- round(Var_df_Both_Resp$Num0_Of1076/1076,4)
Var_df_Both_Resp$Percent_MostFreq <- round(Var_df_Both_Resp$NumMostFreq_Of1076/1076,4)

Var_df_Both_Resp$Combine_Code <- ""
Var_df_Both_Resp$Remove <- ""
Var_df_Both_Resp$THG_predictor <- ""

Var_df_Both_Resp <- Var_df_Both_Resp %>% dplyr::select(-LakeCat, -NARS) %>% relocate(Percent_NA, .after = NumNA_Of1076) %>% relocate(Percent_0, .after = Num0_Of1076) %>% relocate(Percent_MostFreq, .after = NumMostFreq_Of1076)

write.csv(Var_df_Both_Resp, "Tables/Variable_summary_LakesInBoth_wResp_AllVariables.csv", row.names = F)





#### Plots
plot(Fe2O3Ws~Fe2O3Cat, data=Data_LksInBoth)
plot(HydrlCondWs~HydrlCondCat, data=Data_LksInBoth)

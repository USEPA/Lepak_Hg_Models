library(tidyverse)
library(readxl)
library(stringr)

# o	Implement both random forest and elastic net analyses for THg and %MeHg
#   	Identify optimal predictor subsets and relative importance
#   	Assess predictive ability of reduced models
#   	Quantify/visualize predictor-response relationships and interactions
# o	Implement similar analysis for Hg isotopes
#   	Identify optimal predictor subsets for multivariate isotope response
#   	Understand influence/importance of predictors on each individual isotope


# R: randomForest, cforest, randomForestSRC for MVRF
# permimp 

# R: glmnet

# MV:	Identify which/how variable importance metrics will work with the multivariate approaches. Will likely want some aggregate measure of importance across all responses, as well as importance/influence of predictors on each individual response.
# Correlated predictors - functions for permuting groups of correlated preds, or conditional var impt (permimp)

# All lakes and data combined - 1268
# Data_ALL <- read_csv("Formatted_Data/AllLakes_AllVariables_final_2022-09-28.csv")


# All lakes in both sets - 1112
Data <- read_csv("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_2022-09-28.csv")



# Acidity condition class marked both for removal and THg predictor - marked as ID to retain for info
# Note to use pH_result for isotope model pH



var_list <- read_excel("Tables/Variable_summary_LakesInBoth_AllVariables_113022.xlsx")
var_list <- var_list[,c(1:15,17)] %>% dplyr::select(-Remove)
names(var_list)

ID_vars <- var_list %>% filter(!is.na(ID))
Resp_vars <- var_list %>% filter(!is.na(Response_var))
THg_preds <- var_list %>% filter(!is.na(THG_predictor))
Extra_Iso_preds <- var_list %>% filter(!is.na(Iso_pred))
Remove_vars <-  var_list %>% filter(!is.na(Ryan_remove))

# Check all variables fall into one category
var_list %>% filter(is.na(ID)&is.na(Response_var)&is.na(THG_predictor)&is.na(Iso_pred)&is.na(Ryan_remove))
  


# Average historical data <= 2012
NonCombine_preds <- THg_preds %>% filter(is.na(Combine_Code))

Combine_preds <- THg_preds %>% filter(!is.na(Combine_Code))
length(unique(Combine_preds$Combine_Code)) # 37 predictors to combine

# New variable name will be the Variable name with year removed (all cases are combining multiple years)
Combine_preds$New_Variable <- paste0(gsub('[0-9]+', '', Combine_preds$Variable), "_Mean")
paste0(gsub('[0-9]+', '', Combine_preds$Variable), "_Mean")

# Add new aggregated variables to Data
newvars <- unique(Combine_preds$New_Variable)
for (i in 1:length(newvars)){
  combine_cols_i <- Combine_preds %>% filter(New_Variable==newvars[i]) %>% pull(Variable)
  
  dat_i <- Data %>% dplyr::select(combine_cols_i)
  
  Data[paste0(newvars[i])] <- rowMeans(dat_i, na.rm=TRUE)
  
}


NewCombined_preds <- Combine_preds
NewCombined_preds$Variable <- NewCombined_preds$New_Variable 
NewCombined_preds <- NewCombined_preds %>% dplyr::select(-New_Variable)
NewCombined_preds <- NewCombined_preds[!duplicated(NewCombined_preds$Variable),]

NewCombined_preds$Definition <- paste0("Multi-Year Mean (disregard year in following description): ", NewCombined_preds$Definition)


New_THg_preds <- rbind(NonCombine_preds, NewCombined_preds) # 194 - 75 fewer preds now




# Actually maybe need step here to add together predictors a priori

# Extra predictors to create by summing

# Land use predictors
# Forest = Deciduous + Evergreen + Mixed
# Wetlands = Woody wetlands + emergent herbaceous wetlands
# Low development = Urban open + Urban low
# High development = Urban med + Urban high

# Just create these on the fly in code and replace rows in variable data frame with summed versions

# NLCD_preds <- New_THg_preds %>% filter( str_detect(New_THg_preds$Definition, "NLCD") )


# High/Low Development
dat_devlowC <- Data %>% dplyr::select(PctUrbLoCat_Mean, PctUrbOpCat_Mean)
dat_devhighC <- Data %>% dplyr::select(PctUrbHiCat_Mean,  PctUrbMdCat_Mean)

Data$SumDevelopedLowCat <- rowSums(dat_devlowC, na.rm=FALSE) 
Data$SumDevelopedHighCat <- rowSums(dat_devhighC, na.rm=FALSE) 

dat_devlowW <- Data %>% dplyr::select(PctUrbLoWs_Mean, PctUrbOpWs_Mean)
dat_devhighW <- Data %>% dplyr::select(PctUrbHiWs_Mean,  PctUrbMdWs_Mean)

Data$SumDevelopedLowWs <- rowSums(dat_devlowW, na.rm=FALSE) 
Data$SumDevelopedHighWs <- rowSums(dat_devhighW, na.rm=FALSE) 

# Remove PctUrbOpCat_Mean, PctUrbOpWs_Mean
New_THg_preds <- New_THg_preds %>% filter(!Variable %in% c("PctUrbOpCat_Mean", "PctUrbOpWs_Mean"))

# Replace PctUrbLoCat_Mean  with SumDevelopedLowCat
New_THg_preds$Variable[New_THg_preds$Variable=="PctUrbLoCat_Mean"] <- "SumDevelopedLowCat"
New_THg_preds$Definition[New_THg_preds$Variable=="SumDevelopedLowCat"] <- "Sum of PctUrbOpCat_Mean and PctUrbLoCat_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumDevelopedLowCat"] <- NA

# Replace PctUrbLoWs_Mean  with SumDevelopedLowWs
New_THg_preds$Variable[New_THg_preds$Variable=="PctUrbLoWs_Mean"] <- "SumDevelopedLowWs"
New_THg_preds$Definition[New_THg_preds$Variable=="SumDevelopedLowWs"] <- "Sum of PctUrbOpWs_Mean and PctUrbLoWs_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumDevelopedLowWs"] <- NA

# Remove PctUrbMdCat_Mean, PctUrbMdWs_Mean
New_THg_preds <- New_THg_preds %>% filter(!Variable %in% c("PctUrbMdCat_Mean", "PctUrbMdWs_Mean"))

# Replace PctUrbHiCat_Mean  with SumDevelopedHighCat
New_THg_preds$Variable[New_THg_preds$Variable=="PctUrbHiCat_Mean"] <- "SumDevelopedHighCat"
New_THg_preds$Definition[New_THg_preds$Variable=="SumDevelopedHighCat"] <- "Sum of PctUrbMdCat_Mean and PctUrbHiCat_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumDevelopedHighCat"] <- NA

# Replace PctUrbHiWs_Mean  with SumDevelopedHighWs
New_THg_preds$Variable[New_THg_preds$Variable=="PctUrbHiWs_Mean"] <- "SumDevelopedHighWs"
New_THg_preds$Definition[New_THg_preds$Variable=="SumDevelopedHighWs"] <- "Sum of PctUrbMdWs_Mean and PctUrbHiWs_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumDevelopedHighWs"] <- NA




# Forests
dat_forC <- Data %>% dplyr::select(PctConifCat_Mean, PctDecidCat_Mean, PctMxFstCat_Mean)
Data$SumForestCat <- rowSums(dat_forC, na.rm=FALSE)    # replace other forest variables

dat_forW <- Data %>% dplyr::select(PctConifWs_Mean, PctDecidWs_Mean, PctMxFstWs_Mean)
Data$SumForestWs <- rowSums(dat_forW, na.rm=FALSE)    # replace other forest variables


# Remove PctDecidCat_Mean, PctDecidWs_Mean, PctMxFstCat_Mean, PctMxFstWs_Mean
New_THg_preds <- New_THg_preds %>% filter(!Variable %in% c("PctDecidCat_Mean", "PctDecidWs_Mean", "PctMxFstCat_Mean", "PctMxFstWs_Mean"))

# Replace PctConifCat_Mean  with SumForestCat
New_THg_preds$Variable[New_THg_preds$Variable=="PctConifCat_Mean"] <- "SumForestCat"
New_THg_preds$Definition[New_THg_preds$Variable=="SumForestCat"] <- "Sum of PctConifCat_Mean, PctDecidCat_Mean, and PctMxFstCat_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumForestCat"] <- NA

# Replace PctConifWs_Mean  with SumForestWs
New_THg_preds$Variable[New_THg_preds$Variable=="PctConifWs_Mean"] <- "SumForestWs"
New_THg_preds$Definition[New_THg_preds$Variable=="SumForestWs"] <- "Sum of PctConifWs_Mean, PctDecidWs_Mean, and PctMxFstWs_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumForestWs"] <- NA




# Wetlands
dat_wetlandC <- Data %>% dplyr::select(PctHbWetCat_Mean, PctWdWetCat_Mean)
Data$SumWetlandsCat <- rowSums(dat_wetlandC, na.rm=FALSE)  # replace other wetland  variables

dat_wetlandW <- Data %>% dplyr::select(PctHbWetWs_Mean, PctWdWetWs_Mean)
Data$SumWetlandsWs <- rowSums(dat_wetlandW, na.rm=FALSE)  # replace other wetland  variables

# Remove PctWdWetCat_Mean, PctWdWetWs_Mean
New_THg_preds <- New_THg_preds %>% filter(!Variable %in% c("PctWdWetCat_Mean", "PctWdWetWs_Mean"))

# Replace PctHbWetCat_Mean  with SumWetlandsCat
New_THg_preds$Variable[New_THg_preds$Variable=="PctHbWetCat_Mean"] <- "SumWetlandsCat"
New_THg_preds$Definition[New_THg_preds$Variable=="SumWetlandsCat"] <- "Sum of PctHbWetCat_Mean and PctWdWetCat_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumWetlandsCat"] <- NA

# Replace PctHbWetWs_Mean  with SumWetlandsWs
New_THg_preds$Variable[New_THg_preds$Variable=="PctHbWetWs_Mean"] <- "SumWetlandsWs"
New_THg_preds$Definition[New_THg_preds$Variable=="SumWetlandsWs"] <- "Sum of PctHbWetWs_Mean and PctWdWetWs_Mean (NLCD means)"
New_THg_preds$`Original Variable Name`[New_THg_preds$Variable=="SumWetlandsWs"] <- NA





# Now test for correlations for catchment vs. watershed vars

CatWs_preds <- New_THg_preds %>% filter(!is.na(Both_Cat_Ws))  %>% arrange(Both_Cat_Ws) # 126
NonCatWs_preds <- New_THg_preds %>% filter(is.na(Both_Cat_Ws)) # 58


# Test correlations and remove Ws predictors if correlated
bothvars <- unique(CatWs_preds$Both_Cat_Ws)

Rm_Ws_Vars <- NA
CatWs_preds_Filt <- CatWs_preds

for (i in 1:length(bothvars)){
  both_cols_i <- CatWs_preds %>% filter(Both_Cat_Ws==bothvars[i]) %>% pull(Variable)
  both_cols_i <- sort(both_cols_i)
  
  dat_i <- Data %>% dplyr::select(all_of(both_cols_i))
  
  cor_i <- cor(dat_i, use = "complete.obs")[1,2]
  
  if(cor_i > .7){
    Rm_Ws_Vars <- c(Rm_Ws_Vars, both_cols_i[2])
    CatWs_preds_Filt <- CatWs_preds_Filt %>% filter(!Variable==both_cols_i[2])
  }
  
}

      
CatWs_preds_Filt$Variable

Retained_Ws <- CatWs_preds_Filt %>% filter( str_detect(CatWs_preds_Filt$Variable, "Ws") ) # only 3



plot(PctOwWs_Mean ~ PctOwCat_Mean, data= Data)
plot(PctIceWs_Mean   ~ PctIceCat_Mean , data= Data)
plot(NPDESDensWs  ~ NPDESDensCat, data= Data)



# Final predictor set:
Final_THg_preds <- rbind(NonCatWs_preds, CatWs_preds_Filt) # 127
write.csv(Final_THg_preds, paste0("Tables/Final_THg_preds_",Sys.Date(),".csv"), row.names = F)


# Write data with newly created predictors
write.csv(Data, paste0("Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_",Sys.Date(),".csv"), row.names = F)

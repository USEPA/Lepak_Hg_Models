# Lepak_Hg_Models

### To do
- What the project does
- Why the project is useful
- How users can get started with the project
- Where users can get help with the project


### Data scripts

#### Check_bias.R – Data/NARS_Hg_isotopes_031321.xlsx and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Select 10 variables to check for similar distributions between lakes with isotopes and the rest
-	Used above datasets to see if had different distributions and to pare down test variables
    -	Subsetted LakeCat data to just lakes in NARS_all after NARS file was edited by Ryan to remove lakes not of interest
-	NARS variables: LOI_PERCENT, logArea, ELEVATION, logChla, logChloride, logMinOXYGEN, Particle_Hg_HgPConc_ng_m3, WetLossLS_kg_s, Omernik_II 
-	LakeCat variables: Fe2O3Ws
-	Justifications to choosing variables are in code
-	Corresponds to table of initial differences in Bias_3-16-21.docx 
    -	Only Omernik II variable differed

#### Sample_lakes.R - Data/NARS_Hg_isotopes_031321.xlsx  and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Similar to above, subsets LakeCat data to lakes in NARS
-	Sample lakes for Omernik II until p>.05 (using alpha=.05 for this single test)
-	Filtered lakes for selection: STHG_ng_g > 30, not missing USGS_ID, not missing Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3
-	Ended up with 28 ‘Ready’ lakes and 8 from the ‘Rest’
-	List of lakes to add in Data/Lakes_To_Add.csv – written 3-16-21
-	Redid comparison of distributions after sampling lakes – corresponds to last table of differences in Bias_3-16-21.docx
-	Not redo-ing this with corrected data files – sampled lakes are what they are based on these files

#### Format_Data_And_Final_Bias_Check.R
-	Adds new isotope data – 36 lakes
    -	Data/032521 NLA Seds.xlsx
-	Uses LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Uses NARS file with 1127 lakes
    -	Data/NARS_Hg_isotopes_060622.xlsx
    -	Renames/shortens NARS variable names
    -	Rename SITE_ID to NLA12_ID to match LakeCat
    -	Remove empty rows – get 1124
-	Uses "Data/Weird_Data/Mismatch_Data_between_NARS_LakeCat_RYAN_CORRECTED.csv" to resolve discrepancies between NARS and LakeCat (see 5-13-21 email)
-	Replaces missing USGS IDs in NARS with LakeCat ID from Weird_Data/Mismatch_ID_between_NARS_LakeCat.csv
-	Replace LOI or dry weight values >100% with NA
-	Does final bias check on 10 vars with corrected values
    -	Corresponds to Bias _9-28-22.docx
    -	Note this does not filter to lakes with data in both LakeCat and NARS, checks variable distributions of all isotope lakes (regardless of predictor availability) against distributions of all other lakes (regardless of predictor availability)
-	Writes NARS_final_Date.csv, LakeCat_final_Date.csv, AllLakes_AllVariables_final_Date.csv, LakesInLakeCatAndNARS_All_Variables_final_Date.csv
    -	Did this way so we have data files of all lakes in NARS and LakeCat separately, plus merged datasets with full join of all lakes vs. full join of only lakes in both datasets, as requested by Ryan for other uses in project (see 5-10-21 NLA help email)

#### Format_Data_And_Final_Bias_Check2_rmIsoLksNotInBoth.R
-	Checked whether lakes with isotopes still have same distribution in 10 vars as rest of data when 3 lakes that aren’t in both NARS and LakeCat are removed
-	Still have non-signif differences in distributions with them removed
-	Uses AllLakes_AllVariables_final_Date.csv and LakesInLakeCatAndNARS_AllVariables_final_Date.csv output from Format_Data_And_Final_Bias_Check.R

#### Generate_Variable_Table.R
-	Generates initial tables for making decisions about predictor variables – since been heavily modified in Excel so doesn’t correspond to this script, but keeping for posterity
-	Uses AllLakes_AllVariables_final_2021-05-19.csv (all lakes, all variables) and LakesInLakeCatAndNARS_AllVariables_final_2021-05-19.csv (lakes that are in both LakeCat and NARS) from Format_Data_And_Final_Bias_Check.R  with these file versions:
    -	Data/032521 NLA Seds.xlsx
    -	Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
    -	Data/NARS_Hg_isotopes_031321.xlsx
-	NARS file has since been corrected so the distributional values may be off for these
-	Summarizes type, domain, # NAs, # 0s for variables in both datasets.
-	Creates codes for combining predictors, removing predictors, and designating as THg predictor for first analyses
-	Writes Tables/Variable_summary_AllLakes_AllVariables.csv (summary including all lakes), Variable_summary_LakesInBoth_AllVariables.csv (summary including only lakes in both NARS and LakeCat), and Variable_summary_LakesInBoth_wResp_AllVariables.csv (lakes in both NARS and LakeCat plus actually have LOI, THg, and MeHg)
-	Using Variable_summary_LakesInBoth_AllVariables.csv (lakes in both datasets)
    -	Has been edited manually and dated - final version is Variable_summary_LakesInBoth_AllVariables_113022.xlsx
    -	Excel files now have columns that don’t correspond to original output table
-	Not rerunning this with updated data, even though variable characteristics may shift. Just using this file now for the indicator variables.

#### Select_predictors.R
-	Uses coding system in “Variable_summary_LakesInBoth_AllVariables_113022.xlsx” to average multi-year predictors, sum certain land use predictors, use correlation to select between catchment vs. watershed variables, and generate final predictor list and data with new variables
    -	Final predictor set: Tables/Final_THg_preds_Date.csv
    -	Data with new variables: Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_Date.csv

#### Impute_NA.R
-	Reads in "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv" created from Select_Predictors.R
-	Recodes 3 ordinal cat variables as numeric and recodes LAKE_ORIGIN12 as binary
-	Creates 90-10 train/test split, stratified sampling by Omernik II
    -	Test set includes only lakes with THg
-	Imputes missing values for predictor variables using missForest
    -	Uses just training data plus lakes with missing THg
    -	Excludes ID, THg, MHg, LOI from imputation
-	Writes imputed training data: "Formatted_Data/THg_MHg_Imputed_Training_Data.csv"
-	Writes imputed test data: "Formatted_Data/THg_MHg_Imputed_Test_Data.csv"
-	Writes which variables were imputed: "Tables/List_Imputed_Training_Preds_THg_MHg.csv"


#### Impute_NA_Iso.R
- Similar to above but for isotope lakes
-	Reads in "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv" created from Select_Predictors.R
    - Creates 90-10 train/test split, stratified sampling by Omernik II
    - Test set includes only lakes with isotope data
- Imputes missing values for predictor variables using missForest
    - Uses iso training data plus lakes with missing THg and lakes without iso data
    - INCLUDES THg, MeHg, LOI in imputation
    - Excludes ID, isotopes from imputation.
    - Possibly add lat/longs
- Writes imputed training data: "Formatted_Data/ISO_Imputed_Training_Data.csv "
- Writes imputed test data: "Formatted_Data/ISO_Imputed_Test_Data.csv"



### RF modeling scripts

#### THg_Models/THg_noLOI_RF.R
-	log(THg) model with RFE – exclude LOI as predictor

#### THg_Models/THg_RF.R
-	log(THg) model with RFE – include LOI as predictor

#### THg_Models/THgLOI_RF.R
-	log(THG/LOI) model with RFE

#### THg_Models/THg_RF_CV_Subset_Selection.R
-	log(THg) model with subset selection – include LOI as predictor
- Uses RFE output from THg_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs

#### THg_Models/THgLOI_RF_CV_Subset_Selection.R
-	log(THG/LOI) model with subset selection
- Uses RFE output from THgLOI_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs


#### MeHg_Models/MeHg_wTHgLOI_RF.R
-	log(MeHg) model with THg and LOI as predictors

#### MeHg_Models/MeHg_noTHgLOI_RF.R
-	log(MeHg) model without THg or LOI

#### MeHg_Models/MeHgTHg_wLOI_RF.R
-	log(MeHg/THg) model with LOI

#### MeHg_Models/MeHgTHg_noLOI_RF.R
-	log(MeHg/THg) model without LOI

#### MeHg_Models/MeHg_wTHgLOI_RF_CV_Subset_Selection.R
-	log(MeHg) model with THg and LOI as predictors with subset selection
- Uses RFE output from MeHg_wTHgLOI_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs

#### MeHg_Models/MeHgTHg_wLOI_RF_CV_Subset_Selection.R
-	log(MeHg/THg) model with LOI with subset selection
- Uses RFE output from MeHgTHg_wLOI_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs


#### LOI_Models/LOI_RF.R
-	log(LOI) model with all predictors (not THg)

#### LOI_Models/LOInoHgPreds_RF.R
-	log(LOI) model excluding 9 GEOS-Chem Hg predictors (and not THg)

#### Compilte_Results/Compile_THg_MeHg_LOI_Model_Results.R
-	Code to compile results from above models and compare top predictors



##### Disclaimer: The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 
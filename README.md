## Lepak_Hg_Models

### <ins>Overview</ins>
- This repository contains code and data to fit total mercury (abbreviated THg in script description below vs. HgT in the manuscript) concentration, methylmercury (MeHg) concentration, loss on ignition (LOI), and multivariate mercury isotope (D199, D200, d202) random forest models as presented in the Lepak et al. (submitted) manuscript, "Generating a mercury isoscape for CONUS lakes to map mercury depositional pathways and understand predictors for mercury and methylmercury concentrations in sediment."
- This study seeks to predict Hg concentrations and Hg isotope values across the contiguous United States (CONUS) to generate novel insights into the utility of Hg isotope values in understanding depositional pathways to lakes.
- Below, the modeling scripts used to fit the random forest models are emphasized and described first. Following the modeling scripts, we describe the preliminary scripts that were used to select lakes to include in the isotope portion of the study, as well as scripts used to select/aggregate predictor data, create train/test splits, and impute missing values. For users wishing only to run/review the models, these latter preliminary scripts can be skipped, and users can use the available files already prepped for running the analyses as described below.
  - For THg, MeHg, and LOI models, prepped files are here:
    - Formatted_Data/THg_MHg_Imputed_Training_Data.csv
    - Formatted_Data/THg_MHg_Imputed_Test_Data.csv
  - For isotope models, prepped files are here:
    - Formatted_Data/ISO_Imputed_Training_Data.csv
    - Formatted_Data/ISO_Imputed_Test_Data.csv
- For help with this code, please contact Kelsey Vitense (vitense.kelsey@epa.gov or kelsey.vitense@gmail.com)



### <ins>RF modeling scripts in **Code** directory </ins>
- Each response below has two primary scripts: 
  - The first script (ending in **"_RF"** or **"_MVRF"** does parameter tuning on the full model and conducts recursive feature elimination (RFE) to determine the order in which predictors should be removed from the model based on permutation variable importance.
  - The second script (ending in **"_CV_Subset_Selection"**) uses the RFE order from the above script to calculate cross-validation (CV) error at each RFE iteration to determine the optimal subset using the 1-SE rule, then refits this final model and calculates error estimates. This script also creates predicted vs. observed plots and spatial residual plots, in addition to conducting Mantel spatial autocorrelation tests. For THg, MeHg, and LOI, code to create partial dependence plots (PDPs) can also be found in this script; for the isotope models, the PDP plots are created using additional custom scripts and are described below.
- Note that only primary modeling scripts relevant for the manuscript are described below. Some directories contain an 'Extra' directory with additional models fit during development and are not described.


#### THg_Models/THg_RF.R
-	Total mercury concentration model with response modeled on log<sub>10</sub>-scale: $log_{10}(THg)$ 
- Includes GEOS-Chem, LakeCat, and NLA predictor data, plus LOI
- Conducts recursive feature elimination (RFE) 

#### THg_Models/THg_RF_CV_Subset_Selection.R
-	$log_{10}(THg)$ model with subset selection
- Uses RFE output from THg_RF.R, estimates CV errors, and selects best subset using 1-SE rule
- Creates error tables, visualizations, and PDPs


#### MeHg_Models/MeHg_wTHgLOI_RF.R
-	Methylmercury concentration model with response modeled on log<sub>10</sub>-scale: $log_{10}(MeHg)$ 
- Includes GEOS-Chem, LakeCat, and NLA predictor data, plus THg and LOI
- Conducts recursive feature elimination (RFE) 

#### MeHg_Models/MeHg_wTHgLOI_RF_CV_Subset_Selection.R
-	$log_{10}(MeHg)$ model with subset selection
- Uses RFE output from MeHg_wTHgLOI_RF.R, estimates CV errors, and selects best subset using 1-SE rule
- Creates error tables, visualizations, and PDPs

#### LOI_Models/LOInoHgPreds_RF.R
-	LOI model with response modeled on log<sub>10</sub>-scale: $log_{10}(LOI)$ 
- Includes LakeCat and NLA predictor data
- Conducts recursive feature elimination (RFE) 


#### LOI_Models/LOInoHgPreds_RF_CV_Subset_Selection.R
-	$log_{10}(LOI)$ model with subset selection
- Uses RFE output from LOInoHgPreds_RF.R, estimates CV errors, and selects best subset using 1-SE rule
- Creates error tables, visualizations, and PDPs



#### Isotope_Models/1_ISO_MVRF.R
- Multivariate isotope model: (D199, D200, d202)
- Includes GEOS-Chem, LakeCat, and NLA predictor data, plus THg, MeHg, and LOI
- Conducts recursive feature elimination (RFE) 

#### Isotope_Models/2_ISO_MVRF_CV_Subset_Selection.R
-	Multivariate isotope model model with subset selection
- Uses RFE output from 1_ISO_MVRF.R, estimates CV errors, and selects best subset using 1-SE rule
- Creates error tables, predicted vs. obs plots, spatial residual plots


#### Isotope_Models/3_Predict_ISO_MVRF_subset.R
- Refits final isotope subset model to all 410 isotope lakes (train+test combined)
- Uses fitted model to predict isotope response for all 1112 lakes
- Writes predictions in **"Model_Output/Iso/Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24.csv"**
  - Note that LOI was unnecessarily divided by 100 an extra time in isotope model fits and in the .csv file above (i.e., the original percentage was divided by 1000 instead of just 100). This is how it should be input into models and for generating PDPs, but for visualizations it was multiplied by 100 to be interpreted as a proportion.
  

#### Isotope_Models/4_IsoPreds_Voronoi.R
- Uses SKATER algorithm to generate spatially constrained clusters of all 1112 lakes based on the 10 predictors in the reduced isotope model
- Creates map of the resulting SKATER clusters
- Writes data with original cluster numbers to: **"Model_Output/Iso/Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"**
- Writes data with reassigned cluster numbers to: **"Model_Output/Iso/Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS_NEWnumbers.csv"**


#### Isotope_Models/5_Custom_PDPs_SKATER_20clust.R
- Creates univariate PDPs (all lakes together and by cluster) and bivariate PDPs


---

## The remaining scripts below were run before the modeling scripts above to prep the data for modeling (first scripts in **Select_Isotope_Lakes**, then scripts in **Model_Prep**)



### <ins>Preliminary scripts used to select isotope lakes</ins>


#### Select_Isotope_Lakes/1_Check_bias.R 
- Uses: Data/NARS_Hg_isotopes_031321.xlsx and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Select 10 variables to check for similar distributions between lakes with isotopes and the rest
-	Uses lakes in both LakeCat and NARS

#### Select_Isotope_Lakes/2_Sample_lakes.R 
- Uses: Data/NARS_Hg_isotopes_031321.xlsx  and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Randomly samples lakes in both LakeCat and NARS until 10 variable distributions are similar
-	List of lakes to add in Data/Lakes_To_Add.csv – written 3-16-21

#### Select_Isotope_Lakes/3_Format_Data_And_Final_Bias_Check.R
-	Adds new isotope data – 36 lakes
    -	Data/032521 NLA Seds.xlsx
-	Uses Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Uses Data/NARS_Hg_isotopes_060622.xlsx
-	Formats data and does final bias check on 10 vars with corrected NARS values
-	Uses "Data/Weird_Data/Mismatch_Data_between_NARS_LakeCat_RYAN_CORRECTED.csv" to resolve discrepancies between NARS and LakeCat
-	Replaces missing USGS IDs in NARS with LakeCat ID from Weird_Data/Mismatch_ID_between_NARS_LakeCat.csv
-	Renames/shortens NARS variable names
-	Writes in **Formatted_Data**: NARS_final_[Date].csv, LakeCat_final_[Date].csv, AllLakes_AllVariables_final_[Date].csv, LakesInLakeCatAndNARS_All_Variables_final_[Date].csv
    -	I.e., data files of all lakes in NARS and LakeCat separately, plus merged datasets with full join of all lakes or only lakes in both datasets, as requested



### <ins>Scripts used to select/aggregate predictor data, create train/test splits, and impute missing values</ins>

#### Model_Prep/1_Generate_Variable_Table.R
-	Generates initial tables to help make decisions about predictor variables 
-	Uses Formatted_Data/AllLakes_AllVariables_final_2021-05-19.csv (all lakes, all variables) and Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_2021-05-19.csv (lakes that are in both LakeCat and NARS) created from Format_Data_And_Final_Bias_Check.R with these file versions:
    -	Data/032521 NLA Seds.xlsx
    -	Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
    -	Data/NARS_Hg_isotopes_031321.xlsx
-	Creates codes for combining predictors, removing predictors, and designating model predictors
-	Initial writes:
    - Tables/Variable_summary_AllLakes_AllVariables.csv (includes all lakes) 
    - Tables/Variable_summary_LakesInBoth_AllVariables.csv (includes only lakes in both NARS and LakeCat)
    - Tables/Variable_summary_LakesInBoth_wResp_AllVariables.csv (includes lakes in both NARS and LakeCat and that have LOI, THg, and MeHg measurements)
-	Coding system ultimately used was created manually starting with Variable_summary_LakesInBoth_AllVariables.csv 
    -	Has been edited manually in Excel several times and dated 
- Final version used is Tables/Variable_summary_LakesInBoth_AllVariables_113022.xlsx
    -	Note there are manually added columns that don’t correspond to original output table

#### Model_Prep/2_Select_predictors.R
-	Uses coding system in “Variable_summary_LakesInBoth_AllVariables_113022.xlsx” to average multi-year predictors, sum certain land use predictors, use correlation to select between catchment vs. watershed variables, and generate final predictor list and data with new variables
-	Writes final predictor set: Tables/Final_THg_preds_Date.csv
-	Writes data with new variables: Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_[Date].csv

#### Model_Prep/3_Impute_NA.R
- Creates train/test splits and imputes missing data for THg, MeHg, and LOI models
-	Uses "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv"
-	Recodes 3 ordinal cat variables as numeric and recodes LAKE_ORIGIN12 as binary
-	Creates 90-10 train/test split, stratified sampling by Omernik II
    -	Test set includes only lakes with THg
-	Imputes missing values for predictor variables using missForest
    -	Excludes ID, THg, MHg, LOI from imputation
-	Writes imputed training data: "Formatted_Data/THg_MHg_Imputed_Training_Data.csv"
-	Writes imputed test data: "Formatted_Data/THg_MHg_Imputed_Test_Data.csv"
-	Writes which variables were imputed: "Tables/List_Imputed_Training_Preds_THg_MHg.csv"


#### Model_Prep/4_Impute_NA_Iso.R
- Similar to above but for isotope lakes
-	Uses "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv"
- Creates 90-10 train/test split, stratified sampling by Omernik II
    - Test set includes only lakes with isotope data
- Imputes missing values for predictor variables using missForest
    - INCLUDES THg, MeHg, LOI in imputation
    - Excludes ID, isotopes from imputation.
- Writes imputed training data: "Formatted_Data/ISO_Imputed_Training_Data.csv "
- Writes imputed test data (subset of below): "Formatted_Data/ISO_Imputed_Test_Data.csv"
- Writes imputed full data: "Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv"







##### Disclaimer: The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 
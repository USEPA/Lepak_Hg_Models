## Lepak_Hg_Models

### <ins>Overview</ins>
- This repository contains code and data to fit total mercury (abbreviated THg in script description below vs. HgT in the manuscript) concentration, methylmercury (MeHg) concentration, loss on ignition (LOI), and multivariate mercury isotope (D199, D200, d202) random forest models as presented in the Lepak et al. (submitted) manuscript "Generating a mercury isoscape for CONUS lakes to map mercury depositional pathways and understand predictors for mercury and methylmercury concentrations in sediment."
- This study seeks to be able to predict Hg concentrations and Hg isotope values across the contiguous United States (CONUS) to generate novel insights into the utility of Hg isotope values in understanding depositional pathways to lakes.
- Below, the modeling scripts used to fit the random forest models are emphasized and described first. Following the modeling scripts, we describe the preliminary scripts that were used to select lakes to include in the isotope portion of the study, as well as scripts used to select/aggregate predictor data, create train/test splits, and impute missing values. For users wishing only to run/review the models, these latter preliminary scripts can be skipped, and users can use the available files already prepped for running the analyses as described below.
  - For THg, MeHg, and LOI models, prepped files are here:
    - Formatted_Data/THg_MHg_Imputed_Training_Data.csv
    - Formatted_Data/THg_MHg_Imputed_Test_Data.csv
  - For isotope models, prepped files are here:
    - Formatted_Data/ISO_Imputed_Training_Data.csv
    - Formatted_Data/ISO_Imputed_Test_Data.csv
- For help with this code, please contact Kelsey Vitense (vitense.kelsey@epa.gov or kelsey.vitense@gmail.com)



### RF modeling scripts
- Each response below generally has two primary scripts: 
  - The first script (ending in **"_RF"** or **"_MVRF"** does parameter tuning on the full model and conducts recursive feature elimination (RFE) to determine the order in which predictors should be removed from the model based on permutation variable importance.
  - The second script (ending in **"_CV_Subset_Selection"**) uses the RFE order from the above script to calculate cross-validation (CV) error at each RFE iteration to determine the optimal subset using the 1-SE rule, then refits this final model and calculates error estimates. This script also creates predicted vs. observed plots and spatial residual plots, plus conducts Mantel spatial autocorrelation tests. For THg, MeHg, and LOI, code to create partial dependence plots (PDPs) can also be found in this script; for the isotope models, the PDP plots are created using additional custom scripts and are described below.
- Note that only primary modeling scripts relevant for the manuscript are described below. Some directories contain an 'Extra' directory with additional models fit during development and are not described below.


#### THg_Models/THg_RF.R
-	Total mercury concentration model with response modeled on log~10~-scale: $log_{10}(THg)$ 
- Includes GEOS-Chem, LakeCat, and NLA predictor data, plus LOI
- Conducts recursive feature elimination (RFE) 

#### THg_Models/THg_RF_CV_Subset_Selection.R
-	$log_{10}(THg)$ model with subset selection
- Uses RFE output from THg_RF.R, estimates CV errors, and selects best subset using 1-SE rule
- Creates error tables, visualizations, and PDPs


#### MeHg_Models/MeHg_wTHgLOI_RF.R
-	log(MeHg) model with THg and LOI as predictors

#### MeHg_Models/MeHg_wTHgLOI_RF_CV_Subset_Selection.R
-	log(MeHg) model with THg and LOI as predictors with subset selection
- Uses RFE output from MeHg_wTHgLOI_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs



#### LOI_Models/LOInoHgPreds_RF.R
-	log(LOI) model excluding 9 GEOS-Chem Hg predictors and THg

#### LOI_Models/LOInoHgPreds_RF_CV_Subset_Selection.R
-	log(LOI) model excluding 9 GEOS-Chem Hg predictors and THg with subset selection
- Uses RFE output from LOInoHgPreds_RF.R, adds CV errors to RFE
- Selects best subset using 1-SE rule and MAE
- Creates error tables, visualizations, PDPs


#### ADD ISOTOPE MODELS AND SCRIPTS





### <ins>Preliminary scripts used to select isotope lakes</ins>

#### Select_Isotope_Lakes/1_Check_bias.R 
- Uses: Data/NARS_Hg_isotopes_031321.xlsx and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Select 10 variables to check for similar distributions between lakes with isotopes and the rest
-	Uses lakes in both LakeCat and NARS

#### Select_Isotope_Lakes/2_Sample_lakes.R 
- Uses: Data/NARS_Hg_isotopes_031321.xlsx  and Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Randomly samples lakes in both LakeCat and NARS until 10 variable distributions are similar
-	Lake criteria for selection: STHG_ng_g > 30, not missing USGS_ID, not missing Gas_Hg_Hg0Conc_ng_m3, Ionic_Hg2Conc_ng_m3, Particle_Hg_HgPConc_ng_m3
-	List of lakes to add in Data/Lakes_To_Add.csv – written 3-16-21

#### Select_Isotope_Lakes/3_Format_Data_And_Final_Bias_Check.R
-	Adds new isotope data – 36 lakes
    -	Data/032521 NLA Seds.xlsx
-	Uses Data/LakeCat_NLA_Hg_isotopes_020421.xlsx
-	Uses Data/NARS_Hg_isotopes_060622.xlsx
-	Formats data and does final bias check on 10 vars with corrected NARS values
    -	Does not filter to lakes with data in both LakeCat and NARS, checks variable distributions of all isotope lakes against distributions of all other lakes regardless of predictor availability
-	Uses "Data/Weird_Data/Mismatch_Data_between_NARS_LakeCat_RYAN_CORRECTED.csv" to resolve discrepancies between NARS and LakeCat
-	Replaces missing USGS IDs in NARS with LakeCat ID from Weird_Data/Mismatch_ID_between_NARS_LakeCat.csv
-	Renames/shortens NARS variable names
-	Writes in /Formatted_Data/: NARS_final_[Date].csv, LakeCat_final_[Date].csv, AllLakes_AllVariables_final_[Date].csv, LakesInLakeCatAndNARS_All_Variables_final_[Date].csv
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
-	Writes data with new variables: Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_[Date\.csv

#### Model_Prep/3_Impute_NA.R
-	Uses "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv" created from Select_Predictors.R
-	Recodes 3 ordinal cat variables as numeric and recodes LAKE_ORIGIN12 as binary
-	Creates 90-10 train/test split, stratified sampling by Omernik II
    -	Test set includes only lakes with THg
-	Imputes missing values for predictor variables using missForest
    -	Uses just training data plus lakes with missing THg
    -	Excludes ID, THg, MHg, LOI from imputation
-	Writes imputed training data: "Formatted_Data/THg_MHg_Imputed_Training_Data.csv"
-	Writes imputed test data: "Formatted_Data/THg_MHg_Imputed_Test_Data.csv"
-	Writes which variables were imputed: "Tables/List_Imputed_Training_Preds_THg_MHg.csv"


#### Model_Prep/4_Impute_NA_Iso.R
- Similar to above but for isotope lakes
-	Uses "Formatted_Data/LakesInLakeCatAndNARS_AllVariables_final_ADDNEWVARS_2023-01-10.csv" created from Select_Predictors.R
    - Creates 90-10 train/test split, stratified sampling by Omernik II
    - Test set includes only lakes with isotope data
- Imputes missing values for predictor variables using missForest
    - Uses iso training data plus lakes with missing THg and lakes without iso data
    - INCLUDES THg, MeHg, LOI in imputation
    - Excludes ID, isotopes from imputation.
- Writes imputed training data: "Formatted_Data/ISO_Imputed_Training_Data.csv "
- Writes imputed test data: "Formatted_Data/ISO_Imputed_Test_Data.csv"







##### Disclaimer: The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 
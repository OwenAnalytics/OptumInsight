Data description of ..\Box Sync\OptumInsight_DataManagement\data_freeze\20190110

**diagInpatient_long.csv**: 

  - Inpatient diagnosis codes in a long format. Inpatient records are identified from medical claims and confinement claims data sets. These consist of medical claims where conf_id != "", and all confinement claims. We restrict our attention to diagnoses after index VTE date and before 2015-10-01, when ICD-10 became effective. 
  - Noticing that the confinement claims are a summary of the medical claims, we remove overlapping diagnosis information in the following way. We identify (a) unique claim(s) using patid, fst_dt, and conf_id. 
    - Case 1: If a claim only appears in confinement data, then it is likely a declined claim by the insurance company; we include this claim in our data. There are 5007 rows.
    - Case 2: If a claim appears both medical and confinement data, then we believe that the confinement claim is a summary of its medical claim(s) counterpart; we include the medical claim(s) in our data because it contains more granular information. There are 311,208 rows.
    - Case 3: If a claim only appears in medical data, I am sorry that I do not understand what this implies; we include the medical claim(s) in our data because having a non-empty conf_id implies they may be inpatient. There are 638,278 rows.
  - The total number of rows is 954,493 with 11 variables: 
```
 [1] "patid"       "index_dt"    "fst_dt"      "charge"     
 [5] "copay"       "pos"         "description" "category"   
 [9] "MedOrConf"   "diag"        "icd9_raw"
```
``MedOrConf`` = "conf" for case 1, "both" for case 2, and "med" for case 3.

``diag`` = "diagX" indicates the primary diagnosis or the other diagnoses.

  - Each row is a unique combination of the 11 variables.


**diagOutpatient_long.csv**:

  - Outpatient diagnosis codes in a long format. Outpatient records are identified from medical claims data. These consist of medical claims where conf_id == "" and fst_dt < 2015-10-01. There are 3645,054 rows.
  - The total number of rows is 3,645,054 with 10 variables:
```
 [1] "patid"       "index_dt"    "fst_dt"      "charge"     
 [5] "copay"       "pos"         "description" "category"   
 [9] "diag"        "icd9_raw"  
```
  - Each row is a unique combination of the 10 variables.
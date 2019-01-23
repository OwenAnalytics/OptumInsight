Data description of ..\Box Sync\OptumInsight_DataManagement\data_freeze\20190110

Note that the data files are not uploaded to GitHub.

--------------------------------------------------------------

**diagInpatient_long.csv**: 

  - Inpatient diagnosis codes in a long format. Inpatient records are identified from medical claims and confinement claims data sets. These consist of medical claims where conf_id != "", and all confinement claims. We restrict our attention to diagnoses after index VTE date and before 2015-10-01, when ICD-10 became effective. 
  - Noticing that the confinement claims are a summary of the medical claims, we remove overlapping diagnosis information in the following way. We identify (a) unique claim(s) using patid, fst_dt, and conf_id. 
    - Case 1: If a claim only appears in confinement data, then it is likely a declined claim by the insurance company; we include this claim in our data. There are 5001 rows.
    - Case 2: If a claim appears both medical and confinement data, then we believe that the confinement claim is a summary of its medical claim(s) counterpart; we include the medical claim(s) in our data because it contains more granular information. There are 444,723 rows.
    - Case 3: If a claim only appears in medical data, I am sorry that I do not understand what this implies; we include the medical claim(s) in our data because having a non-empty conf_id implies they may be inpatient. There are 920,162 rows.
  - The total number of rows is 1,369,886 with 12 variables: 
```
 [1] "patid"        "index_dt"     "conf_id"      "fst_dt"      
 [5] "charge"       "copay"        "pos"          "description" 
 [9] "category"     "MedOrConf"    "diag"         "icd9_raw"    
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


In addition,

- The two data files above are generated from diagData_20181020_freeze.csv, which is in a wide format.
- There are some ICD-10 codes before 2015-10-01 in diagData_20181020_freeze.csv.
  - The inpatient data contains 152 ICD-10 diagnoses with 80 unique ICD-10 codes. They are:
```r
 [1] "I824Z3"  "M8458XA" "T80211A" "E11649"  "S7222XA" "L03116" 
 [7] "C50919"  "I82622"  "I824Y2"  "S065X0D" "J15212"  "G40901" 
[13] "I63232"  "I82402"  "J95811"  "L89153"  "I82612"  "D61810" 
[19] "J95821"  "D61818"  "Z96652"  "Z86711"  "I82512"  "Z85118" 
[25] "T814XXA" "T17898A" "I82220"  "Z86718"  "L03221"  "J15211" 
[31] "I82432"  "L03313"  "Z87891"  "L89152"  "T8172XA" "T17990A"
[37] "I824Z1"  "J45909"  "I82421"  "S72332A" "T402X5A" "G40909" 
[43] "M86651"  "F17200"  "I82813"  "T451X5A" "D72828"  "T82897A"
[49] "I69351"  "Z87440"  "L03115"  "Z85850"  "L89154"  "Z90710" 
[55] "I82409"  "T8389XA" "C50911"  "L03314"  "I82502"  "L89314" 
[61] "Z90721"  "Z85038"  "T83498A" "S12112D" "L89324"  "156400" 
[67] "L02416"  "160000"  "I82413"  "F17210"  "C50912"  "Z79899" 
[73] "H53462"  "T40605A" "I25110"  "M868X8"  "I82401"  "Z96643" 
[79] "C44311"  "Z86010" 
```
  - The outpatient data contains 360 ICD-10 diagnoses with 64 unique ICD-10 codes. They are:
```r
 [1] "I69954"  "I824Z3"  "M8458XA" "T80211A" "S31109S" "E11649" 
 [7] "C50919"  "S7222XA" "L89154"  "S42211D" "I82622"  "I824Y2" 
[13] "J15212"  "G40901"  "I63232"  "I97190"  "S0096XA" "S79109D"
[19] "J95811"  "I824Y9"  "L97529"  "D72829"  "I82621"  "I82612" 
[25] "I82409"  "D61810"  "D61818"  "I82512"  "I82402"  "T814XXA"
[31] "T17898A" "L03221"  "I69354"  "I82432"  "Z87440"  "Z87891" 
[37] "L89152"  "T8172XA" "719418"  "Z86711"  "I69922"  "I824Z1" 
[43] "J45909"  "Z86718"  "S72332A" "F17200"  "D72828"  "I69351" 
[49] "L03115"  "I69998"  "Z85038"  "L03116"  "L03314"  "I82502" 
[55] "Z86011"  "T83498A" "Z85850"  "Z85828"  "F17210"  "T451X5A"
[61] "Z79899"  "I82401"  "Z90710"  "C44311" 
```
  - Among the 80 ICD-10 codes in inpatient data and 64 in outpatient data, 32 of them appears in either but not both data sets. This seems a fairly large amount of overlap.



--------------------------------------------------------------
**procedures_long.csv**:
  - Procedure codes in a long format. These codes are from proc_cd fields in medical claims and facility claims. Because we only focus on diagnoses after index VTE date and before 2015-10-01, it seems more reasonable to focus on procedure codes within the same range of time.
  - The total number of rows is 10,336,645 with 10 variables: 
```
 [1] "patid"       "index_dt"    "fst_dt"      "source"     
 [5] "conf_id"     "pos"         "description" "category"   
 [9] "proc"        "proc_cd"  
```
The `source` variable can be "medical.outpatient", "medical.inpatient", or "facility".
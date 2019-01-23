data_processing.R:
 1. create an outpatient diagnosis data set in long format, comprised of outpatient diagnoses from medical claims.
    - Output: "../diagOutpatient_long.csv"
 2. create an inpatient diagnosis data set in long format, that combines inpatient diagnoses from medical claims and confinement claims. We remove duplicate inpatient claims information from medical and confinement claims. The final data set consists of two parts.
  1) Medical claims that have corresponding patid, fst_dt, and conf_id in confinement claims. We keep the medical claims and remove the corresponding confinement claims, because the former contains more granular information than the latter.
  2) Confinement claims that do not have corresponding patid, fst_dt, and conf_id in medical claims. These confinement claims were rejected claims.
    - Output: "../diagInpatient_long.csv"
 3. Extract ICD-10 codes from the inpatient and outpatient data sets and save them into ICD10_diagInpatient_long.csv and ICD10_diagOutpatient_long.csv for review.
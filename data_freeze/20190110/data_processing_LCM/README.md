The data_LCM.rds file contains a sparse data matrix. Rows are individual patients. Each column is a diagnosis code at 3, 4, or 5-digit level. The entries X_ij = 1 if code j appeared in patient i at least one code j throughout his/her record, and = 0 if code j did not appear at all. Specific patients and codes can be identified from the row names and column names, respectively. Only codes that appeared at least once in the study cohort are included in the matrix. 

The matrix has dimension 11455 x 3626. The first 69 columns are for 3-digit ICD-9 codes, the next 1584 columns are for 4-digit ICD-9 codes, and the rest 1973 columns are for 5-digit ICD-9 codes.
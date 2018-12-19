Run LDA on diagnosis data

data_LDADiagnosis_createDTM: create a document-term matrix of the diagnosis codes
  - diagnosisText_DTM.RData: the generated document-term matrix

data_LDADiagnosis_selectk: select the number of topics in LDA
  - data_LDADiagnosis_selectk_plot.pdf: the plot generated based on three criteria

data_LDADiagnosis: run LDA on diagnosisText_DTM
  - diagnosisLDA.RData: 3 models with k = 2, 10, 20

data_LDADiagnosis_results: result of the three models
  - plot_LDADiagnosis.pdf: 
    1) The most common ICD-9 codes within each topic
    2) Distribution of the most likely topics 
    3) Probability of assignment to the most likely topic
  - plot_topicDistributions.pdf: Posterior topic distribution for individual patients 
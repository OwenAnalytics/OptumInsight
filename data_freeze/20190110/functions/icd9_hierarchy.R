#' Extract the structure of ICD-9 code
#' The basic icd-9 code consists of 3 digits, while some are more specific
#' with 4 or five digits subdivision.
#' E codes and V codes represent external causes of injury and
#'  supplemental classification
#' V codes format: VXX(.XX)
#' E codes format: EXXX(.X)

icd9_3digits <- function(data, x){
  x <- eval(substitute(x), data)
  ifelse(substr(x, 1, 1)=="E",
        substr(x, 1, 4),
        substr(x, 1, 3))
}


icd9_4digits <- function(data, x){
  x <- eval(substitute(x), data)
  ifelse(substr(x, 1, 1)=="E",
   ifelse(nchar(x) < 5, "", substr(x, 1, 5)),
   ifelse(nchar(x) < 4, "", substr(x, 1, 4)))
}

icd9_5digits <- function(data, x){
  x <- eval(substitute(x), data)
  ifelse(substr(x, 1, 1)=="E",
         "",
         ifelse(nchar(x) < 5, "", substr(x, 1, 5)))
}


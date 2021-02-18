
#' Methods of calculation for benchmarking metrics depending on ensemble size
#'
#' A lookup table with benchmerking metric names and ids along with the 
#' approaches that should be taken to obtain metric scores depending on whether 
#' or not the model simulation contains more than one ensemble member or not.
#'
#' @name metric_ensemble_calc
#' @docType data
#' @keywords datasets
#' @format data frame, 
#' \describe{
#'  \item{metric_id}{Benchmarking metric BETYdb id number}
#'  \item{name}{Benchmarking metric BETYdb name}
#'  \item{ensemble_calc}{Metric calculation approach for ensemble size larger than 1}
#'  \item{single_calc}{Metric calculation approach for ensemble size of 1}
#'
"metric_ensemble_calc"
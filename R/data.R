#' Simulated data of ICD-10 codes
#'
#' These are simulated data where the ICD-10 codes and additional variables are all
#' simulated independent of each other. These data do not have any clinical value.
#'
#' @format ## `simulated`
#' A data frame with 20,000 rows and 105 columns:
#' \describe{
#'   \item{X.x}{Patient identifier}
#'   \item{dim_fall_bk_pseudo}{Hospitalisation identifier}
#'   \item{icd20.x}{ICD-10 code ordered by importance (.1 is primary code)}
#'   \item{clinic_admission}{Medical clinic of admission}
#'   \item{clinic_discharge}{Medical clinic of discharge}
#'   \item{vital_state}{If patient is still alive "leben" or deceased "verstorben")}
#'   \item{case_duration}{Number of days a patient was hospitalised}
#'   \item{age_admission}{Age at admission}
#'   \item{death_date}{Death date if the patient is deceased}
#'   \item{gender}{Sex of the patient}
#'   \item{BMI}{BMI at admission}
#'   \item{X_1}{Max number of cases starting with letter X}
#'   \item{X_2}{Mean number of cases starting with letter X}
#'   \item{X_3}{Sum of number of cases starting with letter X}
#' }
#' @source Simulated
"simulated_data"

#' Training data for w2v model
#'
#' These are simulated data where the ICD-10 codes and additional variables are all
#' simulated independent of each other. These data do not have any clinical value.
#'
#' @format ## `training ICD-10 data`
#' A data frame with 20,000 rows and 1 column:
#' \describe{
#'   \item{sentence}{All ICD-10 codes combined per hospitalisation}
#' }
#' @source Simulated
"training_data"

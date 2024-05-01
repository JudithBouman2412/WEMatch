#' Function to perform cox proportional hazard analysis given the output of one
#' of the matching functions
#'
#' @param matched_object result of WEMatch or Match_prop_ICD
#' @param figure should a figure be produced? (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
perform_mortality_analysis <- function( matched_object,
                                        figure = TRUE){

  # data of cases
  matched_exposed <- as.data.frame(matched_object[[2]])

  # data of controls
  matched_cohort <- as.data.frame(matched_object[[1]])

  # add vital state, time since hospitalisation, case statues and method status to data.
  survival_matched_exposed <- matched_exposed %>%
    mutate( status = ifelse(vital_state=="leben (ZAS)", 1, ifelse( vital_state=="verstorben",2,1)),
            time_since = ifelse(vital_state=="verstorben",
                                difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
                                difftime(as.Date("20221112", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ),
            case_status = 1,
            method_status = 0 )  %>%
    select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration, method_status )

  survival_matched_cohort <- matched_cohort %>%
    mutate( status = ifelse(vital_state=="leben (ZAS)", 1, ifelse( vital_state=="verstorben",2,1)),
            time_since = ifelse(vital_state=="verstorben",
                                difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
                                difftime(as.Date("20221112", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ),
            case_status = 0,
            method_status = 1 )  %>%
    select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration, method_status )

  # combine exposed and matched cohort
  survival_prop <- rbind(survival_matched_cohort, survival_matched_exposed)

  # perform kaplan-meier analysis
  survival_Function_prop = survfit(Surv(survival_prop$time_since,
                                        survival_prop$status == 2, type = "right") ~ survival_prop$case_status )

  if (figure){
    # visualize result of survival analysis
    plot = survminer::ggsurvplot(fit = survival_Function_prop, data = survival_prop, xlab = "Time (days)", ylab = "Proportion patients alive",
                          legend.title = "Cohort", conf.int = TRUE,
                          legend = "bottom", surv.scale = "percent" )
  }

  return(list(survival_Function_prop, plot))
}

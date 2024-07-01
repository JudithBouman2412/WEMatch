#' WEMatch: Perform patient matching using ICD-10 codes
#'
#' This package contains functions that can be used to
#' perform gold standard matching using ICD-10 codes. For this we rely
#' on functionality from X and X packages. Additionaly, we provide an easy
#' to use function to perform W2v Enhanced patient MATCHing (WEMATCH) as
#' described by Nguyen et al. (2019).
#'
#' @docType package
#' @name WEMatch
#' @examples
#'
#' # General example of how to use this package
#' library(tidyverse)
#' library(MatchIt)
#' library(word2vec)
#' library(survival)
#'
#' data("simulated_data")
#'
#' # training data
#' data("training_data")
#' training_data <- as.vector(training_data)
#' W2V_model = word2vec(x = training_data, type = "cbow", dim = 100, iter = 20)
#'
#' # select primary ICD-10 code of interest
#' code = "C34.1"
#' simulated_data_selected = simulated_data %>% filter(icd10.1 == code)
#'
#' n = floor(dim(simulated_data_selected)[1]*0.25) # number of cases to match
#'
#' # randomly select n hospitalisations with disease X = I35.0
#' treated_group <- simulated_data_selected[sample(nrow(simulated_data_selected), size = n),] %>% mutate(case_status = 1)
#' # add "sentence" of all ICD-10 codes
#' treated_group <- treated_group %>% unite( "sentence", starts_with("icd10."), remove = FALSE, sep = " ", na.rm = TRUE)
#'
#' # create control database
#' # remove selected individuals
#' controls <- simulated_data %>% filter( !dim_fall_bk_pseudo %in% treated_group$dim_fall_bk_pseudo )
#' q  = dim(controls)[1]
#' controls_q <- controls %>% mutate(case_status = 0)
#' controls_q <- controls_q %>% unite( "sentence", starts_with("icd10."), remove = FALSE, sep = " ", na.rm = TRUE)
#'
#' # run PSM algorithms
#' matched_prop_mod_0 <- Match_prop_ICD(  treated_group = treated_group,
#'                                        possible_controls = controls_q,
#'                                       model = "mod_0" )
#'
#' matched_prop_mod_1 <- Match_prop_ICD(  treated_group = treated_group,
#'                                       possible_controls = controls_q,
#'                                       model = "mod_1" )
#'
#' matched_prop_mod_2_3 <- Match_prop_ICD(  treated_group = treated_group,
#'                                         possible_controls = controls_q,
#'                                         model = "mod_2_3" )
#' # run WEMatch algorithm
#' matched_wematch <- WEMatch( treated_group = treated_group,
#'                            possible_controls = controls_q,
#'                            W2V_model = W2V_model )
#'
#' # perform mortality analyses
#' # run analysis
#' survival_cases <- treated_group %>%
#'  mutate( status = ifelse(vital_state=="leben (ZAS)", 0, 1),
#'          time_since = ifelse(vital_state=="verstorben",
#'                              difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
#'                              difftime(as.Date("20230831", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ) )  %>%
#'  select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration )
#'
#'
#' # also do for the advanced model
#' survival_control_prop_0 <- matched_prop_mod_0[[1]] %>%
#'  mutate( status = ifelse(vital_state=="leben (ZAS)", 0, 1),
#'          time_since = ifelse(vital_state=="verstorben",
#'                              difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
#'                              difftime(as.Date("20230831", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ) )  %>%
#'  select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration )
#'
#' survival_control_prop_1 <- matched_prop_mod_1[[1]] %>%
#'  mutate( status = ifelse(vital_state=="leben (ZAS)", 0, 1),
#'          time_since = ifelse(vital_state=="verstorben",
#'                              difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
#'                              difftime(as.Date("20230831", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ) )  %>%
#'  select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration )
#'
#' survival_control_prop_2_3 <- matched_prop_mod_2_3[[1]] %>%
#'  mutate( status = ifelse(vital_state=="leben (ZAS)", 0, 1),
#'          time_since = ifelse(vital_state=="verstorben",
#'                              difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
#'                              difftime(as.Date("20230831", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ) )  %>%
#'  select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration )
#'
#' survival_W2V_seq <- as.data.frame(matched_wematch[[1]]) %>%
#'  mutate( status = ifelse(vital_state=="leben (ZAS)", 0, 1),
#'          time_since = ifelse(vital_state=="verstorben",
#'                              difftime(as.Date(as.character(death_date), format="%Y-%m-%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" ),
#'                              difftime(as.Date("20230831", format="%Y%m%d"), as.Date(as.character(case_start_day), format="%Y%m%d"), units="days" )  ) )  %>%
#'  select(dim_fall_bk_pseudo, status, vital_state, case_start_day, age_admission, death_date, gender, BMI, time_since, case_status, case_duration )
#'
#'
#' # calculate kaplan-meier curve
#' survival_prop <- rbind(survival_control_prop_0 %>% mutate(method_status = 1),
#'                       survival_control_prop_1 %>% mutate(method_status = 2),
#'                       survival_control_prop_2_3 %>% mutate(method_status = 3),
#'                       survival_W2V_seq %>% mutate(method_status = 4),
#'                       survival_cases %>% mutate(method_status = 0) )
#'
#' # perform kaplan-meier analysis
#' surv_obj_prop <- Surv(survival_prop$time_since,
#'                      event = survival_prop$status,
#'                      type = "right")
#'
#' survival_Function_prop = survfit(surv_obj_prop ~ survival_prop$method_status )
#'
#' # visualize result of survival analysis
#' survminer::ggsurvplot(fit = survival_Function_prop, data = survival_prop, xlab = "Time (days)",
#'                      legend.title = "Cohort", conf.int = TRUE,
#'                      legend = "bottom", surv.scale = "percent" , legend.labs = c("Exposed group",
#'                                                                                  "Prop mod 0",
#'                                                                                  "Prop mod 1",
#'                                                                                  "Prop mod 2_3",
#'                                                                                  "WEMatch"))
#'
#' # calculate difference in hazard ratios
#' survival_prop$method_status <- factor(survival_prop$method_status,
#'                                      labels =c("Exposed group",
#'                                                "Prop mod 0",
#'                                                "Prop mod 1",
#'                                                "Prop mod 2_3",
#'                                                "WEMatch"))
#'
#' fit.coxph <- coxph(surv_obj_prop ~ method_status,
#'                   data = survival_prop )
#'
NULL

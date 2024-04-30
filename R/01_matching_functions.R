#' Function performing the WEMatch algorithm.
#'
#' @param exposed_group a dataframe containing the data for the exposed group,
#' the dataframe should at least contain a column called "age_admission" with
#' the age of the individual at admission into the hospital, "BMI" with the BMI,
#' "gender" with gender of the individual, "sentence" with a string with all
#' ICD-10 codes in sequantial order, and a column for each individual ICD-10
#' code where the columns are ordered in sequential order.
#' @param dat_controls A dataframe of potential controls with the same structure
#' as `exposed_group`.
#' @param W2V_model A trained Word2Vec model object.
#' @param age_dif Maximum allowed difference in age for matching (default is 10).
#' @param BMI_dif Maximum allowed difference in BMI for matching (default is 5).
#'
#' @return A list containing hospitalizations in the matched cohort, the hospitalizations
#' for whom a match was found, the number of perfect matches, the number of matches based
#' on sequential ICD-10 order, and a vector with for each match the number of ICD-10 codes
#' which match perfectly.
#' @export
#'
#' @examples
#' # Assuming `exposed_group` and `possible_controls` are loaded and `W2V_model` is trained:
#' results <- WEMatch(exposed_group, possible_controls, W2V_model)
#'
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
WEMatch <- function( exposed_group,
                    possible_controls,
                    W2V_model,
                    age_dif=10,
                    BMI_dif=5){

  # Code, inspired by https://github.com/nphdang/WVM/blob/master/word2vec_based_matching.R

  # remove ICD codes with frequency less than 30?
  n_case = dim(dat_cases)[1]

  # initialize empty vector for saving matched controls
  matched_controls <-  c()
  # initialize empty vector for saving number of ICD-10 codes that match exactly
  ICD_matched <- c()

  # keep track of how many patients are matched within each route:
  n_case1 = 0 # perfect matches
  n_case2 = 0 # match using sequential order of ICD-10 codes

  # loop over all hospital visits in the exposed group to match with other potential matches
  for (i in 1:n_case){

    # print which patient is being matched to keep track of progress
    print(paste0(c("Matching: ", i)) )

    # take data of current exposed hospitalisation to match
    patient_now <- dat_cases[i,]

    # Find individuals with similar age (within distance age_dif years) & BMI (distance less than BMI_dif points)
    # and the same sex
    possible_controls <- dat_controls %>% filter( age_admission > (patient_now$age_admission-round(age_dif/2)) &
                                                    age_admission < (patient_now$age_admission+round(age_dif/2)) &
                                                    BMI > (patient_now$BMI-round(BMI_dif/2,2)) &
                                                    BMI < (patient_now$BMI+round(BMI_dif/2,2)) &
                                                    gender == patient_now$gender  )

    # print how many potential matches are available for this hospitalization
    print(paste0(c("Select from ", dim(possible_controls)[1], " possible controls.")))

    if (dim(possible_controls)[1]>0){
      # try to find perfect match
      print("trying to find perfect match")

      # CASE 1: Find perfect match
      perfect_match <- possible_controls %>% filter( sentence %in% patient_now$sentence )

      # If there is an exact match, add it to the control group
      if (dim(perfect_match)[1]>0){

        match = perfect_match[sample(nrow(perfect_match), N, replace=F),]

        matched_controls[i,2:dim(matched_controls)[2]] <- as_vector( match  )

        if (i == 1 ){
          matched_controls = c(patient_now$dim_fall_bk_pseudo, match )
          ICD_matched = c(as.numeric(Inf))
        } else {
          matched_controls = rbind(matched_controls, c(patient_now$dim_fall_bk_pseudo, unlist(match) ) )
          ICD_matched = cbind(ICD_matched, as.numeric(Inf))
        }

        n_case1 = n_case1+1 # count match to first categorie of matches

        print("found a perfect match")

        # remove this match from the possible controls for the other hospitalisations in the exposed group
        dat_controls <- dat_controls %>% filter(!dim_fall_bk_pseudo == match$dim_fall_bk_pseudo )

      } else { # Else try to find the best possible match that comes close
        print("trying to match based on closest sequential codes")

        p = 1
        dim_matched = dim(possible_controls)[1]

        matched_plus_1 <- possible_controls

        # find the most similar individual by checking each following code if it is the same for both patients
        while( dim_matched > 1 ){ # some while the next two ICD10 codes are the same

          matched_plus_2 <- matched_plus_1
          matched_plus_1 <- matched_plus_1[ matched_plus_1[3+p] == patient_now[,3 + p], ] %>% filter(!is.na(X.x))

          dim_matched <- dim(matched_plus_1)[1]

          if(dim_matched ==1){

            # If there is exactly one possible match left over, than this one should be selected
            match = matched_plus_1[1,]

            n_case2 = n_case2 + 1 # count this match as the second type

            # add match to matched control dataset
            if (i == 1 ){
              matched_controls = c(patient_now$dim_fall_bk_pseudo, unlist(match) )
              ICD_matched = c(as.numeric(p))
            } else {
              matched_controls = rbind(matched_controls, c(patient_now$dim_fall_bk_pseudo, unlist(match) ) )
              ICD_matched = cbind( ICD_matched, as.numeric(p))
            }

            # remove this match from the dat_controls
            dat_controls <- dat_controls %>% filter(!dim_fall_bk_pseudo == match$dim_fall_bk_pseudo )

          } else if( dim_matched == 0 ){ # if there are zero matches left over, return to previous p value (which is saved in matched_plus_2)

            # calculate the closest ICD10 code of these matches
            embedding1 <- word2vec::predict(W2V_model, patient_now[, 3 +p], type = "embedding")

            distance <- rep(NA, dim(matched_plus_2)[1])

            for (q in 1:(dim(matched_plus_2)[1])){
              embedding2 <-  word2vec::predict(W2V_model, matched_plus_2[q, 3 + p], type = "embedding")
              distance[q] <- word2vec_similarity(embedding1, embedding2)
            }

            if (max(distance, na.rm =TRUE)==-Inf){
              print("no match found")
            } else {
              matched_plus_1 <- matched_plus_2[distance==max(distance, na.rm = TRUE),] %>% filter(!is.na(X.x))

              print("Matched on sequential order")
              n_case2 = n_case2 + 1 # count this match as the second type

              match = matched_plus_1[sample(dim(matched_plus_1[1]), 1),]

              #matched_controls[i,2:(dim(matched_controls)[2])] = match[1,1:(dim(match)[2])]
              if (i == 1 ){
                matched_controls = c(patient_now$dim_fall_bk_pseudo, unlist(match) )
                ICD_matched = c( as.numeric(p-1)) # perfect ICD-10 matches is one less than p in this round
              } else {
                matched_controls = rbind(matched_controls, c(patient_now$dim_fall_bk_pseudo, unlist(match) ) )
                ICD_matched = cbind( ICD_matched, as.numeric(p-1)) # perfect ICD-10 matches is one less than p in this round
              }

              # remove this match from the dat_controls
              dat_controls <- dat_controls %>% filter(!dim_fall_bk_pseudo == match$dim_fall_bk_pseudo )

            }

          }

          p = p + 1


        }

      }

    } else {
      print("No suitable match was found")
    }
  }

  # output cases that were actually matched in the algorithm
  matched_cases <- dat_cases %>% filter(dim_fall_bk_pseudo %in% matched_controls[,1])

  return(list(matched_controls, matched_cases, n_case1, n_case2, as.vector(ICD_matched[1,]) ))
}


#' Title
#'
#' @param exposed_group a dataframe containing the data for the exposed group,
#' the dataframe should at least contain a column called "age_admission" with
#' the age of the individual at admission into the hospital, "BMI" with the BMI,
#' "gender" with gender of the individual, "sentence" with a string with all
#' ICD-10 codes in sequantial order, and a column for each individual ICD-10
#' code where the columns are ordered in sequential order.
#' @param dat_controls A dataframe of potential controls with the same structure
#' as `exposed_group`.
#' @param method A string specifying the distance method for finding matches; default is 'nearest'.
#'               Options include 'nearest' for nearest neighbor matching and 'linear_nearest' for linear
#'               nearest matching which uses logit transformations.
#' @param age_dif Maximum permissible age difference for matching (default is 10 years).
#' @param BMI_dif Maximum permissible BMI difference for matching (default is 5 points).
#' @param model A string identifier for the model used to calculate propensity scores.
#'              Different models use different sets of covariates for more refined control.
#'
#'
#' @return A list containing hospitalizations in the matched cohort, the hospitalizations
#' for whom a match was found, the matchit output object for further analysis,
#' and the dataframe with added propensity scores and inverse probability
#' weights (IPW) adjusted for case status.
#'
#' @export
#'
#' @examples
#' # Assuming `exposed_group` and `possible_controls` are properly prepared:
#' results <- Match_prop_ICD(exposed_group, possible_controls, method="nearest", model="mod_0")
#'
#' @importFrom MatchIt matchit
#' @import dplyr
#' @importFrom stats setNames
Match_prop_ICD <- function(  exposed_group,
                             possible_controls,
                             method = "nearest",
                             age_dif=10,
                             BMI_dif=5,
                             model = "mod_0"){

  # remove ICD codes with frequency less than 30?
  n_case = dim(dat_cases)[1]
  matched_controls = as.data.frame(matrix(NA, nrow=n_case*N, ncol=dim(dat_cases)[2]+1))
  colnames(matched_controls) = c("Covid_match_ID", colnames(dat_cases))
  matched_controls[,1] = rep(dat_cases$dim_fall_bk_pseudo, each = N)

  propensity <- rbind(dat_controls, dat_cases) %>% mutate(I35 = as.logical(I35))

  # calculate propensity for all depending on the model
  if (model == "mod_0"){
    m.out1 <- matchit(case_status ~ age_admission + BMI + gender ,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_1"){
    m.out1 <- matchit(case_status ~ age_admission + BMI + gender + clinic_discharge ,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_2_1"){
    m.out1 <- matchit(case_status ~ age_admission + BMI + gender + A_1 + B_1+ C_1 +
                        D_1+ E_1+ F_1+ G_1+ H_1+ I_1+ J_1+ K_1 +
                        L_1+ M_1+ N_1+ O_1+ P_1+ Q_1+ R_1+ S_1+ T_1+ V_1+ W_1+ X_1 +
                        Y_1+ Z_1 + clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_2_2"){ # this needs to be adapted still...
    m.out1 <- matchit(case_status ~ A_2 + B_2+ C_2+ D_2+ E_2+ F_2+ G_2+ H_2+ I_2+ J_2+ K_2 +
                        L_2+ M_2+ N_2+ O_2+ P_2+ Q_2+ R_2+ S_2+ T_2+ V_2+ W_2+ X_2 +
                        Y_2+ Z_2 + clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_2_3"){ # this needs to be adapted still...
    m.out1 <- matchit(case_status ~ A_3 + B_3+ C_3+ D_3+ E_3+ F_3+ G_3+ H_3+ I_3+ J_3+ K_3 +
                        L_3+ M_3+ N_3+ O_3+ P_3+ Q_3+ R_3+ S_3+ T_3+ V_3+ W_3+ X_3 +
                        Y_3+ Z_3 + clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_3_1"){
    m.out1 <- matchit(case_status ~ A_3 + B_3+ C_3+ D_3+ E_3+ F_3+ G_3+ H_3+ I_3+ J_3+ K_3 +
                        L_3+ M_3+ N_3+ O_3+ P_3+ Q_3+ R_3+ S_3+ T_3+ V_3+ W_3+ X_3 +
                        Y_3+ Z_3 +
                        I_1_1 + I_2_1 + I_3_1 + I_4_1 + I_5_1 + I_6_1 + I_7_1 + I_8_1 + I_9_1 + I_10_1 +
                        clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_3_2"){
    m.out1 <- matchit(case_status ~ A_3 + B_3+ C_3+ D_3+ E_3+ F_3+ G_3+ H_3+ I_3+ J_3+ K_3 +
                        L_3+ M_3+ N_3+ O_3+ P_3+ Q_3+ R_3+ S_3+ T_3+ V_3+ W_3+ X_3 +
                        Y_3+ Z_3 +
                        I_1_2 + I_2_2 + I_3_2 + I_4_2 + I_5_2 + I_6_2 + I_7_2 + I_8_2 + I_9_2 + I_10_2 +
                        clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  } else if (model == "mod_3_3"){
    m.out1 <- matchit(case_status ~ A_3 + B_3+ C_3+ D_3+ E_3+ F_3+ G_3+ H_3+ I_3+ J_3+ K_3 +
                        L_3+ M_3+ N_3+ O_3+ P_3+ Q_3+ R_3+ S_3+ T_3+ V_3+ W_3+ X_3 +
                        Y_3+ Z_3 +
                        I_1_3 + I_2_3 + I_3_3 + I_4_3 + I_5_3 + I_6_3 + I_7_3 + I_8_3 + I_9_3 + I_10_3 +
                        clinic_discharge,
                      data = propensity,
                      method = "nearest", distance = "glm")
  }


  dat_controls$prop_scores = m.out1$distance[1:dim(dat_controls)[1]]
  dat_cases$prop_scores = m.out1$distance[(dim(dat_controls)[1]+1):(dim(dat_controls)[1]+n_case)]

  # add proprensity scores to the dataframe
  propensity$prop_scores = m.out1$distance
  dat_propensity = propensity %>% mutate(ipw=(case_status/prop_scores)+(1-case_status)/(1-prop_scores) )

  n_case1 = 0
  n_case2 = 0
  n_case3 = 0

  # loop over all COVID cases to match with other cases
  for (i in 1:n_case){
    print(c("starting with", i))
    patient_now <- dat_cases[i,]

    # Find individuals with similar age (within distance <5 years) & BMI (distance less than 2 points)
    possible_controls <- dat_controls %>% filter( age_admission > (patient_now$age_admission-round(age_dif/2)) &
                                                    age_admission < (patient_now$age_admission+round(age_dif/2)) &
                                                    BMI > (patient_now$BMI-round(BMI_dif/2)) &
                                                    BMI < (patient_now$BMI+round(BMI_dif/2)) &
                                                    gender == patient_now$gender  )

    if (method == "nearest"){

      distance = abs(patient_now$prop_scores - possible_controls$prop_scores)

      pos_matches <- possible_controls[ distance == min(distance , na.rm = TRUE ) , ] %>% filter(!is.na(X.x))

      match = pos_matches[sample(dim(pos_matches[1]), 1),]
    } else if ( method == "linear_nearest"){

      distance = abs(logit(patient_now$prop_scores) - logit(possible_controls$prop_scores) )

      pos_matches <- possible_controls[ distance == min(distance , na.rm = TRUE ) , ] %>% filter(!is.na(X.x))

      match = pos_matches[sample(dim(pos_matches[1]), 1),]
    }

    if (dim(match)[1]>0){
      matched_controls[i,2:(dim(matched_controls)[2])] <- match
    } else {
      matched_controls[i,2:(dim(matched_controls)[2])] <- NA
    }

  }

  # get dataframe for matched cases
  matched_cases <- dat_cases %>% filter(dim_fall_bk_pseudo %in% matched_controls$Covid_match_ID)

  return( list(matched_controls, matched_cases, m.out1, dat_propensity ) )
}


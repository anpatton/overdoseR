#' Train and test Supper Vector Machine classifier.
#' @param training_data formatted EMS data for model training
#' @param testing_data formatted EMS data for model training
#' @param save_model Logical flag to save model as .RDS object
#' @param model_name Filepath and name of model to save, .RDS extension not needed
#' @param target Binary (0/1) for class labels
#' @param features Character vector of features, "default" for recommended options
#' @param kernel_type Radial or linear kernel for SVM
#' @param cost_grid Vector of cost parameters to tune over
#' @return List object with model and test data with predictions
#' \itemize{
#'   \item model - Fitted GLM object
#'   \item results - Test data with predictions
#'   \item tune_object - SVM tuning object
#' }
#' @examples
#' tune_svm(training_data = train, testing_data = test, kernel_type = "linear")
tune_svm <- function(training_data,
                     testing_data,
                     save_model = TRUE,
                     model_name = paste0("opioid_svm_model_", Sys.Date()),
                     target = "opioid",
                     features = "defaults",
                     kernel_type = c("radial", "linear"),
                     cost_grid = c(0.01, 0.1, 1, 10, 100)){

  if(features == "defaults") {

    features <- c("opioid_agonist_admin", "benzodiazepine_admin", "epinephrine_admin",
                  "opioid_agonist_success", "drug_related_pi", "traumatic_injury_pi",
                  "attempt", "booking", "bottles", "boyfriend", "cocaine", "deputies",
                  "drug", "empty", "fetanyl", "harm", "herion", "heroin", "heroine",
                  "hydrocodone", "ineffective", "inform", "ingestion", "injecting",
                  "intent", "knowledge", "lorazepam", "meth", "methadone", "narcan",
                  "narcotic", "norco", "npa", "opiates", "opiods", "overdose",
                  "overdosing", "oxycodone", "oxycontin", "paraphernalia", "pill",
                  "pills", "pin", "pinpoint", "powder", "pupils", "respirations",
                  "snoring", "suboxone", "substance", "taking", "thrashing", "tta",
                  "unresponsive", "use", "white", "xanax", "administered_narcan",
                  "alcohol_drugs", "appeared_responsive", "bed_unresponsive", "bedroom_pt",
                  "boyfriend_denied", "boyfriend_stated", "cardiac_arrest", "cc_intentional",
                  "cell_pt", "coming_mouth", "denies_taking", "drug_paraphernalia",
                  "drug_use", "easily_arousable", "eyes_fixed", "fentanyl_patch",
                  "floor_breathing", "floor_room", "found_inside", "found_pinpoint",
                  "found_unresponsive", "front_desk", "gcs_custody", "gw_medic",
                  "heroin_cocaine", "heroin_pt", "heroin_use", "inform_patient",
                  "inside_cell", "intent_harm", "male_who", "medication_bottles",
                  "medications_alcohol", "methadone_clinic", "methadone_pt", "mg_methadone",
                  "mg_narcan", "narcan_administered", "narcan_administration",
                  "narcan_given", "narcan_improvement", "narcan_iv", "narcan_pt",
                  "oriented_events", "overdose_pt", "painful_stimuli", "paraphernalia_found",
                  "paraphernalia_scene", "patent_airway", "patient_overdosing",
                  "pd_informs", "pin_pupils", "pinpoint_pupils", "positive_respiratory",
                  "post_narcan", "potential_overdose", "prison_staff", "pt_heroin",
                  "pt_medication", "pt_narcan", "pt_slowly", "pt_unresponsive",
                  "pts_girlfriend", "pts_respirations", "pupils_found", "pupils_pinpoint",
                  "pupils_pt", "purposeful_movements", "rate_drive", "respiratory_rate",
                  "respond_questions", "responders_the", "response_to", "responsive_pt",
                  "s_bg", "snoring_respirations", "t_medic", "taking_today", "the_pt",
                  "tolerate_npa", "total_mg", "units_arrived", "white_powder",
                  "administered_mg_narcan", "all_times_approximate", "ao_gcs_custody",
                  "appeared_responsive_pt", "boyfriend_stated_pt", "complaints_head_toe",
                  "denies_complaints_head", "denies_taking_medications", "denies_taking_today",
                  "drug_paraphernalia_found", "established_total_mg", "eyes_fixed_dilated",
                  "found_inside_cell", "found_scene_pt", "gurney_ambulance_obtained",
                  "handed_nurse_change", "incident_gw_medic", "kill_herself_pt",
                  "lying_supine_floor", "m_scene_yo", "male_who_was", "narcan_administration_pt",
                  "narcan_given_pt", "no_change_patient", "noted_physical_assessment",
                  "oral_trauma_pt", "patent_airway_pt", "pinpoint_pupils_pt", "pt_administered_narcan",
                  "pt_appeared_responsive", "pt_condition_improved", "pt_denies_etoh",
                  "pt_found_unresponsive", "pt_loaded_flat", "pt_positive_respiratory",
                  "pt_reports_taking", "pt_respiratory_rate", "pt_slow_respond",
                  "report_handed_nurse", "respiratory_rate_drive", "responders_the_pt",
                  "revealed_patient_agitated", "room_the_pt", "signs_oral_trauma",
                  "sinus_tach_pt", "slow_respond_questions", "stable_pt_care",
                  "times_approximate_arrived", "total_mg_narcan", "transport_full_pt",
                  "trauma_assessment_pt", "was_able_to", "yo_male_who")

    features <- c(target, features)


  } else {

    features <- c(target, features)

  }

  training_data <- training_data %>%
    dplyr::select(features)

  message("Modeling: Start \n ---")
  message("Depending on size of data this might take several minutes \n ---")

  mod_tune <- e1071::tune(svm,
                          opioid ~ .,
                          kernel = "linear",
                          ranges = list(cost = c(0.11)),
                          scale = FALSE,
                          data = training_data)

  mod <- e1071::svm(opioid ~ .,
                    data = training_data,
                    type = "C",
                    kernel = kernel_type,
                    cost = summary(mod_tune)$best.parameters$cost,
                    scale = FALSE,
                    probability = TRUE)

  testing_data <- testing_data %>%
    dplyr::select(features)

  preds <- predict(mod, newdata = testing_data, probability = TRUE)
  preds <- unname(attr(preds, "probabilities")[, 2])

  testing_data <- testing_data %>%
    dplyr::mutate(predicted_probability = preds)

  message("Modeling: Finished \n ---")

  if(save_model == TRUE) {

    message(paste0("Saving model to " , paste0(model_name, ".rds")))
    saveRDS(mod, file = paste0(model_name, ".rds"))

  }

  resList <- list("model" = mod,
                  "tune_object" = mod_tune,
                  "results" = testing_data)

  return(resList)


}

#' Train and test logistic regression classifier.
#' @param training_data formatted EMS data for model training
#' @param testing_data formatted EMS data for model training
#' @param save_model Logical flag to save model as .RDS object
#' @param model_name Filepath and name of model to save, .RDS extension not needed
#' @param target Binary (0/1) for class labels
#' @param features Character vector of features, "default" for recommended options
#' @return List object with model and test data with predictions
#' \itemize{
#'   \item model - Fitted XGBoost object
#'   \item results - Test data with predictions
#'   \item tune_object - SVM tuning object
#' }
#' @examples
#' tune_logistic(training_data = train, testing_data = test)
tune_logistic <- function(training_data,
                          testing_data,
                          save_model = TRUE,
                          model_name = paste0("opioid_logistic_model_", Sys.Date()),
                          target = "opioid",
                          features = "defaults"){

  if(features == "defaults") {

    features <- c("opioid_agonist_admin", "benzodiazepine_admin", "epinephrine_admin",
                  "opioid_agonist_success", "drug_related_pi", "traumatic_injury_pi",
                  "attempt", "booking", "bottles", "boyfriend", "cocaine", "deputies",
                  "drug", "empty", "fetanyl", "harm", "herion", "heroin", "heroine",
                  "hydrocodone", "ineffective", "inform", "ingestion", "injecting",
                  "intent", "knowledge", "lorazepam", "meth", "methadone", "narcan",
                  "narcotic", "norco", "npa", "opiates", "opiods", "overdose",
                  "overdosing", "oxycodone", "oxycontin", "paraphernalia", "pill",
                  "pills", "pin", "pinpoint", "powder", "pupils", "respirations",
                  "snoring", "suboxone", "substance", "taking", "thrashing", "tta",
                  "unresponsive", "use", "white", "xanax", "administered_narcan",
                  "alcohol_drugs", "appeared_responsive", "bed_unresponsive", "bedroom_pt",
                  "boyfriend_denied", "boyfriend_stated", "cardiac_arrest", "cc_intentional",
                  "cell_pt", "coming_mouth", "denies_taking", "drug_paraphernalia",
                  "drug_use", "easily_arousable", "eyes_fixed", "fentanyl_patch",
                  "floor_breathing", "floor_room", "found_inside", "found_pinpoint",
                  "found_unresponsive", "front_desk", "gcs_custody", "gw_medic",
                  "heroin_cocaine", "heroin_pt", "heroin_use", "inform_patient",
                  "inside_cell", "intent_harm", "male_who", "medication_bottles",
                  "medications_alcohol", "methadone_clinic", "methadone_pt", "mg_methadone",
                  "mg_narcan", "narcan_administered", "narcan_administration",
                  "narcan_given", "narcan_improvement", "narcan_iv", "narcan_pt",
                  "oriented_events", "overdose_pt", "painful_stimuli", "paraphernalia_found",
                  "paraphernalia_scene", "patent_airway", "patient_overdosing",
                  "pd_informs", "pin_pupils", "pinpoint_pupils", "positive_respiratory",
                  "post_narcan", "potential_overdose", "prison_staff", "pt_heroin",
                  "pt_medication", "pt_narcan", "pt_slowly", "pt_unresponsive",
                  "pts_girlfriend", "pts_respirations", "pupils_found", "pupils_pinpoint",
                  "pupils_pt", "purposeful_movements", "rate_drive", "respiratory_rate",
                  "respond_questions", "responders_the", "response_to", "responsive_pt",
                  "s_bg", "snoring_respirations", "t_medic", "taking_today", "the_pt",
                  "tolerate_npa", "total_mg", "units_arrived", "white_powder",
                  "administered_mg_narcan", "all_times_approximate", "ao_gcs_custody",
                  "appeared_responsive_pt", "boyfriend_stated_pt", "complaints_head_toe",
                  "denies_complaints_head", "denies_taking_medications", "denies_taking_today",
                  "drug_paraphernalia_found", "established_total_mg", "eyes_fixed_dilated",
                  "found_inside_cell", "found_scene_pt", "gurney_ambulance_obtained",
                  "handed_nurse_change", "incident_gw_medic", "kill_herself_pt",
                  "lying_supine_floor", "m_scene_yo", "male_who_was", "narcan_administration_pt",
                  "narcan_given_pt", "no_change_patient", "noted_physical_assessment",
                  "oral_trauma_pt", "patent_airway_pt", "pinpoint_pupils_pt", "pt_administered_narcan",
                  "pt_appeared_responsive", "pt_condition_improved", "pt_denies_etoh",
                  "pt_found_unresponsive", "pt_loaded_flat", "pt_positive_respiratory",
                  "pt_reports_taking", "pt_respiratory_rate", "pt_slow_respond",
                  "report_handed_nurse", "respiratory_rate_drive", "responders_the_pt",
                  "revealed_patient_agitated", "room_the_pt", "signs_oral_trauma",
                  "sinus_tach_pt", "slow_respond_questions", "stable_pt_care",
                  "times_approximate_arrived", "total_mg_narcan", "transport_full_pt",
                  "trauma_assessment_pt", "was_able_to", "yo_male_who")

    features <- c(target, features)


  } else {

    features <- c(target, features)

  }

  training_data <- training_data %>%
    select(features)

  message("Modeling: Start \n ---")
  mod <- glm(as.formula(paste(target, ".", sep = " ~ ")),
             data = training_data,
             family = "binomial")
  message("Modeling: Finished \n ---")

  testing_data <- testing_data %>%
    select(features)

  preds <- predict(mod, newdata = testing_data, type = "response")

  testing_data <- testing_data %>%
    select(features) %>%
    mutate(predicted_probability = preds)

  if(save_model == TRUE) {

    message(paste0("Saving model to " , paste0(model_name, ".rds")))
    saveRDS(mod, file = paste0(model_name, ".rds"))

  }

  resList <- list("model" = mod,
                  "results" = testing_data)

  return(resList)

}

#' Train and test XGBoost classifier.
#' @param training_data formatted EMS data for model training
#' @param testing_data formatted EMS data for model training
#' @param save_model Logical flag to save model as .RDS object
#' @param model_name Filepath and name of model to save, .RDS extension not needed
#' @param target Binary (0/1) for class labels
#' @param features Character vector of features, "default" for recommended options
#' @param cv_iters Number of cross validation folds for use in tuning - must be greater than 1
#' @param tune_iters Number of overall tuning iterations - must be greater than 1
#' @return List object with model and test data with predictions
#' \itemize{
#'   \item model - Fitted XGBoost object
#'   \item tune_obj - XGBoost tuning object
#'   \item results - Test data with predictions
#' }
#' @examples
#' tune_logistic(training_data = train, testing_data = test)
tune_xgboost <- function(training_data,
                         testing_data,
                         save_model = TRUE,
                         model_name = paste0("opioid_xgboost_model_", Sys.Date()),
                         target = "opioid",
                         features = "defaults",
                         cv_iters = 5,
                         tune_iters = 50){


  if(features == "defaults") {

    features <- c("opioid_agonist_admin", "benzodiazepine_admin", "epinephrine_admin",
                  "opioid_agonist_success", "drug_related_pi", "traumatic_injury_pi",
                  "attempt", "booking", "bottles", "boyfriend", "cocaine", "deputies",
                  "drug", "empty", "fetanyl", "harm", "herion", "heroin", "heroine",
                  "hydrocodone", "ineffective", "inform", "ingestion", "injecting",
                  "intent", "knowledge", "lorazepam", "meth", "methadone", "narcan",
                  "narcotic", "norco", "npa", "opiates", "opiods", "overdose",
                  "overdosing", "oxycodone", "oxycontin", "paraphernalia", "pill",
                  "pills", "pin", "pinpoint", "powder", "pupils", "respirations",
                  "snoring", "suboxone", "substance", "taking", "thrashing", "tta",
                  "unresponsive", "use", "white", "xanax", "administered_narcan",
                  "alcohol_drugs", "appeared_responsive", "bed_unresponsive", "bedroom_pt",
                  "boyfriend_denied", "boyfriend_stated", "cardiac_arrest", "cc_intentional",
                  "cell_pt", "coming_mouth", "denies_taking", "drug_paraphernalia",
                  "drug_use", "easily_arousable", "eyes_fixed", "fentanyl_patch",
                  "floor_breathing", "floor_room", "found_inside", "found_pinpoint",
                  "found_unresponsive", "front_desk", "gcs_custody", "gw_medic",
                  "heroin_cocaine", "heroin_pt", "heroin_use", "inform_patient",
                  "inside_cell", "intent_harm", "male_who", "medication_bottles",
                  "medications_alcohol", "methadone_clinic", "methadone_pt", "mg_methadone",
                  "mg_narcan", "narcan_administered", "narcan_administration",
                  "narcan_given", "narcan_improvement", "narcan_iv", "narcan_pt",
                  "oriented_events", "overdose_pt", "painful_stimuli", "paraphernalia_found",
                  "paraphernalia_scene", "patent_airway", "patient_overdosing",
                  "pd_informs", "pin_pupils", "pinpoint_pupils", "positive_respiratory",
                  "post_narcan", "potential_overdose", "prison_staff", "pt_heroin",
                  "pt_medication", "pt_narcan", "pt_slowly", "pt_unresponsive",
                  "pts_girlfriend", "pts_respirations", "pupils_found", "pupils_pinpoint",
                  "pupils_pt", "purposeful_movements", "rate_drive", "respiratory_rate",
                  "respond_questions", "responders_the", "response_to", "responsive_pt",
                  "s_bg", "snoring_respirations", "t_medic", "taking_today", "the_pt",
                  "tolerate_npa", "total_mg", "units_arrived", "white_powder",
                  "administered_mg_narcan", "all_times_approximate", "ao_gcs_custody",
                  "appeared_responsive_pt", "boyfriend_stated_pt", "complaints_head_toe",
                  "denies_complaints_head", "denies_taking_medications", "denies_taking_today",
                  "drug_paraphernalia_found", "established_total_mg", "eyes_fixed_dilated",
                  "found_inside_cell", "found_scene_pt", "gurney_ambulance_obtained",
                  "handed_nurse_change", "incident_gw_medic", "kill_herself_pt",
                  "lying_supine_floor", "m_scene_yo", "male_who_was", "narcan_administration_pt",
                  "narcan_given_pt", "no_change_patient", "noted_physical_assessment",
                  "oral_trauma_pt", "patent_airway_pt", "pinpoint_pupils_pt", "pt_administered_narcan",
                  "pt_appeared_responsive", "pt_condition_improved", "pt_denies_etoh",
                  "pt_found_unresponsive", "pt_loaded_flat", "pt_positive_respiratory",
                  "pt_reports_taking", "pt_respiratory_rate", "pt_slow_respond",
                  "report_handed_nurse", "respiratory_rate_drive", "responders_the_pt",
                  "revealed_patient_agitated", "room_the_pt", "signs_oral_trauma",
                  "sinus_tach_pt", "slow_respond_questions", "stable_pt_care",
                  "times_approximate_arrived", "total_mg_narcan", "transport_full_pt",
                  "trauma_assessment_pt", "was_able_to", "yo_male_who")

    features <- c(target, features)


  } else {

    features <- c(target, features)

  }

  training_data <- training_data %>%
    dplyr::select(features) %>%
    dplyr::mutate_all(as.integer)

  testing_data <- testing_data %>%
    dplyr::select(features) %>%
    dplyr::mutate_all(as.integer)

  train_task <- mlr::makeClassifTask(data = training_data, target = target, positive = "1")
  test_task <- mlr::makeClassifTask(data = testing_data, target = target, positive = "1")

  #create learner
  learner <- mlr::makeLearner("classif.xgboost", predict.type = "prob")

  learner$par.vals <- list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 1L,
    eta = 0.1
  )

  #set parameter space
  params <- ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam("booster",values = c("gbtree")),
    ParamHelpers::makeIntegerParam("max_depth",lower = 2L, upper = 200L),
    ParamHelpers::makeNumericParam("min_child_weight", lower = 1L, upper = 100L),
    ParamHelpers::makeNumericParam("subsample", lower = 0.5, upper = 1),
    ParamHelpers::makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
  )

  #set resampling strategy
  rdesc <- mlr::makeResampleDesc("CV", stratify = TRUE, iters = cv_iters)

  #search strategy
  ctrl <- mlr::makeTuneControlRandom(maxit = tune_iters)

  mod_tune <- mlr::tuneParams(learner = learner,
                              task = train_task,
                              resampling = rdesc,
                              par.set = params,
                              control = ctrl,
                              show.info = TRUE)

  tuned_learner <- mlr::setHyperPars(learner, par.vals = mod_tune$x)

  mod <- mlr::train(learner = tuned_learner,
                    task = train_task)

  preds <- predict(mod, test_task)$data$prob.1

  testing_data <- testing_data %>%
    dplyr::select(features) %>%
    dplyr::mutate(predicted_probability = preds)

  if(save_model == TRUE) {

    message(paste0("Saving model to " , paste0(model_name, ".rds")))
    saveRDS(mod, file = paste0(model_name, ".rds"))

  }

  resList <- list("model" = mod,
                  "tune_object" = mod_tune,
                  "results" = testing_data)

  return(resList)

}

#' Removes stop words from care narratives. Intended to be used internally with apply.
#'
#' @param str String of single narrative
#' @param stop_words Corpus of stop words to use, defaults to tidytext::stop_words
#' @param remove_numbers Logical flag to remove numbers from text
#' @return Cleaned narrative
#' @export

remove_stop_words <- function(str, stop_words, remove_numbers = TRUE) {

  if(is.na(str) == TRUE) {

    x <- NA

  } else {

    if(remove_numbers == TRUE){

      x <- unlist(strsplit(str, " "))
      x <- paste(x[!x %in% stop_words], collapse = " ")
      x <- stringr::str_trim(gsub("[^[:alpha:] ]", "", x))
      x <- stringr::str_squish(x)
      tolower(x)

    } else {

      x <- unlist(strsplit(str, " "))
      x <- paste(x[!x %in% stop_words], collapse = " ")
      x <- stringr::str_trim(gsub("[^[:alnum:] ]", "", x))
      x <- stringr::str_squish(x)
      tolower(x)

    }


  }

  return(x)

}

#' Creates one-hot-matrix of terms in narrative. Intended to be used internally with apply.
#'
#' @param row_data_in String of single narrative
#' @param word_tokens Words to tokenize on
#' @param bigram_tokens Bigrams to tokenize on
#' @param trigram_tokens Trigrams to tokenize on
#' @return Vector of one-hot terms
#' @export

make_one_hot <- function(row_data_in,
                         word_tokens,
                         bigram_tokens,
                         trigram_tokens){

  if(is.na(row_data_in) == TRUE) {

    return(c(rep(0, length(word_tokens)),
             rep(0, length(bigram_tokens)),
             rep(0, length(trigram_tokens))))

  } else {

    word_counts <- stringr::str_count(row_data_in, word_tokens)
    bigram_counts <- stringr::str_count(row_data_in, bigram_tokens)
    trigram_counts <- stringr::str_count(row_data_in, trigram_tokens)
    return(c(word_counts, bigram_counts, trigram_counts))

  }

}


#' Creates one-hot-matrix of terms in narrative and binds to data.
#'
#' @param data_in Formatted data to use
#' @param col_to_one_hot Field name for specific narrative to one-hot
#' @param words Words to tokenize on, defaults to high_value_terms
#' @param bigrams Bigrams to tokenize on, defaults to high_value_terms
#' @param trigrams Trigrams to tokenize on, defaults to high_value_terms
#' @return Dataframe of inputted data and one-hot fields
#' @examples
#' \dontrun{one_hot_single_row_ems_data(formatted_data_frame)}
#' @importFrom rlang .data
#' @export
one_hot_single_row_ems_data <- function(data_in,
                                        col_to_one_hot = "care_narrative_adj",
                                        words = dplyr::pull(dplyr::select(dplyr::filter(overdoseR:::high_value_terms, .data$type == "word"), .data$token)),
                                        bigrams = dplyr::pull(dplyr::select(dplyr::filter(overdoseR:::high_value_terms, .data$type == "bigram"), .data$token)),
                                        trigrams = dplyr::pull(dplyr::select(dplyr::filter(overdoseR:::high_value_terms, .data$type == "trigram"), .data$token))) {

  message("Tokenizing narrative and one-hot encoding high value terms. \n --- ")
  one_hot <- data.frame(t(apply(data_in[, which(names(data_in) == col_to_one_hot)], 1,
                                function(x) make_one_hot(row_data_in = x,
                                                         word_tokens = words,
                                                         bigram_tokens = bigrams,
                                                         trigram_tokens = trigrams))))

  colnames(one_hot) <- gsub(" ", "_", c(words, bigrams, trigrams))

  return(dplyr::bind_cols(data_in, one_hot) %>% dplyr::ungroup())

}

#' Converts multi-row data to single row data
#'
#' @param data_in raw EMS data to use
#' @param event_id_name Name of id column, defaults to "CAD Incident Number (eResponse.03)",
#' @param patient_first_name Name of first name column, defaults to "Patient First Name (ePatient.03)",
#' @param patient_last_name Name of last name column, defaults to (ePatient.02)",
#' @param patient_age_name Name of age column, defaults to (ePatient.15)",
#' @param primary_impression_name Name of primary impression column, defaults to "Situation Provider Primary Impression (eSituation.11)",
#' @param primary_complaint_name Name of primary complaint column, defaults to "Situation Primary Complaint Statement List (eSituation.04)",
#' @param care_narrative_name Name of care narrative column, defaults to "Patient Care Report Narrative (eNarrative.01)",
#' @param medication_given_name Name of medication administered column, defaults to "Medication Given Description (eMedications.03)",
#' @param medication_response_name Name of medication response column, defaults to "Medication Response (eMedications.07)",
#' @param remove_numbers_from_text Logical flag to remove numbers from text
#' @param stop_words Corpus of stop words, defaults to tidytext::stop_words (tidytext not required)
#' @return One row per record dataframe of inputted data and one-hot fields
#' @examples
#' \dontrun{format_multirow_ems_data(data_in = raw_ems_data)}
#' @importFrom rlang .data
#' @export
format_multirow_ems_data <- function(data_in,
                                    event_id_name = "CAD Incident Number (eResponse.03)",
                                    patient_first_name = "Patient First Name (ePatient.03)",
                                    patient_last_name = "Patient Last Name (ePatient.02)",
                                    patient_age_name = "Patient Age (ePatient.15)",
                                    primary_impression_name = "Situation Provider Primary Impression (eSituation.11)",
                                    primary_complaint_name = "Situation Primary Complaint Statement List (eSituation.04)",
                                    care_narrative_name = "Patient Care Report Narrative (eNarrative.01)",
                                    medication_given_name = "Medication Given Description (eMedications.03)",
                                    medication_response_name = "Medication Response (eMedications.07)",
                                    remove_numbers_from_text = TRUE,
                                    stop_words = overdoseR:::stop_words) {

  message("Beginning data reformat \n ---")
  cols_to_keep <- c(event_id_name,
                    patient_first_name,
                    patient_last_name,
                    patient_age_name,
                    primary_impression_name,
                    primary_complaint_name,
                    care_narrative_name,
                    medication_given_name,
                    medication_response_name)

  message("Reformatting care narrative  \n ---")
  care_narrative_adj <- tolower(unlist(apply(data_in[, which(names(data_in) == care_narrative_name)],
                                             MARGIN = 1,
                                             function(x) remove_stop_words(x, stop_words = stop_words,
                                                                     remove_numbers = remove_numbers_from_text))))

  message("Reformatting primary complaint  \n ---")
  primary_complaint_adj <- tolower(unlist(apply(data_in[, which(names(data_in) == primary_complaint_name)],
                                                MARGIN = 1,
                                                function(x) remove_stop_words(x, stop_words = stop_words,
                                                                        remove_numbers = remove_numbers_from_text))))

  formatted_data <- data_in %>%
    dplyr::select(cols_to_keep) %>%
    dplyr::mutate(care_narrative_adj = care_narrative_adj) %>%
    dplyr::mutate(primary_complaint_adj = primary_complaint_adj) %>%
    dplyr::group_by(!!dplyr::sym(event_id_name),
             !!dplyr::sym(patient_first_name),
             !!dplyr::sym(patient_last_name),
             !!dplyr::sym(patient_age_name),
             !!dplyr::sym(primary_impression_name),
             !!dplyr::sym(primary_complaint_name),
             !!dplyr::sym(care_narrative_name)) %>%
    dplyr::mutate(person_event_id = paste0("idx-", dplyr::group_indices())) %>%
    dplyr::ungroup() %>%
    unique()

  message("Tabulating medication administrations and responses  \n ---")
  med_admin_col <- which(names(formatted_data) == medication_given_name)
  med_resp_col <- which(names(formatted_data) == medication_response_name)

  opioid_agonist_admin <- apply(formatted_data, 1,
                                function(x) grepl("naloxone|narcan", x[med_admin_col], ignore.case = TRUE))
  opioid_pain_admin <- apply(formatted_data, 1,
                             function(x) grepl("morphine|fentanyl", x[med_admin_col], ignore.case = TRUE))
  benzodiazepine_admin <- apply(formatted_data, 1,
                                function(x) grepl("midozolam|diazepam", x[med_admin_col], ignore.case = TRUE))
  epinephrine_admin <- apply(formatted_data, 1,
                             function(x) grepl("epi ", x[med_admin_col], ignore.case = TRUE))
  response_to_admin <- apply(formatted_data, 1,
                             function(x) grepl("improved", x[med_resp_col], ignore.case = TRUE))
  drug_related_pi <- apply(formatted_data, 1,
                           function(x) grepl("overdose|behavioral|altered", x[primary_impression_name], ignore.case = TRUE))
  traumatic_injury_pi <- apply(formatted_data, 1,
                               function(x) grepl("traumatic", x[primary_impression_name], ignore.case = TRUE))
  cardiac_pi <- apply(formatted_data, 1,
                      function(x) grepl("cardiac", x[primary_impression_name], ignore.case = TRUE))

  age_adj <- apply(formatted_data, 1, function(x) as.numeric(x[patient_age_name]))

  formatted_data <- formatted_data %>%
    dplyr::mutate(opioid_agonist_admin = opioid_agonist_admin) %>%
    dplyr::mutate(opioid_pain_admin = opioid_pain_admin) %>%
    dplyr::mutate(benzodiazepine_admin = benzodiazepine_admin) %>%
    dplyr::mutate(epinephrine_admin = epinephrine_admin) %>%
    dplyr::mutate(response_to_admin = response_to_admin) %>%
    dplyr::mutate(opioid_agonist_success = ifelse(opioid_agonist_admin == TRUE & response_to_admin == TRUE, 1, 0)) %>%
    dplyr::mutate(drug_related_pi = drug_related_pi) %>%
    dplyr::mutate(traumatic_injury_pi = traumatic_injury_pi) %>%
    dplyr::mutate(cardiac_pi = cardiac_pi) %>%
    dplyr::mutate(age_adj = age_adj) %>%
    dplyr::group_by(!!dplyr::sym(event_id_name),
                    !!dplyr::sym(patient_first_name),
                    !!dplyr::sym(patient_last_name),
                    !!dplyr::sym(patient_age_name),
                    !!dplyr::sym(primary_impression_name),
                    .data$primary_complaint_adj,
                    !!dplyr::sym(primary_complaint_name),
                    !!dplyr::sym(care_narrative_name),
                    .data$care_narrative_adj,
                    .data$person_event_id) %>%
    dplyr::summarise(opioid_agonist_admin = sum(.data$opioid_agonist_admin, na.rm = TRUE),
                     benzodiazepine_admin = sum(.data$benzodiazepine_admin, na.rm = TRUE),
                     epinephrine_admin = sum(.data$epinephrine_admin, na.rm = TRUE),
                     opioid_agonist_success = sum(.data$opioid_agonist_success, na.rm = TRUE),
                     drug_related_pi = max(.data$drug_related_pi, na.rm = TRUE),
                     traumatic_injury_pi = max(.data$traumatic_injury_pi, na.rm = TRUE),
                     cardiac_pi = max(.data$cardiac_pi, na.rm = TRUE),
                     age_adj = mean(.data$age_adj, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cardiac_flag = ifelse(.data$cardiac_pi == 1 & .data$opioid_agonist_success == 0, 1, 0)) %>%
    dplyr::mutate(cardiac_flag = ifelse(is.na(.data$cardiac_flag) == TRUE, 0, .data$cardiac_flag)) %>%
    dplyr::mutate(age_flag = ifelse((.data$age_adj < 18 | .data$age_adj > 70) & .data$opioid_agonist_success == 0, 1, 0)) %>%
    dplyr::mutate(age_flag = ifelse(is.na(.data$age_flag) == TRUE, 0, .data$age_flag)) %>%
    dplyr::select(-.data$age_adj, -.data$cardiac_pi)

  message(paste0("There are now ", nrow(formatted_data), " unique patient records. \n ---"))
  return(formatted_data)

}



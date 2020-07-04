
remove_stop_words <- function(str, stop_words, remove_numbers = TRUE) {

  if(is.na(str) == TRUE) {

    x <- NA

  } else {

    if(remove_numbers == TRUE){

      x <- unlist(strsplit(str, " "))
      x <- paste(x[!x %in% stop_words], collapse = " ")
      x <- str_trim(gsub("[^[:alpha:] ]", "", x))
      x <- str_squish(x)
      tolower(x)

    } else {

      x <- unlist(strsplit(str, " "))
      x <- paste(x[!x %in% stop_words], collapse = " ")
      x <- str_trim(gsub("[^[:alnum:] ]", "", x))
      x <- str_squish(x)
      tolower(x)

    }


  }

  return(x)

}

make_one_hot <- function(row_data_in,
                         word_tokens = words,
                         bigram_tokens = bigrams,
                         trigram_tokens = trigrams){

  if(is.na(row_data_in) == TRUE) {

    return(c(rep(0, length(words)), rep(0, length(bigrams)), rep(0, length(trigrams))))

  } else {

    word_counts <- str_count(row_data_in, word_tokens)
    bigram_counts <- str_count(row_data_in, bigram_tokens)
    trigram_counts <- str_count(row_data_in, trigram_tokens)
    return(c(word_counts, bigram_counts, trigram_counts))

  }

}



one_hot_single_row_ems_data <- function(data_in,
                                        col_to_one_hot = "care_narrative_adj",
                                        words = pull(select(filter(high_value_terms, type == "word"), token)),
                                        bigrams = pull(select(filter(high_value_terms, type == "bigram"), token)),
                                        trigrams = pull(select(filter(high_value_terms, type == "trigram"), token))) {

  message("Tokenizing narrative and one-hot encoding high value terms. \n --- ")
  one_hot <- data.frame(t(apply(data_in[, which(names(data_in) == col_to_one_hot)], 1, function(x) make_one_hot(row_data_in = x,
                                                                                                                word_tokens = words,
                                                                                                                bigram_tokens = bigrams,
                                                                                                                trigram_tokens = trigrams))))
  names(one_hot) <- c(words, bigrams, trigrams)
  return(bind_cols(data_in, one_hot))

}


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
                                    stop_words = tidytext::stop_words) {

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
    select(cols_to_keep) %>%
    mutate(care_narrative_adj = care_narrative_adj) %>%
    mutate(primary_complaint_adj = primary_complaint_adj) %>%
    group_by(!!sym(event_id_name),
             !!sym(patient_first_name),
             !!sym(patient_last_name),
             !!sym(patient_age_name),
             !!sym(primary_impression_name),
             !!sym(primary_complaint_name),
             !!sym(care_narrative_name)) %>%
    mutate(person_event_id = paste0("idx-", group_indices())) %>%
    ungroup() %>%
    unique()

  message("Tabulating medication administrations and responses  \n ---")
  opioid_agonist_admin <- apply(formatted_data, 1, function(x) grepl("naloxone|narcan", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
  opioid_pain_admin <- apply(formatted_data, 1, function(x) grepl("morphine|fentanyl", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
  benzodiazepine_admin <- apply(formatted_data, 1, function(x) grepl("midozolam|diazepam", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
  epinephrine_admin <- apply(formatted_data, 1, function(x) grepl("epi ", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
  response_to_admin <- apply(formatted_data, 1, function(x) grepl("improved", x[which(names(formatted_data) == medication_response_name)], ignore.case = TRUE))

  formatted_data <- formatted_data %>%
    mutate(opioid_agonist_admin = opioid_agonist_admin) %>%
    mutate(opioid_pain_admin = opioid_pain_admin) %>%
    mutate(benzodiazepine_admin = benzodiazepine_admin) %>%
    mutate(epinephrine_admin = epinephrine_admin) %>%
    mutate(response_to_admin = response_to_admin) %>%
    mutate(opioid_agonist_success = ifelse(opioid_agonist_admin == TRUE & response_to_admin == TRUE, 1, 0)) %>%
    group_by(!!sym(event_id_name),
             !!sym(patient_first_name),
             !!sym(patient_last_name),
             !!sym(patient_age_name),
             !!sym(primary_impression_name),
             primary_complaint_adj,
             !!sym(primary_complaint_name),
             !!sym(care_narrative_name),
             care_narrative_adj,
             person_event_id) %>%
    summarise(opioid_agonist_admin = sum(opioid_agonist_admin, na.rm = TRUE),
              benzodiazepine_admin = sum(benzodiazepine_admin, na.rm = TRUE),
              epinephrine_admin = sum(epinephrine_admin, na.rm = TRUE),
              opioid_agonist_success = sum(opioid_agonist_success, na.rm = TRUE))

  #formatted_data <- bind_cols(formatted_data, one_hot_single_row_ems_data(data_in = formatted_data))

  message(paste0("There are now ", nrow(formatted_data), " unique patient records. \n ---"))
  return(formatted_data)

}



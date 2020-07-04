library(tidyverse)
library(tidytext)
data_in <- read_csv("C:/Users/anpat/OneDrive/Desktop/masterEMS.csv")

event_id_name = "CAD Incident Number (eResponse.03)"
patient_first_name = "Patient First Name (ePatient.03)"
patient_last_name = "Patient Last Name (ePatient.02)"
patient_age_name = "Patient Age (ePatient.15)"
event_date_name = "Incident Date Time"
primary_impression_name = "Situation Provider Primary Impression (eSituation.11)"
primary_complaint_name = "Situation Primary Complaint Statement List (eSituation.04)"
care_narrative_name = "Patient Care Report Narrative (eNarrative.01)"
medication_given_name = "Medication Given Description (eMedications.03)"
medication_response_name = "Medication Response (eMedications.07)"


cols_to_keep <- c(event_id_name,
                  patient_first_name,
                  patient_last_name,
                  patient_age_name,
                  primary_impression_name,
                  primary_complaint_name,
                  care_narrative_name,
                  medication_given_name,
                  medication_response_name)

care_narrative_adj <- tolower(unlist(apply(data_in[, which(names(data_in) == care_narrative_name)],
                                           MARGIN = 1,
                                           function(x) remove_stop_words(x, stop_words = tidytext::stop_words,
                                                                         remove_numbers = TRUE))))
primary_complaint_adj <- tolower(unlist(apply(data_in[, which(names(data_in) == primary_complaint_name)],
                                              MARGIN = 1,
                                              function(x) remove_stop_words(x, stop_words = tidytext::stop_words,
                                                                            remove_numbers = TRUE))))

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
  unique()

opioid_agonist_admin <- apply(formatted_data, 1, function(x) grepl("naloxone|narcan", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
opioid_pain_admin <- apply(formatted_data, 1, function(x) grepl("morphine|fentanyl", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
benzodiazepine_admin <- apply(formatted_data, 1, function(x) grepl("midozolam|diazepam", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
epinephrine_admin <- apply(formatted_data, 1, function(x) grepl("epi ", x[which(names(formatted_data) == medication_given_name)], ignore.case = TRUE))
response_to_admin <- apply(formatted_data, 1, function(x) grepl("improved", x[which(names(formatted_data) == medication_response_name)], ignore.case = TRUE))

formatted_data <- formatted_data %>%
  ungroup() %>%
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

dat <- read_csv("C:/Users/anpat/OneDrive/Desktop/masterEMS.csv")[1:1000, ]

result <- format_multirow_ems_data(data_in = dat)

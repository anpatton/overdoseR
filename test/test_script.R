library(tidyverse)
library(tidytext)
dat <- read_csv("C:/Users/anpat/OneDrive/Desktop/masterEMS.csv")[1:1000, ]

high_value_terms <- read_csv("high_value_terms.csv")

result <- format_multirow_ems_data(data_in = dat)

one_hot <- one_hot_single_row_ems_data(data_in = result)

training_data <- one_hot[1:600, ] %>%
  mutate(opioid = sample(c(0, 1), size = 600, replace = TRUE, prob = c(0.75, 0.25)))

testing_data <- one_hot[601:715, ] %>%
  mutate(opioid = sample(c(0, 1), size = 115, replace = TRUE, prob = c(0.75, 0.25)))

log_res <- tune_logistic(training_data = training_data,
                         testing_data = testing_data,
                         save_model = FALSE)

predict(log_res[['model']], newdata = testing_data)

svm_res <- tune_svm(training_data = training_data,
                         testing_data = testing_data,
                         save_model = FALSE,
                         kernel_type = "linear")

predict(svm_res[['model']], newdata = testing_data)

xgb_res <- tune_xgboost(training_data = training_data,
                        testing_data = testing_data,
                        save_model = FALSE)



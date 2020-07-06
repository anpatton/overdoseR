
# What does overdoseR do?

This package is based entirely on a system outlined in the following talks and presentations.

*University of Miami Department of Public Health Sciences. Distinguished Lecture Series. Machine Learning and EMS Data for Opioid Overdose Surveillance. February 2020. Patton, A.*

*Development of Text-Based Algorithm for Opioid Overdose Identification in EMS Data. International Society for Disease Surveillance Annual Meeting 2019. Oral Presentation. Patton, A.*

*Preventing the Next Overdose: An Emergency Medical Services-Based Non-Fatal Opioid Overdose Surveillance and Telephone Outreach Pilot Program. Council of State and Territorial Epidemiologists Annual Meeting 2019. Breakout Presentation. Arambula, K., Hannah, H., Patton, A., Willis, M., Ereman, R.*

*Cost-effectiveness of Offering Treatment to Non-Fatal Opioid Overdoses Encountered by Emergency Medical Services (EMS) in Marin County, California. Council of State and Territorial Epidemiologists Annual Meeting 2019. Breakout Presentation. Hannah, H., Arambula, K., Patton, A., Hansen, R. Willis, M., Ereman, R.*

Briefly, it is designed to serve as the predictive modeling component for an in-house surveillance system for opioid related events in EMS (911) data. The intended audience is local government health departments or academic researchers. The model takes in NEMSIS 3.0 EMS data and predicts whether an event was or was not an opioid related event. This methodology was proven to be over 99% specific and over 95% sensitive in the program that ran in Marin County, California over 2017-2019. This model can work retrospectively, identifying a historical baseline, or prospectively, predicting records as they come in. 

# How does overdoseR work?

In general, overdoseR works through a combination of several factors built primarily on the contents of the care narrative, medication administered, and primary impression fields. 

## 1) High Value Terms 

Using information learned during system development in Marin County, a group of several hundred words, bigrams (pairs of words), and trigrams (triplets of words) were identified to be broadly more common in opioid related events than non-opioid related events. These terms serve as the main features in the models based on their presence or absence in the care narratives.

## 2) Patient Care

Information such as medication administered and the response to the medication is also included in the model. Specifically naloxone, morhpine, fentanyl, and midazolam administrations are encoded as features and naloxone response is coded as a feature.

## 3) Lessons Learned

Based on manual review of tens of thousands of EMS records, certain rule based clinical criteria were found to be high effectively in increasing model accuracy. 

1. If an event had a cardiac related primary impression and did not have a positive naloxone response, it was coded as not an opioid related event

1.	If an event occurred in a patient under the age of 18 or over the age of 70 without a positive naloxone response, it was coded as not an opioid related event

1.	If an event had a positive naloxone response, it was coded as an opioid related event regardless of other information

# How do I use overdoseR

In order to use overdoseR, you will need EMS data (ideally NEMSIS 3.0) that contains at a minimum the following columns.

1. "CAD Incident Number (eResponse.03)"
1. "Patient First Name (ePatient.03)"
1. "Patient Last Name (ePatient.02)"
1. "Patient Age (ePatient.15)"
1. "Situation Provider Primary Impression (eSituation.11)"
1. "Situation Primary Complaint Statement List (eSituation.04)"
1. "Patient Care Report Narrative (eNarrative.01)"
1. "Medication Given Description (eMedications.03)"
1. "Medication Response (eMedications.07)"

You will also need a column that indicates whether or not an event was an opioid related event/overdose based on your own internal clinical criteria. For example, when this system was deployed in Marin County, your package author read and manually classified approximately 30,000 individual records to create a pool of training and testing data.

The first step is to use the following function. As written this accepts all the defaults for the column names (see above), but you can provide your own as well.

```{r, echo = TRUE, eval = FALSE}
formatted_result <- format_multirow_ems_data(data_in = your_raw_EMS_data)
```

This converts multiple row per event data into single row per event data while at the same time creating and adjusting fields to prepare for modeling. The output is a dataframe, so make sure to assign it to something when you call the function. The next step before modeling is to take all the high value terms and turn the data into a [one-hot matrix](https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f) of their presence or abscence.

```{r, echo = TRUE, eval = FALSE}
one_hot_result <- one_hot_single_row_ems_data(data_in = formatted_result)
```

After that step, the data is ready for modeling. There are currently three options for classification built into overdoseR. There is logistic regression, a [support vector machine (SVM)](https://en.wikipedia.org/wiki/Support_vector_machine), and [XGBoost](https://xgboost.ai/about). Each function is used in the same manner with the same syntax more or less. It is important to note that each of the following functions has slightly different arugments. You can read the documentation if you want to be able to understand more about the specifics of each parameter.

You are encouraged to split the data into training and test sets in whatever manner you choose following best practices for randomization and reproducibility. 

```{r, echo = TRUE, eval = FALSE}
log_res <- tune_logistic(training_data = one_hot_result_training,
                         testing_data = one_hot_result_testing)

svm_res <- tune_svm(training_data = one_hot_result_training,
                    testing_data = one_hot_result_testing)

xgb_res <- tune_xgboost(training_data = one_hot_result_training,
                        testing_data = one_hot_result_testing)
```

## Logistic Regression

The logistic regression model is fit with a `glm(family = "binomial")` and returns that object. The model results are not exponentiated. The returned result is a list with the model object and the predictions on the test data.

## Support Vector Machine

The SVM is fit using `e1071::tune` and `e1071::svm`. More information about how `e1071` works can be found [here](https://cran.r-project.org/web/packages/e1071/index.html). 

## XGBoost

The XGBoost model is tuned and fir using a combination of functions from `xgboost`, `mlr`, and `ParamHelpers`. In order to use the saved model properly on additional test data, you will need to run the following code (or something similar) on the dataframe to be used. The returned result is a list with the model object, tuning object, and the predictions on the test data.

```{r, eval = FALSE, echo = TRUE}

  new_testing_data <- new_testing_data %>%
    dplyr::select(features_that_you_want) %>%
    dplyr::mutate_all(as.integer)

```

### Very important note: the predictions are in terms of probability. It is paramount that you, the user, optimize the threshold for class prediction based on your clinical goals. It is extremely strongly recommended not to use a naive 0.5 threshold. 

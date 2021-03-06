% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeling.R
\name{tune_svm}
\alias{tune_svm}
\title{Train and test Supper Vector Machine classifier.}
\usage{
tune_svm(
  training_data,
  testing_data,
  save_model = TRUE,
  model_name = paste0("opioid_svm_model_", Sys.Date()),
  target = "opioid",
  features = "defaults",
  kernel_type = c("radial", "linear"),
  cost_grid = c(0.01, 0.1, 1, 10, 100)
)
}
\arguments{
\item{training_data}{formatted EMS data for model training}

\item{testing_data}{formatted EMS data for model training}

\item{save_model}{Logical flag to save model as .RDS object}

\item{model_name}{Filepath and name of model to save, .RDS extension not needed}

\item{target}{Binary (0/1) for class labels}

\item{features}{Character vector of features, "default" for recommended options}

\item{kernel_type}{Radial or linear kernel for SVM}

\item{cost_grid}{Vector of cost parameters to tune over}
}
\value{
List object with model and test data with predictions
\itemize{
  \item model - Fitted GLM object
  \item results - Test data with predictions
  \item tune_object - SVM tuning object
}
}
\description{
Train and test Supper Vector Machine classifier.
}
\examples{
\dontrun{tune_svm(training_data = train, testing_data = test)}
}

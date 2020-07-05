library(tidyverse)
library(tidytext)
dat <- read_csv("C:/Users/anpat/OneDrive/Desktop/masterEMS.csv")[1:1000, ]

high_value_terms <- read_csv("high_value_terms.csv")

result <- format_multirow_ems_data(data_in = dat)

one_hot <- one_hot_single_row_ems_data(data_in = result)

target <- "opioid"

features <- c("opioid_agonist_admin", "benzodiazepine_admin",
  "epinephrine_admin", "opioid_agonist_success", "drug_related_pi",
  "traumatic_injury_pi", "attempt", "booking", "bottles", "boyfriend",
  "cocaine", "deputies", "drug", "empty", "fetanyl", "harm", "herion",
  "heroin", "heroine", "hydrocodone", "ineffective", "inform",
  "ingestion", "injecting", "intent", "knowledge", "lorazepam",
  "meth", "methadone", "narcan", "narcotic", "norco", "npa", "opiates",
  "opiods", "overdose", "overdosing", "oxycodone", "oxycontin",
  "paraphernalia", "pill", "pills", "pin", "pinpoint", "powder",
  "pupils", "respirations", "snoring", "suboxone", "substance",
  "taking", "thrashing", "tta", "unresponsive", "use", "white",
  "xanax", "administered narcan", "alcohol drugs", "appeared responsive",
  "bed unresponsive", "bedroom pt", "boyfriend denied", "boyfriend stated",
  "cardiac arrest", "cc intentional", "cell pt", "coming mouth",
  "denies taking", "drug paraphernalia", "drug use", "easily arousable",
  "eyes fixed", "fentanyl patch", "floor breathing", "floor room",
  "found inside", "found pinpoint", "found unresponsive", "front desk",
  "gcs custody", "gw medic", "heroin cocaine", "heroin pt", "heroin use",
  "inform patient", "inside cell", "intent harm", "male who", "medication bottles",
  "medications alcohol", "methadone clinic", "methadone pt", "mg methadone",
  "mg narcan", "narcan administered", "narcan administration",
  "narcan given", "narcan improvement", "narcan iv", "narcan pt",
  "oriented events", "overdose pt", "painful stimuli", "paraphernalia found",
  "paraphernalia scene", "patent airway", "patient overdosing",
  "pd informs", "pin pupils", "pinpoint pupils", "positive respiratory",
  "post narcan", "potential overdose", "prison staff", "pt heroin",
  "pt medication", "pt narcan", "pt slowly", "pt unresponsive",
  "pts girlfriend", "pts respirations", "pupils found", "pupils pinpoint",
  "pupils pt", "purposeful movements", "rate drive", "respiratory rate",
  "respond questions", "responders the", "response to", "responsive pt",
  "s bg", "snoring respirations", "t medic", "taking today", "the pt",
  "tolerate npa", "total mg", "units arrived", "white powder",
  "administered mg narcan", "all times approximate", "ao gcs custody",
  "appeared responsive pt", "boyfriend stated pt", "complaints head toe",
  "denies complaints head", "denies taking medications", "denies taking today",
  "drug paraphernalia found", "established total mg", "eyes fixed dilated",
  "found inside cell", "found scene pt", "gurney ambulance obtained",
  "handed nurse change", "incident gw medic", "kill herself pt",
  "lying supine floor", "m scene yo", "male who was", "narcan administration pt",
  "narcan given pt", "no change patient", "noted physical assessment",
  "oral trauma pt", "patent airway pt", "pinpoint pupils pt", "pt administered narcan",
  "pt appeared responsive", "pt condition improved", "pt denies etoh",
  "pt found unresponsive", "pt loaded flat", "pt positive respiratory",
  "pt reports taking", "pt respiratory rate", "pt slow respond",
  "report handed nurse", "respiratory rate drive", "responders the pt",
  "revealed patient agitated", "room the pt", "signs oral trauma",
  "sinus tach pt", "slow respond questions", "stable pt care",
  "times approximate arrived", "total mg narcan", "transport full pt",
  "trauma assessment pt", "was able to", "yo male who")

features <- c(target, features)
training_data <- one_hot[1:600, ] %>%
  mutate(opioid = sample(c(0, 1), size = 600, replace = TRUE, prob = c(0.75, 0.25))) %>%
  select(features) %>%
  mutate_all(as.integer)

colnames(training_data) <- gsub(" ", "_", colnames(training_data))

testing_data <- one_hot[601:715, ] %>%
  mutate(opioid = sample(c(0, 1), size = 115, replace = TRUE, prob = c(0.75, 0.25))) %>%
  select(features) %>%
  mutate_all(as.integer)

colnames(testing_data) <- gsub(" ", "_", colnames(testing_data))

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
rdesc <- mlr::makeResampleDesc("CV", stratify = TRUE, iters = 5L)

#search strategy
ctrl <- mlr::makeTuneControlRandom(maxit = 50)

mod_tune <- mlr::tuneParams(learner = learner,
                            task = train_task,
                            resampling = rdesc,
                            par.set = params,
                            control = ctrl,
                            show.info = TRUE)




tuned_learner <- mlr::setHyperPars(learner, par.vals = mod_tune$x)

xgmodel <- mlr::train(learner = tuned_learner,
                      task = train_task)

xgpred <- predict(xgmodel, test_task)

pred_s$data$prob.1

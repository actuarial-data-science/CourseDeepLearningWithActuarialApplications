library(mgcv)
library(keras)
library(magrittr)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(gridExtra)
library(splitTools)
library(tidyr)
library(OpenML)
library(farff)

options(encoding = 'UTF-8')

# set seed to obtain best reproducibility. note that the underlying architecture may affect results nonetheless, so full reproducibility cannot be guaranteed across different platforms.
seed <- 100
Sys.setenv(PYTHONHASHSEED = seed)
set.seed(seed)
reticulate::py_set_seed(seed)
tensorflow::tf$random$set_seed(seed)

# Poisson deviance
PoissonDeviance <- function(pred, obs) {
    200 * (sum(pred) - sum(obs) + sum(log((obs/pred)^(obs)))) / length(pred)
}

# Local loading
# you can download it from https://github.com/JSchelldorfer/DeepLearningWithActuarialApplications/blob/master/freMTPL2freq.RData
load("freMTPL2freq.RData")

# download (only if company firewall allows it)
#freMTPL2freq <- getOMLDataSet(data.id = 41214)$data

# Grouping id
distinct <- freMTPL2freq %>% 
  distinct_at(vars(-c(IDpol, Exposure, ClaimNb))) %>% 
  mutate(group_id = row_number())

dat <- freMTPL2freq %>% 
  left_join(distinct) %>% 
  mutate(ClaimNb = pmin(as.integer(ClaimNb), 4),
         VehAge = pmin(VehAge,20),
         DrivAge = pmin(DrivAge,90),
         BonusMalus = pmin(BonusMalus,150),
         Density = round(log(Density),2),
         VehGas = factor(VehGas),
         Exposure = pmin(Exposure, 1))

# Group sizes of suspected clusters
table(table(dat[, "group_id"]))

# initialize table to store all model results for comparison
df_cmp <- tibble(
 model = character(),
 epochs = numeric(),
 run_time = numeric(),
 parameters = numeric(),
 in_sample_loss = numeric(),
 out_sample_loss = numeric(),
 avg_freq = numeric(),
)

# MinMax scaler
preprocess_minmax <- function(varData) {
  X <- as.numeric(varData)
  2 * (X - min(X)) / (max(X) - min(X)) - 1
}

# Dummy coding 
preprocess_catdummy <- function(data, varName, prefix) {
  varData <- data[[varName]]
  X <- as.integer(varData)
  n0 <- length(unique(X))
  n1 <- 2:n0
  addCols <- purrr::map(n1, function(x, y) {as.integer(y == x)}, y = X) %>%
    rlang::set_names(paste0(prefix, n1))
  cbind(data, addCols)
}

# Feature pre-processing using MinMax Scaler and Dummy Coding
preprocess_features <- function(data) {
  data %>%
    mutate_at(
      c(AreaX = "Area", VehPowerX = "VehPower", VehAgeX = "VehAge",
        DrivAgeX = "DrivAge", BonusMalusX = "BonusMalus", DensityX = "Density"),
      preprocess_minmax
    ) %>%
    mutate(
      VehGasX = as.integer(VehGas) - 1.5
    ) %>%
    preprocess_catdummy("VehBrand", "Br") %>%
    preprocess_catdummy("Region", "R")
}


dat2 <- preprocess_features(dat)

head(dat2)

str(dat2)

summary(dat2)

ind <- partition(dat2[["group_id"]], p = c(train = 0.8, test = 0.2), 
                 seed = seed, type = "grouped")
train <- dat2[ind$train, ]
test <- dat2[ind$test, ]

# size of train/test
sprintf("Number of observations (train): %s", nrow(train))
sprintf("Number of observations (test): %s", nrow(test))

# Claims frequency of train/test
sprintf("Empirical frequency (train): %s", round(sum(train$ClaimNb) / sum(train$Exposure), 4))
sprintf("Empirical frequency (test): %s", round(sum(test$ClaimNb) / sum(test$Exposure), 4))

# select the feature space
col_start <- ncol(dat) + 1
col_end <- ncol(dat2)
features <- c(col_start:col_end) # select features, be careful if pre-processing changes
print(colnames(train[, features]))

# feature matrix
Xtrain <- as.matrix(train[, features])  # design matrix training sample
Xtest <- as.matrix(test[, features])    # design matrix test sample

# available optimizers for keras
# https://keras.io/optimizers/
optimizers <- c('sgd', 'adagrad', 'adadelta', 'rmsprop', 'adam', 'adamax', 'nadam')

# homogeneous model (train)
lambda_hom <- sum(train$ClaimNb) / sum(train$Exposure)

# define network and load pre-specified weights
q0 <- length(features)                  # dimension of features

sprintf("Neural network with K=0 hidden layer")
sprintf("Input feature dimension: q0 = %s", q0)
sprintf("Output dimension: %s", 1)

# set seeds for reproducability of model fit
# needs to be set before the keras_model function!
Sys.setenv(PYTHONHASHSEED = seed)
set.seed(seed)
reticulate::py_set_seed(seed)
tensorflow::tf$random$set_seed(seed)

Design  <- layer_input(shape = c(q0), dtype = 'float32', name = 'Design') 
LogVol  <- layer_input(shape = c(1), dtype = 'float32', name = 'LogVol')

Network <- Design %>%
  layer_dense(units = 1, activation = 'linear', name = 'Network',
              weights = list(array(0, dim = c(q0, 1)), array(log(lambda_hom), dim = c(1))))

Response <- list(Network, LogVol) %>%
  layer_add(name = 'Add') %>%
  layer_dense(units = 1, activation = k_exp, name = 'Response', trainable = FALSE,
              weights = list(array(1, dim = c(1, 1)), array(0, dim = c(1))))

model_glm <- keras_model(inputs = c(Design, LogVol), outputs = c(Response))

model_glm %>% compile(
  loss = 'poisson',
  optimizer = optimizers[7]
)

summary(model_glm)

# set hyperparameters
epochs <- 2800
batch_size <- nrow(Xtrain)
validation_split <- 0 # set to >0 to see train/validation loss in plot(fit)
verbose <- 1

# expected run-time on Renku 8GB environment around 70 seconds
exec_time <- system.time(
  fit <- model_glm %>% fit(
    list(Xtrain, as.matrix(log(train$Exposure))), as.matrix(train$ClaimNb),
    epochs = epochs,
    batch_size = batch_size,
    validation_split = validation_split,
    verbose = verbose,
    callbacks = list(callback_early_stopping(patience=5))
  )
)
exec_time[1:5]

plot(fit)

# calculating the predictions
train$fitglmNN <- as.vector(model_glm %>% predict(list(Xtrain, as.matrix(log(train$Exposure)))))
test$fitglmNN <- as.vector(model_glm %>% predict(list(Xtest, as.matrix(log(test$Exposure)))))

# average in-sample and out-of-sample losses (in 10^(-2))
sprintf("100 x Poisson deviance shallow network (train): %s", PoissonDeviance(train$fitglmNN, train$ClaimNb))
sprintf("100 x Poisson deviance shallow network (test): %s", PoissonDeviance(test$fitglmNN, test$ClaimNb))

# average frequency
sprintf("Average frequency (test): %s", round(sum(test$fitglmNN) / sum(test$Exposure), 4))

trainable_params <- sum(unlist(lapply(model_glm$trainable_weights, k_count_params)))
df_cmp %<>% bind_rows(
  data.frame(model = "GLM with keras", epochs = epochs,
             run_time = round(exec_time[[3]], 0), parameters = trainable_params,
             in_sample_loss = round(PoissonDeviance(train$fitglmNN, train$ClaimNb), 4),
             out_sample_loss = round(PoissonDeviance(test$fitglmNN, test$ClaimNb), 4),
             avg_freq = round(sum(test$fitglmNN) / sum(test$Exposure), 4)
  ))

df_cmp

exec_time <- system.time(
  glm1 <- glm(ClaimNb ~ AreaX + VehPowerX + VehAgeX + DrivAgeX + BonusMalusX + DensityX + VehGasX + VehBrand + Region,
              data = train, offset = log(Exposure), family = poisson())
)
exec_time[1:5]
summary(glm1)

# Predictions
train$fitGLM1 <- fitted(glm1)
test$fitGLM1 <- predict(glm1, newdata = test, type = "response")
dat$fitGLM1 <- predict(glm1, newdata = dat2, type = "response")

# in-sample and out-of-sample losses (in 10^(-2))
sprintf("100 x Poisson deviance GLM (train): %s", PoissonDeviance(train$fitGLM1, train$ClaimNb))
sprintf("100 x Poisson deviance GLM (test): %s", PoissonDeviance(test$fitGLM1, test$ClaimNb))

# Overall estimated frequency
sprintf("average frequency (test): %s", round(sum(test$fitGLM1) / sum(test$Exposure), 4))

df_cmp %<>% bind_rows(
  data.frame(model = "GLM with glm", epochs = NA, run_time = round(exec_time[[3]], 0), parameters = length(coef(glm1)),
             in_sample_loss = round(PoissonDeviance(train$fitGLM1, as.vector(unlist(train$ClaimNb))), 4),
             out_sample_loss = round(PoissonDeviance(test$fitGLM1, as.vector(unlist(test$ClaimNb))), 4),
             avg_freq = round(sum(test$fitGLM1) / sum(test$Exposure), 4))
)
df_cmp

plot_claims_freq <- function(xvar, yvar, xlab, ylab) {
  axis_min <- log(max(test[[xvar]], test[[yvar]]))
  axis_max <- log(min(test[[xvar]], test[[yvar]]))
  
  ggplot(test, aes(x = log(!!sym(xvar)), y = log(!!sym(yvar)), colour = Exposure)) + geom_point() +
    geom_abline(colour = "#000000", slope = 1, intercept = 0) +
    xlim(axis_max, axis_min) + ylim(axis_max, axis_min) +
    labs(x = xlab, y = ylab, title = "Claims frequency prediction (log-scale)") +
    scale_colour_gradient(low = "green", high = "red")
}

plot_claims_freq("fitglmNN", "fitGLM1", "glmNN", "GLM")

df_coef <- data.frame(glm=coef(glm1),keras=c(get_weights(model_glm)[[2]],get_weights(model_glm)[[1]]))

ggplot(df_coef,aes(x=glm,y=keras)) + geom_point(size=2) + geom_abline(intercept=0, slope=1, col="green")

head(df_coef, n=10)

sessionInfo()

reticulate::py_config()

tensorflow::tf_version()

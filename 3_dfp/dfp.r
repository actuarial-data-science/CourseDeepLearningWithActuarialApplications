## -----------------------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(gridExtra)
library(repr)  # not needed in the Rmarkdown version, only for Jupyter notebook


## -----------------------------------------------------------------------------
# plotting parameters in R Markdown notebook
knitr::opts_chunk$set(fig.width = 18, fig.height = 9)
# plotting parameters in Jupyter-notebook
# https://stackoverflow.com/questions/42729049/how-to-change-the-size-of-r-plots-in-jupyter
options(repr.plot.width = 18, repr.plot.height = 9)


## -----------------------------------------------------------------------------
ylims <- c(0.14, 0.33)
ylims_freq <- c(0, 0.33)
# random seed for reproducibility
seed <- 100


## -----------------------------------------------------------------------------
plot_prices <- function(data, title, legend_title, y_label,
                        labels, linetypes, colors) {
  data_gg <- gather(df, key = price_type, value = price, -ages)

  ggplot(data_gg, aes(x = ages, y = price, group = price_type,
                      colour = price_type, linetype = price_type)) +
    geom_line(size = 1) +
    scale_linetype_manual(labels = labels, values = linetypes, name = legend_title) +
    scale_color_manual(labels = labels, values = colors, name = legend_title) +
    labs(title = title, x = "age", y = y_label) +
    theme(legend.position = c(0.75, 0.87), legend.text = element_text(size = 12),
          text = element_text(size = 15), legend.key.width = unit(1.5, "cm"))
}


## -----------------------------------------------------------------------------
ages <- c(15:80)
nAges <- length(ages)


## -----------------------------------------------------------------------------
claimCost <- c(0.5, 0.9, 0.1)


## -----------------------------------------------------------------------------
alpha0 <- c(-40, 38.5)
beta0 <- c(-2, 0.004, 0.1, 0.2)
gamma0 <- c(-2, 0.01)


## -----------------------------------------------------------------------------
price_true_best_estimate <- array(NA, c(nAges, 4), dimnames = list(NULL, NULL))
colnames(price_true_best_estimate) <- c("woman-non-smoker", "woman-smoker",
                                        "man-non-smoker", "man-smoker")

# woman and non-smoker
price_true_best_estimate[, "woman-non-smoker"] <-
  claimCost[1] * exp(alpha0[1] + alpha0[2]*(ages >= 20)*(ages <= 40)) +
  claimCost[2] * exp(beta0[1] + beta0[2]*ages + beta0[4]) +
  claimCost[3] * exp(gamma0[1] + gamma0[2]*ages)

# woman and smoker
price_true_best_estimate[, "woman-smoker"] <-
  claimCost[1] * exp(alpha0[1] + alpha0[2]*(ages >= 20)*(ages <= 40)) +
  claimCost[2] * exp(beta0[1] + beta0[2]*ages + beta0[3] + beta0[4]) +
  claimCost[3] * exp(gamma0[1] + gamma0[2]*ages)

# man and non-smoker
price_true_best_estimate[, "man-non-smoker"] <-
  claimCost[1] * exp(alpha0[1]) +
  claimCost[2] * exp(beta0[1] + beta0[2]*ages) +
  claimCost[3] * exp(gamma0[1] + gamma0[2]*ages)

# man and smoker
price_true_best_estimate[, "man-smoker"] <-
  claimCost[1] * exp(alpha0[1]) +
  claimCost[2] * exp(beta0[1] + beta0[2]*ages + beta0[3]) +
  claimCost[3] * exp(gamma0[1] + gamma0[2] * ages)


## -----------------------------------------------------------------------------
summary(price_true_best_estimate)


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <- c("best-estimate price (women)", "best-estimate price (men)")
linetypes <- c("solid", "solid")
colors <- c("blue", "black")

# Smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"])

p2 <- plot_prices(df, title = "Non-Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
womanProp <- 0.45


## -----------------------------------------------------------------------------
price_true_disc_free <- array(NA, c(nAges, 2), dimnames = list(NULL, NULL))
colnames(price_true_disc_free) <- c("non-smoker", "smoker")
# non-smoker non-discriminating
price_true_disc_free[, "non-smoker"] <-
  womanProp * price_true_best_estimate[, "woman-non-smoker"] +
  (1 - womanProp) * price_true_best_estimate[, "man-non-smoker"]
# smoker non-discriminating
price_true_disc_free[, "smoker"] <-
  womanProp * price_true_best_estimate[, "woman-smoker"] +
  (1 - womanProp) * price_true_best_estimate[, "man-smoker"]


## -----------------------------------------------------------------------------
summary(price_true_disc_free)


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <- c("best-estimate price (women)", "best-estimate price (men)",
            "discrimination-free price")
linetypes <- c("solid", "solid", "longdash")
colors <- c("blue", "black", "green")

# Smokers
df <- data.frame(ages, be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"],
                 df = price_true_disc_free[, "smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"],
                 df = price_true_disc_free[, "non-smoker"])

p2 <- plot_prices(df, title = "Non-smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
smokerProp <- 0.3
womanSmokerProp <- 0.8


## -----------------------------------------------------------------------------
# woman conditional on non-smoking
womanNonSmokerProp <- (womanProp - womanSmokerProp * smokerProp) / (1 - smokerProp)

price_true_unaware <- array(NA, c(nAges, 2), dimnames = list(NULL, NULL))
colnames(price_true_unaware) = c("non-smoker", "smoker")
# non-smoker conditional
price_true_unaware[, "non-smoker"] <-
  womanNonSmokerProp * price_true_best_estimate[, "woman-non-smoker"] +
  (1 - womanNonSmokerProp) * price_true_best_estimate[, "man-non-smoker"]
# smoker conditional
price_true_unaware[, "smoker"] <-
  womanSmokerProp * price_true_best_estimate[, "woman-smoker"] +
  (1 - womanSmokerProp) * price_true_best_estimate[, "man-smoker"]


## -----------------------------------------------------------------------------
summary(price_true_unaware)


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <- c("best-estimate price (women)", "best-estimate price (men)",
            "discrimination-free price", "unawareness price")
linetypes <- c("solid", "solid", "longdash", "dotted")
colors <- c("blue", "black", "green", "red")

# Smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"],
                 df = price_true_disc_free[, "smoker"],
                 un = price_true_unaware[, "smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"],
                 df = price_true_disc_free[, "non-smoker"],
                 un = price_true_unaware[, "non-smoker"])

p2 <- plot_prices(df, title = "Non-smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
nPolicies <- 100000


## -----------------------------------------------------------------------------
agePattern <- dnorm(x = ages / 100, mean = 0.45, sd = 0.2)
ageProp <- agePattern / sum(agePattern)


## ---- fig.height=9, fig.width=9-----------------------------------------------
# custom plot size for this plot (Jupyter, done with chunk options R Markdown)
options(repr.plot.width = 9, repr.plot.height = 9)
ggplot(data = data.frame(ages = ages, ageProp = ageProp), aes(x = ages, y = ageProp)) +
  geom_step() +
  labs(title = "Age distribution", y = "age", y = "frequency")
# reset original
options(repr.plot.width = 18, repr.plot.height = 9)


## -----------------------------------------------------------------------------
X <- matrix(0, nrow = nPolicies, ncol = 2)
D <- matrix(0, nrow = nPolicies, ncol = 1)

set.seed(seed)
nWomen <- rbinom(n = 1, size = nPolicies, prob = womanProp)
nSmokingWomen <- rbinom(n = 1, size = nWomen,
                        prob = womanSmokerProp * smokerProp / womanProp)
nSmokingMen <- rbinom(n = 1, size = nPolicies - nWomen,
                      prob = (1 - womanSmokerProp) * smokerProp / (1 - womanProp))
probWoman <- nWomen / nPolicies

D[1:nWomen] <- 1
X[, 1] <- matrix(ages, nrow = 1, ncol = nAges) %*%
          rmultinom(n = nPolicies, size = 1, prob = ageProp)
X[1:nSmokingWomen, 2] <- 1
X[(nWomen + 1):(nWomen + 1 + nSmokingMen), 2] <- 1

logMu1 <- alpha0[1] + alpha0[2] * (X[, 1] >= 20) * (X[, 1] <= 40) * D
logMu2 <- beta0[1] + beta0[2] * X[, 1] + beta0[3] * X[, 2] + beta0[4] * D
logMu3 <- gamma0[1] + gamma0[2] * X[, 1]

X1 <- X[, 1]
X2 <- X[, 2]

N1 <- rpois(n = nPolicies, lambda = exp(logMu1))
N2 <- rpois(n = nPolicies, lambda = exp(logMu2))
N3 <- rpois(n = nPolicies, lambda = exp(logMu3))

fullData <- data.frame(N1 = N1, N2 = N2, N3 = N3, X1 = X1, X2 = X2, D = D)
# randomize order because this can influence fitting
set.seed(50)  # NOTE: there is no strict need to set a new seed explicitly here
fullData <- fullData[sample(1:nrow(fullData)), ]
str(fullData)


## -----------------------------------------------------------------------------
womanSmoker <- data.frame(matrix(0, nrow = nAges, ncol = 3))
names(womanSmoker) <- c("X1", "X2", "D")
womanSmoker[, "X1"] <- ages
womanNonSmoker <- manSmoker <- manNonSmoker <- womanSmoker
womanSmoker[, "X2"] <- 1
manSmoker[, "X2"] <- 1
womanSmoker[, "D"] <- 1
womanNonSmoker[, "D"] <- 1


## -----------------------------------------------------------------------------
glm1_be <- glm(N1 ~ X1 + X2 + D, data = fullData, family = poisson(link = "log"))
glm2_be <- glm(N2 ~ X1 + X2 + D, data = fullData, family = poisson(link = "log"))
glm3_be <- glm(N3 ~ X1 + X2 + D, data = fullData, family = poisson(link = "log"))


## -----------------------------------------------------------------------------
legend_title <- "Frequency"
y_label <- "estimated frequency"
labels <- c("man non-smoker", "man smoker",
            "woman non-smoker", "woman smoker")
linetypes <- c("solid", "dotdash", "solid", "dotdash")
colors <- c("black", "black", "blue", "blue")

# claims type 1
df <- data.frame(ages,
                 be_women_smoker = predict(glm1_be, newdata = womanSmoker, type = "response"),
                 be_women_nonsmoker = predict(glm1_be, newdata = womanNonSmoker, type = "response"),
                 be_men_smoker = predict(glm1_be, newdata = manSmoker, type = "response"),
                 be_men_nonsmoker = predict(glm1_be, newdata = manNonSmoker, type = "response"))

p1 <- plot_prices(df, title = "Claims type 1", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)

# claims type 2
df <- data.frame(ages,
                 be_women_smoker = predict(glm2_be, newdata = womanSmoker, type = "response"),
                 be_women_nonsmoker = predict(glm2_be, newdata = womanNonSmoker, type = "response"),
                 be_men_smoker = predict(glm2_be, newdata = manSmoker, type = "response"),
                 be_men_nonsmoker = predict(glm2_be, newdata = manNonSmoker, type = "response"))

p2 <- plot_prices(df, title = "Claims type 2", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)

# claims type 3
df <- data.frame(ages,
                 be_women_smoker = predict(glm3_be, newdata = womanSmoker, type = "response"),
                 be_women_nonsmoker = predict(glm3_be, newdata = womanNonSmoker, type = "response"),
                 be_men_smoker = predict(glm3_be, newdata = manSmoker, type = "response"),
                 be_men_nonsmoker = predict(glm3_be, newdata = manNonSmoker, type = "response"))

p3 <- plot_prices(df, title = "Claims type 3", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)


grid.arrange(p1, p2, p3, ncol = 3)


## -----------------------------------------------------------------------------
price_glm_best_estimate <- array(0, c(nAges, 4), dimnames = list(NULL, NULL))
colnames(price_glm_best_estimate) <- colnames(price_true_best_estimate)

# woman and non-smoker
price_glm_best_estimate[, "woman-non-smoker"] <-
  claimCost[1] * predict(glm1_be, newdata = womanNonSmoker, type = "response") +
  claimCost[2] * predict(glm2_be, newdata = womanNonSmoker, type = "response") +
  claimCost[3] * predict(glm3_be, newdata = womanNonSmoker, type = "response")
# woman and smoker
price_true_best_estimate[, "woman-smoker"] <-
  claimCost[1] * predict(glm1_be, newdata = womanSmoker, type = "response") +
  claimCost[2] * predict(glm2_be, newdata = womanSmoker, type = "response") +
  claimCost[3] * predict(glm3_be, newdata = womanSmoker, type = "response")
# man and non-smoker
price_true_best_estimate[, "man-non-smoker"] <-
  claimCost[1] * predict(glm1_be, newdata = manNonSmoker, type = "response") +
  claimCost[2] * predict(glm2_be, newdata = manNonSmoker, type = "response") +
  claimCost[3] * predict(glm3_be, newdata = manNonSmoker, type = "response")
# man and smoker
price_true_best_estimate[, "man-smoker"] <-
  claimCost[1] * predict(glm1_be, newdata = manSmoker, type = "response") +
  claimCost[2] * predict(glm2_be, newdata = manSmoker, type = "response") +
  claimCost[3] * predict(glm3_be, newdata = manSmoker, type = "response")


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <- c("best-estimate price (women)", "best-estimate price (men)")
linetypes <- c("solid", "solid")
colors <- c("blue", "black")

# Smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages,
                 be_women = price_glm_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"])

p2 <- plot_prices(df, title = "Non-smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
price_glm_disc_free <- array(NA, c(nAges, 2), dimnames = c(NULL, NULL))
colnames(price_glm_disc_free) <- c("non-smoker", "smoker")
# non-smoker non-discriminating
price_glm_disc_free[, "non-smoker"] <-
  probWoman * price_glm_best_estimate[, "woman-non-smoker"] +
  (1 - probWoman) * price_true_best_estimate[, "man-non-smoker"]
# smoker non-discriminating
price_glm_disc_free[, "smoker"] <-
  probWoman * price_true_best_estimate[, "woman-smoker"] +
  (1 - probWoman) * price_true_best_estimate[, "man-smoker"]


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <-  c("best-estimate price (women)", "best-estimate price (men)",
             "discrimination-free price")
linetypes <- c("solid", "solid", "longdash")
colors <- c("blue", "black", "green")

# Smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"],
                 df = price_glm_disc_free[, "smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages, be_women = price_glm_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"],
                 df = price_glm_disc_free[, "non-smoker"])

p2 <- plot_prices(df, title = "Non-smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
glm1_un <- glm(N1 ~ X1 + X2 , data = fullData, family = poisson(link = "log"))
glm2_un <- glm(N2 ~ X1 + X2 , data = fullData, family = poisson(link = "log"))
glm3_un <- glm(N3 ~ X1 + X2 , data = fullData, family = poisson(link = "log"))


## -----------------------------------------------------------------------------
legend_title <- "Frequency"
y_label <- "estimated frequency"
labels <-  c("non-smoker", "smoker")
linetypes <- c("twodash", "dotdash")
colors <- c("yellow", "orange")

# claims type 1
df <- data.frame(ages,
                 be_smoker = predict(glm1_un, newdata = womanSmoker, type = "response"),
                 be_nonsmoker = predict(glm1_un, newdata = womanNonSmoker, type = "response"))

p1 <- plot_prices(df, title = "Claims type 1 (without gender)", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)

# claims type 2
df <- data.frame(ages,
                 be_smoker = predict(glm2_un, newdata = womanSmoker, type = "response"),
                 be_nonsmoker = predict(glm2_un, newdata = womanNonSmoker, type = "response"))

p2 <- plot_prices(df, title = "Claims type 2 (without gender)", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)

# claims type 3
df <- data.frame(ages,
                 be_smoker = predict(glm3_un, newdata = womanSmoker, type = "response"),
                 be_nonsmoker = predict(glm3_un, newdata = womanNonSmoker, type = "response"))

p3 <- plot_prices(df, title = "Claims type 3 (without gender)", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims_freq)

grid.arrange(p1, p2, p3, ncol = 3)


## -----------------------------------------------------------------------------
price_glm_unaware <- array(0, c(nAges, 2), dimnames = c(NULL, NULL))
colnames(price_glm_unaware) <- c("non-smoker", "smoker")
# non-smoker
price_glm_unaware[, "non-smoker"] <-
  claimCost[1] * predict(glm1_un, newdata = womanNonSmoker, type = "response") +
  claimCost[2] * predict(glm2_un, newdata = womanNonSmoker, type = "response") +
  claimCost[3] * predict(glm3_un, newdata = womanNonSmoker, type = "response")
# smoker
price_glm_unaware[, "smoker"] <-
  claimCost[1] * predict(glm1_un, newdata = womanSmoker, type = "response") +
  claimCost[2] * predict(glm2_un, newdata = womanSmoker, type = "response") +
  claimCost[3] * predict(glm3_un, newdata = womanSmoker, type = "response")


## -----------------------------------------------------------------------------
legend_title <- "Price"
y_label <- "true prices"
labels <- c("best-estimate price (women)", "best-estimate price (men)",
            "discrimination-free price", "unawareness price")
linetypes <- c("solid", "solid", "longdash", "dotted")
colors <-  c("blue", "black", "green", "red")

# Smokers
df <- data.frame(ages,
                 be_women = price_true_best_estimate[, "woman-smoker"],
                 be_men = price_true_best_estimate[, "man-smoker"],
                 df = price_glm_disc_free[, "smoker"],
                 un = price_glm_unaware[, "smoker"])

p1 <- plot_prices(df, title = "Smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

# Non-smokers
df <- data.frame(ages,
                 be_women = price_glm_best_estimate[, "woman-non-smoker"],
                 be_men = price_true_best_estimate[, "man-non-smoker"],
                 df = price_glm_disc_free[, "non-smoker"],
                 un = price_glm_unaware[, "non-smoker"])

p2 <- plot_prices(df, title = "Non-smokers", legend_title, y_label,
                  labels, linetypes, colors) + ylim(ylims)

grid.arrange(p1, p2, ncol = 2)


## -----------------------------------------------------------------------------
sessionInfo()


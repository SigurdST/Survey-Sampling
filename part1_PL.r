#Data importation
library(readr)
rec99 <- read.csv("~/GitHub/Survey-Sampling/rec99htegne.csv")
View(rec99)

#transformation of the columns in vectors
attach(rec99)
# Data details
attributes(rec99)
#Preliminary steps
dim(rec99)
ty=sum(LOGVAC)
ty
var(LOGVAC)

# Poststratum sizes (Nq)
Nq <- table(stratlog)
Nq  # Frequency table for each poststratum

# Totals Yq (sum of LOGVAC within each poststratum)
Yq <- by(LOGVAC, stratlog, sum)
Yq

# Variances S^2_yq (variance of LOGVAC within each poststratum)
var_yq <- by(LOGVAC, stratlog, var)
var_yq


####2)
#install.packages("survey")
#install.packages("sampling")
library(sampling)
library(survey)

# Ensure reproducibility
set.seed(66542)

# Draw a sample using SRSWOR
si.rec99 <- srswor(70, 554)  # Draw 70 samples from 554 communes without replacement

# Define the sample design
ech.si <- svydesign(
  id = ~CODE_N,                      # Primary sampling unit (commune codes)
  weights = rep(554/70, 70),         # Sampling weights (inverse probability of selection)
  fpc = rep(554, 70),                # Finite population correction
  data = rec99[which(si.rec99 == 1),] # Subset of sampled communes
)

# Calculate HT estimator for total number of empty dwellings
est <- svytotal(~LOGVAC, ech.si)

# Extract attributes of the result
attributes(est)

# Variance of the estimator
SE(est)^2

# Standard deviation (SE)
SE(est)

# Coefficient of variation (CV = SD/mean)
SE(est) / est[1]

# Confidence interval (95%)
est[1] - 1.96 * SE(est)  # Lower bound
est[1] + 1.96 * SE(est)  # Upper bound

#2.3.2 Calculating the poststratication estimator

# Calculate population distribution for stratlog
pop_dist <- as.data.frame(table(rec99$stratlog))
names(pop_dist) <- c("stratlog", "Freq")

# Apply poststratification
post_strata_design <- postStratify(
  design = ech.si,          # Survey design object
  strata = ~stratlog,       # Poststratification variable
  population = pop_dist     # Population distribution
)

# Calculate the poststratified total estimate for LOGVAC
post_est <- svytotal(~LOGVAC, post_strata_design)
post_est


#Recall the defnition of the poststratifed estimator. Use a calculator
#and detail the calculus in order to check that you and the same
#values than R for the poststratifed estimate.


#2.4 Simulations

# Load required packages
library(survey)
library(sampling)

# Set seed for reproducibility
set.seed(66542)

# Initialization
nsimu <- 1000  # Number of simulations
tu.esti <- numeric(nsimu)  # HT estimates
var.esti.tu <- numeric(nsimu)  # Variances of HT estimates

# Simulations
for (i in 1:nsimu) {
  # Draw SRSWOR sample
  si.rec99 <- srswor(70, 554)
  
  # Create survey design for the sample
  ech.si <- svydesign(
    id = ~CODE_N, 
    weights = rep(554/70, 70),
    fpc = rep(554, 70),
    data = rec99[which(si.rec99 == 1),]
  )
  
  # HT estimator and variance
  est <- svytotal(~LOGVAC, ech.si)
  tu.esti[i] <- as.numeric(est[1])  # HT estimate
  var.esti.tu[i] <- SE(est)^2  # Variance of the estimate
}

# Monte Carlo Mean, SD, CV
mc_mean <- mean(tu.esti)
mc_sd <- sqrt(var(tu.esti))
mc_cv <- (mc_sd / mc_mean) * 100

# Print results
cat("Monte Carlo Mean:", mc_mean, "\n")
cat("Monte Carlo Standard Deviation:", mc_sd, "\n")
cat("Monte Carlo Coefficient of Variation (%):", mc_cv, "\n")

# Histogram
hist(tu.esti, main = "Distribution of HT Estimates", xlab = "HT Estimates", col = "lightblue")


    
# Load required packages
library(survey)
library(sampling)

# Set seed for reproducibility
set.seed(66542)

# Initialization
nsimu <- 1000  # Number of simulations
post_esti <- numeric(nsimu)  # Poststratified estimates
var_post_esti <- numeric(nsimu)  # Variances of poststratified estimates

# Prepare the population distribution for poststratification
pop_dist <- as.data.frame(table(rec99$stratlog))
names(pop_dist) <- c("stratlog", "Freq")

# Simulations
for (i in 1:nsimu) {
  # Draw SRSWOR sample
  si.rec99 <- srswor(70, 554)
  
  # Create survey design for the sample
  ech.si <- svydesign(
    id = ~CODE_N,
    weights = rep(554/70, 70),
    fpc = rep(554, 70),
    data = rec99[which(si.rec99 == 1), ]
  )
  
  # Apply poststratification
  post_strata_design <- postStratify(
    design = ech.si,
    strata = ~stratlog,
    population = pop_dist
  )
  
  # Poststratified estimator and variance
  est <- svytotal(~LOGVAC, post_strata_design)
  post_esti[i] <- as.numeric(est[1])  # Poststratified estimate
  var_post_esti[i] <- SE(est)^2  # Variance of the estimate
}

# Monte Carlo Mean, SD, CV for Poststratified Estimates
mc_mean_post <- mean(post_esti)
mc_sd_post <- sqrt(var(post_esti))
mc_cv_post <- (mc_sd_post / mc_mean_post) * 100

# Print results
cat("Monte Carlo Mean (Poststratified):", mc_mean_post, "\n")
cat("Monte Carlo SD (Poststratified):", mc_sd_post, "\n")
cat("Monte Carlo CV (Poststratified, %):", mc_cv_post, "\n")

# Histograms for comparison
par(mfrow = c(1, 2))  # Two plots side-by-side
hist(tu.esti, main = "HT Estimates", xlab = "HT Estimates", col = "lightblue", breaks = 20)
hist(post_esti, main = "Poststratified Estimates", xlab = "Poststratified Estimates", col = "lightgreen", breaks = 20)















































#3 ratio estimator
#3.3 The one sample case
#
# Scatterplot of LOG (auxiliary) vs LOGVAC (variable of interest)
par(mfrow = c(1, 1))
plot(rec99$LOG, rec99$LOGVAC,
     main = "Scatterplot of LOG vs LOGVAC",
     xlab = "Number of Housing Units (LOG)",
     ylab = "Number of Empty Housing Units (LOGVAC)",
     col = "blue", pch = 20)
abline(lm(LOGVAC ~ LOG, data = rec99), col = "red", lwd = 2)  # Add regression line


#heck for linear relationship: If yy and xx have a strong linear relationship, the ratio estimator will likely perform well.

# Calculate the ratio estimator
est.ratio <- svyratio(~LOGVAC, ~LOG, ech.si)

# Predict the total using the ratio estimator
predict(est.ratio, total = sum(rec99$LOG))  # Total of LOG in the population

#compute 'manually':
rec99_sample=rec99[which(si.rec99 == 1),]
sigma_Y=sum(rec99_sample$LOGVAC)
sigma_X=sum(rec99_sample$LOG)
R=sigma_Y/sigma_X
Y_hat=R*sum(rec99$LOG)

#3.4 Simulations

# Initialization
ratio_esti <- numeric(nsimu)  # Ratio estimates
var_ratio_esti <- numeric(nsimu)  # Variances of ratio estimates

# Simulations
for (i in 1:nsimu) {
  # Draw SRSWOR sample
  si.rec99 <- srswor(70, 554)
  
  # Create survey design for the sample
  ech.si <- svydesign(
    id = ~CODE_N,
    weights = rep(554 / 70, 70),
    fpc = rep(554, 70),
    data = rec99[which(si.rec99 == 1), ]
  )
  
  # Ratio estimator
  est.ratio <- svyratio(~LOGVAC, ~LOG, ech.si)
  pred <- predict(est.ratio, total = sum(rec99$LOG))
  ratio_esti[i] <- pred$total  # Ratio estimate
  var_ratio_esti[i] <- pred$se  # Variance of the estimate
}


# Monte Carlo statistics
mc_mean_ratio <- mean(ratio_esti)
mc_sd_ratio <- var(ratio_esti)
mc_cv_ratio <- (mc_sd_ratio / mc_mean_ratio) * 100

# Print results
cat("Monte Carlo Mean (Ratio):", mc_mean_ratio, "\n")
cat("Monte Carlo SD (Ratio):", mc_sd_ratio, "\n")
cat("Monte Carlo CV (Ratio, %):", mc_cv_ratio, "\n")


par(mfrow = c(1, 2))  # Two plots side-by-side
hist(tu.esti, main = "HT Estimates", xlab = "HT Estimates", col = "lightblue", breaks = 20)
hist(ratio_esti, main = "Ratio Estimates", xlab = "Ratio Estimates", col = "lightgreen", breaks = 20)
































#4 Regression estimator

ech.si.cal <- calibrate(ech.si, ~LOG, c(554, 197314))
svytotal(~LOGVAC, ech.si.cal)

# Initialize vectors to store results
regression_esti <- numeric(nsimu)  # Store regression estimates
var_regression_esti <- numeric(nsimu)  # Store variance of regression estimates

# Run the simulations
for (i in 1:nsimu) {
  # Draw SRSWOR sample
  si.rec99 <- srswor(70, 554)
  
  # Create survey design for the sample
  ech.si <- svydesign(
    id = ~CODE_N,
    weights = rep(554 / 70, 70),
    fpc = rep(554, 70),
    data = rec99[which(si.rec99 == 1), ]
  )
  
  # Apply regression calibration
  ech.si.cal <- calibrate(ech.si, ~LOG, c(554, 197314))
  
  # Estimate total of empty housing units (LOGVAC)
  total_est <- svytotal(~LOGVAC, ech.si.cal)
  
  # Store results
  regression_esti[i] <- total_est[1]  # Total estimate
  var_regression_esti[i] <- SE(total_est)^2  # Variance of the estimate
}

# Calculate the Monte Carlo mean and standard deviation
mean_regression <- mean(regression_esti)
sd_regression <- sd(regression_esti)
cv_regression <- (sd_regression / mean_regression) * 100

# Display results
mean_regression
sd_regression
cv_regression

# Plot histogram of regression estimates
hist(regression_esti, main = "Histogram of Regression Estimates", xlab = "Total Empty Housing Units")

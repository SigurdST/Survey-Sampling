########## PART 1 ##########


##### 2 - The post-stratified estimator #####


# 2.1

#Data importation
library(readr)
rec99 <- read_csv("Desktop/M2S1/Survey Sampling/Survey-Sampling/rec99htegne.csv")
#transformation of the columns in vectors
attach(rec99)
# Data details
attributes(rec99)
#Preliminary steps
N=dim(rec99)
N
ty=sum(LOGVAC)
ty
vary=var(LOGVAC)
vary


# 2.2

table(stratlog) # 1: 221, 2: 169, 3: 110, 4: 54
by(LOGVAC,stratlog,sum) # 1: 895, 2: 1807, 3: 3341, 4: 4725
by(LOGVAC,stratlog,var) # 1: 1.06569, 2: 47.13095, 3: 459.7589, 4: 4184.142
by(LOGVAC,stratlog,mean) # 1: 4.057466, 2: 10.68817, 3: 30.37273, 4: 87.5


# 2.3

library(sampling)
library(survey)

set.seed(66542)  # needed if you want to obtain always the same results
si.rec99<-srswor(70,554)
ech.si <- svydesign(id=~CODE_N, weights=rep(554/70,70),fpc = rep(554, 70),
                    data=rec99[which(si.rec99==1),])
est<- svytotal(~LOGVAC, ech.si)
est
attributes(est)
SE(est)^2
SE(est)
SE(est)/est[1]
est[1]-1.96 * SE(est)
est[1]+1.96 * SE(est)


# 2.4

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


# 2.5

# We recuparate the values of the post-stratified mean and variance to compute the estimator manually
table(post_strata_design$variables$stratlog)
tapply(post_strata_design$variables$LOGVAC, post_strata_design$variables$stratlog, mean)
tapply(post_strata_design$variables$LOGVAC, post_strata_design$variables$stratlog, var)


# 2.6

# Simulations for SRSWOR
# Initialization
nsimu<-1000
nb.simul<-matrix(1:nsimu,nsimu,1);
tu.esti<-matrix(1,nsimu,1);
var.esti.tu<-matrix(1,nsimu,1);
# Simulations
for(i in 1:nsimu)
{
  si.rec99<-srswor(70,554)
  ech.si <- svydesign(id=~CODE_N, weights=rep(554/70,70),
                      fpc = rep(554, 70),data=rec99[which(si.rec99==1),])
  a<-svytotal(~LOGVAC, ech.si)
  tu.esti[i]<-a[1]
  var.esti.tu[i]<-SE(a)^2
}
#Monte Carlo mean and standard deviation
mean(tu.esti)
sqrt(var(tu.esti))
sqrt(var(tu.esti))/mean(tu.esti) * 100

# Histogram
hist(tu.esti)


# 2.7

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


# 2.8

# Histograms for comparison
par(mfrow = c(1, 2))  # Two plots side-by-side
hist(tu.esti, main = "HT Estimates", xlab = "HT Estimates", col = "lightblue", breaks = 20)
hist(post_esti, main = "Poststratified Estimates", xlab = "Poststratified Estimates", col = "lightgreen", breaks = 20)




##### 3 - The ratio estimator #####


# 3.1

# Scatterplot of LOG (auxiliary) vs LOGVAC (variable of interest)
par(mfrow = c(1, 1))
plot(rec99$LOG, rec99$LOGVAC,
     main = "Scatterplot of LOG vs LOGVAC",
     xlab = "Number of Housing Units (LOG)",
     ylab = "Number of Empty Housing Units (LOGVAC)",
     col = "blue", pch = 20)
abline(lm(LOGVAC ~ LOG, data = rec99), col = "red", lwd = 2)  # Add regression line


#heck for linear relationship: If yy and xx have a strong linear relationship, the ratio estimator will likely perform well.


# 3.2

# Calculate the ratio estimator
est.ratio <- svyratio(~LOGVAC, ~LOG, ech.si)

# Predict the total using the ratio estimator
predict(est.ratio, total = sum(rec99$LOG))  # Total of LOG in the population


# 3.3

#compute 'manually':
rec99_sample=rec99[which(si.rec99 == 1),]
sigma_Y=sum(rec99_sample$LOGVAC)
sigma_X=sum(rec99_sample$LOG)
R=sigma_Y/sigma_X
Y_hat=R*sum(rec99$LOG)

# 3.4 

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


# 3.5

par(mfrow = c(1, 2))  # Two plots side-by-side
hist(tu.esti, main = "HT Estimates", xlab = "HT Estimates", col = "lightblue", breaks = 20)
hist(ratio_esti, main = "Ratio Estimates", xlab = "Ratio Estimates", col = "lightgreen", breaks = 20)




##### 4 - The regression estimator #####

# 4.1

ech.si.cal <- calibrate(ech.si, ~LOG, c(554, 197314))
svytotal(~LOGVAC, ech.si.cal)

# 4.2

# Extract the auxiliary variable (LOG)
x <- ech.si.cal$variables$LOG

# Extract the variable of interest (LOGVAC)
y <- ech.si.cal$variables$LOGVAC

# Extract the weights from the survey design object

w <- weights(ech.si.cal)

# Weighted sample mean for x
x_bar_sample <- sum(w * x) / sum(w)

# Weighted sample mean for y
y_bar_sample <- sum(w * y) / sum(w)

# Print the results
cat("Sample mean for x:", x_bar_sample, "\n")
cat("Sample mean for y:", y_bar_sample, "\n")

# Weighted covariance between x and y
cov_xy <- sum(w * (x - x_bar_sample) * (y - y_bar_sample)) / sum(w)

# Weighted variance of x
var_x <- sum(w * (x - x_bar_sample)^2) / sum(w)

# Regression coefficient beta
beta <- cov_xy / var_x

# Print the result
cat("Regression coefficient (beta):", beta, "\n")

# Known total for x (T_X) and population size (N)
T_X <- 197314          # Replace with known total for LOG
N <- length(w)      # Approximate population size from the sample

# Calculate regression-adjusted total
Y_reg <- sum(w * y) + beta * (T_X - x_bar_sample * N)

# Print the result
cat("Regression estimate (Y_reg):", Y_reg, "\n")

# Initialize vectors to store results
regression_esti <- numeric(nsimu)  # Store regression estimates
var_regression_esti <- numeric(nsimu)  # Store variance of regression estimates

# 4.3

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

# 4.4

# Plot histogram of regression estimates
hist(tu.esti, main = "HT Estimates", xlab = "HT Estimates", col = "lightblue", breaks = 20)
hist(regression_esti, main = "Histogram of Regression Estimates", xlab = "Total Empty Housing Units", col = "lightgreen", breaks = 20)

# Correlation between LOG and LOGVAC
correlation <- cor(rec99$LOG, rec99$LOGVAC)
correlation

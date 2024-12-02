########## PART 1 ##########

##### The post-stratified estimator #####


# 1

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

# 2

table(stratlog) # 1: 221, 2: 169, 3: 110, 4: 54
by(LOGVAC,stratlog,sum) # 1: 895, 2: 1807, 3: 3341, 4: 4725
by(LOGVAC,stratlog,var) # 1: 1.06569, 2: 47.13095, 3: 459.7589, 4: 4184.142
by(LOGVAC,stratlog,mean) # 1: 4.057466, 2: 10.68817, 3: 30.37273, 4: 87.5

# 3

library(sampling)
library(survey)

set.seed(66542)  # needed if you want to obtain always the same results
si.rec99<-srswor(70,554)
ech.si <- svydesign(id=~CODE_N, weights=rep(554/70,70),fpc = rep(554, 70),
                    data=rec99[which(si.rec99==1),])
est<- svytotal(~LOGVAC, ech.si)
attributes(est)
SE(est)^2
SE(est)
SE(est)/est[1]
est[1]-1.96 * SE(est)
est[1]+1.96 * SE(est)


# 4

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

table(post_strata_design$variables$stratlog)
tapply(post_strata_design$variables$LOGVAC, post_strata_design$variables$stratlog, mean)
tapply(post_strata_design$variables$LOGVAC, post_strata_design$variables$stratlog, var)

# 6

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
sqrt(var(tu.esti))/mean(tu.esti)
# Histogram
hist(tu.esti)

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



#4 Regression estimator

# 4.1

ech.si.cal <- calibrate(ech.si, ~LOG, c(554, 197314))
svytotal(~LOGVAC, ech.si.cal)

# 4.2

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

# Extract the auxiliary variable (LOG)
x <- ech.si.cal$variables$LOG

# Extract the variable of interest (LOGVAC)
y <- ech.si.cal$variables$LOGVAC

w <- weights(ech.si)
x
y
w

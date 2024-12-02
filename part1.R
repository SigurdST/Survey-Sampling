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
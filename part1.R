#Data importation
library(readr)
rec99 <- read_csv("Desktop/M2S1/Survey Sampling/Survey-Sampling/rec99htegne.csv")
#transformation of the columns in vectors
attach(rec99)
# Data details
attributes(rec99)
#Preliminary steps
dim(rec99)
ty=sum(LOGVAC)
ty
var(LOGVAC)

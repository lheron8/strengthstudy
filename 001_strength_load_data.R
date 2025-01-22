# load data
# 20.9.24

data <- read.csv("qaly_data.csv")

# rename variables

names(data) <- c("Participant", "Assigned", "EQ5D_BL", "EQ5D_12WK", "EQ5D_6M")



# load packages

library(flextable)
library(officer)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)  

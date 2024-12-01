getwd()
options(max.print=999999)

#### Load in the tidyverse
library(tidyverse)


#### Data 

# Remember to check your working directory if needed using getwd() and setwd()

# Read in the data
chechen <- read.csv("./data/raw/chechen.csv")

# check it
glimpse(chechen)
colnames(chechen)
nrow(chechen)
library(dplyr)
class(chechen)
chechen %>% 
  arrange(desc(star)) %>% 
  slice(1:10)
star %>% 
  arrange(star) %>% 
  slice(2:10)

top_n(star,-5,race)
top_n(star,-5,pupilID)
#############################################
getwd()
options(max.print=999999)
library(tidyverse)
library(dplyr)
chechen <- read.csv("./data/raw/chechen.csv")

#################################################################################
# 1.1. How many villages were and were not attacked by Russians? Give the       #
# overall breakdown. (10 points)                                                #
#################################################################################
# Part 1                      #
# Number of villages attacked #
###############################
villagesRussiaAttacked <- chechen %>% 
  filter(preattack > 0 | postattack > 0)

nrow(villagesRussiaAttacked)
count(villagesRussiaAttacked)

###################################
# Part 2                          #
# Number of villages Not attacked #
###################################
villagesRussiaDidNotAttack <- chechen %>% 
  filter(preattack == 0 & postattack == 0)

nrow(villagesRussiaDidNotAttack)
count(villagesRussiaDidNotAttack)


totalNumberOfVillages  <- villagesRussiaDidNotAttack + villagesRussiaDidNotAttack

nrow(villagesRussiaDidNotAttack) + nrow(villagesRussiaAttacked) 

##############################################################################
# 1.2. Did Russian artillery result in a greater number of deaths in Groznyy #
# compared to the villages outside of Groznyy? Conduct the comparison        #
# in terms of the mean and median. (10 points)                               #
##############################################################################

# artilleryDeathsInGrozny dplyr way 
artilleryDeathsInGrozny <- chechen %>% 
  filter(groznyy == 1 & fire == 1 & deaths > 0)

meanArtilleryDeathsInGrozny <- mean(artilleryDeathsInGrozny$deaths, na.rm = T)
meanArtilleryDeathsInGrozny
medianArtilleryDeathsInGrozny <- median(artilleryDeathsInGrozny$deaths, na.rm = T)
medianArtilleryDeathsInGrozny

# artilleryDeathsNotInGrozny dplyr way 
artilleryDeathsNotInGrozny <- chechen %>% 
  filter(groznyy == 0  & fire == 1 &   deaths > 0)
meanArtilleryDeathsNotInGrozny <- mean(artilleryDeathsNotInGrozny$deaths, na.rm = T)
meanArtilleryDeathsNotInGrozny
medianArtilleryDeathsNotInGrozny <- median(artilleryDeathsNotInGrozny$deaths, na.rm = T)
medianArtilleryDeathsNotInGrozny

# artilleryDeathsInGrozny  R way 
mean(chechen$deaths[chechen$groznyy == 1 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)
median(chechen$deaths[chechen$groznyy == 1 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)

# artilleryDeathsNotInGrozny R way 
mean(chechen$deaths[chechen$groznyy == 0 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)
median(chechen$deaths[chechen$groznyy == 0 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)

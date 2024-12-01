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
numberOfVillagesRussiaAttacked <- nrow(villagesRussiaAttacked)
###################################
# Part 2                          #
# Number of villages Not attacked #
###################################
villagesRussiaDidNotAttack <- chechen %>% 
  filter(preattack == 0 & postattack == 0)
nrow(villagesRussiaDidNotAttack)
count(villagesRussiaDidNotAttack)

numberOfVillagesRussiaDidNotAttack <- nrow(villagesRussiaDidNotAttack)

nrow(villagesRussiaDidNotAttack) + nrow(villagesRussiaAttacked) 
numberOfVillagesInChechen <- nrow(chechen)
sprintf("Out of %d villages, Russia attacked %d and did not attack %d.", nrow(chechen),nrow(villagesRussiaAttacked),  nrow(villagesRussiaDidNotAttack))
sprintf("Out of %d villages, Russia attacked %d and did not attack %d.", numberOfVillagesInChechen,numberOfVillagesRussiaAttacked,  numberOfVillagesRussiaDidNotAttack)

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

if(meanArtilleryDeathsInGrozny > meanArtilleryDeathsNotInGrozny){
  meanDeathsInGroznyyGreaterOrLesser <- "greater"
}else{
  meanDeathsInGroznyyGreaterOrLesser <- "lesser"  
}
if(medianArtilleryDeathsInGrozny > medianArtilleryDeathsNotInGrozny){
  medianDeathsInGroznyyGreaterOrLesser <- "greater"
}else{
  medianDeathsInGroznyyGreaterOrLesser <- "lesser"  
}
sprintf("Based on the mean average, Russian artillery resulted in a %s number of Deaths in Groznyy (%f) compared to outside of Grozny (%f)", meanDeathsInGroznyyGreaterOrLesser,meanArtilleryDeathsInGrozny,meanArtilleryDeathsNotInGrozny)
sprintf("and based on the median average, Russian artillery resulted in a %s number of Deaths in Groznyy (%f) compared to outside of Grozny (%f)", medianDeathsInGroznyyGreaterOrLesser,medianArtilleryDeathsInGrozny,medianArtilleryDeathsNotInGrozny)



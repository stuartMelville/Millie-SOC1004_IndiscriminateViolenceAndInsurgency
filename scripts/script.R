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

# 1.1 ANSWER
numberOfVillagesRussiaAttacked <- nrow(villagesRussiaAttacked)
numberOfVillagesRussiaDidNotAttack <- nrow(villagesRussiaDidNotAttack)
numberOfVillagesInChechen <- nrow(chechen)
sprintf("Out of %d villages, Russia attacked %d and did not attack %d.", numberOfVillagesInChechen,numberOfVillagesRussiaAttacked,  numberOfVillagesRussiaDidNotAttack)
rm(villagesRussiaAttacked)
rm(numberOfVillagesRussiaAttacked)
rm(villagesRussiaDidNotAttack)
rm(numberOfVillagesRussiaDidNotAttack)
rm(numberOfVillagesInChechen)
##############################################################################
# 1.2. Did Russian artillery result in a greater number of deaths in Groznyy #
# compared to the villages outside of Groznyy? Conduct the comparison        #
# in terms of the mean and median. (10 points)                               #
##############################################################################
# artilleryDeathsInGrozny dplyr way 
artilleryDeathsInGrozny <- chechen %>% 
  filter(groznyy == 1 & fire == 1 & deaths > 0)

meanArtilleryDeathsInGrozny <- mean(artilleryDeathsInGrozny$deaths, na.rm = T)
medianArtilleryDeathsInGrozny <- median(artilleryDeathsInGrozny$deaths, na.rm = T)

# artilleryDeathsNotInGrozny dplyr way 
artilleryDeathsNotInGrozny <- chechen %>% 
  filter(groznyy == 0  & fire == 1 &   deaths > 0)
meanArtilleryDeathsNotInGrozny <- mean(artilleryDeathsNotInGrozny$deaths, na.rm = T)
medianArtilleryDeathsNotInGrozny <- median(artilleryDeathsNotInGrozny$deaths, na.rm = T)

# artilleryDeathsInGrozny  R way 
mean(chechen$deaths[chechen$groznyy == 1 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)
median(chechen$deaths[chechen$groznyy == 1 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)

# artilleryDeathsNotInGrozny R way 
mean(chechen$deaths[chechen$groznyy == 0 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)
median(chechen$deaths[chechen$groznyy == 0 & chechen$fire == 1 & chechen$deaths > 0], na.rm = T)

# 1.2 ANSWER
# Function which returns "greater" or "lesser" based on two numbers
greaterOrLesser <- function(first,second){
  if(first > second){
    return ("greater")
  }else{
    return("lesser")
  }
}

meanDeathsInGroznyyGreaterOrLesser <- greaterOrLesser(meanArtilleryDeathsInGrozny, meanArtilleryDeathsNotInGrozny)
medianDeathsInGroznyyGreaterOrLesser <- greaterOrLesser(medianArtilleryDeathsInGrozny, medianArtilleryDeathsNotInGrozny)

sprintf("Based on the mean average, Russian artillery resulted in a %s number of Deaths in Groznyy (%f) compared to outside of Grozny (%f)", meanDeathsInGroznyyGreaterOrLesser,meanArtilleryDeathsInGrozny,meanArtilleryDeathsNotInGrozny)
sprintf("and based on the median average, Russian artillery resulted in a %s number of Deaths in Groznyy (%f) compared to outside of Grozny (%f)", medianDeathsInGroznyyGreaterOrLesser,medianArtilleryDeathsInGrozny,medianArtilleryDeathsNotInGrozny)

rm(artilleryDeathsInGrozny)
rm(meanArtilleryDeathsInGrozny)
rm(medianArtilleryDeathsInGrozny)
rm(artilleryDeathsNotInGrozny)
rm(meanArtilleryDeathsNotInGrozny)
rm(medianArtilleryDeathsNotInGrozny)
rm(greaterOrLesser)
rm(meanDeathsInGroznyyGreaterOrLesser)
rm(medianDeathsInGroznyyGreaterOrLesser)


####################################
rm(chechen)


## START OF SYNTAX ##

########################################################################################################################
## Install Required Packages
#######################################################################################################################

## Create a list of all required packages ----------------------------------------------------------------------------
list.of.packages <- c("dplyr",
                      "tidyverse",
                      "fmsb",		
                      "cluster",	
                      "pryr",		
                      "NbClust",	
                      "class",
                      "fossil",
                      "rcompanion",
                      "plyr",
                      "fpc")


## Select packages from list.of.packages that are not already installed ----------------------------------------------
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

## Install new.packages ----------------------------------------------------------------------------------------------
if(length(new.packages)){install.packages(new.package)}

## Load list.of.packages
lapply(list.of.packages, require, character.only=T)

#=====================================================================================================================
## Rationale for each required packages, in context of the Workflow
#=====================================================================================================================

      # dplyr      for data manipulation
      # tidyverse  for data manipilation
      # fmsb       for kmeans function
      # cluster    for gap statistic and silhouette plots
      # pryr       for storing plot objects (sections of R code) in base R
      # NbClust    for Internal Cluster Validity indices
      # class      for k-nearest neighbors  
      # fossil     for adjusted rand index 
      # rcompanion for cramer's V
      # plyr       for rename function 
      # fpc        for jaccard index


#####################################################################################################################
## Read in & prepare data for analysis
#####################################################################################################################

## Read in data & save data as simdata ------------------------------------------------------------------------------
mydata <- read.csv("Raw Data/Simulated_Data.csv", header = TRUE, stringsAsFactors = FALSE)

## Drop “ID” column and save as mydata ------------------------------------------------------------------------------
mydata <- mydata %>% 
  select(-"X") 

## Create "total taste score” as the sum of all 5 perception scores --------------------------------------------------
mydata$tot_taste <- with(mydata, (sweet+salt+sour+bitter+umami))

## END OF SYNTAX ##

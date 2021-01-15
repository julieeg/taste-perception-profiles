## START OF SYNTAX ##

#################################################################################################
## Install Required Packages
#################################################################################################

## Create a list of all required packages ------------------------------------------------------
list.of.packages <- c("fmsb",		
                      "cluster",	
                      "pryr",		
                      "NbClust",	
                      "class",
                      "fossil",
                      "rcompanion",
                      "plyr",
                      "fpc")


## Select packages from list.of.packages that are not already installed ------------------------
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

## Install new.packages ------------------------------------------------------------------------
if(length(new.packages)){install.packages(new.package)}

#===============================================================================================
## Rationale for each required packages, in context of the Workflow
#===============================================================================================

fmsb       # for kmeans function
cluster    # for gap statistic and silhouette plots
pryr       # for storing plot objects (sections of R code) in base R
NbClust    # for Internal Cluster Validity indices
class      # for k-nearest neighbors  
fossil     # for adjusted rand index 
rcompanion # for cramer's V
plyr       # for rename function 
fpc        # for jaccard index


#################################################################################################
## Read in & prepare data for analysis
#################################################################################################

## Read in data & save data as simdata ----------------------------------------------------------
simdata <- read.csv("Data/simdata1.csv", header = TRUE, stringsAsFactors = FALSE)

## Drop “ID” column and save as mydata ----------------------------------------------------------
mydata <- simdata %>% select(sweet, 
                             salt,
                             sour,
                             bitter,
                             umami)

## Create "total taste score” as the sum of all 5 perception scores ----------------------------- 
mydata$tot_taste <- with(mydata, (sweet+salt+sour+bitter+umami))

## END OF SYNTAX ##

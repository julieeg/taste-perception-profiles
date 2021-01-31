## Rscript for a data-driven clustering approach to derive taste perception profiles from sweet, salt, sour, bitter 
## and umami perception scores
## Code Written by: Julie E. Gervis
## Last updated: 2020-01-29

## START OF SYNTAX ##

########################################################################################################################
## Package Installation & Data Preparation
#######################################################################################################################

#=====================================================================================================================
## Install required packages
#=====================================================================================================================

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
## Read in & prepare data for analysis
#=====================================================================================================================

## Read in data & save data as simdata -------------------------------------------------------------------------------
mydata <- read.csv("Raw Data/Simulated_Data.csv", header = TRUE, stringsAsFactors = FALSE)


## Drop “ID” column and save as mydata --------------------------------------------------------------------------------
mydata <- mydata %>% 
  select(-"X") 

# Check data dimenstions & structure ----------------------------------------------------------------------------------
dim(mydata); str(mydata)


#=======================================================================================================================
## Housekeeping
#=======================================================================================================================

## Set plot parameters and margins -------------------------------------------------------------------------------------
par(mfrow=c(1,1), mar=c(5.1,5.1,4.1,2.1), xpd=T)

## Create a list of Ck values to run for each reproducibility index ----------------------------------------------------
Cks<-(2:10)

## Createcolor palette for all plots -------------------------------------------------------------------
greys1<-c("#A0A0A0", "#404040")
greys2<-c("#E1E1E190", "#62626290")


########################################################################################################################
## Step 1: Identify a reproducible cluster algorithm via Split Half Cross Validation (SHCV)
########################################################################################################################

#=======================================================================================================================
## Write functions to run SHCV for any given cluster algorithm (KCA or Ward's D) 
#=======================================================================================================================

## Write functions to print cluster solutions for each cluster algorithm, for any Ck -----------------------------------
# KCA
kca.fun<-function(data, Ck){
  as.numeric(kmeans(data, Ck, nstart=50)$cluster)
}
# Ward's D
wardD.fun<-function(data, Ck){
  as.numeric(cutree(hclust(dist(data), method = "ward.D"), Ck))
}

## write function to run SHCV for any given cluster algorithm and Ck ---------------------------------------------------
shcv.fun<-function(data, Ck, FUN){
  # Set.seed to ensure results are replicatable
  set.seed(123, kind="default")
  
  # Create temporary matrix to store data; 20 rows (20 estimates for each Ck) x 2 cols (1 for each index)
  temp<-matrix(NA,20,2, dimnames = list(c(1:20), c(paste("ARI.", Ck, sep=""), 
                                                   paste("CramV.", Ck, sep=""))))
  
  # Use for loop to generate 10 random test/train sets; for each set, generate 2 estimates for each index (ARI and CramV)
  for (i in 1:10){
    # 1. split data into train and test set
    train.n<-sample(nrow(data), nrow(data)*0.5, replace=F) 
    train.dat<-data[train.n,]; test.dat<-data[-(train.n),]
    
    # 2. Run cluster algorithm on train.dat
    train.clust<-FUN(train.dat, Ck)
    
    # 3. Perform K- Nearest Neighbors on test.dat using train.clust
    test.knn<-knn(train.dat, test.dat, cl=train.clust, Ck, use.all = T, prob=T, l=0.5)
    
    # 4. Run cluster algorithm on test.dat
    test.clust<-FUN(test.dat, Ck)
    
    # 5. Compute ari & cramer's V between test.clust and test.knn 
    ## a. store data in "temp" matrix
    temp[i,1]<- adj.rand.index(test.clust, as.numeric(test.knn))
    temp[i,2]<- cramerV(test.clust, as.numeric(test.knn))
    
    # 6.Repeate steps 2-5 using test sample as the "train" sample
    test.clust<-FUN(test.dat, Ck)
    train.knn<-knn(test.dat, train.dat, cl=test.clust, Ck, use.all = T, prob=T, l=0.5)
    train.clust<-FUN(train.dat, Ck)
    
    ## a. store data temporarily
    temp[i+10,1]<- adj.rand.index(train.clust, as.numeric(train.knn))
    temp[i+10,2]<- cramerV(train.clust, as.numeric(train.knn))
  }
  
  # Print temp matrix with 20 estimates for each index per alg for each 
  return(temp)
}


#=======================================================================================================================
## Run SHCV for each Ck; store results separately for each index 
#=======================================================================================================================

## create matrices for storing results ---------------------------------------------------------------------------------
## Dimensions: 40 rows (20 for each algorithm) x 10 columns (1 per Ck, from 2-10)
ari.dat<-matrix(c(rep(1,20), rep(2,20)),40,10, 
                dimnames = list(1:40, c(paste("Ck.", 2:10, sep=""), "alg")))
cramv.dat<-matrix(c(rep(1,20), rep(2,20)),40,10, 
                  dimnames = list(1:40, c(paste("Ck.", 2:10, sep=""), "alg")))


## Use for loop to run SHCV for each index, for Cks from 2:10 -----------------------------------------------------------
for (Ck in Cks){
  temp.kca<-shcv.fun(mydata, Ck, kca.fun)  # Run shcv for KCA (output = 20 row x 2 col)
  temp.wardD<-shcv.fun(mydata, Ck, wardD.fun)   # Run shcv for Ward's D (output = 20 rows x 2 col)
  ari.dat[,Ck-1]<-cbind(temp.kca[,1], temp.wardD[,1])  # concetenate ARI estimates for all Ck, from KCA=1 & WardD=2
  cramv.dat[,Ck-1]<-cbind(temp.kca[,2], temp.wardD[,2])  # concetenate Ward's D estimates for all Ck, from KCA=1 & WardD=2
}


#=======================================================================================================================
## Visualize data using side-by-side boxplot
#=======================================================================================================================

## Prepare data for plot by converting each matrix to long format ------------------------------------------------------
# ARI
ari_long.dat<-reshape(as.data.frame(ari.dat),  # choose ari.dat
                      varying = list(colnames(ari.dat)[1:9]), # columns that will be collapsed into long form
                      timevar = "Ck", # Var in long form that differentiates the ARI values
                      v.names = "ARI", # name of new var in long form that includes the vars being collapsed
                      direction = "long") 
# CramV
cramv_long.dat<-reshape(as.data.frame(cramv.dat), 
                        varying = list(colnames(cramv.dat)[1:9]),
                        timevar = "Ck", 
                        v.names = "CramV", 
                        direction = "long")


## Calculate mean value for both indices, for each approach using lapply (KCA=1; wardD=2) ------------------------------
ari_means <- lapply(1:2, function(x){
  mean(ari_long.dat$ARI[ari_long.dat$alg==x])})  # ARI

cramv_means <- lapply(1:2, function(x){
  mean(cramv_long.dat$CramV[cramv_long.dat$alg==x])})  # CramV 


## A. Create ARI Plot  ----------------------------------------------------------------------------------------------
ari.plot %<a-% {
  boxplot(ari_long.dat$ARI~ari_long.dat$alg, col=greys1, frame=T, axes=F, ylim=c(0,0.9),
               boxwex = 0.5, varwidth=F, ylab="ARI",
               xlab=NA, cex.lab=1.5)
  axis(side=1, at=c(1,2), labels = c("KCA", "Ward's D"), cex.axis=1.25)
  axis(side=2, at=seq(0,0.9,0.1), cex.axis=1.25)
  points(1:2, ari_means, pch=9, cex=1.5)
  legend("topright", legend = " Mean ARI", pch=9, pt.cex=1.25, cex=1)
}

## View ari summary box plot ------------------------------------------------------------------------------------------
ari.plot

## B. Cramer’s V -------------------------------------------------------------------------------------------------------
cramv.plot %<a-% {
  boxplot(cramv_long.dat$CramV~cramv_long.dat$alg, col=greys1, frame=T, axes=F, ylim=c(0.2,0.95), 
               boxwex = 0.5, varwidth=F, ylab="Cramer's V",
               xlab=NA, cex.lab=1.5)
  axis(side=1, at=c(1,2), labels = c("KCA", "Ward's D"), cex.axis=1.25)
  axis(side=2, at=seq(0.2,0.95,0.1), cex.axis=1.25)
  points(1:2, cramv_means, pch=9, cex=1.5)
  legend("topright", legend = "Mean Cramer's V", pch=9, pt.cex=1.25, cex=1)
}

## Print CramV Summary Box Plot ----------------------------------------------------------------------------------------
cramv.plot

## Create 2 Panel Figure -----------------------------------------------------------------------------------------------
panel.repro.plot %<a-%{
  par(mar=c(5.1,5.1,2.1,2.1), mfrow=c(1,2))
  ari.plot
  cramv.plot
}


########################################################################################################################
## Step 2: Determine the optimal number of clusters (Ck) for the data
########################################################################################################################

#=======================================================================================================================
## A. Examine total within-cluster variation across Cks (Wk) via Elbow Plot 
#=======================================================================================================================

## Write a function to calculate the Wk, for any Ck --------------------------------------------------------------------
Wk.fun<-function(Ck){
  set.seed(123, kind = "default") # choose a random set.seed, so results can be replicated
  kmeans(mydata, Ck, nstart=50)$tot.withinss
}


## Run Wk.fun over the range of Ck & visualize to identidy Elbow -------------------------------------------------------
Wk.dat<-sapply(Cks, Wk.fun) # Generate data
plot(Cks, Wk.dat, type="b", pch=19) # Visualize: Elbow @ Ck = 4


# Compile data for inset of plot (2 through 5)
Ck_inset <- 2:5
Wk_inset.dat<-sapply(Ck_inset, Wk.fun)


## Plot Wk.dat, "bind" & view ------------------------------------------------------------------------------------------
elbow_inset.plot %<a-% {
  par(mfrow=c(1,1), mar=c(5.1,5.1,1.1,2.1))
  plot(Cks, Wk.dat, axes=F, type="b", pch=19, frame = T,
       xlab = expression("Number of Clusters (C"[k]*")"),
       ylab = expression("Total Within-Cluster Variation (W"[k]*")"), font.lab=2, cex.lab=1.15, cex.main=1.5) 
  axis(side=1, at=2:10) # Add x-axis tick marks 
  axis(side=2) # Add y axis
  
  # add inset plot
  par(fig = c(.39,0.99,0.45,1), mar=c(3.1,6.1,2.1,3.1), new=TRUE)
  plot(Ck_inset, Wk_inset.dat, type="b", pch=19, frame=T, axes=F,
       cex.lab=1, xlab = " ", ylab=" ", ylim = c(1700,2600))
  axis(side=1, at=seq(2,5,1), cex.axis=0.8, padj = -1)
  axis(side=2, at=seq(1700,2600,200), cex.axis=0.8, padj=0.5)
}

# View plot
elbow_inset.plot


#=======================================================================================================================
## B. Create Average Silhouette Statistic Plot 
#=======================================================================================================================

## Write function to calculate average silhouette, for any given Ck ----------------------------------------------------
sil.fun<-function(Ck){
  set.seed(123, kind = "default")
  avg.sil<-mean(silhouette(kmeans(mydata, Ck, nstart=50)$cluster, 
                           dist(mydata))[,3])}

## Run sil.fun over the range of Ck ------------------------------------------------------------------------------------
sil.dat<-sapply(Cks, sil.fun)

## Plot sil.dat, "bind" & view  ----------------------------------------------------------------------------------------
dev.off() # reset plot parameters from Elbow plot with inset
sil.plot %<a-%{
  par(mar=c(5.1,5.1,1.1,2.1))
  plot(Cks, sil.dat, type = "b", pch = 19, frame = T, 
       xlab = expression("Number of Clusters (C"[k]*")"),
       ylab = "Average Silhouette", cex.lab=1.5, cex.axis = 1.25)
  axis(side=1, at=seq(3,9,2), cex.axis=1.25)
}; sil.plot

## View 
sil.plot

#=======================================================================================================================
## C. Create Gap Statistic Plot 
#=======================================================================================================================

## Compile Gap Statistics using clusGap function, for any given Ck -----------------------------------------------------
## NOTE: May see time lag when running, since function relies on bootstrap sampling
set.seed(123, kind = "default")  #set.seed() for repeatability 
gap.dat<-clusGap(mydata, kmeans, nstart=25,
                 K.max = 10, B=50, verbose = FALSE) 


# Plot gap statistic, "bind" & view ------------------------------------------------------------------------------------
gap.plot %<a-%{
  plot(gap.dat$Tab[2:10,3], type="b", pch=19, frame=T, 
       xlab=expression("Number of Clusters (C"[k]*")"), 
       ylab="Gap Statistic", xlim=c(1,9), axes=F, cex.lab=1.5, cex.axis = 1.25)
  axis(side=1, at=1:9, labels = 2:10, cex.axis=1.25)
  axis(side=2, cex.axis=1.25)
}; gap.plot


## View plot
gap.plot


########################################################################################################################
## Step 3: Derive & visualize the taste perception profiles
########################################################################################################################

#=======================================================================================================================
## Derive taste perception profiles using KCA with Ck = 4 
#=======================================================================================================================

## Derive & save profile assignments to mydata -------------------------------------------------------------------------
Ck<-4 # Set Ck as 4, based on results from step 2
set.seed(123, kind = "default") # set.seed for repeatability
prof.dat<-kmeans(mydata, Ck, nstart=25)$cluster # run kmeans and extract cluster assignments
mydata$profile<-as.factor(prof.dat) #Add cluster assignments to mydata


#=======================================================================================================================
## Aggregate mean (SD) perception scores for each taste per profile and for the overall cohort
#=======================================================================================================================

## Summarize mean (+/- 1SD) for the overall cohort ---------------------------------------------------------------------

# Write functions to calculate lower (-1SD) and upper (+1SD) values
lower<-function(x){mean(x)-sd(x)}
upper<-function(x){mean(x)+sd(x)}

# Use lower/upper functions to compile data 
cohort.sum.dat<-rbind(upper=sapply(mydata[,1:5], upper), 
                      lower=sapply(mydata[,1:5], lower), 
                      mean=sapply(mydata[,1:5], mean))


## Summarize mean perception for each profile via for loop -------------------------------------------------------------
# Create empty matrix to store results 
prof.sum.dat<-matrix(NA, Ck, 5, # dimensions: 4 rows (1 per profile) 5 columns ( 1 per taste)
                     dimnames = list(paste("prof", 1:Ck, sep=""), # labels rows as prof_#
                                     names(mydata[,1:5])))  #labels columns with tastes

# Run via for loop and sapply
for (Ck in 1:Ck){
  prof.sum.dat[Ck,]<-sapply(mydata[mydata$profile==Ck,1:5], mean) # For Ck==each profile, tabulate mean for each taste
}


#=======================================================================================================================
## Plot each taste perception profile using Radar Plots
### Each plot will include the mean perception scores of each taste for the profile and for the cohort overall
### +/- 1 SD for the cohort overall will also be included and shaded in gray
#=======================================================================================================================

## Write function to create radar plot with shading for +/-1 SD --------------------------------------------------------
par(mar=c(3.1, 1.5, 2.1, 1.5))
prof_plot.fun = function(Ck){
  
  # Define margines for plot1 (with shading)
  par(fig=c(0.01,0.99,0.01,0.99))
  
  # Compile data; use cohort.sum.dat & prof.sum.dat
  data<-as.data.frame(rbind(rep(5,5), rep(0,5), cohort.sum.dat, prof.sum.dat[Ck,]))
  
  # Create spider plot with shading for +/- 1 SD                            
  radarchart(data, maxmin = T, caxislabels = NA, seg=5, pty=NA, plwd = c(2,2,3,6), plty=c(1,1,1,1), 
             pcol= c(greys2[1], greys2[1], greys2[2], "#000000"), cglcol = "grey",
             pfcol = c(greys2[1],"#FFFFFF",NA,NA), cglwd = 1, cglty = 3, vlabels = NA)
  
  #Define margins for plot2 - (without shading)
  par(fig=c(0.01,0.99,0.01,0.99), new=TRUE)
  
  # Create spider plot to overlay
  radarchart(data, maxmin=T, caxislabels = NA, seg=5,pty=NA, plwd = c(2,2,3,6), plty=c(1,1,1,1), 
             pcol= c(greys2[1], greys2[1], greys2[2], "#000000"), cglcol = "grey",
             axislabcol = "black", cglwd = 1, cglty = 3, vlcex = 1)
}


## Plot radar plots for each taste perception profile, "bind" & view----------------------------------------------------
profs.plot %<a-% {
  for (i in 1:Ck){
    (prof_plot.fun(i))
  }
}; profs.plot


########################################################################################################################
## STEP 4: Assess internal validity & stability ##
########################################################################################################################

#=======================================================================================================================
## Cluster Validity (Internal Cluster Validity Indices)
#=======================================================================================================================

## 1. Calinski-Harabasz Index ------------------------------------------------------------------------------------------
# Run NbClust to compute CH over the range of Cks 
set.seed(123, kind = "default")
ch.dat<-NbClust(data=mydata1, distance = "euclidean", min.nc = 2, max.nc = 10, 
                method = "kmeans", index = "ch")


## 2. Davies-Bouldin Index ---------------------------------------------------------------------------------------------
# Run NbClust to compute DB over the range of Cks
set.seed(123, kind = "default")
db.dat<-NbClust(data=mydata1, distance = "euclidean", min.nc = 2, max.nc = 10, 
                method = "kmeans", index = "db")


## Compile and save internal cluster validity indices (ch.dat, db.dat) -------------------------------------------------
valid.dat<-data.frame(cbind(as.numeric(ch.dat$All.index), as.numeric(db.dat$All.index)))
dimnames(valid.dat)<-list(c(paste(2:10)),c("CH", "DB"))


## View & save data.frame ----------------------------------------------------------------------------------------------
# View
print(valid.dat)

#=======================================================================================================================
## Cluster Stability (Jaccard Similarity Index, JI)
#=======================================================================================================================

## Write a function to calculate the JI  for any Ck --------------------------------------------------------------------
jac.fun<-function(Ck){
  # Calculate & store JI parameters for any given Ck
  set.seed(123, kind = "default")
  temp<-clusterboot(mydata1, clustermethod = kmeansCBI, nstart=50, k=Ck, B=100, count=F)
  
  # Save cluster assignment, cluster characteristics (perception scores) & JI as a "list" for each Ck
  list(prof.assign = temp$result$partition,
       jac.dat = cbind(Ck=rep(Ck, Ck), # print Ck
                       round(temp$result$result$centers, digits=2), # print mean perception score for each taste
                       JI=round(temp$bootmean, digits=3))) # print JI for each profile
}

## Run jac.fun using for loop over Ck from 2:10 ------------------------------------------------------------------------
jac.sum.dat<-lapply(Cks, jac.fun)  


# Print cluster characteristics and JI for all clusters with a Ck from 2:10 -------------------------------------------
for (i in 1:9){
  x<-(jac.sum.dat[[i]]$jac.dat)
  print(x)
}

## Aggregate into a table and save ------------------------------------------------------------------------------------
jac_all.dat <- matrix(NA, sum(2:10), 7, dimnames=list(NULL, c("Ck", names(mydata1), "JI")))
for (i in 1:9){
  if (i == 1){rows = 1:2} else{rows = (sum(2:i)+1) : (sum(2:(i+1)))}
  jac_all.dat[rows,]<-jac.sum.dat[[i]]$jac.dat
}

## END OF SYNTAX ## 

# Last Updated: 2020-01-2019 By: Julie E. Gervis


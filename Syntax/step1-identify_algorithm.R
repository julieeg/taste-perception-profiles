## START OF SYNTAX ##

########################################################################################################################
## Step 1: Identify a reproducible cluster algorithm 
########################################################################################################################

#=======================================================================================================================
## Set up objects & Prepare
#=======================================================================================================================

## Set plot parameters and margins -------------------------------------------------------------------------------------
par(mfrow=c(1,1), mar=c(5.1,5.1,4.1,2.1), xpd=T)

## Create a list of Ck values to run for each reproducibility index ----------------------------------------------------
Cks<-(2:10)

## Create a list of colors for side-by-side boxplots -------------------------------------------------------------------
Greys<-c("#A0A0A0", "#404040")


#=======================================================================================================================
## Write main & supporting functions to run split half cross validation (SHCV) for each cluster algorithm 
#=======================================================================================================================

## Write functions to print cluster solutions for each cluster algorithm, for any Ck -----------------------------------
kca.fun<-function(data, Ck){
  as.numeric(kmeans(data, Ck, nstart=50)$cluster)
  }
wardD.fun<-function(data, Ck){
  as.numeric(cutree(hclust(dist(data), method = "ward.D"), Ck))
  }

## Create dataframe with only the 5 tastes for cluster analysis (only the 5 tastes) ------------------------------------
mydata1<-mydata[,1:5]

## write function to run SHCV for any given cluster algorithm and Ck ---------------------------------------------------
shcv.fun<-function(data, Ck, FUN){
  # Create temporary matrix to store data; 20 rows (20 estimates for each index) x 2 cols (1 for each index)
  temp<-matrix(NA,20,2, dimnames = list(c(1:20), c(paste("ARI.", Ck, sep=""), 
                                                   paste("CramV.", Ck, sep=""))))
  
  # Use for loop to generate 10 random test/train sets; for each set, generate 2 estimates for each index
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

## create matrices for storage -----------------------------------------------------------------------------------------
### 40 rows (20 per algorithm) x 10 columns (1 per Ck, from 2-10)
ari.dat<-matrix(c(rep(1,20), rep(2,20)),40,10,
                dimnames = list(1:40, c(paste("Ck.", 2:10, sep=""), "alg")))
cramv.dat<-matrix(c(rep(1,20), rep(2,20)),40,10,
                  dimnames = list(1:40, c(paste("Ck.", 2:10, sep=""), "alg")))

## Use for loop to run SHCV for each index, for Cks from 2:10 -----------------------------------------------------------
for (Ck in Cks){
  set.seed(2468)
  temp.kca<-shcv.fun(mydata1, Ck, kca.fun)  
  temp.wardD<-shcv.fun(mydata1, Ck, wardD.fun)  
  ari.dat[,Ck-1]<-cbind(temp.kca[,1], temp.wardD[,1])
  cramv.dat[,Ck-1]<-cbind(temp.kca[,2], temp.wardD[,2])
}

## save ari.dat and cramv.dat as .rda files ----------------------------------------------------------------------------
saveRDS(as.data.frame(ari.dat), file = "Output/Dat.ARI.rda")
saveRDS(cramv.dat, file = "Output/Dat.CramV.rda")


#=======================================================================================================================
## Visualize data using side-by-side boxplot
#=======================================================================================================================

## Convert each matrix to long format and "alg" (algorithm) to a factor var --------------------------------------------
ari_long.dat<-reshape(as.data.frame(ari.dat), varying = list(colnames(ari.dat)[1:9]),
                   timevar = "Ck", v.names = "ARI", direction = "long")
cramv_long.dat<-reshape(as.data.frame(cramv.dat), varying = list(colnames(cramv.dat)[1:9]),
                     timevar = "Ck", v.names = "CramV", direction = "long")


## Calculate mean index for each approach (1=KCA; 2=ward's D) 
ari_means <- lapply(1:2, function(x){mean(ari_long.dat$ARI[ari_long.dat$alg==x])})  
cramv_means <- lapply(1:2, function(x){mean(cramv_long.dat$CramV[cramv_long.dat$alg==x])})  


## A. Adjusted Rand Index ----------------------------------------------------------------------------------------------
ari.plot %<a-% {
  with(ari_long.dat,
       boxplot(ARI~alg, col=Greys, frame=T, axes=F, ylim=c(0,0.9),
               boxwex = 0.5, varwidth=F, ylab="ARI",
               xlab=NA, cex.lab=1.15, cex.main=1.25))
  axis(side=1, at=c(1,2), labels = c("KCA", "Ward's D"))
  axis(side=2, at=seq(0,0.9,0.1))
  points(1:2, ari_means, pch=9, cex=1.5)
  legend("topright", legend="Mean ARI",pch=9, pt.cex = 1.15, cex = 0.8)
  }

## Print ari summary box plot ------------------------------------------------------------------------------------------
ari.plot

## B. Cramer’s V -------------------------------------------------------------------------------------------------------
cramv.plot %<a-% {
  with(cramv.l.dat,
       boxplot(CramV~alg, col=Greys, frame=T, axes=F, ylim=c(0.2,0.92),
               boxwex = 0.5, varwidth=F, ylab="Cramer's V",
               xlab=NA, cex.lab=1.15, cex.main=1.25))
  axis(side=1, at=c(1,2), labels = c("KCA", "Ward's D"))
  axis(side=2, at=seq(0.2,0.92,0.1))
  points(1:2, cramv_means, pch=9, cex=1.5)
  legend("topright", legend = "Mean Cramer's V", pch=9, pt.cex=1.15, cex=0.8)
}

## Print CramV Summary Box Plot ----------------------------------------------------------------------------------------
cramv.plot

## Create 2 Panel Figure -----------------------------------------------------------------------------------------------
panel.repro.plot %<a-%{
  par(mar=c(5.1,5.1,4.1,2.1), mfrow=c(1,2))
  ari.plot
  cramv.plot
}

## Print panel plot of reproducibility indices ----------------------------------------------------------------------------
pdf("Output/Fig.Panel_ID_Alg.pdf", height=5, width=7)
panel.repro.plot
dev.off()

## END OF SYNTAX ##


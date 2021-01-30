## START OF SYNTAX ##

########################################################################################################################
## Step 2: Determine the optimal number of clusters (Ck) for the data
########################################################################################################################

#=======================================================================================================================
## Set Up 
#=======================================================================================================================

## Create a list of Ck values for each of the following indices --------------------------------------------------------  
Cks<-2:10

## Set plot margins and parameters ------------------------------------------------------
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), xpd=T)

#=======================================================================================================================
## A. Elbow Plot 
#=======================================================================================================================

## Write a function to calculate the total within-cluster sum of squares (Wk), for any Ck ------------------------------
Wk.fun<-function(Ck){
  set.seed(123, kind = "default") # choose a random set.seed, so results can be replicated
  kmeans(mydata1, Ck, nstart=50)$tot.withinss
}

## Run Wk.fun over the range of Ck -------------------------------------------------------------------------------------  
Wk.dat<-sapply(Cks, Wk.fun)

## Visualize to identify plot elbow
plot(Cks, Wk.dat, type="b", pch=19) # Elbow @ Ck = 4

# Compile data for inset plot (2 through 5)
Ck_inset <- 2:5
Wk_inset.dat<-sapply(Ck_inset, Wk.fun)

## Plot Wk.dat & save as elbow.plot  -----------------------------------------------------------------------------------
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

## View & Save Elbow plot ---------------------------------------------------------------------------
elbow_inset.plot

# Save as .pdf
pdf("Output/Fig.Step2-Ck_Elbow.pdf", height = 6, width = 6.5)
elbow_inset.plot
dev.off()

#=======================================================================================================================
## B. Average Silhouette Statistic Plot 
#=======================================================================================================================

## Write function to calculate average silhouette, for any given Ck ---------------------
sil.fun<-function(Ck){
  set.seed(123, kind = "default")
  avg.sil<-mean(silhouette(kmeans(mydata1, Ck, nstart=50)$cluster, 
                           dist(mydata1))[,3])}

## Run sil.fun over the range of Ck -----------------------------------------------------
sil.dat<-sapply(Cks, sil.fun)

## Plot sil.dat and set graphical parameters; save as sil.plot --------------------------
dev.off() # reset plot parameters from Elbow plot with inset
sil.plot %<a-%{
  par(mar=c(5.1,5.1,1.1,2.1))
  plot(Cks, sil.dat, type = "b", pch = 19, frame = T, 
       xlab = expression("Number of Clusters (C"[k]*")"),
       ylab = "Average Silhouette", cex.lab=1.5, cex.axis = 1.25)
  axis(side=1, at=seq(3,9,2), cex.axis=1.25)
}

## Print Average Silhouette Plot --------------------------------------------------------
sil.plot

## Save sil.plot as .pdf ----------------------------------------------------------------
pdf("Output/Fig.Step2-Ck_Sil.pdf")
sil.plot
dev.off()


#=======================================================================================================================
## C. Gap Statistic Plot 
#=======================================================================================================================

## Calculate Gap Statistic using clusGap function, for any given Ck --------------------
## NOTE: May see time lag when running, since function relies on bootstrap sampling
set.seed(123, kind = "default")
gap.dat<-clusGap(mydata1, kmeans, nstart=25,
                 K.max = 10, B=50, verbose = FALSE) 

# plot gap statistic and set graphical parameters; save as gap.plot ---------------------
gap.plot %<a-%{
  plot(gap.dat$Tab[2:10,3], type="b", pch=19, frame=T, 
       xlab=expression("Number of Clusters (C"[k]*")"), 
       ylab="Gap Statistic", xlim=c(1,9), axes=F, cex.lab=1.5, cex.axis = 1.25)
  axis(side=1, at=1:9, labels = 2:10, cex.axis=1.25)
  axis(side=2, cex.axis=1.25)
}


## Print Gap Statistic Plot -------------------------------------------------------------
gap.plot


## save gap.plot as .pdf ----------------------------------------------------------------
pdf("Output/Fig.Step2-Ck_Gap.pdf")
gap.plot
dev.off()

## END OF SYNTAX ##

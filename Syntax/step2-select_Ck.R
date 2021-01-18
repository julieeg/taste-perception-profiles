## START OF SYNTAX ##

########################################################################################################################
## Step 2: Determine the optimal number of clusters (Ck) for the data
########################################################################################################################

#=======================================================================================================================
## Set Up 
#=======================================================================================================================

## Use a random number generator to set.seed() -------------------------------------------------------------------------
sample(10000,1) # 2468
N<-2468

## Create a list of Ck values for each of the following indices --------------------------------------------------------  
Cks<-2:10

## Set plot margins and parameters ------------------------------------------------------
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), xpd=T)

#=======================================================================================================================
## A. Elbow Plot 
#=======================================================================================================================

## Write a function to calculate the total within-cluster sum of squares (Wk), for any Ck ------------------------------
Wk.fun<-function(Ck){
  set.seed(N) # choose a random set.seed, so results can be replicated
  kmeans(mydata, Ck, nstart=50)$tot.withinss
}

## Run Wk.fun over the range of Ck ------------------------------------------------------  
Wk.dat<-sapply(Cks, Wk.fun)


## Plot Wk.dat & save as elbow.plot  --------------------------
elbow.plot %<a-% {
  plot(Cks, Wk.dat, axes=F, type="b", pch=19, frame = T,
       xlab=expression(bold("Number of Clusters, C"[k])),
       ylab = expression(bold("W"[k])), font.lab=2, cex.lab=1.15, cex.main=1.5) 
  axis(side=1, at=2:10) 
  axis(side=2) 
  legend("topright", legend = "Optimal Ck: 'Elbow'", bty="n", 
         text.col = "red", text.font = 3)
}

## Print elbow plot ---------------------------------------------------------------------
elbow.plot

## Save elbow plot as .pdf --------------------------------------------------------------
pdf("Output/Fig.Elbow.pdf")
elbow.plot
dev.off()

#=======================================================================================================================
## B. Average Silhouette Statistic Plot 
#=======================================================================================================================

## Write function to calculate average silhouette, for any given Ck ---------------------
sil.fun<-function(Ck){
  set.seed(N)
  avg.sil<-mean(silhouette(kmeans(mydata, Ck, nstart=50)$cluster, 
                           dist(mydata))[,3])}

## Run sil.fun over the range of Ck -----------------------------------------------------
sil.dat<-sapply(Cks, sil.fun)

## Plot sil.dat and set graphical parameters; save as sil.plot --------------------------
sil.plot %<a-%{
  plot(Cks, sil.dat, type = "b", pch = 19, frame = T, 
       xlab = expression(bold("Number of Clusters C"[k])),
       ylab = "Average Silhouette", font.lab=2, cex.lab=1.15, cex.main=1.5)
  axis(side=1, at=seq(3,9,2))
  legend("topright", legend = ("Optimal Ck: Maximum"), bty="n", 
         text.col = "red", text.font = 3)
}

## Print Average Silhouette Plot --------------------------------------------------------
sil.plot

## Save sil.plot as .pdf ----------------------------------------------------------------
pdf("Output/Fig.Avg_Sil.pdf")
sil.plot
dev.off()


#=======================================================================================================================
## C. Gap Statistic Plot 
#=======================================================================================================================

## Calculate Gap Statistic using clusGap function, for any given Ck --------------------
## NOTE: May see time lag when running, since function relies on bootstrap sampling
set.seed(N)
gap.dat<-clusGap(mydata1, kmeans, nstart=50,
                 K.max = Cks[9], B=50, verbose = FALSE) 

# plot gap statistic and set graphical parameters; save as gap.plot ---------------------
gap.plot %<a-%{
  plot(gap.dat$Tab[2:10,3], type="b", pch=19, frame=T, 
       xlab=expression(bold("Number of Clusters, C"[k])), 
       ylab="Gap Statistic", xlim=c(1,9), font.lab=2, cex.lab=1.15, axes=F, cex.main=1.5)
  axis(side=1, at=1:9, labels = 2:10)
  axis(side=2)
  legend("topleft", legend = ("Optimal Ck: Maximum"), bty = "n",
         text.col = "red", text.font = 3)
}


## Print Gap Statistic Plot -------------------------------------------------------------
gap.plot

## save gap.plot as .pdf ----------------------------------------------------------------
pdf("Output/Fig.Gap.pdf")
gap.plot
dev.off()

#=======================================================================================================================
## Prepare 3 panel figure summarizing the data 
#=======================================================================================================================

## Prepare panel figure
panel.Ck.plot %<a-%{
  par(mfrow=c(1,3))
  elbow.plot
  sil.plot
  gap.plot
}

## Print panel.plot --------------------------------------------------------------------------
panel.Ck.plot

## Save panel.plot as .pdf -------------------------------------------------------------------
pdf("Output/Fig.Panel_Ck.pdf", height=3.5, width=7)
panel.Ck.plot
dev.off()



#################################################################################################

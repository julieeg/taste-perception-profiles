## START OF SYNTAX ##

########################################################################################################################
## STEP 4: Assess internal validity & stability ##
########################################################################################################################

#=======================================================================================================================
## A. Cluster Validity (Internal Cluster Validity Indices)
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

#Save as .rda
saveRDS(valid.dat, file="Output/Tab.Step5-Validity.rda")


#=======================================================================================================================
## B. Cluster Stability (Jaccard Similarity Index, JI)
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

## Save JI data as .rda file -----------------------------------------------------------------------------------------
saveRDS(jac_all.dat, file="Output/Dat.JI.rda")


## END OF SYNTAX ## 

# Last Updated: 2020-01-2019 By: Julie E. Gervis




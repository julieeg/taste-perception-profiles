## START OF SYNTAX ##

########################################################################################################################
## Step 3: Derive & visualize the taste perception profiles
########################################################################################################################

#=======================================================================================================================
## Set up objects & Prepare
#=======================================================================================================================

## Specify Ck selection (based on PART 2) ------------------------------------------------------------------------------
Ck<-4

#=======================================================================================================================
## A. Derive taste perception profiles using KCA with Ck = 4 
#=======================================================================================================================

## Derive & save profile assignments to mydata -------------------------------------------------------------------------
set.seed(123, kind = "default")
prof.dat<-kmeans(mydata1, Ck, nstart=25)$cluster
mydata$profile<-as.factor(prof.dat)


#=======================================================================================================================
## Aggregate mean (SD) perception scores for each taste per profile and overall cohort
#=======================================================================================================================

## Summarize mean (+/- 1SD) for the overall cohort -----------------------------------------------------------------------
# Write functions to calculate lower (-1SD) and upper (+1SD) values
lower<-function(x){mean(x)-sd(x)}
upper<-function(x){mean(x)+sd(x)}

# Use lower/upper functions to compile data 
cohort.sum.dat<-rbind(upper=sapply(mydata1, upper), lower=sapply(mydata1, lower), mean=sapply(mydata1, mean))


## Summarize mean perception for each profile via for loop -------------------------------------------------------------
# Create empty matrix to store results 
prof.sum.dat<-matrix(NA, Ck, 5, dimnames = list(paste("prof", 1:Ck, sep=""), names(mydata1)))

# Run via for loop
for (Ck in 1:Ck){
  prof.sum.dat[Ck,]<-sapply(mydata1[mydata$profile==Ck,], mean)
}


#=======================================================================================================================
## Examine taste perception profiles (within & between-cluster variation) 
#=======================================================================================================================

## All F-test P-values (for each taste) < 0.001 ------------------------------------------------------------------------
with(mydata, for(taste in list(sweet, salt, sour, bitter, umami)){
  print(anova(lm(taste~profile))$'Pr(>F)'[1])
  })

#=======================================================================================================================
## B. Plot each taste perception profile using Radar Plots
## Each plot will include the mean perception scores of each taste for the profile and for the cohort overall
## +/- 1 SD for the cohort overall will also be included and shaded in gray
#=======================================================================================================================

## Load in color pallete -----------------------------------------------------------------------------------------------
greys<-c("#E1E1E190", "#62626290")

## Write function to create radar plot with shading for +/-1 SD --------------------------------------------------------
par(mar=c(3.1, 1.5, 2.1, 1.5))
prof_plot.fun = function(Ck){
  # Define margines for plot1 (with shading)
  par(fig=c(0.01,0.99,0.01,0.99))
  
  # Compile data; use descr.dat from Part 1 (specify order: Mean, +1SD, -1SD)
  data<-as.data.frame(rbind(rep(5,5), rep(0,5), cohort.sum.dat, prof.sum.dat[Ck,]))
  
  # Create spider plot with shading                              
  radarchart(data, maxmin = T, caxislabels = NA, seg=5, pty=NA, plwd = c(2,2,3,6), plty=c(1,1,1,1), 
             pcol= c(greys[1], greys[1], greys[2], "#000000"), cglcol = "grey",
             pfcol = c(greys[1],"#FFFFFF",NA,NA), cglwd = 1, cglty = 3, vlabels = NA)
  
  #Define margins for plot2 - (without shading)
  par(fig=c(0.01,0.99,0.01,0.99), new=TRUE)
  
  # Create spider plot to overlay
  radarchart(data, maxmin=T, caxislabels = NA, seg=5,pty=NA, plwd = c(2,2,3,6), plty=c(1,1,1,1), 
             pcol= c(greys[1], greys[1], greys[2], "#000000"), cglcol = "grey",
             axislabcol = "black", cglwd = 1, cglty = 3, vlcex = 2.5)
  }


#### Prepare Figure 3 ########################################################################

## Plot spider plots for each taste perception profile ----------------------------------
profs.plot %<a-% {
  for (i in 1:Ck){
    (prof_plot.fun(i))
  }
}

## print plots, 1 at a time -------------------------------------------------------------
profs.plot

## Save plots as .pdf -------------------------------------------------------------------
pdf("Output/Fig.Step3-Plot_Profs.pdf", height = 7.5, width = 7.5)
par(mar=c(3.1, 1.1, 3.1, 0.1))
profs.plot
dev.off()

## END OF SYNTAX ##


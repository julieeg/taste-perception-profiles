## Saving file outputs generated in Data-Driven-Clustering.R
## Created by: Julie E. Gervis
## Created on: 2021-02-01

## START OF SYNTAX ##

##########################################
## Save relevant figures as .pdf files  ## 
##########################################

## Save reproducibility panel plot 
pdf("../Output/Fig.Step2-Alg_Reproduc-2.pdf", height = 6, width = 10)
par(mar=c(5.1,5.1,1.1,2.1))
panel.repro.plot
dev.off()


## Save elbow plot
pdf("../Output/Fig.Step2-Ck_Elbow-2.pdf", height = 5, width = 6)
par(mar=c(5.1,5.1,1.1,2.1))
elbow_inset.plot
dev.off()


## Save average silhouette & gap statistics plots
pdf("../Output/Fig.Step2-Ck_Sil_Gap.pdf", height = 6, width = 10)
par(mar=c(5.1,5.1,1.1,2.1), mfrow=c(1,2))
sil.plot
gap.plot
dev.off()


## Save Profile plots
pdf("../Output/Fig.Step3-Profiles.pdf", height=7.5, width=7.5)
par(mar=c(3.1, 1.1, 3.1, 0.1))
profs.plot
dev.off()

#############################################
## Save relevant data.frames as .csv files ## 
############################################3

## Export ARI.dat to .csv
write.csv(ari.dat, "../Output/ARI.csv")


## Export Cramer's V data to .csv
write.csv(cramv.dat, "../Output/CramV.csv")


## Export jac.dat to .csv
write.csv(jac_all.dat, "../Output/JI.csv")


## END OF SYNTYAX ##

## Last Updated: 2020-02-01



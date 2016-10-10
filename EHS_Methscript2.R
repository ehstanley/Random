#Histograms and Scatterplots for MethDB review paper- LCL code- Histograms_LakeMethDB_2016-07-23
#All observations have been aggregated at the site level
#Input Download Date

# Code outputs to 'Bastviken' subfolder
# Summary stats table (LakeFluxTable1_Date)
# Histograms of fluxes (LakeFlux_Histrograms_Date)
# Boxplots of eutrophic status vs fluxes (LakeFlux_EutrophicBoxplots_Date)

rm(list=ls(all=TRUE))

# set working directory to your machine. 
setwd("C:/Users/ehstanley/Dropbox/Methanogenesis/Bastviken")

Date<-as.Date("2016-07-23")

# =============================
# Don't change anything below. 
# =============================

Flux1<-read.csv(paste("LakeFlux_mmol_m2_d_converted_", Date, ".csv", sep=""), header=TRUE, stringsAsFactors = F)

#select for just lakes, and then eliminate littoral/macrophyte habitats within lakes
FluxL<- Flux1[Flux1$System.Type %in% c("lake", "multiple lakes"), ]
Flux <-FluxL[FluxL$Site.Type %in% c("whole lake", "pelagic", "unknown"), ]

# set factors for eutrophic column; Histogram will plot in this order
Flux$eutrophic.[Flux$eutrophic.=="unknown"] <-"unk"
Flux$eutrophic. = factor(Flux$eutrophic.,c("unk", "no", "yes"))

# Convert to numeric
Flux$CH4_Diffusion_mmol_m2_d <- as.numeric(as.character(Flux$CH4_Diffusion_mmol_m2_d))
Flux$CH4_Ebullition_mmol_m2_d <- as.numeric(as.character(Flux$CH4_Ebullition_mmol_m2_d))
Flux$CH4_Diff_plus_Eb_mmol_m2_d <- as.numeric(as.character(Flux$CH4_Diff_plus_Eb_mmol_m2_d))
Flux$CH4_Storage_mmol_m2_d <- as.numeric(as.character(Flux$CH4_Storage_mmol_m2_d))
Flux$CH4_Annual_Budget_mmol_m2_d<-as.numeric(as.character(Flux$CH4_Annual_Budget_mmol_m2_d))
Flux$Surface.Area..m2. <-as.numeric(as.character(Flux$Surface.Area..m2.))
Flux$Latitude <-as.numeric(as.character(Flux$Latitude))

###Assign sites to a latitude band based on Bastviken et al. 2011##
Flux$Zone[Flux$Latitude>66] <-"Arctic"
Flux$Zone[Flux$Latitude>54 & Flux$Latitude <=66] <- "Boreal"
Flux$Zone[Flux$Latitude>25 & Flux$Latitude <=54] <- "Temperate"
Flux$Zone[Flux$Latitude <=25] <- "Tropical"
  
View(Flux)



# Summary Stats

# Diff Flux
Diff<-Flux[which(is.na(Flux$CH4_Diffusion_mmol_m2_d)==FALSE),]
Diff.summary<-summary(Diff$CH4_Diffusion_mmol_m2_d)

aggregate(Diff$CH4_Diffusion_mmol_m2_d, list(Diff$eutrophic.), mean)

Diff.n<-length(unique(Diff$Site.Name)) 
print(Diff.n)

Diff.0<-length(which(Diff$CH4_Diffusion_mmol_m2_d<=0))
print(Diff.0)

Diff.sd<-sd(Diff$CH4_Diffusion_mmol_m2_d)
Diff.se<-sd(Diff$CH4_Diffusion_mmol_m2_d)/sqrt(length(Diff$CH4_Diffusion_mmol_m2_d))

# Ebullition Flux
Eb<-Flux[which(is.na(Flux$CH4_Ebullition_mmol_m2_d)==FALSE),]
Eb.summary<-summary(Eb$CH4_Ebullition_mmol_m2_d)

Eb.n<-length(unique(Eb$Site.Name)) 
print(Eb.n)

Eb.0<-length(which(Eb$CH4_Ebullition_mmol_m2_d<=0))
print(Eb.0)

Eb.sd<-sd(Eb$CH4_Ebullition_mmol_m2_d)
Eb.se<-sd(Eb$CH4_Ebullition_mmol_m2_d)/sqrt(length(Eb$CH4_Ebullition_mmol_m2_d))

# Diff plus Eb Flux
Diff_Eb<-Flux[which(is.na(Flux$CH4_Diff_plus_Eb_mmol_m2_d)==FALSE),]
Diff_Eb.summary<-summary(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d)

Diff_Eb.n<-length(unique(Diff_Eb$Site.Name)) 
print(Diff_Eb.n)

Diff_Eb.0<-length(which(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d<=0))
print(Diff_Eb.0)

Diff_Eb.sd<-sd(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d)
Diff_Eb.se<-sd(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d)/sqrt(length(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d))

# Diff Only Flux
JustDiff<-Flux[which(!is.na(Flux$CH4_Diffusion_mmol_m2_d) & is.na(Flux$CH4_Diff_plus_Eb_mmol_m2_d)),]
JustDiff.summary<-summary(JustDiff$CH4_Diffusion_mmol_m2_d)

JustDiff.n<-length(unique(JustDiff$Site.Name)) 
print(JustDiff.n)

JustDiff.0<-length(which(JustDiff$CH4_Diffusion_mmol_m2_d<=0))
print(JustDiff.0)

JustDiff.sd<-sd(JustDiff$CH4_Diffusion_mmol_m2_d)
JustDiff.se<-sd(JustDiff$CH4_Diffusion_mmol_m2_d)/sqrt(length(JustDiff$CH4_Diffusion_mmol_m2_d))

# Storage Flux
Store<-Flux[which(is.na(Flux$CH4_Storage_mmol_m2_d)==FALSE),]
Store.summary<-summary(Store$CH4_Storage_mmol_m2_d)

Store.n<-length(unique(Store$Site.Name)) 
print(Store.n)

Store.0<-length(which(Store$CH4_Storage_mmol_m2_d<=0))
print(Store.0)

Store.sd<-sd(Store$CH4_Storage_mmol_m2_d)
Store.se<-sd(Store$CH4_Storage_mmol_m2_d)/sqrt(length(Store$CH4_Storage_mmol_m2_d))

# Annual Budget

Budget<-Flux[which(is.na(Flux$CH4_Annual_Budget_mmol_m2_d)==FALSE),]
Budget.summary<-summary(Budget$CH4_Annual_Budget_mmol_m2_d)

Budget.n<-length(unique(Budget$Site.Name)) 
print(Budget.n)

Budget.0<-length(which(Budget$CH4_Annual_Budget_mmol_m2_d<=0))
print(Budget.0)

Budget.sd<-sd(Budget$CH4_Annual_Budget_mmol_m2_d)
Budget.se<-sd(Budget$CH4_Annual_Budget_mmol_m2_d)/sqrt(length(Budget$CH4_Annual_Budget_mmol_m2_d))


# ===========================
# Summary Table 1
# ===========================

D.Summary<-c(Diff.summary[4],Diff.sd,Diff.summary[3],Diff.n,Diff.0, Diff.summary[1], Diff.summary[6])
names(D.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
D.Summary

E.Summary<-c(Eb.summary[4],Eb.sd,Eb.summary[3],Eb.n,Eb.0, Eb.summary[1], Eb.summary[6])
names(E.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
E.Summary

DE.Summary<-c(Diff_Eb.summary[4],Diff_Eb.sd,Diff_Eb.summary[3],Diff_Eb.n,Diff_Eb.0, Diff_Eb.summary[1], Diff_Eb.summary[6])
names(DE.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
DE.Summary

J.Summary<-c(JustDiff.summary[4],JustDiff.sd,JustDiff.summary[3],JustDiff.n,JustDiff.0, JustDiff.summary[1], JustDiff.summary[6])
names(J.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
J.Summary

S.Summary<-c(Store.summary[4],Store.sd,Store.summary[3],Store.n,Store.0, Store.summary[1], Store.summary[6])
names(S.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
S.Summary

B.Summary<-c(Budget.summary[4],Budget.sd,Budget.summary[3],Budget.n,Budget.0, Budget.summary[1], Budget.summary[6])
names(B.Summary)<-c("Mean", "SD", "Median", "n","sites <= 0", "Min", "Max")
B.Summary


allfluxn<-length(unique(c(Diff$Site.Name, Eb$Site.Name,Diff_Eb$Site.Name,Store$Site.Name,Budget$Site.Name)))
print(allfluxn)

All.Summary<-c(rep(NA, 7))
All.Summary[4]<-allfluxn
names(All.Summary)<-names(B.Summary)

Table1<-as.data.frame(rbind(D.Summary, E.Summary, DE.Summary, J.Summary, S.Summary, B.Summary, All.Summary))
Table1$Type<-c("Diffusive Flux (mmol m-2 d-1)", "Ebullitive Flux (mmol m-2 d-1)","Diff plus Eb Flux (mmol m-2 d-1)", "Diffusive Only Flux (mmol m-2 d-1)", "Storage Flux (mmol m-2 d-1)",   "Annual Budget (mmol m-2 d-1)", "Any Flux")
Table2<-Table1[,c(8,1:7)]

write.table(Table2, file=paste("LakeFluxTable1_", Date, ".csv", sep=""), col.names=TRUE, row.names=F, sep=",")

DiffTable <-data.frame(aggregate(Diff$CH4_Diffusion_mmol_m2_d, list(Diff$eutrophic.), mean))
EbTable <- data.frame(aggregate(Eb$CH4_Ebullition_mmol_m2_d, list(Eb$eutrophic.), mean))
Diff_EbTable <-data.frame(aggregate(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d, list(Diff_Eb$eutrophic.), mean))
StoreTable <-data.frame(aggregate(Store$CH4_Storage_mmol_m2_d, list(Store$eutrophic.), mean))

names(DiffTable)[names(DiffTable)=="Group.1"] <-"T_state"
names(DiffTable)[names(DiffTable)=="x"] <-"DiffMean"

names(EbTable)[names(EbTable)=="Group.1"] <-"T_state"
names(EbTable)[names(EbTable)=="x"] <-"EbMean"

names(Diff_EbTable)[names(Diff_EbTable)=="Group.1"] <-"T_state"
names(Diff_EbTable)[names(Diff_EbTable)=="x"] <-"Diff_EbMean"

names(StoreTable)[names(StoreTable)=="Group.1"] <-"T_state"
names(StoreTable)[names(StoreTable)=="x"] <-"StoreMean"



#boxplots
par(mfrow=c(1,3))

par(mar=c(5, 6, 4, 6))
Diff$Eutrophic = factor(Diff$eutrophic.,c("unk", "no", "yes","very"))
Diff$Eutrophic[which(Diff$Eutrophic=='NA')] <-"unk"
boxplot(Diff$CH4_Diffusion_mmol_m2_d ~ Diff$eutrophic., ylim= c(0, 55), ylab ="Diffusive CH4 flux (mmol m-2 d-1)")

par(mar=c(5, 6, 4, 6))
Diff_Eb$Eutrophic = factor(Diff_Eb$eutrophic.,c("unk", "no", "yes","very"))
Diff_Eb$Eutrophic[which(Diff_Eb$Eutrophic=='NA')] <-"unk"
boxplot(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d ~ Diff_Eb$eutrophic., ylim= c(0, 55), ylab ="Diff + Eb CH4 flux (mmol m-2 d-1)")

par(mar=c(5, 6, 4, 6))
Store$Eutrophic = factor(Store$eutrophic.,c("unk", "no", "yes","very"))
Store$Eutrophic[which(Store$Eutrophic=='NA')] <-"unk"
boxplot(Store$CH4_Storage_mmol_m2_d ~ Store$eutrophic., ylim= c(0, 10), ylab ="Storage flux (mmol m-2 d-1)")


kruskal.test(Diff$CH4_Diffusion_mmol_m2_d ~ Diff$eutrophic.)
kruskal.test(Eb$CH4_Ebullition_mmol_m2_d ~ Eb$eutrophic.) 
kruskal.test(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d ~ Diff_Eb$eutrophic.)
kruskal.test(Store$CH4_Storage_mmol_m2_d ~ Store$eutrophic.)

par(mfrow=c(1,1))
boxplot(Diff$CH4_Diffusion_mmol_m2_d ~Diff$Anthrome_Class)
par(mfrow=c(1,1))
boxplot(Diff$CH4_Diffusion_mmol_m2_d~Diff$Anthrome_Level)

aggregate(Diff$CH4_Diffusion_mmol_m2_d, list(Diff$eutrophic.), median)
aggregate(Eb$CH4_Ebullition_mmol_m2_d, list(Eb$eutrophic.), median)
aggregate(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d, list(Diff_Eb$eutrophic.), median)
aggregate(Store$CH4_Storage_mmol_m2_d, list(Store$eutrophic.), median)

plot(Flux$Latitude, Flux$CH4_Diffusion_mmol_m2_d)
plot(Flux$Latitude, Flux$CH4_Diff_plus_Eb_mmol_m2_d)
plot(Flux$Latitude, Flux$CH4_Ebullition_mmol_m2_d)

plot(Flux$Surface.Area..m2., Flux$CH4_Diffusion_mmol_m2_d, xlim=c(0, 5.0e+08))
plot(Flux$Surface.Area..m2., Flux$CH4_Diff_plus_Eb_mmol_m2_d, xlim=c(0, 1.0e+08))
plot(Flux$Surface.Area..m2., Flux$CH4_Ebullition_mmol_m2_d, xlim=c(0, 5.0e+06))


##by Zone
par(mfrow=c(1,3))
par(mar=c(5, 6, 4, 6))
boxplot(Diff$CH4_Diffusion_mmol_m2_d ~ Diff$Zone, ylab ="Diffusive CH4 flux (mmol m-2 d-1)")

par(mar=c(5, 6, 4, 6))
boxplot(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d ~ Diff_Eb$Zone, ylab ="Diff + Eb CH4 flux (mmol m-2 d-1)")

par(mar=c(5, 6, 4, 6))
boxplot(Store$CH4_Storage_mmol_m2_d ~ Store$Zone, ylab ="Storage flux (mmol m-2 d-1)")

aggregate(Diff$CH4_Diffusion_mmol_m2_d, list(Diff$Zone), median)
aggregate(Eb$CH4_Ebullition_mmol_m2_d, list(Eb$Zone), median)
aggregate(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d, list(Diff_Eb$Zone), median)
aggregate(Store$CH4_Storage_mmol_m2_d, list(Store$Zone), median)

Diff$Zone = factor(Diff$Zone,c("Arctic", "Boreal", "Temperate","Tropical"))
Eb$Zone = factor(Eb$Zone, c("Arctic", "Boreal", "Temperate","Tropical"))
Diff_Eb$Zone = factor(Diff_Eb$Zone, c("Arctic", "Boreal", "Temperate","Tropical"))
Store$Zone = factor(Store$Zone, c("Arctic", "Boreal", "Temperate","Tropical"))

kruskal.test(Diff$CH4_Diffusion_mmol_m2_d ~ Diff$Zone)
kruskal.test(Eb$CH4_Ebullition_mmol_m2_d ~ Eb$Zone)
kruskal.test(Diff_Eb$CH4_Diff_plus_Eb_mmol_m2_d ~ Diff_Eb$Zone)
kruskal.test(Store$CH4_Storage_mmol_m2_d ~ Store$Zone)

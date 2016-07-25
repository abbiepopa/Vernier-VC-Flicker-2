###Descriptives for Largest Dataset###

###Get the Flicker Phase Participant List###
setwd("~/Documents/Lab/i22q 2016/Data")
flicker<-read.table("FlickerPhase_Outcome_File.txt", header=T)

###Get Vernier Participant List###
setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker/Data")
V<- read.csv("Vernier_Long.csv")

###Get Vernier-Crowded Participant List###
setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker/Data")
VC <- read.csv("VernierCrowded_Long.csv")

colnames(V)[1]<-"CabilID"

V$task = "Vernier"
VC$task = "VernierCrowded"
flicker$task = "Flicker"

parts<-rbind(V[,c("CabilID","task")],VC[,c("CabilID","task")],flicker[,c("CabilID","task")])

participation<-table(parts$CabilID, parts$task)

participation<-as.data.frame.matrix(participation)

participation$sum<-participation$Flicker + participation$Vernier + participation$VernierCrowded

###This will be a useful table if we need to know who did which task###

id<-read.csv("CABILID.csv")

participation$cabil<-factor(row.names(participation))

id$cabil<-factor(substr(as.character(id$STUDY..OldCABILID), 1,3))

id<-id[,c("cabil","Gender","Diagnosis","STUDY..Age")]

tog<-unique(merge(participation, id))
tog<-tog[which(tog$Diagnosis != ""),]

write.csv(tog,"processing.csv",row.names=F)

newtog<-read.csv("processing.csv")

library(psych)

newtog$Dx<-newtog$Diagnosis
newtog[which(newtog$Diagnosis == "Klinefelter's Syndrome"),"Dx"]<-"Klinefelter's Syndrome (XXY)"
newtog[which(newtog$Diagnosis == "Trisomy X"),"Dx"]<-"Trisomy X (XXX)"

describeBy(newtog$Age.In.Months, group=newtog$Dx)

library(car)

Anova(lm(Age.In.Months~Dx, data=newtog))

newtog$DX2<-as.character(newtog$Dx)
newtog[which(newtog$Dx == "Klinefelter's Syndrome (XXY)"),"DX2"]<-"sca"
newtog[which(newtog$Dx == "Trisomy X (XXX)"),"DX2"]<-"sca"

###IQ###
###Who is missing IQ###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
WASI<-read.csv("WASI.csv")
WASI.II<-read.csv("WASI-II.csv")
WISC.III<-read.csv("WISC-III.csv")
WISC.IV<-read.csv("WISC-IV.csv")

WASI<-WASI[,c("CABIL_ID","Study_ID","WASI_Verb_IQ","WASI_Perf_IQ","WASI_Full4_IQ")]
WASI.II<-WASI.II[,c("CABIL_ID","Study_ID","WASI_II_Verbal_Comp_IQ","WASI_II_Perc_Rsng_IQ","WASI_II_Full4_IQ")]
WISC.III<-WISC.III[,c("CABIL_ID","Study_ID","WISCIII_VIQ","WISCIII_PIQ","WISCIII_FSIQ")]
WISC.IV<-WISC.IV[,c("CABIL_ID","Study_ID","WISCIV_VerbalComprehension_C","WISCIV_PerceptualReasoning_C","WISCIV_FullScale_C")]

WASI$test<-"WASI"
WASI.II$test<-"WASI-II"
WISC.III$test<-"WISC-III"
WISC.IV$test<-"WISC-IV"

colnames(WASI)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WASI.II)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.III)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.IV)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")

IQ<-rbind(WASI,WASI.II,WISC.III,WISC.IV)


setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker")

all_IQ<-merge(newtog, IQ, all.x=T, by="")

noFSIQ<-all_IQ[which(is.na(all_IQ$FSIQ)),"cabil"]
noVIQ<-all_IQ[which(is.na(all_IQ$VIQ)),"cabil"]
noPIQ<-all_IQ[which(is.na(all_IQ$PIQ)),"cabil"]

write.csv(noFSIQ,"V_VC_flicker_NoIQ.csv",row.names=F)
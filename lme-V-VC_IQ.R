###Get Vernier Data###
setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker/Data")

V_X1<-read.csv("Vernier_Long_CleanedWithX1.csv")
V_X1X2<-read.csv("Vernier_Long_CleanedWithX1X2.csv")

VC_X1<-read.csv("VernierCrowded_Long_CleanedWithX1.csv")
VC_X1X2<-read.csv("VernierCrowded_Long_CleanedWithX1X2.csv")

missed_IQ<-read.csv("V_VC_flicker_NoIQ_filled.csv", na.strings="XXXX")

###Get Additional IQ Data###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
WASI<-read.csv("WASI.csv", na.strings="-999")
WASI.II<-read.csv("WASI-II.csv", na.strings="-999")
WISC.III<-read.csv("WISC-III.csv", na.strings="-999")
WISC.IV<-read.csv("WISC-IV.csv", na.strings="-999")

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

###IQ Bound Data###
setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker/Data")

IQ<-IQ[,c("cabil","studyid","VIQ","PIQ","FSIQ")]
missed_IQ<-missed_IQ[,c("cabil","VCI","PRI","FSIQ")]
colnames(missed_IQ)<-c("cabil","VIQ","PIQ","FSIQ")

colnames(VC_X1)[1]<-"cabilid"
colnames(VC_X1X2)[1]<-"cabilid"

missed_IQ<-merge(missed_IQ, unique(rbind(V_X1[,c("cabilid","Studyid")],
	V_X1X2[,c("cabilid","Studyid")])), all.x=T, by.x="cabil",by.y="cabilid")
missed_IQ<-missed_IQ[,c("cabil","Studyid","VIQ","PIQ","FSIQ")]
colnames(missed_IQ)[2]<-"studyid"

IQ<-rbind(IQ, missed_IQ)
colnames(IQ)[2]<-"Studyid"

###Output X1 with IQ###
V_X1_iq<-merge(V_X1, IQ, all.x=T, by="Studyid")
write.csv(V_X1_iq, "Vernier_Long_CleanedWithX1_IQ.csv", row.names=F)

###X1 FSIQ Models###
library(nlme)
V_X1_ff<-rbind(V_X1_iq[which(V_X1_iq$DX2 == "1td"),], V_X1_iq[which(V_X1_iq$DX2 == "a22q"),])

V_X1_fit1<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+FSIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit2<- lme(acc~Distance_Num*FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit3<- lme(acc~Distance_Num*FSIQ*as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit4<- lme(acc~Distance_Num*FSIQ+as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)

###X1 PIQ Models###
V_X1_fit1P<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+PIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit2P<- lme(acc~Distance_Num*PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit3P<- lme(acc~Distance_Num*PIQ*as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
##Only Distance and PIQ matter (i.e., not Dx), though be sure to check IQ between groups (obviously 22q is lower)
V_X1_fit4p<- lme(acc~Distance_Num*PIQ+as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)

###X1 VIQ Models###
V_X1_fit1V<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+VIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit2V<- lme(acc~Distance_Num*VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ, data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
V_X1_fit3V<- lme(acc~Distance_Num*VIQ*as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)
###here distance, VIQ and Dx matter, which makes sense since PIQ is more impacted in 22q than VIQ
V_X1_fit4v<- lme(acc~Distance_Num*VIQ+as.factor(DX2), data=V_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Output X1X2 with IQ###
V_X1X2_iq<-merge(V_X1X2, IQ, all.x=T, by="Studyid")
write.csv(V_X1X2_iq, "Vernier_Long_CleanedWithX1X2_IQ.csv", row.names=F)

###X1X2 FSIQ Models###
V_X1X2_ff<-rbind(V_X1X2_iq[which(V_X1X2_iq$DX2 == "1td"),], V_X1X2_iq[which(V_X1X2_iq$DX2 == "a22q"),])

V_X1X2_fit1<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+FSIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit2<- lme(acc~Distance_Num*FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit3<- lme(acc~Distance_Num*FSIQ*as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit4<- lme(acc~Distance_Num*FSIQ+as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###X1X2 PIQ Models###
V_X1X2_fit1P<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+PIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit2P<- lme(acc~Distance_Num*PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit3P<- lme(acc~Distance_Num*PIQ*as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
##Only Distance and PIQ matter (i.e., not Dx), though be sure to check IQ between groups (obviously 22q is lower)
V_X1X2_fit4p<- lme(acc~Distance_Num*PIQ+as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###X1X2 VIQ Models###
V_X1X2_fit1V<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+VIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit2V<- lme(acc~Distance_Num*VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ, data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
V_X1X2_fit3V<- lme(acc~Distance_Num*VIQ*as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)
###here distance, VIQ and Dx matter, which makes sense since PIQ is more impacted in 22q than VIQ
V_X1X2_fit4v<- lme(acc~Distance_Num*VIQ+as.factor(DX2), data=V_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Vernier Crowded###

VC_X1_iq<-merge(VC_X1, IQ, all.x=T, by.x="cabilid", by.y="cabil")
table(VC_X1_iq$cabilid)[which(table(VC_X1_iq$cabilid)==70)]
###170 280 325 357 385 808  dupes###
###remove RO70MS
VC_X1_iq<-VC_X1_iq[which(VC_X1_iq$Studyid != "RO70MS"),]
###remove RO103EW
VC_X1_iq<-VC_X1_iq[which(VC_X1_iq$Studyid != "RO103EW"),]
###remove RO110SK
VC_X1_iq<-VC_X1_iq[which(VC_X1_iq$Studyid != "RO110SK"),]
###remove RO113GS
VC_X1_iq<-VC_X1_iq[which(VC_X1_iq$Studyid != "RO113GS"),]
###remove RO116SB
VC_X1_iq<-VC_X1_iq[which(VC_X1_iq$Studyid != "RO116SB"),]
###remove 808 dupes
VC_X1_iq<-VC_X1_iq[which(!((VC_X1_iq$cabilid == 808) & is.na(VC_X1_iq$FSIQ))),]
write.csv(VC_X1_iq, "VernierCrowded_Long_CleanedWithX1_IQ.csv", row.names=F)

VC_X1X2_iq<-merge(VC_X1X2, IQ, all.x=T, by.x="cabilid", by.y="cabil")
table(VC_X1X2_iq$cabilid)[which(table(VC_X1X2_iq$cabilid)==70)]
###remove RO103EW
VC_X1X2_iq<-VC_X1X2_iq[which(VC_X1X2_iq$Studyid != "RO103EW"),]
###remove RO110SK
VC_X1X2_iq<-VC_X1X2_iq[which(VC_X1X2_iq$Studyid != "RO110SK"),]
###remove RO113GS
VC_X1X2_iq<-VC_X1X2_iq[which(VC_X1X2_iq$Studyid != "RO113GS"),]
###remove RO116SB
VC_X1X2_iq<-VC_X1X2_iq[which(VC_X1X2_iq$Studyid != "RO116SB"),]
###remove 808 dupes
VC_X1X2_iq<-VC_X1X2_iq[which(!((VC_X1X2_iq$cabilid == 808) & is.na(VC_X1X2_iq$FSIQ))),]
write.csv(VC_X1X2_iq, "VernierCrowded_Long_CleanedWithX1X2_IQ.csv", row.names=F)

###X1 FSIQ Models###
library(nlme)
VC_X1_ff<-rbind(VC_X1_iq[which(VC_X1_iq$DX2 == "1td"),], VC_X1_iq[which(VC_X1_iq$DX2 == "a22q"),])
VC_X1_ff<-rbind(VC_X1_ff[which(VC_X1_ff$C2 == "least"),], VC_X1_ff[which(VC_X1_ff$C2=="most"),])

###All Main Effects
VC_X1_fit1<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2) , data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1_fit2<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Including interaction of distance and crowding seems to kill our crowding effect###

###interaction of distance and IQ
VC_X1_fit3<- lme(acc~Distance_Num*FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding and the interaciton of distance and FSIQ grab all the variance

###interaction of crowding and IQ
VC_X1_fit4<- lme(acc~Distance_Num + FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ*as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction

###interaction of Dx and IQ
VC_X1_fit5<- lme(acc~Distance_Num + FSIQ+as.factor(gender)+age_c+as.factor(DX2)*FSIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, lets make the pruned model with:
### interaction of distance and FSIQ, Crowding, and Diagnosis

VC_X1_fit6<- lme(acc~Distance_Num*FSIQ + as.factor(C2) + as.factor(DX2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

#####PIQ#####

###All Main Effects
VC_X1_fit1P<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2) , data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1_fit2P<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Including interaction of distance and crowding seems to kill our crowding effect###

###interaction of distance and IQ
VC_X1_fit3P<- lme(acc~Distance_Num*PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding and the interaciton of distance and PIQ grab all the variance

###interaction of crowding and IQ
VC_X1_fit4P<- lme(acc~Distance_Num + PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ*as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction

###interaction of Dx and IQ
VC_X1_fit5P<- lme(acc~Distance_Num + PIQ+as.factor(gender)+age_c+as.factor(DX2)*PIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, lets make the pruned model with:
### interaction of distance and PIQ, Crowding, and Diagnosis

VC_X1_fit6P<- lme(acc~Distance_Num*PIQ + as.factor(C2) + as.factor(DX2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

#####VIQ#####

###All Main Effects
VC_X1_fit1V<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2) , data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1_fit2V<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Including interaction of distance and crowding seems to kill our crowding effect###

###interaction of distance and IQ
VC_X1_fit3V<- lme(acc~Distance_Num*VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding ALONE grabs all the variance

###interaction of crowding and IQ
VC_X1_fit4V<- lme(acc~Distance_Num + VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ*as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction

###interaction of Dx and IQ
VC_X1_fit5V<- lme(acc~Distance_Num + VIQ+as.factor(gender)+age_c+as.factor(DX2)*VIQ + as.factor(C2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, lets make the pruned model with:
### interaction of distance, VIQ, Crowding, and Diagnosis (no interaction)

VC_X1_fit6P<- lme(acc~Distance_Num + VIQ + as.factor(C2) + as.factor(DX2), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

VC_X1_fit7P<- lme(acc~Distance_Num + VIQ + as.factor(C2) + as.factor(DX2) + as.factor(gender), data=VC_X1_ff, random= ~1|Studyid, na.action=na.omit)

#####X1X2#####

###FSIQ Models###
library(nlme)
VC_X1X2_ff<-rbind(VC_X1X2_iq[which(VC_X1X2_iq$DX2 == "1td"),], VC_X1X2_iq[which(VC_X1X2_iq$DX2 == "a22q"),])
VC_X1X2_ff<-rbind(VC_X1X2_ff[which(VC_X1X2_ff$C2 == "least"),], VC_X1X2_ff[which(VC_X1X2_ff$C2=="most"),])

###All Main Effects
VC_X1X2_fit1<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2) , data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1X2_fit2<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###distance and crowding interaction TREND ONLY, and now we don't have a crowding effect###

###interaction of distance and IQ
VC_X1X2_fit3<- lme(acc~Distance_Num*FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding ALONE grabs all the variance

###interaction of crowding and IQ
VC_X1X2_fit4<- lme(acc~Distance_Num + FSIQ+as.factor(gender)+age_c+as.factor(DX2)+FSIQ*as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction; ONLY distance sig

###interaction of Dx and IQ
VC_X1X2_fit5<- lme(acc~Distance_Num + FSIQ+as.factor(gender)+age_c+as.factor(DX2)*FSIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, 

###Interactions were never intersting, only main effects, and age and gender never even trended

VC_X1X2_fit6<- lme(acc~Distance_Num + as.factor(C2) + FSIQ + as.factor(DX2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

#####PIQ#####

###All Main Effects
VC_X1X2_fit1P<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2) , data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1X2_fit2P<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###distance and crowding interaction trend only, and now we don't have a crowding effect###

###interaction of distance and IQ
VC_X1X2_fit3P<- lme(acc~Distance_Num*PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding grabs all the variance interaciton of distance and PIQ trends

###interaction of crowding and IQ
VC_X1X2_fit4P<- lme(acc~Distance_Num + PIQ+as.factor(gender)+age_c+as.factor(DX2)+PIQ*as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction, only distance sig

###interaction of Dx and IQ
VC_X1X2_fit5P<- lme(acc~Distance_Num + PIQ+as.factor(gender)+age_c+as.factor(DX2)*PIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, 

###because only main effects were sig, and not age or gender we will have the same pruned model as for FSIQ

VC_X1X2_fit6P<- lme(acc~as.factor(C2) + Distance_Num + PIQ + as.factor(DX2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

### include interaction of distance and PIQ because it's a trend
VC_X1X2_fit7P<- lme(acc~Distance_Num*PIQ + as.factor(C2) + Distance_Num + as.factor(DX2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

#####VIQ#####

###All Main Effects
VC_X1X2_fit1V<- lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2) , data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Interaction of distance and crowding
VC_X1X2_fit2V<- lme(acc~Distance_Num*as.factor(C2)+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Including interaction of distance and crowding seems to kill our crowding effect, also it's just a trend###

###interaction of distance and IQ
VC_X1X2_fit3V<- lme(acc~Distance_Num*VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###here crowding ALONE grabs all the variance

###interaction of crowding and IQ
VC_X1X2_fit4V<- lme(acc~Distance_Num + VIQ+as.factor(gender)+age_c+as.factor(DX2)+VIQ*as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###there does not appear to be a crowding and IQ interaction

###interaction of Dx and IQ
VC_X1X2_fit5V<- lme(acc~Distance_Num + VIQ+as.factor(gender)+age_c+as.factor(DX2)*VIQ + as.factor(C2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Dx and IQ interaction also seems uninteresting, 

##same model as earlier

VC_X1X2_fit6V<- lme(acc~Distance_Num + VIQ + as.factor(C2) + as.factor(DX2), data=VC_X1X2_ff, random= ~1|Studyid, na.action=na.omit)

###Just compare IQ###

V_IQ<-unique(V_X1_iq[,c("cabilid","Dx","DX3","VIQ","PIQ","FSIQ")])

summary(lm(FSIQ~DX3, data=V_IQ))
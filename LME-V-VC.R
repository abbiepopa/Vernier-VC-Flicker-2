#################################################################
#                                                               #
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################
###### ### ## ###     VERNIER  LME MODELS       ### ## ### ######
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################
#                                                               #
#################################################################


##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
# setwd, read in files, take out irrelevant data, load libraries #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############

setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker")

library(nlme)

###read in data
V<- read.csv("Vernier_Long.csv")

V$DX2= ifelse(V$Dx%in%c("22q"),"a22q", ifelse(V$Dx%in%c("td"),"1td",as.factor(V$Dx)))
V$DX3= ifelse(V$Dx%in%c("22q"),"a22q", ifelse(V$Dx%in%c("td"),"1td",ifelse(V$Dx%in%c("xxy", "xxx"),"SCA",as.factor(V$Dx))))
V$age_c=V$Age_in_months-mean(V$Age_in_months)
V$gender= ifelse(V$Gender%in%c("female"),"female", ifelse(V$Gender%in%c("male"),"male",as.factor(V$Gender)))
V$Distance_Num<-as.numeric(substr(as.character(V$distance),2,2))

################################### ADDED AS OF 2/24/16 IN ORDER TO FIND THOSE WHO PERFORMED 50% OR ^ AT MULTIPLE DISTANCES
#if accuracy is greater than 50 then 0, if not then 1  ##NAs = visible if they exist, in this case they dont
V$acc_gt50= ifelse(V$acc>50,1,ifelse(is.na(V$acc),NA,0))

table(V$acc_gt50,V$distance,useNA="always")
a=do.call("rbind",as.list(by(V$acc_gt50,V$cabilid,sum,na.rm=T)))
a2=as.data.frame(cbind(V$cabilid[V$distance%in%c("X1")],a))
names(a2)=c("cabilid","num_gt50")
V2=merge(V,a2,by.x="cabilid",by.y="cabilid",all.x=T)
table(a2$num_gt50)
table(V2$num_gt50[V2$distance%in%c("X7")],V2$acc_gt50[V2$distance%in%c("X7")],useNA="always")

a_noX1=do.call("rbind",as.list(by(V$acc_gt50[!is.element(V$distance,"X1")],V$cabilid[!is.element(V$distance,"X1")],sum,na.rm=T)))
a_noX1_2=as.data.frame(cbind(V$cabilid[V$distance%in%c("X1")],a_noX1))
names(a_noX1_2)=c("cabilid","num_gt50")
V3=merge(V2,a_noX1_2,by.x="cabilid",by.y="cabilid",all.x=T)
table(a_noX1_2$num_gt50, useNA="always")

a_noX1X2=do.call("rbind",as.list(by(V$acc_gt50[!is.element(V$distance,c("X1","X2"))],V$cabilid[!is.element(V$distance,c("X1","X2"))],sum,na.rm=T)))
a_noX1X2_2=as.data.frame(cbind(V$cabilid[V$distance%in%c("X1")],a_noX1X2))
names(a_noX1X2_2)=c("cabilid","num_gt50")
table(a_noX1X2_2$num_gt50,useNA="always")
V3=merge(V2,a_noX1X2_2,by.x="cabilid",by.y="cabilid",all.x=T)
##########################################################################################################

## create groups
Q <- V[V$Dx%in%c("22q"),]
TD <- V[V$Dx%in%c("td"),]
Q_TD= V[V$Dx%in%c("22q", "td"),]
XXY <- V[V$Dx%in%c("xxy"),]
XXX <- V[V$Dx%in%c("xxx"),]
Q_TD_V= V[V$Dx%in%c("22q","td"),]

male22Q <- Q[Q$Gender%in%c("male"),]
female22Q <- Q[Q$Gender%in%c("female"),]
maleTD <- TD[TD$Gender%in%c("male"),]
femaleTD <- TD[TD$Gender%in%c("female"),]




###########~~~~~~~~~~~~~~~~~~~##########
#                                      #
#~~~~~~Linear Mixed Effects Models~~~~~#
#                                      #
###########~~~~~~~~~~~~~~~~~~~##########

#~~~~~~~~~~~~~~ OverallACC ~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~all  Main effects
fit_vernier= lme(acc~Distance_Num+as.factor(gender)+age_c+as.factor(DX2), data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier)
#~~~~~~~~~~~~#Findings: Value    Std. Err DF  t-value   p-value
Distance_Num           2.79471  0.186667 503 14.971657  0.0000   # accuracy increases with distance
as.factor(gender)male -6.79510  3.339118  59 -2.034998  0.0464   # accuracy decreases for males



#~~~~~~~~~~~distance and Gender interaction
fit_vernier_int_gd= lme(acc~Distance_Num*as.factor(gender)+age_c+as.factor(DX2), data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier_int_gd)
#~~~~~~~~~~~~#Interactions      Value    Std. Err  DF  t-value   p-value
Distance_Num                    2.61458  0.270547 502  9.664055  0.0000
as.factor(Gender)male          -8.51450  3.826636  59 -2.225061  0.0299



#~~~~~~~~~~~distance and DX and distance and age interaction
fit_vernier_acc_ddx1= lme(acc~as.factor(gender)+Distance_Num*as.factor(DX2)+Distance_Num*age_c, data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier_acc_ddx1)
#~~~~~~~~~~~~#Interactions:                Value    Std. Err  DF  t-value   p-value




#~~~~~~~~~~~distance and Age interaction
fit_vernier_int_age= lme(acc~Distance_Num*age_c+as.factor(gender)+as.factor(DX2), data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier_int_age)
#~~~~~~~~~~~~~#Interactions	 Value    Std. Err  DF  t-value   p-value



#~~~~~~~~~~~distance and Dx interaction
fit_vernier_int_age= lme(acc~Distance_Num*as.factor(DX2)+age_c+as.factor(gender), data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier_int_age)
#~~~~~~~~~~~~~#Interactions	 Value    Std. Err  DF  t-value   p-value



#~~~~~~~~~~~ 3 WAY INTERACTION PERFORMED OUT OF CURIOUSITY
fit_vernier_int= lme(acc~Distance_Num*as.factor(gender)*age_c+as.factor(DX2), data=Q_TD_V, random= ~1|Studyid)
summary(fit_vernier_int)
#~~~~~~~~~~~~#Interactions:                        Value    Std. Err  DF  t-value   p-value





#################################################################
#                                                               #
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################
###### ### ## ###  VERNIER CROWDED LME MODELS   ### ## ### ######
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################
#                                                               #
#################################################################


##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############
# setwd, read in files, take out irrelevant data, load libraries #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~############

setwd("~/Documents/Lab/Vernier Vernier-Crowded Flicker")

###read in data
VC <- read.csv("VernierCrowded_Long.csv")

VC$DX2= ifelse(VC$Dx%in%c("22q"),"a22q", ifelse(VC$Dx%in%c("td"),"1td",as.factor(VC$Dx)))
VC$DX3= ifelse(VC$Dx%in%c("22q"),"a22q", ifelse(VC$Dx%in%c("td"),"1td",ifelse(VC$Dx%in%c("xxy", "xxx"),"SCA",as.factor(VC$Dx))))
VC$age_c=VC$Age-mean(VC$Age)
VC$C2= ifelse(VC$Crowding%in%c("1"),"most", ifelse(VC$Crowding%in%c("5"),"least",as.factor(VC$Crowding)))
VC$Distance_Num<-as.numeric(substr(as.character(VC$distance),2,2))
VC$gender= ifelse(VC$Gender%in%c("female"),"female", ifelse(VC$Gender%in%c("male"),"male",as.factor(VC$Gender)))



################################################################################ BELOW ADDED AS OF 2/24/16
#if accuracy is greater than 50 then 0, if not then 1  ##including NAs in code
VC$acc_gt50= ifelse(VC$acc>50,1,ifelse(is.na(VC$acc),NA,0))



table(VC$acc_gt50[VC$C2%in%c("least")],VC$distance[VC$C2%in%c("least")],useNA="always")


################# ERROR NOT ALL OF THIS PART OF THE CODE RUNS! THIS IS THE PART ASKED ABBIE FOR HELP WITH BUT HAVE NOT RESOLVED YET 2/25/26
VC$acc_gt50= ifelse(VC$acc>50,1,ifelse(is.na(VC$acc),NA,0))

####Abbie Added to Identify Outliers####

VC1<-VC
VC<-VC[which(VC$C2=="least"),]
str(VC)
VC$acc_gt50= ifelse(VC$acc>50,1,ifelse(is.na(VC$acc),NA,0))
 
table(VC$acc_gt50,VC$distance,useNA="always")
a=do.call("rbind",as.list(by(VC$acc_gt50,VC$CabilID,sum,na.rm=T)))
a2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a))
names(a2)=c("CabilID","num_gt50")
VC2=merge(VC,a2,by.x="CabilID",by.y="CabilID",all.x=T)
table(a2$num_gt50)
table(VC2$num_gt50[VC2$distance%in%c("X7")],VC2$acc_gt50[VC2$distance%in%c("X7")],useNA="always")
a_noX1=do.call("rbind",as.list(by(VC$acc_gt50[!is.element(VC$distance,"X1")],VC$CabilID[!is.element(VC$distance,"X1")],sum,na.rm=T)))
a_noX1_2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a_noX1))
names(a_noX1_2)=c("CabilID","num_gt50")
VC3=merge(VC2,a_noX1_2,by.x="CabilID",by.y="CabilID",all.x=T)
 
a_noX1X2=do.call("rbind",as.list(by(VC$acc_gt50[!is.element(VC$distance,c("X1","X2"))],VC$CabilID[!is.element(VC$distance,c("X1","X2"))],sum,na.rm=T)))
a_noX1X2_2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a_noX1X2))
names(a_noX1X2_2)=c("CabilID","num_gt50")
table(a_noX1_2$num_gt50)
a_noX1_2[which(a_noX1_2$num_gt50 < 4),"CabilID"]
table(a_noX1X2_2$num_gt50)
a_noX1X2_2[which(a_noX1X2_2$num_gt50 < 4), "CabilID"]

####Old Code####
table(VC$acc_gt50,VC$distance,useNA="always")
a=do.call("rbind",as.list(by(VC$acc_gt50,VC$CabilID,sum,na.rm=T)))
a2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a))
names(a2)=c("CabilID","num_gt50")
VC2=merge(VC,a2,by.x="CabilID",by.y="CabilID",all.x=T)
table(a2$num_gt50)
table(VC2$num_gt50[VC2$distance%in%c("X7")],VC2$acc_gt50[VC2$distance%in%c("X7")],useNA="always")

a_noX1=do.call("rbind",as.list(by(VC$acc_gt50[!is.element(VC$distance,"X1")],VC$CabilID[!is.element(VC$distance,"X1")],sum,na.rm=T)))
a_noX1_2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a_noX1))
names(a_noX1_2)=c("CabilID","num_gt50")
VC3=merge(VC2,a_noX1_2,by.x="CabilID",by.y="CabilID",all.x=T)

a_noX1X2=do.call("rbind",as.list(by(VC$acc_gt50[!is.element(VC$distance,c("X1","X2"))],VC$CabilID[!is.element(VC$distance,c("X1","X2"))],sum,na.rm=T)))
a_noX1X2_2=as.data.frame(cbind(VC$CabilID[VC$distance%in%c("X1")],a_noX1X2))
names(a_noX1X2_2)=c("CabilID","num_gt50")
VC3=merge(VC2,a_noX1X2_2,by.x="CabilID",by.y="CabilID",all.x=T)
#################

################################################################################ ABOVE ADDED AS OF 2/24/16


## create groups
Q <- VC[VC$Dx%in%c("22q"),]
TD <- VC[VC$Dx%in%c("td"),]
Q_TD_VC= VC[VC$Dx%in%c("22q","td"),]

male22Q <- Q[Q$Gender%in%c("male"),]
female22Q <- Q[Q$Gender%in%c("female"),]
maleTD <- TD[TD$Gender%in%c("male"),]
femaleTD <- TD[TD$Gender%in%c("female"),]


Q_TD_VC_C2= Q_TD_VC[Q_TD_VC$C2%in%c("most","least"),]



###########~~~~~~~~~~~~~~~~~~~##########
#                                      #
#~~~~~~Linear Mixed Effects Models~~~~~#
#                                      #
###########~~~~~~~~~~~~~~~~~~~##########


#~~~~~~~~~~~all variables in a linear regression model
fit_vc= lme(acc~as.factor(C2)+age_c+as.factor(gender)+as.factor(DX2)+Distance_Num, data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc)
#~~~~~~~~~~~~#Findings: Value   Std. Err  DF  t-value   p-value
as.factor(C2)most     -4.91438  1.439119 855 -3.414851  0.0007 #as crowding increases accuracy decreases by -4.9
Distance_Num           3.79464  0.359549 855 10.553900  0.0000 #as distance increases accuracy increases by 3.79



#~~~~~~~~~~~all variables + interaction
fit_vc_int2= lme(acc~as.factor(C2)*as.factor(DX2)+age_c*as.factor(gender)*Distance_Num, data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int2)
#~~~~~~~~~~~~#Interactions:
# main effects seen: distance C2           Value   Std. Err DF  t-value   p-value
Distance_Num                              4.28193  0.478321 850  8.951997  0.0000
as.factor(C2)most                        -6.65202  2.173020 850 -3.061186  0.0023



#~~~~~~~~~C2 and distance interaction
fit_vc_int_crwd_dist= lme(acc~age_c+as.factor(C2)*Distance_Num+as.factor(DX2)+as.factor(gender), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_crwd_dist)
#~~~~~~~~~~~~#Interactions: 
#main effects seen: distance, C2	      Value   Std. Err  DF  t-value   p-value
Distance_Num                             4.27489  0.508246 854  8.411074  0.0000



#~~~~~~~~ Distance and Gender
fit_vc_int_dg= lme(acc~Distance_Num*as.factor(gender)+age_c+as.factor(C2)+as.factor(DX2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_dg)
#~~~~~~~~~~~~#Interactions:
#main effects seen: distance, C2     Value   Std. Err DF  t-value   p-value
as.factor(C2)most                  -4.91442  1.437364 854 -3.419047  0.0007
Distance_Num                        4.34524  0.476413 854  9.120733  0.0000

#### distance and age interaction         --->   ###use this equation (directly below) instead of next two (Distance/DX, Distance/Age) because this equation was used in vernier as well in order to show a relationship
fit_vernierC_acc_ddx1= lme(acc~age_c+as.factor(gender)+Distance_Num*as.factor(DX2)+Distance_Num*age_c+as.factor(C2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vernierC_acc_ddx1)

#~~~~~~~~ Distance and Diagnosis
fit_vc_int_ddx= lme(acc~Distance_Num*as.factor(DX2)+as.factor(gender)+age_c+as.factor(C2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_ddx)
#~~~~~~~~~~~~#Interactions:         #interaction seen between Distance:Dx
#main effects seen: distance, C2  Value   Std. Err  DF  t-value   p-value
Distance_Num                     4.83374  0.540651 854  8.940592  0.0000
as.factor(C2)most               -4.91448  1.434448 854 -3.426045  0.0006
Distance_Num:as.factor(DX2)a22q -1.85353  0.722085 854 -2.566915  0.0104



#~~~~~~~ Distance and Age
fit_vc_int5= lme(acc~Distance_Num*age_c+as.factor(gender)+as.factor(C2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int5)
#~~~~~~~~~~~~#Interactions:
#main effects seen: distance, C2       Value   Std. Err DF  t-value   p-value
as.factor(C2)most                   -4.91419  1.436241 854 -3.421564  0.0007
Distance_Num:age_c                   0.02797  0.013348 854  2.095803  0.0364



#~~~~~~~ Crowding and DX
fit_vc_int_c2dx= lme(acc~Distance_Num+as.factor(DX2)*as.factor(C2)+as.factor(gender)+age_c, data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_c2dx)
#~~~~~~~~~~~~# no sig. interactions



#~~~~~~~ Crowding and Age
fit_vc_int_c2age= lme(acc~Distance_Num+as.factor(DX2)+age_c*as.factor(C2)+as.factor(gender), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_c2age)
#~~~~~~~~~~~~# no sig. interactions



#~~~~~~~ Crowding and Gender
fit_vc_int_c2sex= lme(acc~Distance_Num+as.factor(DX2)+age_c+as.factor(C2)*as.factor(Gender), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_int_c2sex)
#~~~~~~~~~~~~# no sig. interactions



#~~~~~~~~~~~THREE WAY INTERACTION DUE TO CURIOUSITY
fit_vc_inttt= lme(acc~Distance_Num*as.factor(gender)*age_c*as.factor(DX2)*as.factor(C2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_inttt)
#~~~~~~~~~~~~#Interactions: 
#main effects seen:                                 Value   Std. Err DF  t-value   p-value



# THREE WAY INTERACTION #2
fit_vc_inttt5= lme(acc~Distance_Num*as.factor(gender)*age_c*as.factor(DX2)+as.factor(C2), data=Q_TD_VC_C2, random= ~1|CabilID)
summary(fit_vc_inttt5)
#~~~~~~~~~~~~#Interactions: 
#main effects seen:                                 Value   Std. Err DF  t-value   p-value

####Added by Abbie Find Diagnoses For the Vernier Crowded Removed Participants####
out_X1<-a_noX1_2[which(a_noX1_2$num_gt50 < 4),]
out_X1X2<-a_noX1X2_2[which(a_noX1X2_2$num_gt50 < 4),]

out_X1<-unique(merge(out_X1, VC[,c("CabilID","Dx")]))

out_X1X2<-unique(merge(out_X1X2, VC[,c("CabilID","Dx")]))

write.csv(out_X1[,c("CabilID","Dx")], "out_x1.csv", row.names=F)
write.csv(out_X1X2[,c("CabilID","Dx")], "out_x1x2.csv", row.names=F)

####Added by Abbie to Make the Datasets for Michele to Re-Run Analyses On####
keep_x1<-a_noX1_2[which(a_noX1_2$num_gt50 > 3),]
keep_x1x2<-a_noX1X2_2[which(a_noX1X2_2$num_gt50 > 3),]

cleaned_x1<-merge(keep_x1, VC1)
write.csv(cleaned_x1, "VernierCrowded_Long_CleanedWithX1.csv",row.names=F)

cleaned_x1x2<-merge(keep_x1x2, VC1)
write.csv(cleaned_x1x2, "VernierCrowded_Long_CleanedWithX1X2.csv",row.names=F)

###Descriptives for Vernier Crowded###
library(psych)
describeBy(VC$age, group=VC$Dx)


####Clean Vernier####
keep_x1_V<-V3[which(V3$num_gt50.x >4),]
write.csv(keep_x1_V, "Vernier_Long_CleanedWithX1.csv",row.names=F)

keep_x1x2_V<-V3[which(V3$num_gt50.y >4),]
write.csv(keep_x1x2_V, "Vernier_Long_CleanedWithX1X2.csv",row.names=F)
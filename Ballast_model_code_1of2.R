######### Code as used in Chenery et al., 2019 ##########
## MODEL CODE: ballast water model of secondary spread ##
# ===================================================== #
# This code was originally formulated in:
# Drake, D. A. R., Bailey, S. A., & Mandrak, N. E. (2015).
# Predicting the Secondary Spread of Aquatic Invasive Species through Ballast
# Water and Recreational Boating in the Great Lakes Basin. Completion Report
# Submitted to the Fishery Research Program of the Great Lakes Fishery 
# Commission. Sept. 30, 2015.#


## Section 1 of 2: setting up the model ##


# Reading in files on domestic shipping (laker) trips in GLB (rtrips); 
# ballast water density (ALLDENS); invasive species density (ASSEMB)
rtrips<-read.table("C:/Users/emily/OneDrive/01 Work/02 Manuscripts/03 AIS_ExpertElicitation_MEnvSc_2016/data/model code/rtripslakers.txt", sep = "\t", header=T, quote="\"")
alldens<-read.table("C:/Users/emily/OneDrive/01 Work/02 Manuscripts/03 AIS_ExpertElicitation_MEnvSc_2016/data/model code/ALLDENS.txt", header=T, quote="\"")
nisdens<-read.table("C:/Users/emily/OneDrive/01 Work/02 Manuscripts/03 AIS_ExpertElicitation_MEnvSc_2016/data/model code/ASSEMB.txt", header = T, quote="\"")
rtrips<-rtrips[!(rtrips$sourceportcode==147 | rtrips$sourceportcode==148 | rtrips$sourceportcode==276 | rtrips$sourceportcode==166 | rtrips$sourceportcode==275 | rtrips$arrivalportcode==147 | rtrips$arrivalportcode==148 | rtrips$arrivalportcode==276 | rtrips$arrivalportcode==166 | rtrips$arrivalportcode==275),]
sourcenames<-paste(rtrips$sourceport, rtrips$sourcestate)
arrivalnames<-paste(rtrips$arrivalport, rtrips$arrivalstate)
rtrips<-cbind(rtrips, sourcenames, arrivalnames)

first<-unique(rtrips[,c(2:7,21)])
second<-unique(rtrips[,c(11:16,22)])
colnames(first)<-c("port", "portcode", "state", "statecode", "lake", "lakecode", "names")
colnames(second)<-c("port", "portcode", "state", "statecode", "lake", "lakecode", "names")
set<-rbind(first, second)
uniqueports<-unique(set)


sourceset<-rtrips[ ,c("sourceportcode","sourcelakecode","sourcenames")]
sourceset<-unique(sourceset)
sourceset<-sourceset[order(sourceset$sourcelakecode),]


#to handle beta fitting, remove 0 and 1 for propNIS, replace with 0.00001, 0.99999#
alldens$PropNIS[alldens$PropNIS==0]<-0.00001
alldens$PropNIS[alldens$PropNIS==1]<-0.99999

table(alldens[,c(1,2)])
table(rtrips[,c(2,7)])

#Install packages if necessary# 

library(MASS)
library(VGAM)
library(ggplot2)
library(plyr)
library(logspline)

x<-round(alldens[alldens$ArrivalRegion==3 & alldens$SourceRegion==3,4])
fitdistr(x, "negative binomial")
gldsz<-0.4034
gldmu<-123550.7

##Setting up proportion of NIS##

x<-alldens[alldens$ArrivalRegion==3 & alldens$SourceRegion==7,5]
fitdistr(x, "beta", start = list(shape1 = 1, shape2 = 2))
glishape1<-0.7514
glishape2<-0.4004

x<-alldens[alldens$ArrivalRegion==3 & alldens$SourceRegion==3,5]
fitdistr(x, "beta", start = list(shape1 = 1, shape2 = 2))
gldshape1<-0.2411
gldshape2<-1.1468

#CPNIS#
#All trials below with the number sign around them didn't have any non-indig, so we used average cp val (2.357)##

x<-alldens[alldens$ArrivalRegion==3 & alldens$SourceRegion==3,6]
x<-x[x>0]
fitdistr(x, "poisson")
gldlam<-2.3


#########################################################
##############
###########
#Overall Parameter List, zooplankton#

gldsz<-0.4034
gldmu<-123550.7

##################
##Second, PropNIS#

gldshape1<-0.2411
gldshape2<-1.1468



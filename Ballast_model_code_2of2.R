######### Code as used in Chenery et al., 2019 ##########
## MODEL CODE: ballast water model of secondary spread ##
# ===================================================== #
# This code was originally formulated in:
# Drake, D. A. R., Bailey, S. A., & Mandrak, N. E. (2015).
# Predicting the Secondary Spread of Aquatic Invasive Species through Ballast
# Water and Recreational Boating in the Great Lakes Basin. Completion Report
# Submitted to the Fishery Research Program of the Great Lakes Fishery 
# Commission. Sept. 30, 2015.#


## Section 2 of 2: model simulations and plotting ##


##Below is a 20-year set that is written out (rather than looped).
##This is the interaction model in which invaded sites at time 1 can become sources at time t+1 based on
##a logistic function#
#########################
#Things to check when running this model:  
#1) specify the parameters for a given invasiveness scenario (e.g., a ,c, or population growth to 50% max,location = time of 50% logistic, scale = logisticslope), 
#2) specify whether calling on popdens vs imodens vs imotendens etc, 
#3) where is your output being written

#Example for ALG-01

#species specific parameters as used in Chenery et al. (2019)-  alpha (a) and x0 (c)
a<-0.0573
c<-1
location<-5
scale<-1
scalar<-0.66

output1<-NULL
output2<-NULL
for(k in 1:nrow(sourceset)){
  for(j in 1:5){
    
    print(k)
    print(j)
    
    portselect<-sourceset[k,1]
    
    #Y1
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    infectsource<-ifelse(set$sourceportcode==portselect,1,0)
    
    #probability that a single ship is contaminated, to be joined with infect#
    
    pcont1<-ifelse(infectsource==1,plogis(1,location,scale)*scalar,0)
    trial<-runif(length(pcont1))
    
    pcontamoutcome1<-ifelse(pcont1<trial,0,1)
    
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome1==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome1<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    timeinv1<-ifelse(uniqueports$portcode==portselect,1,0)
    
    
    #Y2
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv2<-ifelse(timeinv1>0,timeinv1,ifelse(uniqueports$portcode %in%invadedportcodes,2,0))
    timeinv2bind<-cbind(timeinv2, uniqueports$portcode)
    pcont2<-ifelse(set$sourceportcode %in% timeinv2bind[,2],timeinv2bind[,1],0)
    pcont2<-ifelse(pcont2==1,plogis(2,location,scale)*scalar,ifelse(pcont2==2,plogis(1,location,scale)*scalar,0))
    
    trial<-runif(length(pcont2))
    pcontamoutcome2<-ifelse(pcont2<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome2==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome2<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y3
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv3<-ifelse(timeinv2>0,timeinv2,ifelse(uniqueports$portcode %in%invadedportcodes,3,0))
    timeinv3bind<-cbind(timeinv3, uniqueports$portcode)
    pcont3<-ifelse(set$sourceportcode %in% timeinv3bind[,2],timeinv3bind[,1],0)
    pcont3<-ifelse(pcont3==1,plogis(3,location,scale)*scalar,ifelse(pcont3==2,plogis(2,location,scale)*scalar,ifelse(pcont3==3,plogis(1, location,scale)*scalar,0)))
    
    trial<-runif(length(pcont3))
    pcontamoutcome3<-ifelse(pcont3<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome3==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome3<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y4
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv4<-ifelse(timeinv3>0,timeinv3,ifelse(uniqueports$portcode %in%invadedportcodes,4,0))
    timeinv4bind<-cbind(timeinv4, uniqueports$portcode)
    pcont4<-ifelse(set$sourceportcode %in% timeinv4bind[,2],timeinv4bind[,1],0)
    pcont4<-ifelse(pcont4==1,plogis(4,location,scale)*scalar,ifelse(pcont4==2,plogis(3,location,scale)*scalar,ifelse(pcont4==3,plogis(2, location,scale)*scalar,ifelse(pcont4==4,plogis(1,location,scale)*scalar,0))))
    
    trial<-runif(length(pcont4))
    pcontamoutcome4<-ifelse(pcont4<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome4==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome4<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y5
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv5<-ifelse(timeinv4>0,timeinv4,ifelse(uniqueports$portcode %in%invadedportcodes,5,0))
    timeinv5bind<-cbind(timeinv5, uniqueports$portcode)
    pcont5<-ifelse(set$sourceportcode %in% timeinv5bind[,2],timeinv5bind[,1],0)
    pcont5<-ifelse(pcont5==1,plogis(5,location,scale)*scalar,ifelse(pcont5==2,plogis(4,location,scale)*scalar,ifelse(pcont5==3,plogis(3, location,scale)*scalar,ifelse(pcont5==4,plogis(2,location,scale)*scalar,ifelse(pcont5==5,plogis(1,location,scale)*scalar,0)))))
    
    trial<-runif(length(pcont5))
    pcontamoutcome5<-ifelse(pcont5<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome5==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome5<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y6
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv6<-ifelse(timeinv5>0,timeinv5,ifelse(uniqueports$portcode %in%invadedportcodes,6,0))
    timeinv6bind<-cbind(timeinv6, uniqueports$portcode)
    pcont6<-ifelse(set$sourceportcode %in% timeinv6bind[,2],timeinv6bind[,1],0)
    pcont6<-ifelse(pcont6==1,plogis(6,location,scale)*scalar,ifelse(pcont6==2,plogis(5,location,scale)*scalar,ifelse(pcont6==3,plogis(4, location,scale)*scalar,ifelse(pcont6==4,plogis(3,location,scale)*scalar,ifelse(pcont6==5,plogis(2,location,scale)*scalar,ifelse(pcont6==6,plogis(1,location,scale)*scalar,0))))))
    
    trial<-runif(length(pcont6))
    pcontamoutcome6<-ifelse(pcont6<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome6==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome6<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y7
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv7<-ifelse(timeinv6>0,timeinv6,ifelse(uniqueports$portcode %in%invadedportcodes,7,0))
    timeinv7bind<-cbind(timeinv7, uniqueports$portcode)
    pcont7<-ifelse(set$sourceportcode %in% timeinv7bind[,2],timeinv7bind[,1],0)
    pcont7<-ifelse(pcont7==1,plogis(7,location,scale)*scalar,ifelse(pcont7==2,plogis(6,location,scale)*scalar,ifelse(pcont7==3,plogis(5, location,scale)*scalar,ifelse(pcont7==4,plogis(4,location,scale)*scalar,ifelse(pcont7==5,plogis(3,location,scale)*scalar,ifelse(pcont7==6,plogis(2,location,scale)*scalar,ifelse(pcont7==7,plogis(1,location,scale)*scalar,0)))))))
    
    trial<-runif(length(pcont7))
    pcontamoutcome7<-ifelse(pcont7<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome7==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome7<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y8
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv8<-ifelse(timeinv7>0,timeinv7,ifelse(uniqueports$portcode %in%invadedportcodes,8,0))
    timeinv8bind<-cbind(timeinv8, uniqueports$portcode)
    pcont8<-ifelse(set$sourceportcode %in% timeinv8bind[,2],timeinv8bind[,1],0)
    pcont8<-ifelse(pcont8==1,plogis(8,location,scale)*scalar,ifelse(pcont8==2,plogis(7,location,scale)*scalar,ifelse(pcont8==3,plogis(6, location,scale)*scalar,ifelse(pcont8==4,plogis(5,location,scale)*scalar,ifelse(pcont8==5,plogis(4,location,scale)*scalar,ifelse(pcont8==6,plogis(3,location,scale)*scalar,ifelse(pcont8==7,plogis(2,location,scale)*scalar,ifelse(pcont8==8,plogis(1,location,scale)*scalar,0))))))))
    
    trial<-runif(length(pcont8))
    pcontamoutcome8<-ifelse(pcont8<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome8==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome8<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y9
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv9<-ifelse(timeinv8>0,timeinv8,ifelse(uniqueports$portcode %in%invadedportcodes,9,0))
    timeinv9bind<-cbind(timeinv9, uniqueports$portcode)
    pcont9<-ifelse(set$sourceportcode %in% timeinv9bind[,2],timeinv9bind[,1],0)
    pcont9<-ifelse(pcont9==1,plogis(9,location,scale)*scalar,ifelse(pcont9==2,plogis(8,location,scale)*scalar,ifelse(pcont9==3,plogis(7, location,scale)*scalar,ifelse(pcont9==4,plogis(6,location,scale)*scalar,ifelse(pcont9==5,plogis(5,location,scale)*scalar,ifelse(pcont9==6,plogis(4,location,scale)*scalar,ifelse(pcont9==7,plogis(3,location,scale)*scalar,ifelse(pcont9==8,plogis(2,location,scale)*scalar,ifelse(pcont9==9,plogis(1,location,scale)*scalar,0)))))))))
    
    trial<-runif(length(pcont9))
    pcontamoutcome9<-ifelse(pcont9<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome9==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome9<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y10
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv10<-ifelse(timeinv9>0,timeinv9,ifelse(uniqueports$portcode %in%invadedportcodes,10,0))
    timeinv10bind<-cbind(timeinv10, uniqueports$portcode)
    pcont10<-ifelse(set$sourceportcode %in% timeinv10bind[,2],timeinv10bind[,1],0)
    pcont10<-ifelse(pcont10==1,plogis(10,location,scale)*scalar,ifelse(pcont10==2,plogis(9,location,scale)*scalar,ifelse(pcont10==3,plogis(8, location,scale)*scalar,ifelse(pcont10==4,plogis(7,location,scale)*scalar,ifelse(pcont10==5,plogis(6,location,scale)*scalar,ifelse(pcont10==6,plogis(5,location,scale)*scalar,ifelse(pcont10==7,plogis(4,location,scale)*scalar,ifelse(pcont10==8,plogis(3,location,scale)*scalar,ifelse(pcont10==9,plogis(2,location,scale)*scalar,ifelse(pcont10==10,plogis(1,location,scale)*scalar,0))))))))))
    
    trial<-runif(length(pcont10))
    pcontamoutcome10<-ifelse(pcont10<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome10==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome10<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y11
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv11<-ifelse(timeinv10>0,timeinv10,ifelse(uniqueports$portcode %in%invadedportcodes,11,0))
    timeinv11bind<-cbind(timeinv11, uniqueports$portcode)
    pcont11<-ifelse(set$sourceportcode %in% timeinv11bind[,2],timeinv11bind[,1],0)
    pcont11<-ifelse(pcont11==1,plogis(11,location,scale)*scalar,ifelse(pcont11==2,plogis(10,location,scale)*scalar,ifelse(pcont11==3,plogis(9, location,scale)*scalar,ifelse(pcont11==4,plogis(8,location,scale)*scalar,ifelse(pcont11==5,plogis(7,location,scale)*scalar,ifelse(pcont11==6,plogis(6,location,scale)*scalar,ifelse(pcont11==7,plogis(5,location,scale)*scalar,ifelse(pcont11==8,plogis(4,location,scale)*scalar,ifelse(pcont11==9,plogis(3,location,scale)*scalar,ifelse(pcont11==10,plogis(2,location,scale)*scalar,ifelse(pcont11==11, plogis(1, location, scale)*scalar,0)))))))))))
    
    trial<-runif(length(pcont11))
    pcontamoutcome11<-ifelse(pcont11<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome11==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome11<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y12
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv12<-ifelse(timeinv11>0,timeinv11,ifelse(uniqueports$portcode %in%invadedportcodes,12,0))
    timeinv12bind<-cbind(timeinv12, uniqueports$portcode)
    pcont12<-ifelse(set$sourceportcode %in% timeinv12bind[,2],timeinv12bind[,1],0)
    pcont12<-ifelse(pcont12==1,plogis(12,location,scale)*scalar,ifelse(pcont12==2,plogis(11,location,scale)*scalar,ifelse(pcont12==3,plogis(10, location,scale)*scalar,ifelse(pcont12==4,plogis(9,location,scale)*scalar,ifelse(pcont12==5,plogis(8,location,scale)*scalar,ifelse(pcont12==6,plogis(7,location,scale)*scalar,ifelse(pcont12==7,plogis(6,location,scale)*scalar,ifelse(pcont12==8,plogis(5,location,scale)*scalar,ifelse(pcont12==9,plogis(4,location,scale)*scalar,ifelse(pcont12==10,plogis(3,location,scale)*scalar,ifelse(pcont12==11, plogis(2, location, scale)*scalar,ifelse(pcont12==12, plogis(1, location, scale)*scalar,0))))))))))))
    
    trial<-runif(length(pcont12))
    pcontamoutcome12<-ifelse(pcont12<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome12==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome12<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y13
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv13<-ifelse(timeinv12>0,timeinv12,ifelse(uniqueports$portcode %in%invadedportcodes,13,0))
    timeinv13bind<-cbind(timeinv13, uniqueports$portcode)
    pcont13<-ifelse(set$sourceportcode %in% timeinv13bind[,2],timeinv13bind[,1],0)
    pcont13<-ifelse(pcont13==1,plogis(13,location,scale)*scalar,ifelse(pcont13==2,plogis(12,location,scale)*scalar,ifelse(pcont13==3,plogis(11, location,scale)*scalar,ifelse(pcont13==4,plogis(10,location,scale)*scalar,ifelse(pcont13==5,plogis(9,location,scale)*scalar,ifelse(pcont13==6,plogis(8,location,scale)*scalar,ifelse(pcont13==7,plogis(7,location,scale)*scalar,ifelse(pcont13==8,plogis(6,location,scale)*scalar,ifelse(pcont13==9,plogis(5,location,scale)*scalar,ifelse(pcont13==10,plogis(4,location,scale)*scalar,ifelse(pcont13==11, plogis(3, location, scale)*scalar,ifelse(pcont13==12, plogis(2, location, scale)*scalar,ifelse(pcont13==13,plogis(1,location,scale)*scalar,0)))))))))))))
    
    trial<-runif(length(pcont13))
    pcontamoutcome13<-ifelse(pcont13<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome13==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome13<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y14
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv14<-ifelse(timeinv13>0,timeinv13,ifelse(uniqueports$portcode %in%invadedportcodes,14,0))
    timeinv14bind<-cbind(timeinv14, uniqueports$portcode)
    pcont14<-ifelse(set$sourceportcode %in% timeinv14bind[,2],timeinv14bind[,1],0)
    pcont14<-ifelse(pcont14==1,plogis(14,location,scale)*scalar,ifelse(pcont14==2,plogis(13,location,scale)*scalar,ifelse(pcont14==3,plogis(12, location,scale)*scalar,ifelse(pcont14==4,plogis(11,location,scale)*scalar,ifelse(pcont14==5,plogis(10,location,scale)*scalar,ifelse(pcont14==6,plogis(9,location,scale)*scalar,ifelse(pcont14==7,plogis(8,location,scale)*scalar,ifelse(pcont14==8,plogis(7,location,scale)*scalar,ifelse(pcont14==9,plogis(6,location,scale)*scalar,ifelse(pcont14==10,plogis(5,location,scale)*scalar,ifelse(pcont14==11, plogis(4, location, scale)*scalar,ifelse(pcont14==12, plogis(3, location, scale)*scalar,ifelse(pcont14==13,plogis(2,location,scale)*scalar,ifelse(pcont14==14,plogis(1,location,scale)*scalar,0))))))))))))))
    
    trial<-runif(length(pcont14))
    pcontamoutcome14<-ifelse(pcont14<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome14==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome14<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y15
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv15<-ifelse(timeinv14>0,timeinv14,ifelse(uniqueports$portcode %in%invadedportcodes,15,0))
    timeinv15bind<-cbind(timeinv15, uniqueports$portcode)
    pcont15<-ifelse(set$sourceportcode %in% timeinv15bind[,2],timeinv15bind[,1],0)
    pcont15<-ifelse(pcont15==1,plogis(15,location,scale)*scalar,ifelse(pcont15==2,plogis(14,location,scale)*scalar,ifelse(pcont15==3,plogis(13, location,scale)*scalar,ifelse(pcont15==4,plogis(12,location,scale)*scalar,ifelse(pcont15==5,plogis(11,location,scale)*scalar,ifelse(pcont15==6,plogis(10,location,scale)*scalar,ifelse(pcont15==7,plogis(9,location,scale)*scalar,ifelse(pcont15==8,plogis(8,location,scale)*scalar,ifelse(pcont15==9,plogis(7,location,scale)*scalar,ifelse(pcont15==10,plogis(6,location,scale)*scalar,ifelse(pcont15==11, plogis(5, location, scale)*scalar,ifelse(pcont15==12, plogis(4, location, scale)*scalar,ifelse(pcont15==13,plogis(3,location,scale)*scalar,ifelse(pcont15==14,plogis(2,location,scale)*scalar,ifelse(pcont15==15,plogis(1,location,scale)*scalar,0)))))))))))))))
    
    trial<-runif(length(pcont15))
    pcontamoutcome15<-ifelse(pcont15<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome15==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome15<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    
    #Y16
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv16<-ifelse(timeinv15>0,timeinv15,ifelse(uniqueports$portcode %in%invadedportcodes,16,0))
    timeinv16bind<-cbind(timeinv16, uniqueports$portcode)
    pcont16<-ifelse(set$sourceportcode %in% timeinv16bind[,2],timeinv16bind[,1],0)
    pcont16<-ifelse(pcont16==1,plogis(16,location,scale)*scalar,ifelse(pcont16==2,plogis(15,location,scale)*scalar,ifelse(pcont16==3,plogis(14, location,scale)*scalar,ifelse(pcont16==4,plogis(13,location,scale)*scalar,ifelse(pcont16==5,plogis(12,location,scale)*scalar,ifelse(pcont16==6,plogis(11,location,scale)*scalar,ifelse(pcont16==7,plogis(10,location,scale)*scalar,ifelse(pcont16==8,plogis(9,location,scale)*scalar,ifelse(pcont16==9,plogis(8,location,scale)*scalar,ifelse(pcont16==10,plogis(7,location,scale)*scalar,ifelse(pcont16==11, plogis(6, location, scale)*scalar,ifelse(pcont16==12, plogis(5, location, scale)*scalar,ifelse(pcont16==13,plogis(4,location,scale)*scalar,ifelse(pcont16==14,plogis(3,location,scale)*scalar,ifelse(pcont16==15,plogis(2,location,scale)*scalar,ifelse(pcont16==16,plogis(1,location,scale)*scalar,0))))))))))))))))
    
    trial<-runif(length(pcont16))
    pcontamoutcome16<-ifelse(pcont16<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome16==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome16<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y17
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv17<-ifelse(timeinv16>0,timeinv16,ifelse(uniqueports$portcode %in%invadedportcodes,17,0))
    timeinv17bind<-cbind(timeinv17, uniqueports$portcode)
    pcont17<-ifelse(set$sourceportcode %in% timeinv17bind[,2],timeinv17bind[,1],0)
    pcont17<-ifelse(pcont17==1,plogis(17,location,scale)*scalar,ifelse(pcont17==2,plogis(16,location,scale)*scalar,ifelse(pcont17==3,plogis(15, location,scale)*scalar,ifelse(pcont17==4,plogis(14,location,scale)*scalar,ifelse(pcont17==5,plogis(13,location,scale)*scalar,ifelse(pcont17==6,plogis(12,location,scale)*scalar,ifelse(pcont17==7,plogis(11,location,scale)*scalar,ifelse(pcont17==8,plogis(10,location,scale)*scalar,ifelse(pcont17==9,plogis(9,location,scale)*scalar,ifelse(pcont17==10,plogis(8,location,scale)*scalar,ifelse(pcont17==11, plogis(7, location, scale)*scalar,ifelse(pcont17==12, plogis(6, location, scale)*scalar,ifelse(pcont17==13,plogis(5,location,scale)*scalar,ifelse(pcont17==14,plogis(4,location,scale)*scalar,ifelse(pcont17==15,plogis(3,location,scale)*scalar,ifelse(pcont17==16,plogis(2,location,scale)*scalar,ifelse(pcont17==17,plogis(1,location,scale)*scalar,0)))))))))))))))))
    
    trial<-runif(length(pcont17))
    pcontamoutcome17<-ifelse(pcont17<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome17==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome17<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y18
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv18<-ifelse(timeinv17>0,timeinv17,ifelse(uniqueports$portcode %in%invadedportcodes,18,0))
    timeinv18bind<-cbind(timeinv18, uniqueports$portcode)
    pcont18<-ifelse(set$sourceportcode %in% timeinv18bind[,2],timeinv18bind[,1],0)
    pcont18<-ifelse(pcont18==1,plogis(18,location,scale)*scalar,ifelse(pcont18==2,plogis(17,location,scale)*scalar,ifelse(pcont18==3,plogis(16, location,scale)*scalar,ifelse(pcont18==4,plogis(15,location,scale)*scalar,ifelse(pcont18==5,plogis(14,location,scale)*scalar,ifelse(pcont18==6,plogis(13,location,scale)*scalar,ifelse(pcont18==7,plogis(12,location,scale)*scalar,ifelse(pcont18==8,plogis(11,location,scale)*scalar,ifelse(pcont18==9,plogis(10,location,scale)*scalar,ifelse(pcont18==10,plogis(9,location,scale)*scalar,ifelse(pcont18==11, plogis(8, location, scale)*scalar,ifelse(pcont18==12, plogis(7, location, scale)*scalar,ifelse(pcont18==13,plogis(6,location,scale)*scalar,ifelse(pcont18==14,plogis(5,location,scale)*scalar,ifelse(pcont18==15,plogis(4,location,scale)*scalar,ifelse(pcont18==16,plogis(3,location,scale)*scalar,ifelse(pcont18==17,plogis(2,location,scale)*scalar,ifelse(pcont18==18,plogis(1,location,scale)*scalar,0))))))))))))))))))
    
    trial<-runif(length(pcont18))
    pcontamoutcome18<-ifelse(pcont18<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome18==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome18<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y19
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv19<-ifelse(timeinv18>0,timeinv18,ifelse(uniqueports$portcode %in%invadedportcodes,19,0))
    timeinv19bind<-cbind(timeinv19, uniqueports$portcode)
    pcont19<-ifelse(set$sourceportcode %in% timeinv19bind[,2],timeinv19bind[,1],0)
    pcont19<-ifelse(pcont19==1,plogis(19,location,scale)*scalar,ifelse(pcont19==2,plogis(18,location,scale)*scalar,ifelse(pcont19==3,plogis(17, location,scale)*scalar,ifelse(pcont19==4,plogis(16,location,scale)*scalar,ifelse(pcont19==5,plogis(15,location,scale)*scalar,ifelse(pcont19==6,plogis(14,location,scale)*scalar,ifelse(pcont19==7,plogis(13,location,scale)*scalar,ifelse(pcont19==8,plogis(12,location,scale)*scalar,ifelse(pcont19==9,plogis(11,location,scale)*scalar,ifelse(pcont19==10,plogis(10,location,scale)*scalar,ifelse(pcont19==11, plogis(9, location, scale)*scalar,ifelse(pcont19==12, plogis(8, location, scale)*scalar,ifelse(pcont19==13,plogis(7,location,scale)*scalar,ifelse(pcont19==14,plogis(6,location,scale)*scalar,ifelse(pcont19==15,plogis(5,location,scale)*scalar,ifelse(pcont19==16,plogis(4,location,scale)*scalar,ifelse(pcont19==17,plogis(3,location,scale)*scalar,ifelse(pcont19==18,plogis(2,location,scale)*scalar,ifelse(pcont19==19,plogis(1,location,scale)*scalar,0)))))))))))))))))))
    
    trial<-runif(length(pcont19))
    pcontamoutcome19<-ifelse(pcont19<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome19==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome19<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    #Y20
    ##Randomly pick a year of traffic, and randomly add 440 trips (with spatial factors proportional) to represent salty movements#
    year<-c(2005,2006,2007)
    pick<-sample(year, 1, replace = T)
    set<-rtrips[rtrips$year==pick,]
    set<-set[sample(nrow(set), nrow(set)),]
    saltyset<-set[sample(nrow(set), 440, replace = T),]
    set<-rbind(set, saltyset)
    
    sampledens<-rnbinom(nrow(set), size = gldsz, mu = gldmu) 
    propNIS<-rbeta(nrow(set), shape1 = gldshape1, shape2 = gldshape2) 
    
    popdens<-ifelse(sampledens==0,abs(rnorm(length(sampledens==0),0,1)),ifelse(sampledens>0,abs(rnorm(length(sampledens>0),sampledens,sqrt(sampledens))),0))
    imodens<-ifelse(popdens>10,10,popdens)
    imotendens<-ifelse(popdens>1,1,popdens)
    imohundens<-ifelse(popdens>0.1,0.1,popdens)
    imothoudens<-ifelse(popdens>0.01,0.01,popdens)
    
    disp<-runif(nrow(set))
    
    assem<-nisdens[nisdens$ArrivalRegionCode==3 & nisdens$SourceRegionCode==3,]
    assem1<-as.matrix(assem[,6:115])
    assemnorm<-assem1/rowSums(assem1)
    vals<-assemnorm[assemnorm>0]
    vals<-vals[!is.na(vals)]
    speciesprop<-sample(vals, nrow(set), replace = T)
    x<-popdens*propNIS*speciesprop*disp
    estprobs<-1-exp(1)^-(a*x^c)
    trial<-runif(length(estprobs))
    estoutcome<-ifelse(estprobs<trial,0,1)
    
    timeinv20<-ifelse(timeinv19>0,timeinv19,ifelse(uniqueports$portcode %in%invadedportcodes,20,0))
    timeinv20bind<-cbind(timeinv20, uniqueports$portcode)
    pcont20<-ifelse(set$sourceportcode %in% timeinv20bind[,2],timeinv20bind[,1],0)
    pcont20<-ifelse(pcont20==1,plogis(20,location,scale)*scalar,
                    ifelse(pcont20==2,plogis(19,location,scale)*scalar,ifelse(pcont20==3,plogis(18, location,scale)*scalar,ifelse(pcont20==4,plogis(17,location,scale)*scalar,ifelse(pcont20==5,plogis(16,location,scale)*scalar,ifelse(pcont20==6,plogis(15,location,scale)*scalar,ifelse(pcont20==7,plogis(14,location,scale)*scalar,ifelse(pcont20==8,plogis(13,location,scale)*scalar,ifelse(pcont20==9,plogis(12,location,scale)*scalar,ifelse(pcont20==10,plogis(11,location,scale)*scalar,ifelse(pcont20==11, plogis(10, location, scale)*scalar,ifelse(pcont20==12, plogis(9, location, scale)*scalar,ifelse(pcont20==13,plogis(8,location,scale)*scalar,ifelse(pcont20==14,plogis(7,location,scale)*scalar,ifelse(pcont20==15,plogis(6,location,scale)*scalar,ifelse(pcont20==16,plogis(5,location,scale)*scalar,ifelse(pcont20==17,plogis(4,location,scale)*scalar,ifelse(pcont20==18,plogis(3,location,scale)*scalar,ifelse(pcont20==19,plogis(2,location,scale)*scalar,ifelse(pcont20==20,plogis(1,location,scale)*scalar,0))))))))))))))))))))
    
    trial<-runif(length(pcont20))
    pcontamoutcome20<-ifelse(pcont20<trial,0,1)
    infecttrip<-ifelse(estoutcome==1 & pcontamoutcome20==1, 1,0)
    newportoutcome<-ifelse(infecttrip==1, set$arrivalportcode,0)
    invadedportcodes<-unique(newportoutcome)
    portoutcome20<-ifelse(uniqueports$portcode %in%invadedportcodes==TRUE,1,0)
    
    portout<-cbind(portoutcome1,portoutcome2,portoutcome3,portoutcome4,portoutcome5,portoutcome6,portoutcome7,portoutcome8,portoutcome9,portoutcome10,portoutcome11,portoutcome12,portoutcome13,portoutcome14,portoutcome15,portoutcome16,portoutcome17,portoutcome18,portoutcome19,portoutcome20)
    
    addresults<-apply(portout,1,function(x) cumsum(x))
    addresults<-t(addresults)
    
    portout<-ifelse(addresults>0,1,0)
    
    output1<-cbind(output1,portout)
  }
}
outcome<-output1


write.table(outcome, file = "C:/Users/emily/Desktop/ALG-01.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#At this point, we have a set of results that are all cbinded together (e.g., 20 columns by 5 iterations per source location, repeated over each source locaitons, and 116 rows representing arrival locations.  To format them, we start by
#taking the wide matrix where all sides are just pasted side by side, and pasting each source location in order below.


iterations<-100
outcome<-read.table("C:/Users/emily/Desktop/ALG-01.txt", sep = "", header=T, quote="\"")
names<-rep("portoutcome", times = ncol(outcome))
names(outcome)<-names

start<-seq(from = 1, to = ncol(outcome), by = iterations)
stop<-seq(from = iterations, to = ncol(outcome), by = iterations)
output<-NULL
for(i in 1:length(start)){
  first<-outcome[,c(start[i]:stop[i])]
  output<-rbind(output, first)
}
outcome1<-output
#Result here is a matrix with 100 columns (20 years * 5 iterations), by 12971 rows, which is 109 source
#locations by 116 potential arrival locations

#Now join source and arrival characteristics like port codes etc.
#First, repeat whole chunks of unique ports to reflect their arrival locations
x<-uniqueports[rep(seq_len(nrow(uniqueports)),nrow(outcome1)/nrow(uniqueports)),]

#Second, repeat each individual line of sourceset, 116 times, to reflect the source locaitons
y<-sourceset[rep(seq_len(nrow(sourceset)), each = nrow(uniqueports)),]

#bind the source attributes (y) with arrival attributes (x) and the cumulative results
dataframe<-cbind(y,x,outcome1)

########################
####Calculating the number of invaded lakes through time (without "additional lakes" modification)
##Note that here we simply count the number of unique arrival lakes per source port as a total, regardless of whether or not the species has moved beyond source##

set<-ifelse(dataframe[,c(11:110)]==0,0,dataframe$lakecode)
x<-uniqueports[rep(seq_len(nrow(uniqueports)),nrow(outcome1)/nrow(uniqueports)),]

#Second, repeat each individual line of sourceset, 116 times, to reflect the source locaitons
y<-sourceset[rep(seq_len(nrow(sourceset)), each = nrow(uniqueports)),]

#bind the source attributes (y) with arrival attributes (x) and the cumulative results
set<-cbind(y,x,set)

##Below is the step required to generate a GIS output of probabilities, by year, for a single source port##
#Create a base dataframe for i to j source-arrival probabilities

y1<-seq(from = 11, to = ncol(outcome1), by = 20)
y2<-seq(from = 12, to = ncol(outcome1), by = 20)
y3<-seq(from = 13, to = ncol(outcome1), by = 20)
y4<-seq(from = 14, to = ncol(outcome1), by = 20)
y5<-seq(from = 15, to = ncol(outcome1), by = 20)
y6<-seq(from = 16, to = ncol(outcome1), by = 20)
y7<-seq(from = 17, to = ncol(outcome1), by = 20)
y8<-seq(from = 18, to = ncol(outcome1), by = 20)
y9<-seq(from = 19, to = ncol(outcome1), by = 20)
y10<-seq(from = 20, to = ncol(outcome1), by = 20)
y11<-seq(from = 21, to = ncol(outcome1), by = 20)
y12<-seq(from = 22, to = ncol(outcome1), by = 20)
y13<-seq(from = 23, to = ncol(outcome1), by = 20)
y14<-seq(from = 24, to = ncol(outcome1), by = 20)
y15<-seq(from = 25, to = ncol(outcome1), by = 20)
y16<-seq(from = 26, to = ncol(outcome1), by = 20)
y17<-seq(from = 27, to = ncol(outcome1), by = 20)
y18<-seq(from = 28, to = ncol(outcome1), by = 20)
y19<-seq(from = 29, to = ncol(outcome1), by = 20)
y20<-seq(from = 30, to = ncol(outcome1), by = 20)

out1<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y1]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out1<-rbind(out1, avglakes)
}
out2<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y2]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out2<-rbind(out2, avglakes)
}
out3<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y3]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out3<-rbind(out3, avglakes)
}
out4<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y4]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out4<-rbind(out4, avglakes)
}
out5<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y5]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out5<-rbind(out5, avglakes)
}
out6<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y6]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out6<-rbind(out6, avglakes)
}
out7<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y7]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out7<-rbind(out7, avglakes)
}
out8<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y8]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out8<-rbind(out8, avglakes)
}
out9<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y9]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out9<-rbind(out9, avglakes)
}
out10<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y10]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out10<-rbind(out10, avglakes)
}
out11<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y11]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out11<-rbind(out11, avglakes)
}
out12<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y12]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out12<-rbind(out12, avglakes)
}
out13<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y13]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out13<-rbind(out13, avglakes)
}
out14<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y14]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out14<-rbind(out14, avglakes)
}
out15<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y15]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out15<-rbind(out15, avglakes)
}
out16<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y16]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out16<-rbind(out16, avglakes)
}
out17<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y17]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out17<-rbind(out17, avglakes)
}
out18<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y18]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out18<-rbind(out18, avglakes)
}
out19<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y19]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out19<-rbind(out19, avglakes)
}
out20<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y20]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out20<-rbind(out20, avglakes)
}
lakes<-cbind(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20)

exhighlakes<-cbind(sourceset, lakes)

exhighlakesmeans<-colMeans(lakes)
exhighlakesupperci<-apply(lakes,2,function(x) quantile(x, probs = 0.975))
exhighlakeslowerci<-apply(lakes,2,function(x) quantile(x, probs = 0.025))

##Plotting figure: Lakes invaded against time##
y1<-seq(from = 11, to = ncol(outcome1), by = 20)
y2<-seq(from = 12, to = ncol(outcome1), by = 20)
y3<-seq(from = 13, to = ncol(outcome1), by = 20)
y4<-seq(from = 14, to = ncol(outcome1), by = 20)
y5<-seq(from = 15, to = ncol(outcome1), by = 20)
y6<-seq(from = 16, to = ncol(outcome1), by = 20)
y7<-seq(from = 17, to = ncol(outcome1), by = 20)
y8<-seq(from = 18, to = ncol(outcome1), by = 20)
y9<-seq(from = 19, to = ncol(outcome1), by = 20)
y10<-seq(from = 20, to = ncol(outcome1), by = 20)
y11<-seq(from = 21, to = ncol(outcome1), by = 20)
y12<-seq(from = 22, to = ncol(outcome1), by = 20)
y13<-seq(from = 23, to = ncol(outcome1), by = 20)
y14<-seq(from = 24, to = ncol(outcome1), by = 20)
y15<-seq(from = 25, to = ncol(outcome1), by = 20)
y16<-seq(from = 26, to = ncol(outcome1), by = 20)
y17<-seq(from = 27, to = ncol(outcome1), by = 20)
y18<-seq(from = 28, to = ncol(outcome1), by = 20)
y19<-seq(from = 29, to = ncol(outcome1), by = 20)
y20<-seq(from = 30, to = ncol(outcome1), by = 20)

out1<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y1]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out1<-rbind(out1, avglakes)
}
out2<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y2]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out2<-rbind(out2, avglakes)
}
out3<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y3]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out3<-rbind(out3, avglakes)
}
out4<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y4]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out4<-rbind(out4, avglakes)
}
out5<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y5]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out5<-rbind(out5, avglakes)
}
out6<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y6]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out6<-rbind(out6, avglakes)
}
out7<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y7]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out7<-rbind(out7, avglakes)
}
out8<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y8]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out8<-rbind(out8, avglakes)
}
out9<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y9]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out9<-rbind(out9, avglakes)
}
out10<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y10]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out10<-rbind(out10, avglakes)
}
out11<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y11]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out11<-rbind(out11, avglakes)
}
out12<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y12]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out12<-rbind(out12, avglakes)
}
out13<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y13]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out13<-rbind(out13, avglakes)
}
out14<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y14]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out14<-rbind(out14, avglakes)
}
out15<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y15]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out15<-rbind(out15, avglakes)
}
out16<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y16]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out16<-rbind(out16, avglakes)
}
out17<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y17]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out17<-rbind(out17, avglakes)
}
out18<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y18]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out18<-rbind(out18, avglakes)
}
out19<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y19]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out19<-rbind(out19, avglakes)
}
out20<-NULL
for(i in unique(sourceset$sourceportcode)){
  x<-set[set$sourceportcode==i,]
  xx<-x[,y20]
  result<-apply(xx,2,function(x) length(unique(x[x>0])))
  avglakes<-mean(result)
  out20<-rbind(out20, avglakes)
}
lakes<-cbind(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20)

exhighlakes<-cbind(sourceset, lakes)

exhighlakesmeans<-colMeans(lakes)
exhighlakesupperci<-apply(lakes,2,function(x) quantile(x, probs = 0.975))
exhighlakeslowerci<-apply(lakes,2,function(x) quantile(x, probs = 0.025))

#Note:that typing 'exhighlakesmeans' into R gives you a table of the mean number of lakes vs. time (across 20 years).

plot(exhighlakesmeans,
      xlab = "Years",
      ylab = "Number of lakes invaded")
title(main="ALG-01")


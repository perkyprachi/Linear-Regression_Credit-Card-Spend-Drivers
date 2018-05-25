rm(list=ls())

CustBase<- read.csv(file.choose(),header = TRUE)
sum(is.na(CustBase))   #0
str(CustBase)

help("str")

colSums(is.na(CustBase)|CustBase == '#NULL!')
CustBase[CustBase=="#NULL!"] <- NA
CustBase[CustBase==NA] 
View(CustBase)
#removing variables which have significant no. of missing values,redundant categorical values

CustBase$TotalSpent<- CustBase$cardspent+CustBase$card2spent
NewData <- subset(CustBase,select = -c(cardspent,card2spent,custid,birthmonth,carditems,card2items))
names(CustBase)                                  

colSums(is.na(NewData))
names(NewData)

write.csv(NewData,"D:/AnalitixLabs/New folder/Linear Regression/NewData.csv")

###Splitting Categorical Data with Continuos Data
Contidata<- subset(NewData,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                       employ,empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                       homeown,hometype,address,addresscat,cars,carown,cartype,
                                       carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                       commutemotorcycle,commutecarpool,commutebus,commuterail,
                                       commutepublic,commutebike,commutewalk,commutenonmotor,
                                       telecommute,reason,polview,polparty,polcontrib,vote,card,
                                       cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                       card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                       active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                       voice,pager,internet,callid,callwait,forward,confer,ebill,
                                       owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                       news,response_01,response_02,response_03))
names(Contidata)
xyz<- sapply(Contidata,is.numeric)
NumericData<- Contidata[,xyz]
names(NumericData)
NonNumericData<- Contidata[,!xyz]
names(NonNumericData)

####converting to numeric 
NumericData2<- data.frame(lapply(NonNumericData[,c("lncreddebt","lnothdebt","commutetime","longten","lnlongten" ,
                                                   "lntollmon" ,"lntollten","lnequipmon","lnequipten","lncardmon","cardten",  
                                                    "lncardten", "lnwiremon","lnwireten" )], as.numeric))  


Contidata2<-cbind(NumericData,NumericData2)                                                 
xyz2<- sapply(Contidata2,is.numeric)

CatData<- subset(NewData,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                                    employ,empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                    homeown,hometype,address,addresscat,cars,carown,cartype,
                                    carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                    commutemotorcycle,commutecarpool,commutebus,commuterail,
                                    commutepublic,commutebike,commutewalk,commutenonmotor,
                                    telecommute,reason,polview,polparty,polcontrib,vote,card,
                                    cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                    card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                    active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                    voice,pager,internet,callid,callwait,forward,confer,ebill,
                                    owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                    news,response_01,response_02,response_03))

write.csv(Contidata,"D:/AnalitixLabs/New folder/Linear Regression/ContiData.csv")
#Creation of descriptive statistics(CatData)
mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.95,na.rm=T)
    LC2=quantile(x,0.05,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1_=ot1,ot_m2_=ot2,ot_m3_=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  }
}


diag_stats<-  t(data.frame(apply(Contidata2,2,FUN = mystats)))
View(diag_stats)
write.csv(diag_stats,"D:/AnalitixLabs/New folder/Linear Regression/DiagStat.csv")

###Missing Value Treatment

install.packages("Hmisc")
require(Hmisc)

Contidata2[xyz2]<- data.frame(apply(Contidata2[xyz2],2,function(x) impute(x,mean)))
colSums(is.na(Contidata2)|Contidata2 == 'NA')

names(Contidata2)
write.csv(Contidata2,"D:/AnalitixLabs/New folder/Linear Regression/MissingValueTreated.csv")



####Outlier Treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .95 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
names(Contidata2)
vars1<- c( "age","ed","income","lninc","debtinc","creddebt","othdebt","spoused","reside","pets",      
            "pets_cats","pets_dogs","pets_birds","pets_reptiles","pets_small","pets_saltfish",     
           "pets_freshfish","carvalue","tenure","longmon","lnlongmon","tollmon","tollten","equipmon",       
           "equipten","cardmon","wiremon","wireten","hourstv","TotalSpent","lncreddebt","lnothdebt",        
           "commutetime","longten","lnlongten","lntollmon","lntollten","lnequipmon","lnequipten",           
           "lncardmon","cardten","lncardten","lnwiremon","lnwireten")                  
           

Contidata2[,vars1] <-lapply(Contidata2[,vars1],M1_fun)
Contidata2[Contidata2$carvalue==-1,"carvalue"] <- 0
Contidata2[Contidata2$tollten==0,"tollten"] <- 578
Contidata2[Contidata2$spoused==-1,"spoused"] <- 0

CatData[CatData$spousedcat  ==-1,"spousedcat"] <- 0
write.csv(Contidata2,"D:/AnalitixLabs/New folder/Linear Regression/ContidataFinal.csv")
names(Contidata2)
#####Correlation MAtrix of numerical data

Corrm<- cor(NumericData)

####Removing Highly Correlated Data

Contidata3<- subset(Contidata2,select = -c(lninc,pets_freshfish,lnlongmon,equipten,wiremon))
            
write.csv(Corrm,"D:/AnalitixLabs/New folder/Linear Regression/CorrelationMatrix.csv")
###Converting Categorical variables into Factors
names(CatData)
FactorCatData<- data.frame(lapply(CatData[,c("region","townsize","gender","agecat","edcat",
                                             "jobcat","union","empcat","retire","inccat","default","jobsat","marital","spousedcat",           
                                             "homeown","hometype","address","addresscat","cars","carown","cartype","carcatvalue","carbought","carbuy",  "commute","commutecat", "commutecar","commutemotorcycle" ,"commutecarpool","commutebus","commuterail",
                                             "commutepublic","commutebike","commutewalk","commutenonmotor","telecommute","reason","polview","polparty",            
                                             "polcontrib","vote","card","cardtype","cardbenefit","cardfee","cardtenurecat","card2","card2type",          
                                             "card2benefit","card2fee","card2tenurecat","active","bfast" ,"churn","tollfree","equip","callcard", 
                                             "wireless" ,"multline","voice","pager","internet","callid","callwait","forward","confer" ,"ebill",
                                             "owntv" ,"ownvcr","owndvd","owncd","ownpda","ownpc","ownipod","owngame","ownfax","news","response_01","response_02","response_03"
)],factor))
         
names(FactorCatData)

###Combining Data
CombineData<- cbind(Contidata3,FactorCatData)
write.csv(CombineData,"D:/AnalitixLabs/New folder/Linear Regression/CombineData.csv")
names(FactorCatData)
#####Reducing Categorical Variables using anova with the dependent variable( based on F-values)
AnovaF<- function(x){
  fit1<- lm(TotalSpent~x,CombineData)
  anova(fit1)  
  }
AnovaF(CombineData$region)
AnovaF(CombineData$townsize)###F-value 0.3681
AnovaF(CombineData$agecat)
AnovaF(CombineData$edcat)
AnovaF(CombineData$jobcat)
AnovaF(CombineData$jobsat)
AnovaF(CombineData$union)
AnovaF(CombineData$marital)##F-value 1.6311
AnovaF(CombineData$spousedcat)
AnovaF(CombineData$homeown)
AnovaF(CombineData$addresscat)
AnovaF(CombineData$carcatvalue)
AnovaF(CombineData$carown)
AnovaF(CombineData$cars)
AnovaF(CombineData$commutecat)##RemoveF-value 1.0036
AnovaF(CombineData$vote)
AnovaF(CombineData$wireless)
AnovaF(CombineData$response_01)###F-value 0.3071
AnovaF(CombineData$callid)
AnovaF(CombineData$callwait)
AnovaF(CombineData$forward)
AnovaF(CombineData$pager)
AnovaF(CombineData$confer)
AnovaF(CombineData$owncd)
AnovaF(CombineData$ownpda)
AnovaF(CombineData$ownpc)
AnovaF(CombineData$polview)
AnovaF(CombineData$polparty)##F-value  0.0339  
AnovaF(CombineData$polcontrib)
AnovaF(CombineData$telecommute)##F-value 0.0696 
AnovaF(CombineData$cars)##F-value0.9521
AnovaF(CombineData$active)##F-value 0.9521
AnovaF(CombineData$bfast)
AnovaF(CombineData$churn)#F-value 0.9521
AnovaF(CombineData$hometype)
AnovaF(CombineData$commutecarpool)##F-value 0.2583
AnovaF(CombineData$commutemotorcycle)##F-value  1.4086 
AnovaF(CombineData$commuterail)##F-value  0.2222
AnovaF(CombineData$commutepublic)##F-value 0.2603
AnovaF(CombineData$commutebike)
AnovaF(CombineData$commutewalk)##F-value  0.3378
AnovaF(CombineData$commutenonmotor)##F-value 0.3116
AnovaF(CombineData$commutebus)##F-value 1.3969
AnovaF(CombineData$reason)

CombineData2<- subset(CombineData,select = -c(commutebus,commutenonmotor,commutewalk,commutepublic,commuterail,
                                              commutemotorcycle,commutecarpool,churn,active,cars,telecommute,polparty,
                                              commutecat,response_01,marital,townsize))

####Running Stepwise regression on the whole data

install.packages("MASS")
require(MASS)

fitCombine1<- lm(TotalSpent~., CombineData2)
summary(fitCombine1)
step1<- stepAIC(fitCombine1,direction="both")
?stepAIC()
ls(step1)
step1$anova 
 
FinalModel<- lm(TotalSpent ~ age + ed + income + debtinc + creddebt + othdebt + 
  spoused + carvalue + longmon + lncreddebt + lnothdebt + region +union+ 
  gender + retire + inccat + jobsat + spousedcat + reason + jobcat+
  polview + card + card2 + card2fee + multline + voice + internet + 
  owndvd + response_03,CombineData2)
    
summary(FinalModel)


#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(CombineData2), size = floor(0.70 * nrow(CombineData2)))

training<-CombineData2[train_ind,]
testing<-CombineData2[-train_ind,]

names(training)
names(testing)

###TRAINING DATASET##################################################################

fittrain1<- lm(TotalSpent ~ age + ed + income + debtinc + creddebt + othdebt + 
                 spoused + carvalue + longmon + lncreddebt + lnothdebt + region + 
                 gender + retire + inccat + jobsat + spousedcat + reason + jobcat+
                 polview + card + card2 + card2fee + multline + voice + internet + 
                 owndvd + response_03, training)
summary(fittrain1) ###Multiple R-squared:  0.5253


training$Cd<- cooks.distance(fittrain1)
training1<-subset(training, Cd< (4/3500))
View(training1)

fittrain2 <- lm(TotalSpent~ age + ed + income + debtinc + creddebt + othdebt + 
                  spoused + carvalue + longmon + lncreddebt + lnothdebt + region + 
                  gender + retire + inccat + jobsat + spousedcat + reason + jobcat+
                  polview + card + card2 + card2fee + multline + voice + internet + 
                  owndvd + response_03, data= training1)
summary(fittrain2) ###Multiple R-squared:  0.4198
plot(fittrain2)


###Checking Multicollinearity
library(car)
vif(fittrain2)

###coefficients

CoefTrain<- coefficients(fittrain2)

write.csv(CoefTrain,"D:/AnalitixLabs/New folder/Linear Regression/CoefTrain.csv")


names(training1)
#######################SCORING USING PREDICT FUNCTION

t1<-cbind(training1, Pred_TotalSpent = predict(fittrain2))
names(t1)
View(t1)
t1<- transform(t1, APE = abs(Pred_TotalSpent - TotalSpent)/TotalSpent)
mean(t1$APE) ##  0.4769767
View(t1)


# find the decile locations 
decLocations <- quantile(t1$Pred_TotalSpent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$Pred_TotalSpent,c(-Inf,decLocations, Inf))


require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(Pred_TotalSpent) as avg_pre_spent,   
               avg(TotalSpent) as avg_Actual_spent
               from t1
               group by decile
               order by decile desc")

View(t1_DA)

write.csv(t1_DA,"D:/AnalitixLabs/New folder/Linear Regression/Train_decile.csv")

###################################TESTING DATASET###############################


fittest1<- lm(TotalSpent~ age + ed + income + debtinc + creddebt + othdebt + 
                spoused + carvalue + longmon + lncreddebt + lnothdebt + region + 
                gender + retire + inccat + jobsat + spousedcat + reason + jobcat+
                polview + card + card2 + card2fee + multline + voice + internet + 
                owndvd + response_03, testing)
summary(fittest1) ### R-squared:  0.3825


testing$Cd<- cooks.distance(fittest1)
testing1<-subset(testing, Cd< (4/3500))

fittest2 <- lm(TotalSpent~ age + ed + income + debtinc + creddebt + othdebt + 
                 spoused + carvalue + longmon + lncreddebt + lnothdebt + region + 
                 gender + retire + inccat + jobsat + spousedcat + reason + jobcat+
                 polview + card + card2 + card2fee + multline + voice + internet + 
                 owndvd + response_03, data= testing1)
summary(fittest2) ###Multiple R-squared:  0.5795
plot(fittest2)


###Checking Multicollinearity
library(car)
vif(fittest2)

###coefficients

CoefTest<- coefficients(FinalTestmodel)

write.csv(CoefTest,"D:/AnalitixLabs/New folder/Linear Regression/CoefTest.csv")


names(testing1)
#######################SCORING USING PREDICT FUNCTION

t2<-cbind(testing1, Pred_TotalSpent = predict(fittest2))
names(t2)
View(t2)
t2<- transform(t2, APE = abs(Pred_TotalSpent - TotalSpent)/TotalSpent)
mean(t2$APE) ##   0.3855023
View(t2)

# find the decile locations 
decLocations2 <- quantile(t2$Pred_TotalSpent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$Pred_TotalSpent,c(-Inf,decLocations2, Inf))


require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(Pred_TotalSpent) as avg_pre_spent,   
               avg(TotalSpent) as avg_Actual_spent
               from t2
               group by decile
               order by decile desc")

View(t2_DA)

write.csv(t2_DA,"D:/AnalitixLabs/New folder/Linear Regression/Test_decile.csv")

#################################################################################################################


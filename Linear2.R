rm(list = ls())

setwd("D:/AnalitixLabs/New folder/Linear Resubmission")
getwd()
install.packages("readxl")
require(readxl)

CustBase<- readxl::read_excel(path = "D:/AnalitixLabs/New folder/Linear Resubmission/Linear Regression Case.xlsx",sheet = 1)

sum(is.na(CustBase))   #0
str(CustBase)

colSums(is.na(CustBase)|CustBase == '#NULL!')
CustBase[CustBase=="#NULL!"] <- NA

#removing variables which are insignificant as per the business problem

CustBase$TotalSpent<- CustBase$cardspent+CustBase$card2spent
NewData <- subset(CustBase,select = -c(cardspent,card2spent,custid,birthmonth,carditems,card2items))
names(NewData )                                  

colSums(is.na(NewData))
names(NewData)

write.csv(NewData,"D:/AnalitixLabs/New folder/Linear Resubmission/NewData.csv")

###Splitting Categorical Data with Continuos Data
Contidata<- subset(NewData,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                       empcat,retire,inccat,default,jobsat,marital,spousedcat,
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

CatData<- subset(NewData,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                                    empcat,retire,inccat,default,jobsat,marital,spousedcat,
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

xyz2<- sapply(CatData,is.numeric)

AllData<- cbind(Contidata,CatData)
write.csv(AllData,"D:/AnalitixLabs/New folder/Linear Resubmission/allData.csv")
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


diag_stats<-  t(data.frame(apply(Contidata,2,FUN = mystats)))

write.csv(diag_stats,"D:/AnalitixLabs/New folder/Linear Resubmission/DiagStat.csv")

###Missing Value Treatment

install.packages("Hmisc")
require(Hmisc)

###Variables having missing values
varmiss<- c("lncreddebt","lnothdebt","commutetime","longten","lnlongten","lntollmon","lntollten",
            "lnequipmon","lnequipten","lncardmon","cardten","lncardten","lnwiremon","lnwireten")

Contidata[varmiss]<- data.frame(apply(Contidata[varmiss],2,function(x) impute(x,mean)))
colSums(is.na(Contidata)|Contidata == 'NA')
CatData[,xyz2] <- apply(data.frame(CatData[,xyz2]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


####Outlier Treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .95 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

#Variables having outliers                 
vars1<- c("employ","income","debtinc","creddebt","othdebt","longmon","tollmon","tollten",
          "equipmon","equipten","cardmon","cardten","wiremon","wireten","TotalSpent")           

Contidata[,vars1] <-lapply(Contidata[,vars1],M1_fun)



CombineData<- cbind(Contidata,CatData)
write.csv(CombineData,"D:/AnalitixLabs/New folder/Linear Resubmission/FinalData.csv")

#####Correlation MAtrix of numerical data

Corrm<- cor(CombineData)

write.csv(Corrm,"D:/AnalitixLabs/New folder/Linear Resubmission/CorrelationMatrix.csv")
####Removing Highly Correlated Categorical Data
names(CatData)
CombineData2<- subset(CombineData,select = -c(spousedcat,townsize,union,hometype,cars,churn,card2benefit,card2type,carcatvalue,
                                      cardfee,commutewalk,commutepublic,commutecarpool,commutecar,carbought,cardtenurecat,
                                      card2tenurecat,inccat,wireless,addresscat,agecat,edcat,marital,commute,empcat))
                                          
names(CombineData2)                                           
            
write.csv(CombineData2,"D:/AnalitixLabs/New folder/Linear Resubmission/reducedData.csv")

###Converting Categorical variables into Factors
varcat<- c("region","gender","jobcat","retire","default","jobsat","homeown","address",          
           "carown","cartype","carbuy","commutecat","commutemotorcycle","commutebus","commuterail",
           "commutebike","commutenonmotor","telecommute","reason","polview","polparty","polcontrib",            
           "vote","card","cardtype" ,"cardbenefit","cardtenure","card2","card2fee","card2tenure",      
           "active","bfast","tollfree","equip","callcard","multline","voice","pager","internet",         
           "callid","callwait","forward","confer","ebill","owntv","ownvcr","owndvd", "owngame" ,"ownfax",                                                                               
           "news","owncd","ownpda","ownpc","ownipod","response_01","response_02","response_03" )


CombineData2[,varcat]<- lapply(CombineData2[,varcat],factor)                                                                                  

hist(CombineData2$TotalSpent)
hist(log(CombineData2$TotalSpent))##better distribution

CombineData2$ln_TotalSpent<- log(CombineData2$TotalSpent)

write.csv(CombineData2,"D:/AnalitixLabs/New folder/Linear Resubmission/CombineData.csv")


#####Reducing Categorical Variables using anova with the dependent variable( based on F-values)
AnovaF<- function(x){
  fit1<- lm(TotalSpent~x,CombineData2)
  anova(fit1)  
  }
AnovaF(CombineData2$region)
AnovaF(CombineData2$jobcat)
AnovaF(CombineData2$jobsat)
AnovaF(CombineData2$homeown)
AnovaF(CombineData2$carown)
AnovaF(CombineData2$vote)
AnovaF(CombineData2$response_01)###F-value 0.6161
AnovaF(CombineData2$callid)
AnovaF(CombineData2$callwait)
AnovaF(CombineData2$forward)
AnovaF(CombineData2$pager)
AnovaF(CombineData2$confer)
AnovaF(CombineData2$owncd)
AnovaF(CombineData2$ownpda)
AnovaF(CombineData2$ownpc)
AnovaF(CombineData2$polview)
AnovaF(CombineData2$polparty)##F-value  0.8763
AnovaF(CombineData2$polcontrib)
AnovaF(CombineData2$telecommute)##F-value 0.6846 
AnovaF(CombineData2$active)##F-value 0.4914
AnovaF(CombineData2$bfast)
AnovaF(CombineData2$commutemotorcycle)##F-value 2.9182
AnovaF(CombineData2$commuterail)##F-value   0.411
AnovaF(CombineData2$commutebike)
AnovaF(CombineData2$commutenonmotor)##F-value 0.68
AnovaF(CombineData2$commutebus)##F-value 0.68
AnovaF(CombineData2$reason)

###Checking Distribution of continuous variables
hist(CombineData2$income)
hist(CombineData2$lninc)#better
hist(CombineData2$creddebt)
hist(CombineData2$lncreddebt)#better
hist(CombineData2$othdebt)
hist(CombineData2$lnothdebt)##better distribution
hist(CombineData2$longmon)
hist(CombineData2$lnlongmon)##better distribution
hist(CombineData2$longten)
hist(CombineData2$lnlongten)##better distribution
hist(CombineData2$tollmon)
hist(CombineData2$lntollmon)
hist(CombineData2$tollten)
hist(CombineData2$lntollten)
hist(CombineData2$equipmon)
hist(CombineData2$lnequipmon)
hist(CombineData2$equipten)
hist(CombineData2$lnequipten)
###Removing insignificant categorical variables and skewed distributed variables

ReducedData<- subset(CombineData2,select = -c(active,polparty,commutebus,commutenonmotor,commuterail,
                                            telecommute, commutemotorcycle,response_01))
                                          
names(ReducedData)
write.csv(ReducedData,"D:/AnalitixLabs/New folder/Linear Resubmission/ReducedData.csv")

#Splitting data into Training, Validaton and Testing Dataset

set.seed(123)
train_ind <- sample(1:nrow(ReducedData), size = floor(0.70 * nrow(ReducedData)))

training<-ReducedData[train_ind,]
testing<-ReducedData[-train_ind,]

names(training)
names(testing)

####Running Stepwise regression on the whole data

install.packages("MASS")
require(MASS)
names(training)
fit1<- lm(TotalSpent~age+ed+employ+income+lninc+debtinc+creddebt+lncreddebt+othdebt+
            lnothdebt+spoused+reside+pets+pets_cats+pets_dogs+pets_birds+pets_reptiles+
            pets_small+pets_saltfish+pets_freshfish+carvalue+commutetime+tenure+longmon+
            lnlongmon+longten+lnlongten+tollmon+lntollmon+tollten+lntollten+equipmon+
            lnequipmon+equipten+lnequipten+cardmon+lncardmon+cardten+lncardten+wiremon+
            lnwiremon+wireten+lnwireten+hourstv+region+gender+jobcat+retire+default+
            jobsat+homeown+address+carown+cartype+carbuy+commutecat+commutebike+reason+
            polview+polcontrib+vote+card+cardtype+cardbenefit+cardtenure+card2+card2fee+
            card2tenure+bfast+tollfree+equip+callcard+multline+voice+pager+internet+callid+
            callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+
            owngame+ ownfax+news+response_02+response_03, data=training)
summary(fit1) #Multiple R-squared:  0.3964

fit2<- lm(ln_TotalSpent~age+ed+employ+income+lninc+debtinc+creddebt+lncreddebt+othdebt+
            lnothdebt+spoused+reside+pets+pets_cats+pets_dogs+pets_birds+pets_reptiles+
            pets_small+pets_saltfish+pets_freshfish+carvalue+commutetime+tenure+longmon+
            lnlongmon+longten+lnlongten+tollmon+lntollmon+tollten+lntollten+equipmon+
            lnequipmon+equipten+lnequipten+cardmon+lncardmon+cardten+lncardten+wiremon+
            lnwiremon+wireten+lnwireten+hourstv+region+gender+jobcat+retire+default+
            jobsat+homeown+address+carown+cartype+carbuy+commutecat+commutebike+reason+
            polview+polcontrib+vote+card+cardtype+cardbenefit+cardtenure+card2+card2fee+
            card2tenure+bfast+tollfree+equip+callcard+multline+voice+pager+internet+callid+
            callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+
            owngame+ ownfax+news+response_02+response_03,data = training)
          
summary(fit2)#Multiple R-squared:  0.3796

####Stepwise Regression of the model

step1<- stepAIC(fit1,direction="both")
?stepAIC()
ls(step1)
step1$anova 

####The final model after stepwise

FinalModel<- lm(TotalSpent ~ age + lninc + pets + pets_reptiles + pets_saltfish + 
                  longten + hourstv + region + gender + reason + card + card2 + 
                  card2fee + voice + internet + owntv + owngame,data=training)

summary(FinalModel) 
##Multiple R-squared:  0.3668
###Checking Multicollinearity
library(car)
install.packages("openxlsx")
library(VIF)
vif(FinalModel)

##Removing observations as per cook's distance
training$Cd<- cooks.distance(FinalModel)
training1<-subset(training, Cd< (4/3500))
View(training1)

fit3 <- lm(TotalSpent ~ age + lninc + pets + pets_reptiles + pets_saltfish + 
             longten + hourstv + region + gender + reason + card + card2 + 
             card2fee + voice + internet + owntv + owngame, training1)
summary(fit3) ###Multiple R-squared:  0.4265


###coefficients

CoefTrain<- coefficients(fit3)

write.csv(CoefTrain,"D:/AnalitixLabs/New folder/Linear Resubmission/CoefTrain.csv")

#######################SCORING USING PREDICT FUNCTION
##Training Dataset
t1<-cbind(training1, Pred_TotalSpent = predict(fit3))
names(t1)
View(t1)

##Testing Dataset
t2<-cbind(testing, Pred_TotalSpent = predict(fit3,testing))
names(t2)
View(t2)

# finding the decile locations -training dataset
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

write.csv(t1_DA,"D:/AnalitixLabs/New folder/Linear Resubmission/Train_decile.csv")

# find the decile locations -testing dataset
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

write.csv(t2_DA,"D:/AnalitixLabs/New folder/Linear Resubmission/Test_decile.csv")

#################################################################################################################


#make a report
install.packages("tidyverse")
install.packages("DataExplorer")
install.packages("datasets")

library(tidyverse)        # for data wrangling and visualization
library(DataExplorer)     # for exploratory data analysis
library(datasets)   
 create_report(dataf)
 
 View(dataf)
 
#1.load the dataset and convert the cols to numeric and factor
  dataf<-read.csv("C:/Users/Raja' Mohammad/Desktop/datamining/mydata.csv")
  attach(dataf)
  str(dataf)
  summary(dataf)
  
  dataf <- replace(dataf, dataf == "?", NA)
  
  
  age<-as.numeric(age)
  TT4<-as.numeric(TT4)
  TSH<-as.numeric(TSH)
  T3<-as.numeric(T3)
  T4U<-as.numeric(T4U)
  FTI<-as.numeric(FTI)
  TBG<-as.numeric(TBG)

  binaryClass<-as.factor(binaryClass)
  levels(binaryClass)
#####
#2.remove observations that the percentage of missing >=20% that are if 6att from 30 is null the row will deleted
    install.packages("tidyverse")
    library(tidyverse)
    NAObs <- dataf %>% filter(rowSums(is.na(.))==6)
    dataf <- anti_join(dataf,NAObs)
      
#3.1-The att (TBG) all values are NA we will remove it
      dataf<-subset(dataf, select = -TBG)
#4.2-(TBG.measured) is biased att all is "F" no one make this test we will remove it 
      dataf<-subset(dataf, select = -TBG.measured)
#5.3-(sex missing value)  we will fill the missing in mode the missing percentage is 4.01%
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      dataf$sex[is.na(dataf$sex)]<- getmode(dataf$sex)

#6.4.the att(TSH measurad, T3measured,TT4me,T4Umes,FTIme) all att are biased and the col mean did the patien make a test for this harmon almost ansers are True but anther answes are false then NA in the next col so i want to delete this cols
      dataf<-subset(dataf, select = -TSH.measured)
      dataf<-subset(dataf, select = -T3.measured)
      dataf<-subset(dataf, select = -TT4.measured)
      dataf<-subset(dataf, select = -T4U.measured)
      dataf<-subset(dataf, select = -FTI.measured)


#7.(age)no biased in this att
    
    z<-dnorm(age,mean(age,na.rm=T),sd(age,na.rm=T))
    plot(age,z) #draw normal distribution no biased
    
    #missing value .03%
    mean(age,na.rm = T) #51.735
    median(age,na.rm = T)#54
    #we will fill the missing in median
    age[is.na(age)]<- median(age,na.rm = T)
    #draw boxplot for outliers
     boxplot(age)
     Q1 <- quantile(age, 0.25,na.rm = T)
     Q3 <- quantile(age, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_age<- age[ age >upper_limit]
     #the percentage of outliers is 
     length(trim_age)/nrow(dataf)*100 #.02%
     #then replace the outliers with median or mean roughly the same
     
     dataf$age[dataf$age > upper_limit]<-median(dataf$age)
     #count for each value
     ggplot(dataf, aes(x=reorder(age, age, function(x)-length(x)))) +
       geom_bar(fill='red') +  labs(x='age')
       
 #8 (TSH)   
     
     z<-dnorm(TSH,mean(TSH,na.rm=T),sd(TSH,na.rm=T))
     plot(TSH,z) 
     #the att is positively skewed 
     #missing value 5.29%
     mean(TSH,na.rm = T) #5.07
     median(TSH,na.rm = T)#1.4
     #we will fill the missing in median because it positively skewed and the effected by extremes
     dataf$TSH[is.na(dataf$TSH)]<- median(TSH,na.rm = T)
     #draw boxplot for outliers
     boxplot(TSH)
     Q1 <- quantile(TSH, 0.25,na.rm = T)
     Q3 <- quantile(TSH, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_TSH<- TSH[ TSH <lower_limit|TSH >upper_limit]
     #the percentage of outliers is 
     length(trim_TSH)/nrow(dataf)*100 #11.67%
     #then replace the outliers with median 
     dataf$TSH[dataf$TSH < lower_limit | dataf$TSH > upper_limit]<-median(dataf$TSH)
     

     
     #9 (T3)   
     attach(dataf)
     z<-dnorm(T3,mean(T3,na.rm=T),sd(T3,na.rm=T))
     plot(T3,z) #draw normal distribution no biased
     #missing value 16%
     mean(T3,na.rm = T) #2.01
     median(T3,na.rm = T)#2
     #we will fill the missing in median because it positively skewed and the effected by extremes
     dataf$T3[is.na(dataf$T3)]<- median(T3,na.rm = T)
     #draw boxplot for outliers
     boxplot(T3)
     Q1 <- quantile(T3, 0.25,na.rm = T)
     Q3 <- quantile(T3, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_T3<- T3[ T3 <lower_limit|T3 >upper_limit]
     #the percentage of outliers is 
     length(trim_T3)/nrow(dataf)*100 #26.6%
     #we will replace with median
     dataf$T3[dataf$T3 > upper_limit]<-median(dataf$T3,na.rm = T)
     
     
     #10 (TT4)   
     attach(dataf)
     z<-dnorm(TT4,mean(TT4,na.rm=T),sd(TT4,na.rm=T))
     plot(TT4,z) #draw normal distribution no biased
     
     #missing value 1.39%
     mean(TT4,na.rm = T) #108.3
     median(TT4,na.rm = T)#103
     #we will fill the missing in median because it positively skewed and the effected by extremes
     dataf$TT4[is.na(dataf$TT4)]<- median(TT4,na.rm = T)
     #draw boxplot for outliers
     boxplot(TT4)
     Q1 <- quantile(TT4, 0.25,na.rm = T)
     Q3 <- quantile(TT4, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_TT4<- TT4[ TT4 <lower_limit|TT4 >upper_limit]
     #the percentage of outliers is 
     length(trim_TT4)/nrow(dataf) #.11%
     #we will replace it with median
     #TT4 <- subset(dataf, TT4 > lower_limit & TT4 < upper_limit)    
      
    
     #11 (T4U)   
     attach(dataf)
     z<-dnorm(T4U,mean(T4U,na.rm=T),sd(T4U,na.rm=T))
     plot(T4U,z) #draw normal distribution no biased
     
     #missing value 5.47%
     mean(T4U,na.rm = T) #.99
     median(T4U,na.rm = T)#.98
     #we will fill the missing in median
     dataf$T4U[is.na(dataf$T4U)]<- median(T4U,na.rm = T)
     #draw boxplot for outliers
     boxplot(TT4)
     Q1 <- quantile(T4U, 0.25,na.rm = T)
     Q3 <- quantile(T4U, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_T4U<- T4U[ T4U <lower_limit|T4U >upper_limit]
     #the percentage of outliers is 
     length(trim_T4U)/nrow(dataf) #.15%
     #we will replace it with median
     dataf$T4U[dataf$T4U > upper_limit]<-median(dataf$T4U,na.rm = T)
     
    
     
     #11 (FTI)   
     attach(dataf)
     z<-dnorm(FTI,mean(FTI,na.rm=T),sd(FTI,na.rm=T))
     plot(FTI,z) #draw normal distribution no biased
     
     #missing value 5.68%
     mean(FTI,na.rm = T) #110.4
     median(FTI,na.rm = T)#107
     #we will fill the missing in mean
     dataf$FTI[is.na(dataf$FTI)]<- mean(FTI,na.rm = T)
     #draw boxplot for outliers
     boxplot(FTI)
     Q1 <- quantile(FTI, 0.25,na.rm = T)
     Q3 <- quantile(FTI, 0.75,na.rm = T)
     IQR <- Q3 - Q1
     upper_limit <- Q3 + 1.5*IQR
     lower_limit <- Q1 - 1.5*IQR
     trim_FTI<- FTI[ FTI <lower_limit|FTI >upper_limit]
     #the percentage of outliers is 
     length(trim_FTI)/nrow(dataf) #.16%
     #we will replace it with median
     dataf$FTI[dataf$FTI > upper_limit]<-median(dataf$FTI,na.rm = T)
    ##### now logical cols all logical col donont have missing value 0%
      
     #12.(query.on.thyroxine)
     
     ggplot(dataf, aes(x=reorder(query.on.thyroxine, query.on.thyroxine,
                                 function(x)-length(x)))) +
       geom_bar(fill='red') +  labs(x='query.on.thyroxine')
     #this col is biased to "F" we will remove it
     dataf<-subset(dataf, select = -query.on.thyroxine)
     
     
     #13.pregnant col: this col is biased to "F"
         
     ggplot(dataf, aes(x=reorder(dataf$pregnant, dataf$pregnant, function(x)-length(x)))) +
       geom_bar(fill='blue') +  labs(x='pregnant')
         #convert all values T to F
      dataf$pregnant <- ifelse(dataf$pregnant == TRUE, FALSE, dataf$pregnant)
         #remove this col
      dataf<-subset(dataf, select = -pregnant)
      
     #14.hypopituitary att
         #all row are "F" then delete it
      ggplot(dataf, aes(x=reorder(hypopituitary, hypopituitary, function(x)-length(x)))) +
        geom_bar(fill='yellow') +  labs(x='hypopituitary')
     
      dataf<-subset(dataf, select = -hypopituitary)
      
      #15.referral.source this is string att 
      
      ggplot(dataf, aes(x=reorder(referral.source,referral.source, function(x)-length(x)))) +
        geom_bar(fill='pink') +  labs(x='referral source')
       ###
      dataf$pregnant <- ifelse(dataf$sex == "M", "False", dataf$pregnant)
      
      dataf$age <- ifelse(dataf$age < 0, median(age), dataf$age) 
      
      
      #the binaryClass is the factor but it baised 
      ggplot(dataf, aes(x=reorder(binaryClass, binaryClass, function(x)-length(x)))) +
        geom_bar(fill='green') +  labs(x='binaryClass')
      # so i must divide the all "p" rows in dataset1 and all "N" in dataset2 and make train and test data
      data1<- subset(dataf, binaryClass == "P")
      data2<- subset(dataf, binaryClass == "N")
      nrow(data1)#3301
      nrow(data2)#290
      #for train data take 70% from data1 "p" and 70% from data2 "N"
      #for test data take 30% from data1 "p" and 30% from data2 "N"
      split <- sample.split(data1$binaryClass, SplitRatio = 0.7)
      train <- subset(data1, split == TRUE)
      test <- subset(dataf, split == FALSE)
      split <- sample.split(data2$binaryClass, SplitRatio = 0.7)
      train <- rbind(subset(data2, split == TRUE),train)
      test <- rbind(subset(data2, split == FALSE),test)
      #classification model
      model <- naiveBayes(binaryClass ~ ., data = train)
      predictions <- predict(model, test)
      accuracy <- mean(predictions == test$binaryClass)
      print(accuracy)
      accuracy <- mean(predictions == train$binaryClass)
      print(accuracy)
  
      
      
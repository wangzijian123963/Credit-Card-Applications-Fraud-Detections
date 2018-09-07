setwd("~/Desktop/DS0562-Fraud Analytics")
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
#load data
options(scipen=999)
data<-read_excel("card transactions.xlsx")

# Using only records with Transtype=P
data=data%>%
  filter(Transtype=="P") #total record number changed into 96353

# Removing Outliers (The record with the highest amount (reason: used wrong currency type))
data%>%
  select(Recordnum,Amount)%>%
  arrange(-Amount) #Recordnum=52594 is the outlier

data=data[data$Recordnum!=52594,] #total record number changeed into 96352

# Filling "blank" into MerchantState and MerchantZip
colnames(data)[c(5,6,7)]=c('MerchantDes','MerchantState','MerchantZip')
data$MerchantState[is.na(data$MerchantState)]='Blank'
data$MerchantZip[is.na(data$MerchantZip)]='Blank'

# Filling serial number into Merchantnum
length(data$Merchantnum[is.na(data$Merchantnum)]) #total number of NAs in Merchantnum is 3198
data$Merchantnum[is.na(data$Merchantnum)]=seq(1,3198,1)

# write.csv(data,"cleaned_data.csv")


# Features Enginerring

library(sqldf)

data$Date<-as.Date(data$Date)
colnames(data)[10]='fraud'

fraud_per_cardnum=sqldf("
SELECT a.Recordnum,        sum(CASE WHEN a.date = b.date THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_1,
                           sum(CASE WHEN a.date - b.date BETWEEN 0 AND 2 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_3,
                           sum(CASE WHEN a.date - b.date BETWEEN 0 AND 6 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_7,
                           sum(CASE WHEN a.date - b.date BETWEEN 0 AND 13 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_14,
                           sum(CASE WHEN a.date - b.date BETWEEN 0 AND 29 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_30
                           FROM data a, data b
                           WHERE a.Cardnum = b.Cardnum
                           AND a.Recordnum>=b.Recordnum
                           GROUP BY 1
                           ")


fraud_per_merchantnum=sqldf("
SELECT a.Recordnum,     sum(CASE WHEN a.date = b.date THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantnum_1,
                        sum(CASE WHEN a.date - b.date BETWEEN 0 AND 2 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantnum_3,
                        sum(CASE WHEN a.date - b.date BETWEEN 0 AND 6 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantnum_7,
                        sum(CASE WHEN a.date - b.date BETWEEN 0 AND 13 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantnum_14,
                        sum(CASE WHEN a.date - b.date BETWEEN 0 AND 29 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantnum_30
                        FROM data a, data b
                        WHERE a.Merchantnum = b.Merchantnum
                        AND a.Recordnum>=b.Recordnum
                        GROUP BY 1
                        ")


fraud_per_merchantstate=sqldf("
SELECT a.Recordnum,         sum(CASE WHEN a.date = b.date THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_cardnum_1,
                            sum(CASE WHEN a.date - b.date BETWEEN 0 AND 2 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantstate_3,
                            sum(CASE WHEN a.date - b.date BETWEEN 0 AND 6 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantstate_7,
                            sum(CASE WHEN a.date - b.date BETWEEN 0 AND 13 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantstate_14,
                            sum(CASE WHEN a.date - b.date BETWEEN 0 AND 29 THEN a.fraud ELSE 0 END) - a.fraud AS fraud_per_merchantstate_30
                            FROM data a, data b
                            WHERE a.MerchantState = b.MerchantState
                            AND a.Recordnum>=b.Recordnum
                            GROUP BY 1
                            ")


fraud_variables=cbind(fraud_per_cardnum,fraud_per_merchantnum[,2:6])
fraud_variables=cbind(fraud_variables,fraud_per_merchantstate[,2:6])
write.csv(fraud_variables,'fraud_variables.csv')
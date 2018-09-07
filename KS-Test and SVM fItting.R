## KS Test
library(dplyr)
require(graphics)
setwd("~/Desktop/DS0562-Fraud Analytics")

########### Read and Split the Data
total=read.csv('new_vars.csv')
oot=total[total$record>=77851,]
finaldata=total[total$record<77851,]
############ Sampling 
set.seed(1)
train=sample(77848,62278)
training=finaldata[train,]
testing=finaldata[-train,]
data=training

############ Split by Fraud
data_1=data%>%
  filter(fraud==1)

data_0=data%>%
  filter(fraud==0)


############ Test Demo
a_1=data_1%>%
  group_by(same_ssn_diff_address_28)%>%
  summarise(count1=n())

a_0=data_0%>%
  group_by(same_ssn_diff_address_28)%>%
  summarise(count0=n())

a=merge(a_1,a_0,all = TRUE)
a[is.na(a)]=0

test=ks.test(a$count1,a$count0)$statistic
#The warning message is due to the implementation of the KS test in R, 
#which expects a continuous distribution and thus there should not be any identical values in the two datasets i.e. ties.
#for this issue, should use fitter() to apply a small penalty

#test1=ks.test(jitter(a$count1),jitter(a$count0))$p.value
#plot(a$same_ssn_diff_address_3,a$count1,type="l",col="red")
#lines(a$same_ssn_diff_address_3,a$count0,col="green")

#d=density(a$count1[-1])
#m=density(a$count0[-1])

#plot(d)
#lines(m)


#### Run the Loop for Result

result=numeric()

for(i in 3:157){
 a_1=data_1%>%
   group_by(data_1[,i])%>%
   summarise(count1=n())
 a_0=data_0%>%
   group_by(data_0[,i])%>%
   summarise(count0=n())
 colnames(a_0)[1]='var'
 colnames(a_1)[1]='var'
 a=merge(a_1,a_0,all = TRUE)
 a[is.na(a)]=0
 set.seed(1) # for fix jitter() penalty
 test=(ks.test(jitter(a$count1),jitter(a$count0))$statistic)*10
 result=c(result,test)
}


names=colnames(data[3:157]) #extract all variables 

df = data.frame(names,result) #combine results and variables

top50_vars=df%>%
  arrange(-result)%>%
  slice(1:50)            #Sort out top 30 variables      

top50<-data%>%
  select(top50_vars$names)
corr<-cor(top50)

corr=data.frame(corr)
#write.csv(corr,"corr.csv")

vars<-as.character(top50_vars$names)
vars<-vars[-c(1,2,3,4,5)]
write.csv(vars,"selected_vars_45.csv")


vars=read.csv("selected_vars_45.csv")

training_data=data%>%
  select(2,vars)
testing_data=testing%>%
  select(vars)
training_data$fraud=as.factor(training_data$fraud)

write.csv(training_data,'training_data.csv')

#### SVM Model

# Load the data
library(e1071)
training=read.csv('training.csv')
testing=read.csv('testing.csv')
oot=read.csv('oot.csv')

training_data=training[,-2]
testing_data=testing[,-2]
oot_data=oot[,-2]

training_data$fraud=as.factor(training_data$fraud)
testing_data$fraud=as.factor(testing_data$fraud)
oot$fraud=as.factor(oot$fraud)

# Train the model
model=svm(fraud~.,data=training_data,probability=TRUE)
summary(model)

print(model)

#training error
x_train=training_data[,-1]
y_train=training_data[,1]

pre_train=predict(model,x_train,probability=TRUE)
print(pre_train)
table(pre_train,y_train)

train=attr(pre_train,'probabilities')

training=cbind(training,train[,2])
colnames(training)[48]='score'

training_top10=training%>%
  arrange(-score)%>%
  slice(1:6228)

training_top10%>%
  filter(fraud=='1')%>%
  summarise(count=n())  #Catch 1625 fraud

total_training_fraud=training%>%
  filter(fraud==1)%>%
  summarise(count=n())  #12700 in total fraud in training set

FDR_training_10=1625/12700

#testing error
x_test=testing_data[,-1]
y_test=testing_data[,1]

pre_test=predict(model,x_test,probability=TRUE)
test=attr(pre_test,'probabilities')

testing=cbind(testing,test[,2])
colnames(testing)[48]='score'

testing_top10=testing%>%
  arrange(-score)%>%
  slice(1:1557)

testing_top10%>%
  filter(fraud=='1')%>%
  summarise(count=n())  # Catch 353 fraud

total_testing_fraud=testing%>%
  filter(fraud==1)%>%
  summarise(count=n()) # 3126 in total fraud

FDR_testing_10=353/3126

#### oot error

oot_model_data=rbind(training[,c(1,3:47)],testing[,c(1,3:47)])

oot_model_data$fraud=as.factor(oot_model_data$fraud)

model_ott=svm(fraud~.,data=oot_model_data,probability=TRUE)

pre_oot=predict(model,oot_data[,-1],probability=TRUE)

holdout=attr(pre_oot,'probabilities')

oot=cbind(oot,holdout[,2])
colnames(oot)[48]='score'

oot_top10=oot%>%
  arrange(-score)%>%
  slice(1:1700)

oot_top10%>%
  filter(fraud=='1')%>%
  summarise(count=n())  # Catch 545 fraud

total_oot_fraud=oot%>%
  filter(fraud==1)%>%
  summarise(count=n()) # 4338 in total fraud

FDR_oot_10=545/4338
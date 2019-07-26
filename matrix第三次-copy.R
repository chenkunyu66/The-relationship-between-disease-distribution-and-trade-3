library(foreign)
library(corpcor)
library(tseries)
library(quantmod)
library(plyr)
library(dplyr)
library(DMwR)
library(Hmisc)

data<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DEMO_I.XPT")
data_food2<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2IFF_I.XPT")
data_food3<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR1TOT_I.XPT")
data_food4<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2TOT_I.XPT")
data_food1<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2IFF_I.XPT")

data_all = left_join(data, data_food2, by = "SEQN")
data_all = left_join(data, data_food1, by = "SEQN")
data_all = left_join(data, data_food3, by = "SEQN")
data_all = left_join(data, data_food4, by = "SEQN")

rm(data_food2)
rm(data_food3)
rm(data_food4)
rm(data_food1)
gc()

data_all <- apply(data_all,2, function(data_all){
  if(sum(is.na(data_all))>0){
    data_all[is.na(data_all)]<- quantile(data_all,na.rm = T, probs = 0.5)
  }
  data_all
})

mental <- read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DPQ_I.XPT")

mental <- apply(mental,2, function(mental){
  if(sum(is.na(mental))>0){
    mental[is.na(mental)]<- quantile(mental,na.rm = T, probs = 0.5)
  }
  mental
})


temp = vector(mode="numeric", length = nrow(mental))
mental = cbind(mental,temp)
 for (i in 1:nrow(mental)){
   if ((mental[i,2]>1)|(mental[i,3]>1)|(mental[i,4]>1)|(mental[i,5]>1)|(mental[i,6]>1)|(mental[i,7]>1)|(mental[i,8]>1)|(mental[i,9]>1)|(mental[i,10]>1))
     mental[i,12]<-1
   else mental[i,12]<-0
 }

mental = mental[,c(1,12)]

final <- merge(mental,data_all)


set.seed(243523)
hm <- nrow(final)
data_pick <- sample(hm,0.7*hm)

training <- final[data_pick,]
testing <- final[-data_pick,]

m <- glm(temp~., data=training, family = binomial(link = logit) )
summary(m)

n <- predict(m,newdata=testing,type="response")

l <- prediction(n,testing$temp)

k <- performance(l,'auc')
k@y.values





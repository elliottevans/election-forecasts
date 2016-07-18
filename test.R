setwd("~/election_forecasts")
source("prep.R")
#Functions created: multiplot, sql

#Data sets created: 
#   1. master_state_ref
#   2. polls_2000
#   3. polls_2004
#   4. polls_2008
#   5. polls_2012
#   6. polls_2016
#   7. polls



#########################################################################################################################################
# CREATE WEIGHTED POLLING AVERAGES
#########################################################################################################################################
temp1<-sql("
Select  
  id
  ,election_year
  ,state as state
  ,date
  ,days_till_election
  ,sum(case when party='D' then value else 0 end) -
    sum(case when party='R' then value else 0 end) as dem_plus_minus
from polls
group by 1,2,3
")
names(temp1)<-c('id','election_year','state','date','days_till_election','dem_plus_minus')


#Create running averages of polling margins
years<-c(2000,2004,2008,2012,2016)
polls_altered<-data.frame()
#1:length(years)
for(l in 1:length(years)){
  year<-years[l]
  temp2<-temp1[temp1$election_year==year,]
  states<-unique(temp2$state)
  #1:length(states)
  for(m in 1:length(states)){
    state<-states[m]
    temp3<-temp2[temp2$state==state,]
    temp4<-temp3[order(-temp3$days_till_election),]
    weighted_avg<-c()
    for(p in 1:nrow(temp4)){
      #Note that we give more weight to polls closer to the election day
      
      temp5<-temp4[1:p,]
      n<-nrow(temp5)
      weights<-c()
      weight_sum<-0
      for(i in 1:n){ 
        sum<-1
        j<-1
        while(j >=1 && j<=(n-1)){
          sum_temp<-0
          k<-i
          while(k>=i && k<=j){
            sum_temp<-sum_temp+temp5$days_till_election[k]/temp5$days_till_election[k+1]
            k<-k+1
          }
          if(i>j){sum<-sum+0}else{sum<-sum+expChoose(sum_temp,2)}
          j<-j+1
        }
        sum<-(1-weight_sum)*(sum^(-1))
        weight_sum<-weight_sum+sum
        weights<-append(weights,sum)
      }

      weighted_avg<-append(weighted_avg,sum(temp5$dem_plus_minus*weights))      
          
    }
    temp5<-data.frame(cbind(temp5,weighted_avg))
    polls_altered<-data.frame(rbind(polls_altered,temp5))
  }
}






      n<-nrow(temp5)
      weights<-c()
      weight_sum<-0
      for(i in 1:n){ 
        sum<-1
        j<-1
        while(j >=1 && j<=(n-1)){
          sum_temp<-0
          k<-i
          while(k>=i && k<=j){
            sum_temp<-sum_temp+temp5$days_till_election[k]/temp5$days_till_election[k+1]
            k<-k+1
          }
          if(i>j){sum<-sum+0}else{sum<-sum+exp(sum_temp)}
          j<-j+1
        }
        sum<-(1-weight_sum)*(sum^(-1))
        weight_sum<-weight_sum+sum
        print(weight_sum)
        weights<-append(weights,sum)
      }






bla<-polls_altered[polls_altered$election_year==2016,]
bla<-bla[bla$state=='PA',]




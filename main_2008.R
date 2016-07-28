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

#run_date<-as.Date(Sys.Date())
run_date<-as.Date("2008-11-22")

polls<-polls[polls$Date<=run_date,]

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
years<-c(2000,2004,2008)
polls_altered<-data.frame()
prop_weights<-c()
prop_weight_list<-c()
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
    exp_weighted_avg<-c()
    running_avg<-c()
    prop_weighted_avg<-c()
    
    for(p in 1:nrow(temp4)){
      #Note that we give more weight to polls closer to the election day
      
      #EXPONENTIAL WEIGHTING
      temp5<-temp4[1:p,]
      n<-nrow(temp5)
      exp_weights<-c()
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
          if(i>j){sum<-sum+0}else{sum<-sum+(1.3^sum_temp)}
          j<-j+1
        }
        sum<-(1-weight_sum)*(sum^(-1))
        weight_sum<-weight_sum+sum
        exp_weights<-append(exp_weights,sum)
      }

      exp_weighted_avg<-append(exp_weighted_avg,sum(temp5$dem_plus_minus*exp_weights))      
      
      #PROPORTIONAL WEIGHTING
      running_avg<-append(running_avg,sum(temp4$dem_plus_minus[1:p])/p)
      prop_weighted_avg<-append(prop_weighted_avg,sum(temp4$dem_plus_minus[1:p] * (sort(temp4$days_till_election[1:p]))/sum(temp4$days_till_election[1:p])))
      prop_weights<-(sort(temp4$days_till_election[1:p]))/sum(temp4$days_till_election[1:p])
          
    }
    prop_weight_list<-append(prop_weight_list,prop_weights)
    temp5<-data.frame(cbind(temp5,exp_weighted_avg,exp_weights,prop_weighted_avg))
    polls_altered<-data.frame(rbind(polls_altered,temp5))
  }
}
polls_altered<-data.frame(cbind(polls_altered,prop_weight_list))
names(polls_altered)[ncol(polls_altered)]<-'prop_weights'

polls_altered<-sql("
select
  temp1.*
  ,case when actual='D' then 1
        when actual='R' then 0
        when election_year=2008 then '' end as actual_binary_dem
from
(
  select
    pa.*
    ,case when pa.election_year=2000 then hist_dem_prob_2000
          when pa.election_year=2004 then hist_dem_prob_2004
          when pa.election_year=2008 then hist_dem_prob_2008
          when pa.election_year=2012 then hist_dem_prob end as hist_dem_prob
    ,case when pa.election_year=2000 then msr.`2000`
          when pa.election_year=2004 then msr.`2004`
          when pa.election_year=2008 then msr.`2008`
          when pa.election_year=2012 then '' end as actual
    ,case when pa.election_year=2000 then msr.`2000_dem_margin`
          when pa.election_year=2004 then msr.`2004_dem_margin`
          when pa.election_year=2008 then msr.`2008_dem_margin`
          when pa.election_year=2012 then '' end as actual_dem_margin
  from polls_altered pa 
    inner join master_state_ref msr on msr.abb=pa.state
) as temp1
")
polls_altered[polls_altered$election_year==2012,'actual_binary_dem']<-''
#########################################################################################################################################
# CREATE WEIGHTED POLLING AVERAGES
#########################################################################################################################################





#########################################################################################################################################
# NEAREST NEIGHBOR LEARNING
#########################################################################################################################################
#k_run<-100
temp<-polls_altered
#standardize metrics for nearest neighbor algorithm
temp$days_till_election<-temp$days_till_election-mean(temp$days_till_election)
temp$days_till_election<-temp$days_till_election/sd(temp$days_till_election)
temp$exp_weighted_avg<-temp$exp_weighted_avg-mean(temp$exp_weighted_avg)
temp$exp_weighted_avg<-temp$exp_weighted_avg/sd(temp$exp_weighted_avg)
temp$hist_dem_prob<-temp$hist_dem_prob-mean(temp$hist_dem_prob)
temp$hist_dem_prob<-temp$hist_dem_prob/sd(temp$hist_dem_prob)

##########################
#Take equal parts from each year for nearest neighbor alg
#Creates indices_2000,indices_2000,indices_2008,indices_2012
#Creates train_2000,train_2004,train_2008,train_2012
##########################

#Take 15 distinct most similar states from each year to associate with curret states
years<-c(2000,2004,2008)
for(i in 1:length(years)){
  margins<-data.frame()
  train_year<-paste0("train","_",years[i])
  assign(train_year,temp[temp$election_year==years[i],])
  train_temp_with_info<-train_temp<-temp[temp$election_year==years[i],]
  train_temp<-temp[temp$election_year==years[i],c(
            "days_till_election"
            ,"exp_weighted_avg"
            ,"hist_dem_prob"
  )]
  test_temp<-temp[temp$election_year==2012,c(
            "days_till_election"
            ,"exp_weighted_avg"
            ,"hist_dem_prob"
  )]
  #1:nrow(test_temp)
  for(j in 1:nrow(test_temp)){
    #For each row in the testing set
    unique_states_counter<-0
    k<-1
    
    while(unique_states_counter<15){
      nearest<-get.knnx(train_temp,test_temp[j,],k)
      indices<-nearest$nn.index
      distances<-nearest$nn.dist
      unique_states_counter<-length(unique(train_temp_with_info[unlist(as.list(indices)),]$state))
      k<-k+1
    }
    #We now have a non-unique list of indices that includes 15 different states
    train_temp_with_info_2<-train_temp_with_info[unlist(as.list(indices)),]
    train_temp_with_info_2$distances<-unlist(as.list(distances))
    states_wanted<-sql("
    select
      election_year
      ,state
      ,min(distances) as min_distances
      ,id
      ,actual_dem_margin
    from train_temp_with_info_2 ttwi
    group by state
    order by 3 asc
    limit 15
    ")
    margins<-rbind(margins,as.list(states_wanted$actual_dem_margin))
  }
  if(i==1){margins_total<-margins}else{margins_total<-cbind(margins_total,margins)}
}

##########################
#Take equal parts from each year for nearest neighbor alg
##########################

polls_altered_2012<-polls_altered[polls_altered$election_year==2012,]

means<-c()
sds<-c()
probs<-c()
for(i in 1:nrow(polls_altered_2012)){
  mean<-mean(unlist(as.list(margins_total[i,])))
  sd<-sd(unlist(as.list(margins_total[i,])))
  
  means<-append(means,mean)
  sds<-append(sds,sd)
  probability<-pnorm(q=0,mean=mean,sd=sd,lower.tail = FALSE)
  probs<-append(probs,probability)
}


polls_altered_2012$probs<-probs
polls_altered_2012$mean<-means
polls_altered_2012$sd<-sds
#########################################################################################################################################
# NEAREST NEIGHBOR LEARNING
#########################################################################################################################################

polls_altered_final<-data.frame(cbind(polls_altered,prob_weighted_dem=NA,prob_weighted_rep=NA))

polls_altered_final<-melt(polls_altered_final,id=c('id','election_year','state','date','days_till_election','dem_plus_minus',
                                   'exp_weighted_avg','exp_weights','prop_weighted_avg','prop_weights','hist_dem_prob','actual','actual_dem_margin','actual_binary_dem'))
names(polls_altered_final)[ncol(polls_altered_final)-1]<-'prediction'

polls_altered_final<-sql("
select 
  paf.*
  ,mean 
  ,sd
  ,case when prediction='prob_weighted_rep' then 1-probs
       when prediction='prob_weighted_dem' then probs
  end as nearest_neighbor_value
from polls_altered_final paf
  left join polls_altered_2012 pa on pa.id=paf.id
")
polls_altered_final<-polls_altered_final[,!(colnames(polls_altered_final) %in% c('value'))]

####################################
# Actually create the models
####################################

####################################
# Set indicators for the state's most current prediction date
####################################
#Make indicators for the last prediction for each state-year
temp<-sql("
select 
  election_year
  ,State
  ,min(days_till_election) as final_poll_days_till_election
from polls_altered_final
group by 1,2
")

polls_altered_final<-sql("
select
  paf.*
  ,case when final_poll_days_till_election=days_till_election then 1 else 0 end as final_prediction_ind
from polls_altered_final paf
  inner join temp t on paf.election_year=t.election_year and paf.State=t.State
")

####################################
# Set indicators for the state's most current prediction date
####################################



#########################################################################################################################################
# ELECTION SIMULATION
#########################################################################################################################################
#HERE TO EDIT
state_odds<-sqldf("
select
  msr.state
  ,abb
  ,electoral_votes
  ,ifnull(dem_prob,msr.hist_dem_prob) as dem_prob
  ,mean  
  ,sd
from master_state_ref msr
  left join 
(
select
  state 
  ,hist_dem_prob
  ,nearest_neighbor_value as dem_prob
  ,mean 
  ,sd
from polls_altered_final paf
where final_prediction_ind=1
  and election_year=2012
  and prediction='prob_weighted_dem'
) as t1
 on msr.abb=t1.state
")

state_odds<-sql("
select
  so.state
  ,so.abb
  ,so.electoral_votes
  ,dem_prob
  ,ifnull(mean,(`2000_dem_margin`+`2004_dem_margin`+`2008_dem_margin`)/3) as mean 
  ,ifnull(sd,sqrt(power((((`2000_dem_margin`+`2004_dem_margin`+`2008_dem_margin`)/3)-`2000_dem_margin`),2)+
    power((((`2000_dem_margin`+`2004_dem_margin`+`2008_dem_margin`)/3)-`2004_dem_margin`),2)+
    power((((`2000_dem_margin`+`2004_dem_margin`+`2008_dem_margin`)/3)-`2008_dem_margin`),2))) as sd
from state_odds so
  inner join master_state_ref msr on so.state=msr.state
")
                  

print("RUNNING ELECTION SIMULATIONS",quote=FALSE)
n<-10000
dem_wins<-0
electoral_vote_list<-c()
for(i in 1:n){
  set.seed(seed = NULL)
  if(i %% 1000 == 0){print(paste('CURRENTLY ON ELECTION SIMULATION:',i),quote=FALSE)}
  state_odds_rand<-state_odds[sample(nrow(state_odds)),]
  electoral_votes<-0
  margins<-c()
  for(j in 1:nrow(state_odds_rand)){
    if(j==1){
      margin<-rnorm(1,state_odds_rand$mean[j],state_odds_rand$sd[j])
      if(margin>=0){win_or_lose<-1}else{win_or_lose<-0}
      margins<-append(margins,margin)
      }
    else {
      corr<-t(master_state_ref)
      colnames(corr)<-corr['abb',]
      corr<-data.frame(corr[12:14,],stringsAsFactors = FALSE)
      corr<-sapply(corr,as.numeric)
      corr<-cor(corr[,state_odds_rand$abb[(j-1):j]])
      correlation<-corr[1,2]
      updated_mean<-state_odds_rand$mean[j] + 
        correlation*(state_odds_rand$sd[(j-1)]/state_odds_rand$sd[j])*
        (margins[(j-1)]-state_odds_rand$mean[(j-1)])
      updated_sd<-sqrt(state_odds_rand$sd[j]^2*(1-correlation^2))
      margin<-rnorm(1,updated_mean,updated_sd)
      if(margin>=0){win_or_lose<-1}else{win_or_lose<-0}
      margins<-append(margins,margin)
    }
    
    if(win_or_lose==1){
      electoral_votes<-electoral_votes+state_odds_rand[j,'electoral_votes']
    }
  }
  electoral_vote_list<-append(electoral_vote_list,electoral_votes)
  if(electoral_votes>=270){
    dem_wins<-dem_wins+1
  }
}
 
dem_prob<-dem_wins/n
state_odds$tested_odds<-round(100*pnorm(q=0,mean=state_odds$mean,sd=state_odds$sd,lower.tail = FALSE),1)
#########################################################################################################################################
#ELECTION SIMULATION
#########################################################################################################################################




#########################################################################################################################################
# VISUALIZATION: STATE MARGINS
#########################################################################################################################################
state_odds_temp<-state_odds[is.na(state_odds$mean)==FALSE,]
state_odds_temp<-state_odds_temp[order(state_odds_temp$mean),]
#80% confidence intervals
ci_bands<-aes(ymax=mean+1.28*sd,ymin=mean-1.28*sd)
dodge <- position_dodge(width=0.9)


state_margins<-ggplot(data=state_odds_temp, aes(x=reorder(state, -mean), y=mean,fill=mean)) +
    geom_bar(stat="identity") +
    geom_crossbar(ci_bands, position=dodge, width=0.25,alpha=.5,colour='grey41',show.legend = TRUE)+
    coord_flip() + 
     scale_fill_gradient2(
       low = "red"
       ,high = "blue"
       ,mid = "grey"
       ,midpoint = 0) +
    labs(title = "State Margins of Victory")+
    theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=31))+
    ylab("Margin of Victory")+
    xlab(expression(paste(symbol('\254'),' ','Obama','        ','Romney',' ', symbol('\256'))))+
    scale_y_continuous(breaks = c(-50,-25,0,25,50), labels = c("+50","+25", "0","+25", "+50"))+
    theme(axis.text=element_text(size=25))+
    theme(axis.title=element_text(size=25))+
    theme(legend.text = element_text(size = 19, face = "bold"))+
    theme(legend.position = "none")+
    theme(axis.title.y=element_text(face='bold'))+
    theme(panel.grid.minor = element_blank()
          ,panel.background = element_rect(fill = "white")
          ,panel.grid.major = element_line(colour = "gray93")
    )

#########################################################################################################################################
# VISUALIZATION: STATE MARGINS
#########################################################################################################################################



#########################################################################################################################################
# VISUALIZATION: HISTOGRAMS
#########################################################################################################################################
hist_data<-
data.frame(
  rbind(
    cbind(rep('Clinton',length(electoral_vote_list)),electoral_vote_list)
    ,cbind(rep('Trump',length(electoral_vote_list)),538-electoral_vote_list)
  )
)
names(hist_data)<-c('candidate','electoral_votes')
hist_data$electoral_votes<-as.numeric(as.character(hist_data$electoral_votes))
sum_dat<-ddply(hist_data, "candidate", summarise, electoral_votes.mean=mean(electoral_votes))

if(sum_dat[sum_dat$candidate=='Clinton',2]>=270){
    clinton_label_spot<-sum_dat[sum_dat$candidate=='Clinton',2]+12
    trump_label_spot<-sum_dat[sum_dat$candidate=='Trump',2]-12
}else{
    clinton_label_spot<-sum_dat[sum_dat$candidate=='Clinton',2]-12
    trump_label_spot<-sum_dat[sum_dat$candidate=='Trump',2]+12
}

line_lengths<-sql("
select
  hd.candidate
  ,count(case when electoral_votes>=round(`electoral_votes.mean`)-2 and electoral_votes<=round(`electoral_votes.mean`)+2 then hd.candidate end) as counter
from hist_data hd
  inner join sum_dat sd on sd.candidate=hd.candidate
group by 1
")

simulated_result<-ggplot(hist_data, aes(x=electoral_votes, fill=candidate)) +
    geom_histogram(binwidth=5, alpha=.5, position="identity")+
    scale_fill_manual(values=c("deepskyblue", "firebrick1"))+
    geom_text(aes(x=clinton_label_spot, label=round(sum_dat[sum_dat$candidate=='Clinton',2]), y=60), colour="blue",size=8)+
    geom_text(aes(x=trump_label_spot, label=round(sum_dat[sum_dat$candidate=='Trump',2]), y=60), colour="red3",size=8)+
    geom_text(aes(x=270, label='270 to Win', y=380),size=8)+
    ggtitle("Electoral Votes")+
    ylab("Simulations")+
    xlab("Electoral Votes")+
    guides(fill=guide_legend(title=NULL))+
    theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=31))+
    theme(axis.text=element_text(size=18))+
    theme(axis.title=element_text(size=22))+
    theme(legend.text = element_text(size = 19, face = "bold"))+
    geom_segment(aes(x = round(sum_dat[sum_dat$candidate=='Clinton',2]), y = 0, xend = round(sum_dat[sum_dat$candidate=='Clinton',2]), yend = line_lengths[line_lengths$candidate=='Clinton','counter']), colour = "blue",linetype='dashed',size=1)+
    geom_segment(aes(x = round(sum_dat[sum_dat$candidate=='Trump',2]), y = 0, xend = round(sum_dat[sum_dat$candidate=='Trump',2]), yend = line_lengths[line_lengths$candidate=='Trump','counter']), colour = "red3",linetype='dashed',size=1)+
    geom_segment(aes(x = 270, y = 0, xend = 270, yend = 360),linetype='dashed',size=1)+
    theme(legend.position = "bottom")+
    theme(panel.grid.minor = element_blank()
          ,panel.background = element_rect(fill = "white")
          ,panel.grid.major = element_line(colour = "gray93")
          ,axis.line.x = element_line(color="black")
    )

#########################################################################################################################################
# VISUALIZATION: HISTOGRAMS
#########################################################################################################################################



#########################################################################################################################################
# VISUALIZATION: CARTOGRAM
#########################################################################################################################################
dat <- data.frame(state=as.character(state_odds$abb), value=state_odds$mean, stringsAsFactors=FALSE)
dat[dat$value>=50,'value']<-dat[dat$value>=50,'value']/3
dat[dat$value<=-50,'value']<-dat[dat$value<=-50,'value']/2

map<-statebins(dat
          ,breaks=7
          ,labels=c("Solid R","Likely R","Lean R","Tossup","Lean D","Likely D","Solid D")
          ,brewer_pal="RdBu"
          ,text_color="black"
          ,font_size=6
          ,legend_title=""
          ,legend_position="bottom"
          )
#########################################################################################################################################
# VISUALIZATION: CARTOGRAM
#########################################################################################################################################




#########################################################################################################################################
# VISUALIZATION: STATE GRAPHS
#########################################################################################################################################
temp<-sql("
select
  paf.*
  ,msr.state as state_full
  ,candidate as candidate
from polls_altered_final paf
  inner join polls p on paf.election_year=p.election_year
    and p.State=paf.State
    and (case when prediction in ('prob_unweighted_dem','prob_weighted_dem') then 'D'
              when prediction in ('prob_unweighted_rep','prob_weighted_rep') then 'R'
         end) = p.party
    and p.id=paf.id
  inner join master_state_ref msr on paf.state=msr.abb
where paf.election_year=2012
")
relevant_list<-sort(unique(temp$state_full))
relevant_list_abb<-unique(temp[order(temp$state_full),'state'])
temp$value<-100*as.numeric(temp$nearest_neighbor_value)

temp<-sql("
select
  t.*
  ,case when value>99.9 then '>99.9'
        when value<.1 then '<.1'
        else cast(round(value,1) as text) end as poll_data_value_label 
from temp t
")

plots<-vector('list', length(relevant_list))
for(i in 1:length(relevant_list)){
  state<-relevant_list[i]
  state_abbrev<-relevant_list_abb[i]
  if(nchar(state)>10){state_label<-state_abbrev}else{state_label<-state}
  
  temp_new<-temp[temp$state_full==state,]
  poll_temp<-temp_new[temp_new$final_prediction_ind==1,]

  plot<- ggplot(data=temp_new,aes(x=date,y=value,colour=candidate,group=candidate)) + 
    geom_line(size=1.2) + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    ggtitle(
      paste(state_label," - ",as.character(poll_temp[which.max(poll_temp$value),'candidate']),sub(" ", "",paste(poll_temp[which.max(poll_temp$value),'poll_data_value_label'],"%"),fixed=TRUE))
    )+
    theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=23))+
    theme(legend.position = "none")+
    scale_color_manual(values=c("deepskyblue", "firebrick1"))+
    theme(axis.title.y=element_blank())+
    theme(axis.title.x=element_blank())+
    scale_y_continuous(limits = c(0, 100))+
    theme(panel.grid.minor = element_blank()
          ,panel.background = element_rect(fill = "white")
          ,panel.grid.major = element_line(colour = "gray93")
          ,axis.line.x = element_line(color="black")
          ,axis.line.y = element_blank()
        )
          
  plots[[i]]<-plot
}

#multiplot(plotlist = plots,cols=3)
#########################################################################################################################################
# VISUALIZATION: STATE GRAPHS
#########################################################################################################################################





#########################################################################################################################################
# RESULTS
#########################################################################################################################################

#Biggest blowouts:
republican_blowout<-state_odds[state_odds$mean==min(state_odds$mean),'state']
republican_blowout_margin<-round(abs(state_odds[state_odds$mean==min(state_odds$mean),'mean']),0)

democratic_blowout<-state_odds[state_odds$mean==max(state_odds$mean),'state']
democratic_blowout_margin<-round(abs(state_odds[state_odds$mean==max(state_odds$mean),'mean']),0)

if(dem_prob>=.5){
  winner<-'Hillary Clinton'
  loser<-'Donald Trump'
  winner_prob<-paste0(round(100*dem_prob,1),'%')
  loser_prob<-paste0(100-round(100*dem_prob,1),'%')
  winning_electoral_votes<-round(mean(electoral_vote_list),0)
  losing_electoral_votes<-538-winning_electoral_votes
  color<-'dodgerblue'
  num_states_won<-nrow(state_odds[state_odds$mean>0,])
  
  winner_pronoun<-"She"
  loser_pronoun<-"He"
  
  if(round(100*dem_prob,1)<90 && round(100*dem_prob,1)>=80){a_or_an<-'an'}else{ a_or_an<-'a'}
  
}else{
  winner<-'Donald Trump'
  loser<-'Hillary Clinton'
  winner_prob<-paste0(100-round(100*dem_prob,1),'%')
  loser_prob<-paste0(round(100*dem_prob,1),'%')
  winning_electoral_votes<-538-round(mean(electoral_vote_list),0)
  losing_electoral_votes<-round(mean(electoral_vote_list),0)
  color<-'firebrick1'
  num_states_won<-nrow(state_odds[state_odds$mean<0,])

  
  winner_pronoun<-'He'
  loser_pronoun<-'She'
  
  if(100-round(100*dem_prob,1)<90 && 100-round(100*dem_prob,1)>=80 ){a_or_an<-'an'}else{ a_or_an<-'a'}

}

 # par(bg = color)
 # par(mar = c(0,0,0,0))
 # plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
 # text(x = 0.5, y = 0.5, paste0(winner," has ",a_or_an," ",winner_prob," chance of being president"),
 #      cex = 2, col = "white",font=2)
 # text(x = 0.5, y = 0.45, paste0(winner_pronoun," will win ", num_states_won," states",plus_dc," winning the electoral college ",winning_electoral_votes,"-",losing_electoral_votes),
 #      cex = 1.6, col = "grey88",font=2)


#########################################################################################################################################
# RESULTS
#########################################################################################################################################



#########################################################################################################################################
# HTML TABLE
#########################################################################################################################################

for(i in 1:nrow(state_odds)){
  margin<-paste0(state_odds$abb[i],'_MARGIN')
  assign(margin,paste0('+',abs(round(state_odds$mean[i],1))))
  
  ev<-paste0(state_odds$abb[i],'_EV')
  assign(ev,state_odds$electoral_votes[i])
  
  if(state_odds$mean[i]<0){
    odds_temp_temp<-100-state_odds$tested_odds[i]
  }else{odds_temp_temp<-state_odds$tested_odds[i]}
  
  if(odds_temp_temp==100){odds_temp<-'>99.9%'}
  else if(odds_temp_temp==0){odds_temp<-'<0.1%'}
  else{odds_temp<-paste0(odds_temp_temp,'%')}
  
  probs<-paste0(state_odds$abb[i],'_ODDS')
  assign(probs,odds_temp)
  
  winners<-paste0(state_odds$abb[i],'_WINNER')
  if(state_odds$mean[i]>=0){winner_temp<-'Clinton'}else{winner_temp<-'Trump'}
  assign(winners,winner_temp)

}


#########################################################################################################################################
# HTML TABLE
#########################################################################################################################################



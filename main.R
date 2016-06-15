setwd("~/Desktop/Random Analysis/2016_election_analysis")
library("sqldf", lib.loc="~/R/win-library/3.2")
library("sqldf", lib.loc="~/R/win-library/3.2")
library("reshape2", lib.loc="~/R/win-library/3.2")
library("pollstR", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library(rgdal)
library(rgeos)
library(maptools)
library("stringr", lib.loc="~/R/win-library/3.2")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

sql<-function(str){
  sqldf()
  ret<-sqldf(str,drv="SQLite")
  sqldf()
  return(ret)
}

#############################################################
#Clean old polling data
#Obtained from http://www.electoral-vote.com/evp2012/Info/datagalore.html
#Needs to be election_year, poll_id, candidate, value, start, end, pollster

polls_2004<-read.csv("polls\\polls_2004.csv")
polls_2004<-data.frame(cbind(seq(1,nrow(polls_2004)),'2004',polls_2004))
names(polls_2004)[1:2]<-c('id','election_year')
polls_2004<-melt(polls_2004,id=c("id","election_year","State","Date","Pollster"))
names(polls_2004)[6]<-"Candidate"
polls_2004$Date<-as.Date(paste(polls_2004$Date,'2004'),format='%m/%d %Y')
polls_2004<-sql(
  "
  select
    p.*
    ,case when Candidate='Kerry' then 'D' else 'R' end as party
  from polls_2004 p
  "
)
#Add on days before general election
polls_2004<-data.frame(cbind(polls_2004,difftime(as.Date('2004-11-02'),polls_2004$Date,units="days")))
names(polls_2004)[ncol(polls_2004)]<-'days_till_election'



polls_2008<-read.csv("polls\\polls_2008.csv")
polls_2008<-data.frame(cbind(seq(nrow(polls_2004)+1,nrow(polls_2004)+nrow(polls_2008)),'2008',polls_2008[,c("State","Obama","McCain","Start","Pollster")]))
names(polls_2008)[c(1,2,6)]<-c('id','election_year','Date')
polls_2008<-melt(polls_2008,id=c("id","election_year","State","Date","Pollster"))
names(polls_2008)[6]<-"Candidate"
polls_2008$Date<-as.Date(paste(polls_2008$Date,'2008'),format='%m/%d %Y')
polls_2008<-sql(
  "
  select
  p.*
  ,case when Candidate='Obama' then 'D' else 'R' end as party
  from polls_2008 p
  "
)
#Add on days before general election
polls_2008<-data.frame(cbind(polls_2008,difftime(as.Date('2008-11-05'),polls_2008$Date,units="days")))
names(polls_2008)[ncol(polls_2008)]<-'days_till_election'



#The following was obtained from fivethirtyeight's pollster data
polls_2000<-read.csv("polls\\polls_2000.csv")
polls_2000<-data.frame(cbind(seq(nrow(polls_2008)+1,nrow(polls_2008)+nrow(polls_2000)),'2000',polls_2000[,c("State","Gore","Bush","Start","Pollster")]))
names(polls_2000)[c(1,2,6)]<-c('id','election_year','Date')
polls_2000<-melt(polls_2000,id=c("id","election_year","State","Date","Pollster"))
names(polls_2000)[6]<-"Candidate"
polls_2000$Date<-as.Date(paste(polls_2000$Date,'2000'),format='%m/%d/%Y')
polls_2000<-sql(
  "
  select
  p.*
  ,case when Candidate='Gore' then 'D' else 'R' end as party
  from polls_2000 p
  "
)
#Add on days before general election
polls_2000<-data.frame(cbind(polls_2000,difftime(as.Date('2000-11-07'),polls_2000$Date,units="days")))
names(polls_2000)[ncol(polls_2000)]<-'days_till_election'
#############################################################

############################################
# GET 2012 POLLING
############################################
#Use HuffPollster API
polls_2012<-data.frame()
states<-append(state.abb,"DC")
for(i in 1:length(states)){
#  print(noquote(paste("CURRENTLY GETTING STATE:",states[i])))
  temp1<-pollstr_polls(topic='2012-president',showall=FALSE,state=states[i],max_pages=50)
  temp2<-temp1$polls
  temp3<-temp1$question
  if(dim(temp3)[1]==0) next
  temp4<-sql(
    "
    select 
      id
      ,'2012' as election_year
      ,state as State
      ,start_date
      ,pollster as Pollster
      ,choice as Candidate
      ,value
      ,case when choice='Obama' then 'D' else 'R' end as party
    from temp2 
      inner join temp3 using (id) 
    where topic='2012-president'
      and choice in ('Romney','Obama')
    "
  )
  polls_2012<-rbind(polls_2012,temp4)
}
names(polls_2012)[4]<-'Date'
#Add on days before general election
polls_2012<-data.frame(cbind(polls_2012,difftime(as.Date('2012-11-06'),polls_2012$Date,units="days")))
names(polls_2012)[ncol(polls_2012)]<-'days_till_election'

#Write 2012 polls to csv file
write.csv(polls_2012,'polls\\polls_2012.csv',row.names = FALSE)
############################################
# GET 2012 POLLING
############################################

############################################
# GET 2016 POLLING
############################################
#Use HuffPollster API
polls_2016<-data.frame()
states<-append(state.abb,"DC")
for(i in 1:length(states)){
#  print(noquote(paste("CURRENTLY GETTING STATE:",states[i])))
  temp1<-pollstr_polls(topic='2016-president',showall=FALSE,state=states[i],max_pages=50)
  temp2<-temp1$polls
  temp3<-temp1$question
  if(dim(temp3)[1]==0) next
  temp4<-sql(
    "
    select 
      id
      ,'2016' as election_year
      ,state as State
      ,start_date
      ,pollster as Pollster
      ,choice as Candidate
      ,value
      ,case when choice='Clinton' then 'D' else 'R' end as party
    from temp2 
      inner join temp3 using (id) 
    where topic='2016-president'
      and choice in ('Trump','Clinton')
    "
  )
  polls_2016<-rbind(polls_2016,temp4)
}
names(polls_2016)[4]<-'Date'
#Add on days before general election
polls_2016<-data.frame(cbind(polls_2016,difftime(as.Date('2016-11-08'),polls_2016$Date,units="days")))
names(polls_2016)[ncol(polls_2016)]<-'days_till_election'

#Write 2016 polls to csv file
write.csv(polls_2016,'polls\\polls_2016.csv',row.names = FALSE)
############################################
# GET 2016 POLLING
############################################


polls<-data.frame(rbind(polls_2000,polls_2004,polls_2008,polls_2012,polls_2016))

polls$days_till_election<-as.numeric(polls$days_till_election)

#Write all polls to a master csv file
write.csv(polls,'polls\\polls_total.csv',row.names = FALSE)

############################################################################
#create the logistic models
############################################################################
temp1<-sql("
Select  
  id
  ,election_year
  ,State
  ,date
  ,days_till_election
  ,sum(case when party='D' then value else 0 end) -
    sum(case when party='R' then value else 0 end) as dem_plus_minus
from polls
group by 1,2,3
")

years<-c(2000,2004,2008,2012,2016)
polls_altered<-data.frame()
for(i in 1:length(years)){
  year<-years[i]
  temp2<-temp1[temp1$election_year==year,]
  states<-unique(temp2$State)
  for(j in 1:length(states)){
    state<-states[j]
    temp3<-temp2[temp2$State==state,]
    temp4<-temp3[order(-temp3$days_till_election),]
    running_average<-c()
    weighted_running_average<-c()
    for(k in 1:nrow(temp4)){
      #Note that we give more weight to polls closer to the election day
      running_average<-append(running_average,sum(temp4$dem_plus_minus[1:k])/k)
      weighted_running_average<-append(weighted_running_average,sum(temp4$dem_plus_minus[1:k] * (sort(temp4$days_till_election[1:k]))/sum(temp4$days_till_election[1:k])))
    }
    temp5<-data.frame(cbind(temp4,running_average,weighted_running_average))
    polls_altered<-data.frame(rbind(polls_altered,temp5))
  }
}
state_actuals<-read.csv('data_sets\\state_election_results.csv')
state_actuals_2000<-state_actuals[,c("State","X2000")]
state_actuals_2004<-state_actuals[,c("State","X2004")]
state_actuals_2008<-state_actuals[,c("State","X2008")]
state_actuals_2012<-state_actuals[,c("State","X2012")]
state_abb_temp<-data.frame(list("Washington DC","DC"))
names(state_abb_temp)<-c('name','abb')
state_abb<-data.frame(cbind(state.name,state.abb))
names(state_abb)<-c('name','abb')
state_abb<-rbind(state_abb,state_abb_temp)
polls_altered_2000<-sql("
  select
    pa.*
    ,sact.X2000 as actual
    ,case when sact.X2000='D' then 1 else 0 end as actual_binary_dem
  from polls_altered pa
    inner join state_abb sa on sa.abb=pa.State and pa.election_year=2000
    inner join state_actuals_2000 sact on sact.State=sa.name
")
polls_altered_2004<-sql("
  select
    pa.*
    ,sact.X2004 as actual
    ,case when sact.X2004='D' then 1 else 0 end as actual_binary_dem
  from polls_altered pa
    inner join state_abb sa on sa.abb=pa.State and pa.election_year=2004
    inner join state_actuals_2004 sact on sact.State=sa.name
")
polls_altered_2008<-sql("
  select
    pa.*
    ,sact.X2008 as actual
    ,case when sact.X2008='D' then 1 else 0 end as actual_binary_dem
  from polls_altered pa
    inner join state_abb sa on sa.abb=pa.State and pa.election_year=2008
    inner join state_actuals_2008 sact on sact.State=sa.name
")
polls_altered_2012<-sql("
  select
    pa.*
    ,sact.X2012 as actual
    ,case when sact.X2012='D' then 1 else 0 end as actual_binary_dem
  from polls_altered pa
    inner join state_abb sa on sa.abb=pa.State and pa.election_year=2012
    inner join state_actuals_2012 sact on sact.State=sa.name
")
polls_altered_2016<-sql("
  select
    pa.*
    ,'' as actual
    ,'' as actual_binary_dem
  from polls_altered pa
  where election_year=2016
")
polls_altered_2016$actual_binary_dem<-as.numeric(polls_altered_2016$actual_binary_dem)

polls_altered<-data.frame(rbind(polls_altered_2000,polls_altered_2004,polls_altered_2008,polls_altered_2012,polls_altered_2016))

states<-unique(polls_altered$State)
polls_altered_final<-data.frame()
#for(i in 1:length(states)){
#  state<-states[i]
  #temp<-polls_altered[polls_altered$State==state,]
  temp<-polls_altered
  
  #Fit the linear regression models to each state using unweighted polling
  fit_unweighted<-glm(actual_binary_dem~days_till_election+running_average,data=temp[temp$actual!='',],family=binomial())
  prob_unweighted_dem<-predict(fit_unweighted,temp,type='response')
  prob_unweighted_rep<-1-prob_unweighted_dem
  
  #Fit the linear regression models to each state using weighted polling
  fit_weighted<-glm(actual_binary_dem~days_till_election+weighted_running_average,data=temp[temp$actual!='',],family=binomial())
  prob_weighted_dem<-predict(fit_weighted,temp,type='response')
  prob_weighted_rep<-1-prob_weighted_dem
  
  temp2<-data.frame(cbind(temp,prob_unweighted_dem,prob_unweighted_rep,prob_weighted_dem,prob_weighted_rep))
  polls_altered_final<-data.frame(rbind(polls_altered_final,temp2))
#}
polls_altered_final<-melt(polls_altered_final,id=c('id','election_year','State','Date','days_till_election','dem_plus_minus',
                                   'running_average','weighted_running_average','actual','actual_binary_dem'))
names(polls_altered_final)[ncol(polls_altered_final)-1]<-'prediction'

###############################################################################################
#polls_altered_final gives us our final forecasts
###############################################################################################

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

write.csv(polls_altered_final,'forecasts\\polls_altered_final.csv',row.names = FALSE)

electoral_votes<-read.csv('data_sets\\electoral_votes.csv')
master_forecasts<-sql("
select distinct
  paf.*
  ,sa.name as state_full
  ,`Electoral.Votes` as state_electoral_votes
  ,`X.Location` as x_location
  ,`Y.Location` as y_location
  ,pollster
  ,candidate
  ,p.value as poll_value
  ,party
  ,`X2000` as `2000_state_result`
  ,`X2004` as `2004_state_result`
  ,`X2008` as `2008_state_result`
  ,`X2012` as `2012_state_result`
from polls_altered_final paf
  inner join polls p on paf.election_year=p.election_year
    and p.State=paf.State
    and (case when prediction in ('prob_unweighted_dem','prob_weighted_dem') then 'D'
              when prediction in ('prob_unweighted_rep','prob_weighted_rep') then 'R'
         end) = p.party
    and p.id=paf.id
  inner join state_abb sa on sa.abb=p.State
  inner join electoral_votes ev on ev.State=sa.name
  inner join state_actuals on state_actuals.State=sa.name
")

bla<-master_forecasts[master_forecasts$election_year==2016,]
bla<-bla[bla$prediction %in% c("prob_weighted_dem","prob_weighted_rep"),]
bla<-bla[!is.na(bla$Candidate),]
relevant_list<-sort(unique(bla$state_full))
relevant_list_abb<-unique(bla[order(bla$state_full),'State'])
poll_data<-bla[bla$state_full %in% unlist(relevant_list),]
poll_data$value<-100*as.numeric(poll_data$value)
poll_data<-sql("
select
  pd.*
  ,case when value>99 then '>99'
        when value<1 then '<1' 
        else floor(round(value,0)) end as poll_data_value_label 
from poll_data pd
")

plots<-vector('list', length(relevant_list))
for(i in 1:length(relevant_list)){
  state<-relevant_list[i]
  state_abbrev<-relevant_list_abb[i]
  if(nchar(state)>10){state_label<-state_abbrev}else{state_label<-state}
  
  poll_data_new<-poll_data[poll_data$state_full==state,]
  poll_temp<-poll_data_new[poll_data_new$final_prediction_ind==1,]
  #space_len<-23-(nchar(paste(as.character(poll_temp[which.max(poll_temp$value),'Candidate']),sub(" ", "",paste(poll_temp[which.max(poll_temp$value),'poll_data_value_label'],"%"),fixed=TRUE)))+nchar(as.character(poll_temp[which.max(poll_temp$value),'Candidate'])))-1
  
  plot<- ggplot(data=poll_data_new,aes(x=Date,y=value,colour=Candidate,group=Candidate)) + 
    geom_line(size=1.5) + 
    #theme(panel.grid.major = element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    ggtitle(
      paste(state_label," - ",as.character(poll_temp[which.max(poll_temp$value),'Candidate']),sub(" ", "",paste(poll_temp[which.max(poll_temp$value),'poll_data_value_label'],"%"),fixed=TRUE))
    )+
    #ggtitle(expression(atop(state, atop(italic("Location"), ""))))+
    #xlab("Increasing Reliability -->")+
    theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=19))+
    #geom_text(data = poll_data_new[poll_data_new$final_prediction_ind==1,], aes(colour = State, x = Inf, y = value), hjust = -.1)+
    theme(legend.position = "none")+
    scale_color_manual(values=c("deepskyblue", "firebrick1"))+
    #geom_text(data = poll_data_new[poll_data_new$final_prediction_ind==1,], aes(label = round(value,1),hjust=-.5))+
    #geom_text(data = poll_data_new[poll_data_new$final_prediction_ind==1,], aes(x=Date,y=value))+
    theme(axis.title.y=element_blank())+
    theme(axis.title.x=element_blank())
    #geom_dl(data = poll_data_new[poll_data_new$final_prediction_ind==1,],aes(label =  round(value,1)), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
    #theme(axis.title.x=element_text(size=14,colour="#535353",face="bold",vjust=-.5))  
    
  #print(plot)
  plots[[i]]<-plot
  
}


######################################################################
# MAP
######################################################################
bla<-master_forecasts[master_forecasts$election_year==2016,]
bla<-bla[bla$prediction %in% c("prob_weighted_dem","prob_weighted_rep"),]
poll_data<-bla[!is.na(bla$Candidate),]
relevant_list<-unique(bla$state_full)
poll_data$value<-100*as.numeric(poll_data$value)
poll_data<-poll_data[poll_data$final_prediction_ind==1,c("State","value","prediction")]
poll_data<-sql("
select
  State
  ,max(case when prediction='prob_weighted_rep' then value end)
    -max(case when prediction='prob_weighted_dem' then value end) as diff
from poll_data
group by 1
")
poll_data<-sql("
select
  sabb.name
  ,sabb.abb as iso3166_2
  ,X2000 as `2000_result`
  ,X2004 as `2004_result`
  ,X2008 as `2008_result`
  ,X2012 as `2012_result`
  ,`Electoral.Votes` as `electoral_votes`
  ,diff
from state_abb sabb
  left join poll_data pd on pd.State=sabb.abb
  left join state_actuals sact on sact.State=sabb.name
  left join electoral_votes ev on ev.State=sabb.name
")


us <- readOGR("C:\\Users\\Elliott\\Desktop\\Random Analysis\\2016_election_analysis\\map\\us_states_hexgrid.geojson", "OGRGeoJSON")

centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))

us_map <- fortify(us, region="iso3166_2")

ggplot(data=us_map, aes(map_id=id, x=long, y=lat)) + geom_map(map=us_map, color="black", fill="white")

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id),
                    color="white", size=0.5)
gg <- gg + geom_map(data=poll_data, map=us_map,
                    aes(fill=diff, map_id=iso3166_2))
gg <- gg + geom_map(data=poll_data, map=us_map,
                    aes(map_id=iso3166_2),
                    fill="#ffffff", alpha=0, color="white",
                    show.legend=FALSE)
gg <- gg + geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=6)
gg <- gg + scale_fill_distiller(palette="RdBu", na.value="gray56")
gg <- gg + coord_map()
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position="none")
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())

######################################################################
# MAP
######################################################################



######################################################################
# ELECTORAL SCENARIOS
######################################################################

bla<-master_forecasts[master_forecasts$election_year==2016,]
bla<-bla[bla$prediction %in% c("prob_weighted_dem","prob_weighted_rep"),]
poll_data<-bla[!is.na(bla$Candidate),]
relevant_list<-unique(bla$state_full)
poll_data$value<-as.numeric(poll_data$value)
poll_data<-poll_data[poll_data$final_prediction_ind==1,c("State","value","prediction","Candidate")]
poll_data<-sql("
select
  sabb.name
  ,sabb.abb as iso3166_2
  ,X2000 as `2000_result`
  ,X2004 as `2004_result`
  ,X2008 as `2008_result`
  ,X2012 as `2012_result`
  ,`Electoral.Votes` as `electoral_votes`
  ,value
  ,Candidate
from state_abb sabb
  left join poll_data pd on pd.State=sabb.abb
  left join state_actuals sact on sact.State=sabb.name
  left join electoral_votes ev on ev.State=sabb.name
")



#2000 Political Climate
climate2000<-sql("
select
  Candidate
  ,sum(electoral_votes * value) as exp_electoral_votes
from
(
  select
    name 
    ,case when value is null then 
      case when `2000_result`='R' then 'Trump' else 'Clinton' end
     else Candidate end as Candidate
    ,electoral_votes
    ,case when value is null then 1.000 else round(value,6) end as value
  from poll_data
) as temp1
group by Candidate
")
climate2000[which.max(climate2000$exp_electoral_votes),]
winner2000<-paste(climate2000[which.max(climate2000$exp_electoral_votes),'Candidate'],round(climate2000[which.max(climate2000$exp_electoral_votes),'exp_electoral_votes']))
loser2000<-paste(climate2000[which.max(-climate2000$exp_electoral_votes),'Candidate'],round(climate2000[which.max(-climate2000$exp_electoral_votes),'exp_electoral_votes']))
outcome_2000_climate<-paste(winner2000, '-',loser2000)
margin2000<-round(climate2000[which.max(climate2000$exp_electoral_votes),'exp_electoral_votes'])-round(climate2000[which.max(-climate2000$exp_electoral_votes),'exp_electoral_votes'])
margin2000<-paste(climate2000[which.max(climate2000$exp_electoral_votes),'Candidate'],gsub(" ","",paste('+',margin2000)))

#2004 Political Climate
climate2004<-sql("
select
  Candidate
  ,sum(electoral_votes * value) as exp_electoral_votes
from
(
  select
    name 
    ,case when value is null then 
      case when `2004_result`='R' then 'Trump' else 'Clinton' end
     else Candidate end as Candidate
    ,electoral_votes
    ,case when value is null then 1.000 else round(value,6) end as value
  from poll_data
) as temp1
group by Candidate
")
climate2004[which.max(climate2004$exp_electoral_votes),]
winner2004<-paste(climate2004[which.max(climate2004$exp_electoral_votes),'Candidate'],round(climate2004[which.max(climate2004$exp_electoral_votes),'exp_electoral_votes']))
loser2004<-paste(climate2004[which.max(-climate2004$exp_electoral_votes),'Candidate'],round(climate2004[which.max(-climate2004$exp_electoral_votes),'exp_electoral_votes']))
outcome_2004_climate<-paste(winner2004, '-',loser2004)
margin2004<-round(climate2004[which.max(climate2004$exp_electoral_votes),'exp_electoral_votes'])-round(climate2004[which.max(-climate2004$exp_electoral_votes),'exp_electoral_votes'])
margin2004<-paste(climate2004[which.max(climate2004$exp_electoral_votes),'Candidate'],gsub(" ","",paste('+',margin2004)))

#2008 Political Climate
climate2008<-sql("
select
  Candidate
  ,sum(electoral_votes * value) as exp_electoral_votes
from
(
  select
    name 
    ,case when value is null then 
      case when `2008_result`='R' then 'Trump' else 'Clinton' end
     else Candidate end as Candidate
    ,electoral_votes
    ,case when value is null then 1.000 else round(value,6) end as value
  from poll_data
) as temp1
group by Candidate
")
climate2008[which.max(climate2008$exp_electoral_votes),]
winner2008<-paste(climate2008[which.max(climate2008$exp_electoral_votes),'Candidate'],round(climate2008[which.max(climate2008$exp_electoral_votes),'exp_electoral_votes']))
loser2008<-paste(climate2008[which.max(-climate2008$exp_electoral_votes),'Candidate'],round(climate2008[which.max(-climate2008$exp_electoral_votes),'exp_electoral_votes']))
outcome_2008_climate<-paste(winner2008, '-',loser2008)
margin2008<-round(climate2008[which.max(climate2008$exp_electoral_votes),'exp_electoral_votes'])-round(climate2008[which.max(-climate2008$exp_electoral_votes),'exp_electoral_votes'])
margin2008<-paste(climate2008[which.max(climate2008$exp_electoral_votes),'Candidate'],gsub(" ","",paste('+',margin2008)))
    
    
#2012 Political Climate
climate2012<-sql("
select
  Candidate
  ,sum(electoral_votes * value) as exp_electoral_votes
from
(
  select
    name 
    ,case when value is null then 
      case when `2012_result`='R' then 'Trump' else 'Clinton' end
     else Candidate end as Candidate
    ,electoral_votes
    ,case when value is null then 1.000 else round(value,6) end as value
  from poll_data
) as temp1
group by Candidate
")
climate2012[which.max(climate2012$exp_electoral_votes),]
winner2012<-paste(climate2012[which.max(climate2012$exp_electoral_votes),'Candidate'],round(climate2012[which.max(climate2012$exp_electoral_votes),'exp_electoral_votes']))
loser2012<-paste(climate2012[which.max(-climate2012$exp_electoral_votes),'Candidate'],round(climate2012[which.max(-climate2012$exp_electoral_votes),'exp_electoral_votes']))
outcome_2012_climate<-paste(winner2012, '-',loser2012)
margin2012<-round(climate2012[which.max(climate2012$exp_electoral_votes),'exp_electoral_votes'])-round(climate2012[which.max(-climate2012$exp_electoral_votes),'exp_electoral_votes'])
margin2012<-paste(climate2012[which.max(climate2012$exp_electoral_votes),'Candidate'],gsub(" ","",paste('+',margin2012)))
    

######################################################################
# ELECTORAL SCENARIOS
######################################################################


# 
# ######################################################################
# # INCLDE DUMMY DATA FOR STATES THAT HAVENT BEEN POLLED YET
# ######################################################################
# year_df<-data.frame(t(data.frame(list(2004,2008,2012,2016))))
# names(year_df)<-'year'
# rownames(year_df)<-NULL
# 
# model_df<-data.frame(t(data.frame(list("prob_unweighted_dem","prob_unweighted_rep","prob_weighted_dem","prob_weighted_rep"))))
# names(model_df)<-'model'
# rownames(model_df)<-NULL
# 
# states_df<-data.frame(data.frame(list(electoral_votes$State)))
# names(states_df)<-'state'
# rownames(states_df)<-NULL
# 
# dummy<-sql("
# select distinct  
#   NULL as id
#   ,y.year as election_year
#   ,sa.abb as State
#   ,NULL as Date 
#   ,NULL as days_till_election 
#   ,NULL as dem_plus_minus 
#   ,NULL as running_average 
#   ,NULL as weighted_running_average 
#   ,NULL as actual
#   ,NULL as actual_binary_dem 
#   ,m.model as prediction
#   ,NULL as value
#   ,NULL as final_prediction_ind
#   ,s.state as state_full
#   ,`Electoral.Votes` as state_electoral_votes
#   ,`X.Location` as x_location
#   ,`Y.Location` as y_location
#   ,NULL as Pollster
#   ,NULL as Candidate
#   ,NULL as poll_value
#   ,NULL as party
#   ,`X2004` as `2004_state_result`
#   ,`X2008` as `2008_state_result`
#   ,`X2012` as `2012_state_result`
# from year_df y
#   inner join model_df m on 1=1
#   inner join states_df s on 1=1
#   inner join state_abb sa on sa.name=s.state
#   inner join electoral_votes ev on ev.State=s.state
#   inner join state_actuals on state_actuals.State=sa.name
# ")
# 
# master_forecasts<-data.frame(rbind(master_forecasts,dummy))
# ######################################################################
# # INCLDE DUMMY DATA FOR STATES THAT HAVENT BEEN POLLED YET
# ######################################################################
# 
# 
# write.csv(master_forecasts,'forecasts\\master_forecasts.csv',row.names = FALSE)








#This is the file to prep and clean data, as well as create custom functions

#Functions created: multiplot, sql

#Data sets created: 
#   1. master_state_ref
#   2. polls_2000
#   3. polls_2004
#   4. polls_2008
#   5. polls_2012
#   6. polls_2016
#   7. polls

#   8. nat_polls_2004
#   9. nat_polls_2008
#   10. nat_polls_2012
#   11. nat_polls_2016
#   12. nat_polls

##########################################################################################
# LIBRARIES AND CUSTOM FUNCTIONS
##########################################################################################
library("sqldf", lib.loc="~/R/win-library/3.3")
library("reshape2", lib.loc="~/R/win-library/3.3")
library("pollstR", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")
library("stringr", lib.loc="~/R/win-library/3.3")
library("plyr", lib.loc="~/R/win-library/3.3")
library("arm", lib.loc="~/R/win-library/3.3")
library("FNN", lib.loc="~/R/win-library/3.3")
library("fGarch", lib.loc="~/R/win-library/3.3")
library("statebins", lib.loc="~/R/win-library/3.3")
library("scales", lib.loc="~/R/win-library/3.3")
library("condMVNorm", lib.loc="~/R/win-library/3.3")
library("RCurl", lib.loc="~/R/win-library/3.3")

getMargin<-function(known_state,known_margin,unknown_state){
  corr<-data.frame(
    as.numeric((t(master_state_ref[master_state_ref$abb==known_state,]))[12:15,])
    ,as.numeric((t(master_state_ref[master_state_ref$ab==unknown_state,]))[12:15,])
    )     
  names(corr)<-c(known_state,unknown_state)
  cor_calc<-cor(corr)[1,2] 
  
  updated_mean<-state_odds_rand[state_odds_rand$abb==unknown_state,'mean'] + 
    cor_calc*(state_odds_rand[state_odds_rand$abb==unknown_state,'sd']/state_odds_rand[state_odds_rand$abb==known_state,'sd'])*
    (known_margin-state_odds_rand[state_odds_rand$abb==known_state,'mean'])
  updated_sd<-sqrt(state_odds_rand[state_odds_rand$abb==unknown_state,'sd']^2*(1-cor_calc^2))
  margin<-rnorm(1,updated_mean,updated_sd)
  if(margin>0){win_or_lose<-1}else{win_or_lose<-0}  
  return(c(margin,win_or_lose))
}

expChoose<-function(num,exp){
  return(num^exp)
}

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

# Function for writing SQL in R quickly
sql<-function(str){
  sqldf()
  ret<-sqldf(str,drv="SQLite")
  sqldf()
  return(ret)
}
##########################################################################################
# LIBRARIES AND CUSTOM FUNCTIONS
##########################################################################################


##########################################################################################
# DATA SETS
##########################################################################################
# Load and clean state history data set
state_election_results<-read.csv('data_sets\\state_election_results.csv')
names(state_election_results)<-c("state","1976","1980","1984","1988","1992","1996","2000","2004","2008","2012","2000_dem_margin","2004_dem_margin","2008_dem_margin","2012_dem_margin")

# Load and clean state abbreviations data set
state_abbreviations_temp<-data.frame(list("Washington DC","DC"))
names(state_abbreviations_temp)<-c('state','abb')
state_abbreviations<-data.frame(cbind(state.name,state.abb))
names(state_abbreviations)<-c('state','abb')
state_abbreviations<-rbind(state_abbreviations,state_abbreviations_temp)

# Load and clean state electoral votes data set
# Includes map coordinates for future map use 
electoral_votes<-read.csv('data_sets\\electoral_votes.csv')
names(electoral_votes)<-c("state","electoral_votes","x_location","y_location")

# Create Master State data frame
temp1<-sql("
select
  ser.*
  ,sa.abb
  ,ev.electoral_votes
  ,ev.x_location
  ,ev.y_location
from state_election_results ser 
  inner join state_abbreviations sa using (state)
  inner join electoral_votes ev using (state)
")

temp2<-sql("
select
  state 
  ,case when num_dem_victories=0 then round(round(1,10)/round(11,10),4)
        when num_dem_victories=10 then round(round(10,10)/round(11,10),4)
  else round(round(num_dem_victories,10)/round(10,10),4) end hist_dem_prob
  ,case when num_dem_victories_2012=0 then round(round(1,10)/round(10,10),4)
        when num_dem_victories_2012=9 then round(round(9,10)/round(10,10),4)
  else round(round(num_dem_victories_2012,10)/round(9,10),4) end hist_dem_prob_2012
  ,case when num_dem_victories_2008=0 then round(round(1,10)/round(9,10),4)
        when num_dem_victories_2008=8 then round(round(8,10)/round(9,10),4)
  else round(round(num_dem_victories_2008,10)/round(8,10),4) end hist_dem_prob_2008
  ,case when num_dem_victories_2004=0 then round(round(1,10)/round(8,10),4)
        when num_dem_victories_2004=7 then round(round(7,10)/round(8,10),4)
  else round(round(num_dem_victories_2004,10)/round(7,10),4) end hist_dem_prob_2004
  ,case when num_dem_victories_2000=0 then round(round(1,10)/round(7,10),4)
        when num_dem_victories_2000=6 then round(round(6,10)/round(7,10),4)
  else round(round(num_dem_victories_2000,10)/round(6,10),4) end hist_dem_prob_2000

from
(
  select
    ser.state
    ,sum(case when `1976`='D' then 1 else 0 end
      +case when `1980`='D' then 1 else 0 end
      +case when `1984`='D' then 1 else 0 end
      +case when `1988`='D' then 1 else 0 end
      +case when `1992`='D' then 1 else 0 end
      +case when `1996`='D' then 1 else 0 end
      +case when `2000`='D' then 1 else 0 end
      +case when `2004`='D' then 1 else 0 end
      +case when `2008`='D' then 1 else 0 end
      +case when `2012`='D' then 1 else 0 end) as num_dem_victories
    ,sum(case when `1976`='D' then 1 else 0 end
      +case when `1980`='D' then 1 else 0 end
      +case when `1984`='D' then 1 else 0 end
      +case when `1988`='D' then 1 else 0 end
      +case when `1992`='D' then 1 else 0 end
      +case when `1996`='D' then 1 else 0 end
      +case when `2000`='D' then 1 else 0 end
      +case when `2004`='D' then 1 else 0 end
      +case when `2008`='D' then 1 else 0 end
      ) as num_dem_victories_2012
    ,sum(case when `1976`='D' then 1 else 0 end
      +case when `1980`='D' then 1 else 0 end
      +case when `1984`='D' then 1 else 0 end
      +case when `1988`='D' then 1 else 0 end
      +case when `1992`='D' then 1 else 0 end
      +case when `1996`='D' then 1 else 0 end
      +case when `2000`='D' then 1 else 0 end
      +case when `2004`='D' then 1 else 0 end
      ) as num_dem_victories_2008
    ,sum(case when `1976`='D' then 1 else 0 end
      +case when `1980`='D' then 1 else 0 end
      +case when `1984`='D' then 1 else 0 end
      +case when `1988`='D' then 1 else 0 end
      +case when `1992`='D' then 1 else 0 end
      +case when `1996`='D' then 1 else 0 end
      +case when `2000`='D' then 1 else 0 end
      ) as num_dem_victories_2004
    ,sum(case when `1976`='D' then 1 else 0 end
      +case when `1980`='D' then 1 else 0 end
      +case when `1984`='D' then 1 else 0 end
      +case when `1988`='D' then 1 else 0 end
      +case when `1992`='D' then 1 else 0 end
      +case when `1996`='D' then 1 else 0 end
      ) as num_dem_victories_2000
  from state_election_results ser 
  group by 1
)
")

master_state_ref<-
sql("
select
  temp1.*
  ,temp2.hist_dem_prob
  ,temp2.hist_dem_prob_2012
  ,temp2.hist_dem_prob_2008
  ,temp2.hist_dem_prob_2004
  ,temp2.hist_dem_prob_2000
from temp1 inner join temp2 using (state)
")
##########################################################################################
# DATA SETS
##########################################################################################

##########################################################################################
# POLLING PREP
##########################################################################################

###########################
# 2004 Polling
###########################
#Obtained from http://www.electoral-vote.com/evp2012/Info/datagalore.html
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
polls_2004$days_till_election<-as.numeric(polls_2004$days_till_election)
polls_2004<-sql("
select
  election_year
  ,State
  ,Date 
  ,Candidate
  ,party
  ,days_till_election
  ,min(id) as id
  ,avg(value) as value
from polls_2004
  group by 1,2,3,4,5,6
")
polls_2004<-polls_2004[polls_2004$Date>=as.Date('2004-05-01'),]
polls_2004$Date<-as.Date(polls_2004$Date)
###########################
# 2004 Polling
###########################

###########################
# 2008 Polling
###########################
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
polls_2008$days_till_election<-as.numeric(polls_2008$days_till_election)
polls_2008<-sql("
select
  election_year
  ,State
  ,Date 
  ,Candidate
  ,party
  ,days_till_election
  ,min(id) as id
  ,avg(value) as value
from polls_2008
  group by 1,2,3,4,5,6
")
polls_2008<-polls_2008[polls_2008$Date>=as.Date('2008-05-01'),]
polls_2008$Date<-as.Date(polls_2008$Date)
###########################
# 2008 Polling
###########################

###########################
# 2000 Polling
###########################
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
polls_2000$days_till_election<-as.numeric(polls_2000$days_till_election)
polls_2000<-sql("
select
  election_year
  ,State
  ,Date 
  ,Candidate
  ,party
  ,days_till_election
  ,min(id) as id
  ,avg(value) as value
from polls_2000
  group by 1,2,3,4,5,6
")
polls_2000<-polls_2000[polls_2000$Date>=as.Date('2000-05-01'),]
polls_2000$Date<-as.Date(polls_2000$Date)
###########################
# 2000 Polling
###########################

###########################
# 2012 Polling
###########################
#Used HuffPollster API to get the .csv

polls_2012<-read.csv("polls\\polls_2012.csv")
polls_2012$Date<-as.Date(polls_2012$Date)
polls_2012<-polls_2012[polls_2012$Date>=as.Date('2012-05-01'),]
polls_2012$Date<-as.Date(polls_2012$Date)
polls_2012$election_year<-as.character(polls_2012$election_year)
###########################
# 2012 Polling
###########################

###########################
# 2016 Polling
###########################
#Use HuffPollster API
polls_2016<-data.frame()
state_names_original<-state.name
state_names_abb<-state.abb
state_names_nice<-tolower(state.name)
state_names_nice<-gsub( " ", "-", state_names_nice)
for(i in 1:length(state_names_abb)){
  print(noquote(paste("CURRENTLY GETTING STATE:",state_names_abb[i])))
  url<-paste0('http://elections.huffingtonpost.com/pollster/2016-',state_names_nice[i],'-president-trump-vs-clinton.csv')
  url_alt<-paste0('http://elections.huffingtonpost.com/pollster/2016-',state_names_nice[i],'-presidential-general-election-trump-vs-clinton.csv')
  file<-getURL(url,ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  file_alt<-getURL(url_alt,ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  temp1<-read.csv(textConnection(file),head=T)
  temp1_alt<-read.csv(textConnection(file_alt),head=T)
  
  if(ncol(temp1)==1 && ncol(temp1_alt)==1){
    #No polls found for this state, skip
    next
  }else if(ncol(temp1)==1 && ncol(temp1_alt)>1){
    #Found polls, temp1_alt is the one we want
    temp2<-temp1_alt
  }else{
    #Found polls, temp1 is the one we want
    temp2<-temp1
  }
  id<-as.numeric(str_sub(temp2$Pollster.URL,-5,-1))
  temp2<-temp2[,c('Pollster','Start.Date','End.Date',"Clinton",'Trump')]
  temp2$id<-id
  temp2<-melt(temp2,id=c("Pollster","Start.Date","End.Date","id"))
  temp2$State<-state_names_abb[i]
  
  temp2<-sql("
    select
      2016 as election_year
      ,State
      ,`End.Date` as Date 
      ,variable as Candidate
      ,case when variable='Clinton' then 'D' else 'R' end as party
      ,id
      ,value
    from temp2
    order by id
  ")

  polls_2016<-rbind(polls_2016,temp2)
}

polls_2016<-data.frame(cbind(polls_2016,round(difftime(as.Date('2016-11-08'),polls_2016$Date,units="days"))))
names(polls_2016)[ncol(polls_2016)]<-'days_till_election'
polls_2016$days_till_election<-as.numeric(polls_2016$days_till_election)
polls_2016<-sql("
    select
      election_year
      ,State
      ,Date 
      ,Candidate
      ,party
      ,days_till_election
      ,min(id) as id
      ,round(avg(value)) as value
    from polls_2016
    group by 1,2,3,4,5,6
")
polls_2016<-polls_2016[polls_2016$Date>=as.Date('2016-05-01'),]
polls_2016$election_year<-as.character(polls_2016$election_year)
polls_2016$Date<-as.Date(polls_2016$Date)
write.csv(polls_2016,'polls\\polls_2016.csv',row.names = FALSE)
###########################
# 2016 Polling
###########################

###########################
# Create data frame of all the polls
###########################
polls<-data.frame(rbind(polls_2000,polls_2004,polls_2008,polls_2012,polls_2016))

polls$days_till_election<-as.numeric(polls$days_till_election)

polls$days_till_election<-as.numeric(polls$days_till_election)
polls<-sql("
select
  election_year
  ,State
  ,Date 
  ,Candidate
  ,case when Candidate in ('Clinton','Obama','Kerry','Gore') then 'D' else 'R' end as party
  ,days_till_election
  ,min(id) as id
  ,avg(value) as value
from polls
  group by 1,2,3,4,5,6
")

#Write all polls to a master csv file
write.csv(polls,'polls\\polls_total.csv',row.names = FALSE)
###########################
# Create data frame of all the polls
###########################




###################################
# Get Rid of States without sufficient polling (<5 polls)
###################################



###################################
# Get Rid of States without sufficient polling (<5 polls)
###################################
polls<-sql("
select
*
from polls
where not 
(State in
(select State from
  (
    select
      State 
      ,election_year
      ,count(distinct id) as num
    from polls p
    group by 1,2
    having (num between 1 and 2) and election_year=2016
  )
)
and election_year=2016)
")




###################################
# Fix some polls not picked up by huffpollster
###################################

#huffpollster didn't include a poll with third party candidates included

#PA POLL http://www.qu.edu/news-and-events/quinnipiac-university-poll/2016-presidential-swing-state-polls/release-detail?ReleaseID=2365
polls[polls$id==24871,'value']<-c(34,40)

#FL POLL http://www.qu.edu/news-and-events/quinnipiac-university-poll/2016-presidential-swing-state-polls/release-detail?ReleaseID=2365
polls[polls$id==24869,'value']<-c(36,41)

#OH POLL http://www.qu.edu/news-and-events/quinnipiac-university-poll/2016-presidential-swing-state-polls/release-detail?ReleaseID=2365
polls[polls$id==24870,'value']<-c(36,37)

#This poll lacks a clinton value:
#24941          2016    PA 2016-06-28                                                      Gravis Marketing/OANN     Trump    47     R


#This poll also lacks a clinton value:
#24940          2016    OH 2016-06-28                                                      Gravis Marketing/OANN     Trump    47     R


polls<-sql("select distinct * from polls")

polls$State<-as.factor(polls$State)
polls$Date<-as.Date(polls$Date)
polls$Candidate<-as.character(polls$Candidate)
polls$party<-as.character(polls$party)
polls$days_till_election<-as.numeric(polls$days_till_election)
polls$id<-as.numeric(polls$id)
polls$value<-as.numeric(polls$value)



##################################################################################################################################
#PREP THE NATIONAL POLLS
##################################################################################################################################

#################################################
# ELECTION 2016
#################################################
print("GETTING NATIONAL POLLS",quote=FALSE)
#Get the current 2016 national polls
#These are the polls that include Gary Johnson
url<-'http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton.csv'
file<-getURL(url,ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
nat_polls_2016_temp<-read.csv(textConnection(file),head=T)
nat_polls_2016_temp<-sql("
  select
    2016 as election_year
    ,Pollster
    ,`End.Date` as Date
    ,case when lower(`Question.Text`) not like '%gary johnson%' then 1 else 0 end as lib_ind
    ,max(`Number.of.Observations`) as num_observations
    ,Clinton
    ,Trump
  from nat_polls_2016_temp npt
  group by 1,2,3,4
  order by Date desc
")
nat_polls_2016_temp$dem_plus_minus<-nat_polls_2016_temp$Clinton-nat_polls_2016_temp$Trump
nat_polls_2016<-sql("
  select
    election_year
    ,Date 
    ,round(avg(dem_plus_minus)) as dem_plus_minus
  from nat_polls_2016_temp
  group by 1,2
  order by Date asc
")
running_avg<-c()
for(i in 1:nrow(nat_polls_2016)){
  if(i<=5){
    running_avg<-append(running_avg,mean(nat_polls_2016$dem_plus_minus[1:i]))
  }else{
    running_avg<-append(running_avg,mean(nat_polls_2016$dem_plus_minus[(i-4):i]))
  }
}
nat_polls_2016$running_avg<-running_avg
nat_polls_2016$Date<-as.Date(nat_polls_2016$Date)
date_ref_2016<-data.frame(seq(min(as.Date(polls_2016[,'Date'])), Sys.Date(), "days"))
names(date_ref_2016)<-'date'
nat_polls_2016<-sql("
select
  dr.date as Date 
  ,2016 as election_year
  ,dem_plus_minus
  ,running_avg
from date_ref_2016 dr 
  left join nat_polls_2016 np on np.date=dr.Date
")
nat_polls_2016<-data.frame(cbind(nat_polls_2016,round(difftime(as.Date('2016-11-08'),nat_polls_2016$Date,units="days"))))
names(nat_polls_2016)[ncol(nat_polls_2016)]<-'days_till_election'
nat_polls_2016$days_till_election<-as.numeric(nat_polls_2016$days_till_election)
running_avg_diff<-c()
for(i in 1:nrow(nat_polls_2016)){
  if(i==1 && is.na(nat_polls_2016$running_avg[i])==TRUE){
    nat_polls_2016$running_avg[i]<-nat_polls_2016[is.na(nat_polls_2016$running_avg)==FALSE,'running_avg'][1]
    running_avg_diff<-append(running_avg_diff,0)      
  }
  else if(i!=1 && is.na(nat_polls_2016$running_avg[i])==TRUE){
    nat_polls_2016$running_avg[i]<-nat_polls_2016$running_avg[i-1]
    running_avg_diff<-append(running_avg_diff,nat_polls_2016$running_avg[i]-nat_polls_2016$running_avg[i-1])
  }
  else if(i==1 && is.na(nat_polls_2016$running_avg[i])==FALSE){
    running_avg_diff<-append(running_avg_diff,0)    
  }else{
    running_avg_diff<-append(running_avg_diff,nat_polls_2016$running_avg[i]-nat_polls_2016$running_avg[i-1])
  }
}
nat_polls_2016$running_avg_diff<-running_avg_diff
#################################################
# ELECTION 2016
#################################################




#################################################
# ELECTION 2000
#################################################
nat_polls_2000<-read.csv('national_polls\\national_polls_2000.csv')
nat_polls_2000$Date<-as.Date(nat_polls_2000$Date,format='%m/%d/%Y')
nat_polls_2000$election_year<-2000
nat_polls_2000<-sql("
  select
    election_year
    ,Date 
    ,round(avg(Gore-Bush)) as dem_plus_minus
  from nat_polls_2000
  group by 1,2
  order by Date asc
")
#Calculate last 5 polls running avg
running_avg<-c()
for(i in 1:nrow(nat_polls_2000)){
  if(i<=5){
    running_avg<-append(running_avg,mean(nat_polls_2000$dem_plus_minus[1:i]))
  }else{
    running_avg<-append(running_avg,mean(nat_polls_2000$dem_plus_minus[(i-4):i]))
  }
}
nat_polls_2000$running_avg<-running_avg
date_ref_2000<-data.frame(seq(min(polls_2000[,'Date']), max(polls_2000[,'Date']), "days"))
names(date_ref_2000)<-'date'
nat_polls_2000<-sql("
select
  dr.date as Date 
  ,2000 as election_year
  ,dem_plus_minus
  ,running_avg
from date_ref_2000 dr 
  left join nat_polls_2000 np on np.date=dr.Date
")
nat_polls_2000<-data.frame(cbind(nat_polls_2000,round(difftime(as.Date('2000-11-07'),nat_polls_2000$Date,units="days"))))
names(nat_polls_2000)[ncol(nat_polls_2000)]<-'days_till_election'
nat_polls_2000$days_till_election<-as.numeric(nat_polls_2000$days_till_election)
running_avg_diff<-c()
for(i in 1:nrow(nat_polls_2000)){
  if(i==1 && is.na(nat_polls_2000$running_avg[i])==TRUE){
    nat_polls_2000$running_avg[i]<-nat_polls_2000[is.na(nat_polls_2000$running_avg)==FALSE,'running_avg'][1]
    running_avg_diff<-append(running_avg_diff,0)      
  }
  else if(i!=1 && is.na(nat_polls_2000$running_avg[i])==TRUE){
    nat_polls_2000$running_avg[i]<-nat_polls_2000$running_avg[i-1]
    running_avg_diff<-append(running_avg_diff,nat_polls_2000$running_avg[i]-nat_polls_2000$running_avg[i-1])
  }
  else if(i==1 && is.na(nat_polls_2000$running_avg[i])==FALSE){
    running_avg_diff<-append(running_avg_diff,0)    
  }else{
    running_avg_diff<-append(running_avg_diff,nat_polls_2000$running_avg[i]-nat_polls_2000$running_avg[i-1])
  }
}
nat_polls_2000$running_avg_diff<-running_avg_diff
#################################################
# ELECTION 2000
#################################################


#################################################
# ELECTION 2004
#################################################
nat_polls_2004<-read.csv('national_polls\\national_polls_2004.csv')
nat_polls_2004$Date<-as.Date(nat_polls_2004$Date,format='%m/%d/%Y')
nat_polls_2004$election_year<-2004
nat_polls_2004<-sql("
  select
    election_year
    ,Date 
    ,round(avg(Kerry-Bush)) as dem_plus_minus
  from nat_polls_2004
  group by 1,2
  order by Date asc
")
running_avg<-c()
for(i in 1:nrow(nat_polls_2004)){
  if(i<=5){
    running_avg<-append(running_avg,mean(nat_polls_2004$dem_plus_minus[1:i]))
  }else{
    running_avg<-append(running_avg,mean(nat_polls_2004$dem_plus_minus[(i-4):i]))
  }
}
nat_polls_2004$running_avg<-running_avg
date_ref_2004<-data.frame(seq(min(polls_2004[,'Date']), max(polls_2004[,'Date']), "days"))
names(date_ref_2004)<-'date'
nat_polls_2004<-sql("
select
  dr.date as Date 
  ,2004 as election_year
  ,dem_plus_minus
  ,running_avg
from date_ref_2004 dr 
  left join nat_polls_2004 np on np.date=dr.Date
")
nat_polls_2004<-data.frame(cbind(nat_polls_2004,round(difftime(as.Date('2004-11-02'),nat_polls_2004$Date,units="days"))))
names(nat_polls_2004)[ncol(nat_polls_2004)]<-'days_till_election'
nat_polls_2004$days_till_election<-as.numeric(nat_polls_2004$days_till_election)
running_avg_diff<-c()
for(i in 1:nrow(nat_polls_2004)){
  if(i==1 && is.na(nat_polls_2004$running_avg[i])==TRUE){
    nat_polls_2004$running_avg[i]<-nat_polls_2004[is.na(nat_polls_2004$running_avg)==FALSE,'running_avg'][1]
    running_avg_diff<-append(running_avg_diff,0)      
  }
  else if(i!=1 && is.na(nat_polls_2004$running_avg[i])==TRUE){
    nat_polls_2004$running_avg[i]<-nat_polls_2004$running_avg[i-1]
    running_avg_diff<-append(running_avg_diff,nat_polls_2004$running_avg[i]-nat_polls_2004$running_avg[i-1])
  }
  else if(i==1 && is.na(nat_polls_2004$running_avg[i])==FALSE){
    running_avg_diff<-append(running_avg_diff,0)    
  }else{
    running_avg_diff<-append(running_avg_diff,nat_polls_2004$running_avg[i]-nat_polls_2004$running_avg[i-1])
  }
}
nat_polls_2004$running_avg_diff<-running_avg_diff
#################################################
# ELECTION 2004
#################################################

#################################################
# ELECTION 2008
#################################################
nat_polls_2008<-read.csv('national_polls\\national_polls_2008.csv')
nat_polls_2008$Date<-as.Date(nat_polls_2008$Date)
nat_polls_2008$election_year<-2008
nat_polls_2008<-sql("
  select
    election_year
    ,Date 
    ,round(avg(Obama-McCain)) as dem_plus_minus
  from nat_polls_2008
  group by 1,2
  order by Date asc
")
running_avg<-c()
for(i in 1:nrow(nat_polls_2008)){
  if(i<=5){
    running_avg<-append(running_avg,mean(nat_polls_2008$dem_plus_minus[1:i]))
  }else{
    running_avg<-append(running_avg,mean(nat_polls_2008$dem_plus_minus[(i-4):i]))
  }
}
nat_polls_2008$running_avg<-running_avg
date_ref_2008<-data.frame(seq(min(polls_2008[,'Date']), max(polls_2008[,'Date']), "days"))
names(date_ref_2008)<-'date'
nat_polls_2008<-sql("
select
  dr.date as Date 
  ,2008 as election_year
  ,dem_plus_minus
  ,running_avg
from date_ref_2008 dr 
  left join nat_polls_2008 np on np.date=dr.Date
")
nat_polls_2008<-data.frame(cbind(nat_polls_2008,round(difftime(as.Date('2008-11-04'),nat_polls_2008$Date,units="days"))))
names(nat_polls_2008)[ncol(nat_polls_2008)]<-'days_till_election'
nat_polls_2008$days_till_election<-as.numeric(nat_polls_2008$days_till_election)
running_avg_diff<-c()
for(i in 1:nrow(nat_polls_2008)){
  if(i==1 && is.na(nat_polls_2008$running_avg[i])==TRUE){
    nat_polls_2008$running_avg[i]<-nat_polls_2008[is.na(nat_polls_2008$running_avg)==FALSE,'running_avg'][1]
    running_avg_diff<-append(running_avg_diff,0)      
  }
  else if(i!=1 && is.na(nat_polls_2008$running_avg[i])==TRUE){
    nat_polls_2008$running_avg[i]<-nat_polls_2008$running_avg[i-1]
    running_avg_diff<-append(running_avg_diff,nat_polls_2008$running_avg[i]-nat_polls_2008$running_avg[i-1])
  }
  else if(i==1 && is.na(nat_polls_2008$running_avg[i])==FALSE){
    running_avg_diff<-append(running_avg_diff,0)    
  }else{
    running_avg_diff<-append(running_avg_diff,nat_polls_2008$running_avg[i]-nat_polls_2008$running_avg[i-1])
  }
}
nat_polls_2008$running_avg_diff<-running_avg_diff
#################################################
# ELECTION 2008
#################################################

#################################################
# ELECTION 2012
#################################################
nat_polls_2012<-read.csv('national_polls\\national_polls_2012.csv')
nat_polls_2012$Date<-as.Date(nat_polls_2012$Date,format='%m/%d/%Y')
nat_polls_2012$election_year<-2012
nat_polls_2012<-sql("
  select
    election_year
    ,Date 
    ,round(avg(Obama-Romney)) as dem_plus_minus
  from nat_polls_2012
  group by 1,2
  order by Date asc
")
running_avg<-c()
for(i in 1:nrow(nat_polls_2012)){
  if(i<=5){
    running_avg<-append(running_avg,mean(nat_polls_2012$dem_plus_minus[1:i]))
  }else{
    running_avg<-append(running_avg,mean(nat_polls_2012$dem_plus_minus[(i-4):i]))
  }
}
nat_polls_2012$running_avg<-running_avg
date_ref_2012<-data.frame(seq(min(as.Date(polls_2012[,'Date'])), max(as.Date(polls_2012[,'Date'])), "days"))
names(date_ref_2012)<-'date'
nat_polls_2012<-sql("
select
  dr.date as Date 
  ,2012 as election_year
  ,dem_plus_minus
  ,running_avg
from date_ref_2012 dr 
  left join nat_polls_2012 np on np.date=dr.Date
")
nat_polls_2012<-data.frame(cbind(nat_polls_2012,round(difftime(as.Date('2012-11-06'),nat_polls_2012$Date,units="days"))))
names(nat_polls_2012)[ncol(nat_polls_2012)]<-'days_till_election'
nat_polls_2012$days_till_election<-as.numeric(nat_polls_2012$days_till_election)
running_avg_diff<-c()
for(i in 1:nrow(nat_polls_2012)){
  if(i==1 && is.na(nat_polls_2012$running_avg[i])==TRUE){
    nat_polls_2012$running_avg[i]<-nat_polls_2012[is.na(nat_polls_2012$running_avg)==FALSE,'running_avg'][1]
    running_avg_diff<-append(running_avg_diff,0)      
  }
  else if(i!=1 && is.na(nat_polls_2012$running_avg[i])==TRUE){
    nat_polls_2012$running_avg[i]<-nat_polls_2012$running_avg[i-1]
    running_avg_diff<-append(running_avg_diff,nat_polls_2012$running_avg[i]-nat_polls_2012$running_avg[i-1])
  }
  else if(i==1 && is.na(nat_polls_2012$running_avg[i])==FALSE){
    running_avg_diff<-append(running_avg_diff,0)    
  }else{
    running_avg_diff<-append(running_avg_diff,nat_polls_2012$running_avg[i]-nat_polls_2012$running_avg[i-1])
  }
}
nat_polls_2012$running_avg_diff<-running_avg_diff
#################################################
# ELECTION 2012
#################################################



nat_polls<-data.frame(rbind(nat_polls_2000,nat_polls_2004,nat_polls_2008,nat_polls_2012,nat_polls_2016))


######################################################################################################
#CREATE WEIGHTED POLLING FOR NATIONAL POLLS
######################################################################################################





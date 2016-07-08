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

##########################################################################################
# LIBRARIES AND CUSTOM FUNCTIONS
##########################################################################################
library("sqldf", lib.loc="~/R/win-library/3.2")
library("sqldf", lib.loc="~/R/win-library/3.2")
library("reshape2", lib.loc="~/R/win-library/3.2")
library("pollstR", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("plyr", lib.loc="~/R/win-library/3.2")
library("arm", lib.loc="~/R/win-library/3.2")
library("FNN", lib.loc="~/R/win-library/3.2")
library("fGarch", lib.loc="~/R/win-library/3.2")
library("statebins", lib.loc="~/R/win-library/3.2")
library(rgdal)
library(rgeos)
library(maptools)

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
  from state_election_results ser 
  group by 1
)
")

master_state_ref<-
sql("
select
  temp1.*
  ,temp2.hist_dem_prob
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
###########################
# 2000 Polling
###########################

###########################
# 2012 Polling
###########################
#Use HuffPollster API
polls_2012<-data.frame()
states<-append(state.abb,"DC")
for(i in 1:length(states)){
  print(noquote(paste("CURRENTLY GETTING STATE:",states[i])))
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
###########################
# 2012 Polling
###########################

###########################
# 2016 Polling
###########################
#Use HuffPollster API
polls_2016<-data.frame()
states<-append(state.abb,"DC")
for(i in 1:length(states)){
  print(noquote(paste("CURRENTLY GETTING STATE:",states[i])))
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
  temp4$State<-states[i]
  polls_2016<-rbind(polls_2016,temp4)
}
names(polls_2016)[4]<-'Date'
#Add on days before general election
polls_2016<-data.frame(cbind(polls_2016,difftime(as.Date('2016-11-08'),polls_2016$Date,units="days")))
names(polls_2016)[ncol(polls_2016)]<-'days_till_election'

#Write 2016 polls to csv file
write.csv(polls_2016,'polls\\polls_2016.csv',row.names = FALSE)
###########################
# 2016 Polling
###########################

###########################
# Create data frame of all the polls
###########################
polls<-data.frame(rbind(polls_2000,polls_2004,polls_2008,polls_2012,polls_2016))

polls$days_till_election<-as.numeric(polls$days_till_election)

#Write all polls to a master csv file
write.csv(polls,'polls\\polls_total.csv',row.names = FALSE)
###########################
# Create data frame of all the polls
###########################




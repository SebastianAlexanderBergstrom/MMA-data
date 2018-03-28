library(ggplot2)
library(curl)
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/MMA-data/master/mma_analysis_functions.R")

# Data formating ----------------------------------------------------------
mma_data_fightmetric = read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/MMA-data/master/mma_data_fightmetric.csv"),
                                stringsAsFactors = FALSE)

# First we create a data frame containing only fights in the time period we're interested in.
# What constitutes an appropriate "beginning date" can be discussed. At the moment I chose it 
# so you have approximately the same number of observations before the doping announcement as after and
# so we use data approximately as far ahead in time as back.

mma_data_fightmetric_bounded = subset(mma_data_fightmetric, Date > as.Date("2012-01-18"))
mma_data_fightmetric_bounded$Date = as.Date(mma_data_fightmetric_bounded$Date)
mma_data_fightmetric_bounded$before_or_after_announcement = ifelse(mma_data_fightmetric_bounded$Date < as.Date("2015-02-18"),"Before","After")

# Check that everything has the correct data type
summary(mma_data_fightmetric_bounded)
# Obiously, Sig.Strikes, Takedowns, Sub.Attempts and Guard.Passes should be numeric. We have to convert them.
mma_data_fightmetric_bounded$Sig.Strikes = as.numeric(as.character(mma_data_fightmetric_bounded$Sig.Strikes))
mma_data_fightmetric_bounded$Takedowns = as.numeric(as.character(mma_data_fightmetric_bounded$Takedowns))
mma_data_fightmetric_bounded$Sub.Attempts = as.numeric(as.character(mma_data_fightmetric_bounded$Sub.Attempts))
mma_data_fightmetric_bounded$Guard.Passes = as.numeric(as.character(mma_data_fightmetric_bounded$Guard.Passes))
mma_data_fightmetric_bounded$Round = as.numeric(as.character(mma_data_fightmetric_bounded$Round))

# Metrics for individual fighters -----------------------------------------

metrics_for_individual_fighters(mma_data_fightmetric_bounded,"Sig.Strikes",10)
metrics_for_individual_fighters(mma_data_fightmetric_bounded,"Takedowns",1)
metrics_for_individual_fighters(mma_data_fightmetric_bounded,"Sub.Attempts",1)
metrics_for_individual_fighters(mma_data_fightmetric_bounded,"Guard.Passes",1)

# For "significant strikes" it may be wise to look at density plots as well
ggplot(mma_data_fightmetric_bounded,aes(x=Sig.Strikes,fill=before_or_after_announcement))+
  geom_density(alpha=0.5,position="identity")+
  labs(fill = "Time period", x = "Significant strikes", y = "Kernel density function", title = "Kernel density estimate of significant strikes")

# Visual data analysis of fight durations -----------------------------
mma_data_fightmetric_bounded$fight_duration_minutes = fight_time_minutes(mma_data_fightmetric_bounded$Round,mma_data_fightmetric_bounded$Time)

# Since each fight is recorded twice in mma_data_fightmetric_bounded and we only need one of the results, we create a new
# data frame which only contains every other observation, which will be enough for our purposes.
every_other_row = mma_data_fightmetric_bounded[seq(1,nrow(mma_data_fightmetric_bounded),2),]

ggplot(every_other_row,aes(x=every_other_row$fight_duration_minutes,
                           fill=before_or_after_announcement))+
  geom_density(alpha=0.5,position="identity")+
  labs(fill = "Time period", x = "Fight duration in minutes", y = "Kernel density estimates", title = "Kernel density plots of fight duration")
  # To get an idea of in which part of which round fights end
  #geom_histogram(aes(y=..density..),binwidth=2.5,alpha=0.5,position="identity")+
  #coord_cartesian(xlim = c(0, 25))+
  #labs(fill = "Time period", x = "Fight duration in minutes", y = "Percent", title = "Percentages of fights lasting a given time period")

# Visual data analysis of how fights end -----------------------------------
before = every_other_row[every_other_row$before_or_after_announcement=="Before",]
after = every_other_row[every_other_row$before_or_after_announcement=="After",]
ggplot()+
  geom_bar(data=before,
           aes(x = sapply(before$Method,shorten_method_names),
               y = (..count..)/sum(..count..),
               fill=before$before_or_after_announcement),
           position="dodge",stat="count")+
  geom_bar(data=after,
           aes(x=sapply(after$Method,shorten_method_names),
               y=(..count..)/sum(..count..),
               fill=after$before_or_after_announcement),
           position="dodge",stat="count")+
  labs(fill = "Time period", x = "Fight outcomes", y = "Percent", title = "Frequency of methods")

# Shows differences between different types of KO's, submissions and decisions
outcomes_within_method_type("KO",every_other_row)
outcomes_within_method_type("Submission",every_other_row)
outcomes_within_method_type("Decision",every_other_row)